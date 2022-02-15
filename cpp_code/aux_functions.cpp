#ifndef AUX_FUNCTIONS_CPP
#define AUX_FUNCTIONS_CPP

#include "aux_functions.hpp"


double dist(VectorXd &point1, VectorXd &point2){
	if(point1.size() != point2.size()){
		printf("Comparing distance between two points of different dimensions!\n");
		exit(0);
	}
	VectorXd temp = point1 - point2;
	return temp.norm();
}


double weightedCorrelationCoefficient(vector<double> &dataSet1, vector<double> &dataSet2, vector<double> &weights){
	// First check the lengths are all correct
	if(dataSet1.size() != dataSet2.size() || dataSet1.size() != weights.size()){
		printf("Incorrect usage of weighted correlation, inputed vectors are of different length! Exiting now...\n");
		exit(0);
	}
	int n = (int)dataSet1.size();
	if(n == 1){return 1;}
	// Apply formula!
	double sumWeights = 0.0;
	double nominatorMean1 = 0.0;
	double nominatorMean2 = 0.0;
	for(int i = 0; i < n; i++){
		sumWeights += weights[i];
		nominatorMean1 += weights[i] * dataSet1[i];
		nominatorMean2 += weights[i] * dataSet2[i];
	}
	double mean1 = nominatorMean1 / sumWeights;
	double mean2 = nominatorMean2 / sumWeights;

	double nominatorVar1 = 0.0;
	double nominatorVar2 = 0.0;
	for(int i = 0; i < n; i++){
		nominatorVar1 += weights[i] * pow(dataSet1[i] - mean1, 2);
		nominatorVar2 += weights[i] * pow(dataSet2[i] - mean2, 2);
	}
	double var1 = nominatorVar1 / sumWeights;
	double var2 = nominatorVar2 / sumWeights;
	
	if(abs(var1) < TOL || abs(var2) < TOL){return 0.0;}

	double nominatorCorr = 0.0;
	for(int i = 0; i < n; i++){
		nominatorCorr += weights[i] * (dataSet1[i] - mean1) * (dataSet2[i] - mean2);
	}
	return pow(nominatorCorr / (sumWeights * sqrt(var1) * sqrt(var2)), 2);
}


double relativeRootMeanSquaredError(vector<double> &dataSet1, vector<double> &dataSet2){
	if((int)dataSet1.size() != (int)dataSet2.size()){
		printf("Incorrect usage of RMSE, inputed vectors are of different length! Exiting now...\n");
		exit(0);
	}
	double min = DBL_MIN, max = -DBL_MAX;
	int n = (int)dataSet1.size();
	double total = 0.0;
	for(int i = 0; i < n; i++){
		total += pow(dataSet1[i] - dataSet2[i], 2);
		if(dataSet1[i] < min){min = dataSet1[i];}
		if(dataSet1[i] > max){max = dataSet1[i];}	
	}

	total = total / n;
	total = sqrt(total);
	if(abs(min - max) > TOL){total = total / (max - min);}
	return total;
}

vector<double> calculateFunctionFeatures(BiFidelityFunction* function, int sampleSizeMult, int seed, double r, vector<double> pVals){
	// Work out sample size
	int size = sampleSizeMult * function->d_;
	// Will need a generator
	SampleGenerator* generator = new SampleGenerator(function, seed, false);
	// Get the sample
	vector<VectorXd> sample = generator->randomLHS(size);
	vector<double> highSample = function->evaluateMany(sample);
	vector<double> lowSample = function->evaluateManyLow(sample);
	// Calculate features
	// Define all weights as 1 to get normal correlation coefficient
	vector<double> weightsNormal(size, 1);
	double correlationCoefficient = weightedCorrelationCoefficient(highSample, lowSample, weightsNormal);
	double relativeError = relativeRootMeanSquaredError(highSample, lowSample);
	// Calculate LCC values
	vector<double> localCorrelations = calculateLocalCorrelations(function, sampleSizeMult, seed, r, pVals);
	// Store features
	vector<double> results(2 + (int)localCorrelations.size(), 0.0);
	results[0] = correlationCoefficient;
	results[1] = relativeError;
	for(int i = 0; i < (int)localCorrelations.size(); i++){
		results[2 + i] = localCorrelations[i];
	}
	delete generator;
	return results;
}

vector<double> calculateLocalCorrelations(BiFidelityFunction* function, int sampleSizeMult, int seed, double r, vector<double> pVals){
	int sampleSize = sampleSizeMult * function->d_;
	// Calculate distance for which neighbourhood applies
	double maxDist = 0.0;
	for(int i = 0; i < function->d_; i++){
		maxDist += pow(function->upperBound_[i] - function->lowerBound_[i], 2);
	}
	maxDist = r *sqrt(maxDist);
	// Will need a generator
	SampleGenerator* generator = new SampleGenerator(function, seed, false);
	// Get the sample
	vector<VectorXd> sample = generator->randomLHS(sampleSize);
	vector<double> highSample = function->evaluateMany(sample);
	vector<double> lowSample = function->evaluateManyLow(sample);
	// Cycle through each sample and calculate and store local correlation
	vector<double> localCorrValues(sampleSize, 0.0);
	vector<double> weights;
	vector<double> localHighSample;
	vector<double> localLowSample;
	weights.reserve(sampleSize);
	localHighSample.reserve(sampleSize);
	localLowSample.reserve(sampleSize);
	for(int i = 0; i < sampleSize; i++){
		VectorXd point = sample[i];
		// Cycle through all points, if closer than max dist, add to local vector
		for(int j = 0; j < sampleSize; j++){
			double localDist = dist(point, sample[j]);
			if(localDist > maxDist){continue;}
			localHighSample.push_back(highSample[j]);
			localLowSample.push_back(lowSample[j]);
			weights.push_back(1.0 - localDist / maxDist);
		}
		// Calculate local correlation
		localCorrValues[i] = weightedCorrelationCoefficient(localLowSample, localHighSample, weights);
		// Done! Clear vectors for next iteration
		weights.clear();
		localHighSample.clear();
		localLowSample.clear();
	}
	// Now process values to get features
	vector<double> localCorrs((int)pVals.size(), 0.0);
	// First LCC^r_p values
	// Add to count if larger than or equal to cut off
	for(int i = 0; i < sampleSize; i++){
		for(int j = 0; j < (int)pVals.size(); j++){
			if(localCorrValues[i] >= pVals[j]){localCorrs[j]++;}
		}
	}
	// Divide by total and done
	for(int j = 0; j < (int)pVals.size(); j++){localCorrs[j] = localCorrs[j] / sampleSize;}

	// Now calculate LCC^r_sd and LCC^r_coeff
	double lccMean = 0;
	for(int i = 0; i < sampleSize; i++){
		lccMean += localCorrValues[i];
	}
	lccMean = lccMean / sampleSize;
	double lccSD = 0;
	for(int i = 0; i < sampleSize; i++){
		lccSD += pow(localCorrValues[i] - lccMean, 2);
	}
	lccSD = sqrt(lccSD / (sampleSize - 1));
	localCorrs.push_back(lccSD);
	// Add LCC^r_coeff
	double lccCoeff = lccSD;
	if(lccMean > TOL){lccCoeff = lccCoeff / lccMean;}
	localCorrs.push_back(lccCoeff);

	return localCorrs;
}

void printPoint(VectorXd point){
	printf("(");
	for(int i = 0; i < point.size(); i++){
		printf("%.2f",point(i));
		if(i < point.size()-1){printf(", ");}
		else{printf(")");}
	}
}

#endif