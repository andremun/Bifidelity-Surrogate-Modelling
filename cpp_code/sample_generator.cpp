#ifndef SAMPLE_GENERATOR_CPP
#define SAMPLE_GENERATOR_CPP


#include "sample_generator.hpp"


SampleGenerator::SampleGenerator(Function* function, int randomSeed, bool printInfo):
	function_(function),
	printInfo_(printInfo){

	if(randomSeed != 0){
		mt19937 gen(randomSeed);
		randomGenerator_ = gen;
	}else{
		random_device rd;
		mt19937 gen(rd());
		randomGenerator_ = gen;
	}

	prePrint_ = "";
}

SampleGenerator::~SampleGenerator(){}



vector<VectorXd> SampleGenerator::randomLHS(int sampleSize){

	vector<VectorXd> sample;
	
	// Need to define an ordering for each dimension
	int d = function_->d_;
	vector< vector<int> > orderings;
	orderings.reserve(d);
	for(int i = 0; i < d; i++){
		vector<int> ordering;
		ordering.reserve(sampleSize);
		for(int j = 0; j < sampleSize; j++){
			ordering.push_back(j);
		}
   		shuffle(ordering.begin(), ordering.end(), randomGenerator_);
   		orderings.push_back(ordering);
	}
	// For each sample, get a random value within allowed range
	for(int i = 0; i < sampleSize; i++){
		VectorXd newSample = VectorXd(d);
		// For each dimension, get the range that is "allowed" and get a random number
		for(int j = 0; j < d; j++){
			int position = orderings[j][i];
			double curr_lower_bound = function_->lowerBound_[j] + position*(function_->upperBound_[j] - function_->lowerBound_[j])/sampleSize;
			double curr_upper_bound = function_->lowerBound_[j] + (position+1)*(function_->upperBound_[j] - function_->lowerBound_[j])/sampleSize;
			newSample(j) = (curr_lower_bound + (curr_upper_bound - curr_lower_bound) * ((double) randomGenerator_() / randomGenerator_.max()));
		}
		sample.push_back(newSample);
		if(printInfo_){printf("\r%sRandomLHS: Working on sample %d/%d", prePrint_.c_str(), (int)sample.size(), sampleSize);}
	}
	if(printInfo_){printf("\r%s                                            \r", prePrint_.c_str());}
	if(printInfo_ && prePrint_ != ""){printf("\r%s RandomLHS sample of size %d completed", prePrint_.c_str(), sampleSize);}
	return sample;
}



double SampleGenerator::morrisMitchellCriterion(vector<VectorXd> &pointSet){
	// Simply find the min distance between the points
	double minDist = DBL_MAX;
	for(int i = 0; i < (int)pointSet.size(); i++){
		for(int j = i+1; j < (int)pointSet.size(); j++){
			if(dist(pointSet[i], pointSet[j]) < minDist){
				minDist = dist(pointSet[i], pointSet[j]);
			}
		}
	}
	return minDist;
}


vector<VectorXd> SampleGenerator::morrisMitchellSubset(vector<VectorXd> &pointSet, int subsetSize){
	// outputLocations(pointSet, "../data/pointSet.txt");
	// First get a random initial subset
	int n = (int)pointSet.size();
	if(n < subsetSize){
		printf("Asked for a subset larger than the size of the original set! Initial set of size %d, wanted a set of size %d. Exiting now...\n", n, subsetSize);
		exit(0);
	}
	vector<int> ordering;
	ordering.reserve(pointSet.size());
	for(int i = 0; i < n; i++){
		ordering.push_back(i);
	}
	shuffle(ordering.begin(), ordering.end(), randomGenerator_);
	// Initial subset is read off from initial indexes in ordering
	vector<VectorXd> pointSubset;
	pointSubset.reserve(subsetSize);
	// Array of bools to keep track of what is in the subset, populate subset
	vector<bool> inSubset(n, false);
	for(int i = 0; i < subsetSize; i++){
		pointSubset.push_back(pointSet[ordering[i]]);
		inSubset[ordering[i]] = true;
	}
	// Calculate current score
	double score = morrisMitchellCriterion(pointSubset);
	// outputLocations(pointSubset, "../data/pointSubsetStart.txt");
	// Iterate through entries and see if a single swap can improve the score
	int timeSinceImproved = 0;
	int currIndex = 0;
	while(timeSinceImproved < subsetSize){
		// Create temporary copy
		vector<VectorXd> pointSubsetCopy = pointSubset;
		// Iterate through every point not being used
		// Swap it, and check if improved
		for(int i = 0; i < n; i++){
			if(printInfo_ && subsetSize > 1){printf("\r%sMorrisMitchell Subset: Time since last improvement %d/%d, current subset index %d/%d, checking set index %d/%d, best so far is %.2f", 
				prePrint_.c_str(), timeSinceImproved, subsetSize, currIndex, subsetSize-1, i, n-1, score);}
			if(inSubset[i]){continue;}
			pointSubsetCopy[currIndex] = pointSet[i];
			double newScore = morrisMitchellCriterion(pointSubsetCopy);
			if(newScore > score){
				// printf("Improvement! From %.2f to %.2f\n", score, newScore);
				score = newScore;
				pointSubset[currIndex] = pointSet[i];
				inSubset[ordering[currIndex]] = false;
				inSubset[i] = true;
				ordering[currIndex] = i;
				timeSinceImproved = 0;
			}
		}
		currIndex = (currIndex + 1) % subsetSize;
		timeSinceImproved++;
	}
	if(printInfo_){
		printf("\r%s                                                                                                                                                                     ", prePrint_.c_str());
		printf("\r%sMorrisMitchell Subset sample of size %d completed", prePrint_.c_str(), subsetSize);
		if(subsetSize > 1){printf(", min distance of %.2f", score);}
		
	}
	// outputLocations(pointSubset, "../data/pointSubsetEnd.txt");
	return pointSubset;
}


void SampleGenerator::updateProblem(Function* function){
	function_ = function;
}










#endif