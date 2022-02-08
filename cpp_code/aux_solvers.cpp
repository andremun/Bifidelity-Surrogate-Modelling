#ifndef AUX_SOLVERS_CPP
#define AUX_SOLVERS_CPP

#include "aux_solvers.hpp"

ARSsolver::ARSsolver(Function* function, bool min, int randomSeed, bool printInfo) :
	function_(function),
	min_(min),
	randomSeed_(randomSeed),
	printInfo_(printInfo){
	if(min){multiplier_ = 1;}
	else{multiplier_ = -1;}

	if(randomSeed_ != 0){
		mt19937 gen(randomSeed);
		randomGenerator_ = gen;
	}else{
		random_device rd;
		mt19937 gen(rd());
		randomGenerator_ = gen;
	}
	numSearch_ = 10;
	maxIter_ = 1000;
	sampleGenerator_ = new SampleGenerator(function_, randomSeed_);
	prePrint_ = "";
}

ARSsolver::ARSsolver(Function* function, int numSearch, int maxIter, bool min, int randomSeed, bool printInfo):
	ARSsolver(function, min, randomSeed, printInfo){
		numSearch_ = numSearch;
		maxIter_ = maxIter;
	}

ARSsolver::~ARSsolver(){
	delete sampleGenerator_;
}

void ARSsolver::reseedRandom(int newSeed){
	randomSeed_ = newSeed;
	if(randomSeed_ != 0){
		mt19937 gen(randomSeed_);
		randomGenerator_ = gen;
	}else{
		random_device rd;
		mt19937 gen(rd());
		randomGenerator_ = gen;
	}
	delete sampleGenerator_;
	sampleGenerator_ = new SampleGenerator(function_, randomSeed_);
}

void ARSsolver::updateProblem(Function* function, bool min){
	function_ = function;
	min_ = min;
	if(min){multiplier_ = 1;}
	else{multiplier_ = -1;}
	// Adding logic for the starting generator seed to change as well
	// Only do so if it is not 0
	if(randomSeed_){randomSeed_++;}
	// Also need to update generator
	sampleGenerator_->updateProblem(function);
}

VectorXd ARSsolver::optimise(){
	int numSphere = numSearch_;
	int maxIter = maxIter_;
	vector< double> radii(numSphere, 1.0);
	int d = function_->d_;
	vector<VectorXd> centers = sampleGenerator_->randomLHS(numSphere);
	// Store best point so far
	double f_min = multiplier_ * function_->evaluate(centers[0]);
	VectorXd bestPoint = centers[0];
	for(int i = 1; i < numSphere; i++){
		if(f_min > multiplier_ * function_->evaluate(centers[i])){
			bestPoint = centers[i];
			f_min = multiplier_ * function_->evaluate(centers[i]);
		}
	}

	int iter = 0;
	if(printInfo_){
		printf("\r%sRunning ARS, iteration %d/%d, best point has value %.4f at point (", prePrint_.c_str(), iter, maxIter, multiplier_ * f_min);
		for(int i = 0; i < d; i++){
			printf("%.4f", bestPoint[i]);
			if(i < d-1){printf(", ");}
			else{printf(")");}
		}
	}
	// For each iteration, deal with each sphere
	while(iter < maxIter){

		for(int i = 0; i < numSphere; i++){
			// Get a random point in the d-sphere centered at the point with radius r
			// Need to adapt it to the actual region, and check it is within it
			VectorXd testPoint;
			do{
				testPoint = dBallRandomSample(d);
				for(int j = 0; j < d; j++){
					testPoint(j) = testPoint(j)*radii[i]*(function_->upperBound_[j] - function_->lowerBound_[j]) + centers[i](j);
				}
			}while(!function_->pointWithinBounds(testPoint));

			if(multiplier_ * function_->evaluate(centers[i]) > multiplier_ * function_->evaluate(testPoint)){
				centers[i] = testPoint;
				if(multiplier_ * function_->evaluate(centers[i]) < f_min && abs(multiplier_ * function_->evaluate(centers[i]) - f_min) > TOL){
					bestPoint = centers[i];
					f_min = multiplier_ * function_->evaluate(centers[i]);
				}
				radii[i] = 1;
			
			}else{
				radii[i] = radii[i] / 2;
				if(radii[i] < TOL){radii[i] = 1;}
			}
		}
		iter++;
		if(printInfo_){
			printf("\r%sRunning ARS, iteration %d/%d, best point has value %.4f at point (", prePrint_.c_str(), iter, maxIter, multiplier_ * f_min);
			for(int i = 0; i < d; i++){
				printf("%.4f", bestPoint[i]);
				if(i < d-1){printf(", ");}
				else{printf(")                         ");}
			}
		}
	}
	// Clear print info, if using it with conjunction with something else, otherwise just end with new line
	if(printInfo_){
		if(prePrint_ == ""){printf("\n");}
		else{
			printf("\r%s                                                                                  ", prePrint_.c_str());
			for(int i = 0; i < d; i++){printf("       ");}
		}
	}
	return bestPoint;
}

VectorXd ARSsolver::dBallRandomSample(int d){
	// First define randoms
	normal_distribution<double> normalDis(0, 1);
    // Define direction
    VectorXd direction = VectorXd(d);
    for(int i = 0; i < d; i++){
    	direction(i) = normalDis(randomGenerator_);
    }
    double norm = direction.norm();
    // Define radius
    double radius = pow((double) randomGenerator_() / randomGenerator_.max(), 1.0/d);
    // Combine into a point
    for(int i = 0; i < d; i++){
    	double final_value = radius * direction(i)/norm;
    	direction(i) = final_value;	
    }
    return direction;
}

#endif