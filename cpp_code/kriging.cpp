#ifndef KRIGING_CPP
#define KRIGING_CPP


#include "kriging.hpp"


Kriging::Kriging(Function* ebbFunction, ARSsolver* auxSolver, int sampleBudget, int randomSeed, bool printInfo):
	ebbFunction_(ebbFunction),
	auxSolver_(auxSolver),
	sampleBudget_(sampleBudget),
	randomSeed_(randomSeed),
	printInfo_(printInfo){

	if(randomSeed != 0){
		mt19937 gen(randomSeed);
		randomGenerator_ = gen;
	}else{
		random_device rd;
		mt19937 gen(rd());
		randomGenerator_ = gen;
	}
	auxSolver_->reseedRandom(randomSeed_);
	sampleGenerator_ = new SampleGenerator(ebbFunction_, randomSeed_, printInfo_);

	theta_.reserve(ebbFunction_->d_);
	pVector_.reserve(ebbFunction_->d_);

	trainedModel_ = false;
}

Kriging::~Kriging(){}

void Kriging::createSurrogateModel(){
	generateSample();
	if((int)sampledPoints_.size() == 0){return;}
	trainModel();
	trainedModel_ = true;
}

void Kriging::generateSample(){
	if(sampleBudget_ <= 0){
		// Not enough to train model, do nothing
		return;
	}
	if(printInfo_){
		printf("Generate initial data sample: ");
		sampleGenerator_->prePrint_ = "Generate data sample: ";
	}
	sampledPoints_ = sampleGenerator_->randomLHS(sampleBudget_);
	sampledPointsValues_ = ebbFunction_->evaluateMany(sampledPoints_);
	if(printInfo_){printf("\n");}
	return;
}

void Kriging::trainModel(){
	trainHyperparameters();
	saveMuSigma();	
	return;
}

void Kriging::trainHyperparameters(){
	int d = ebbFunction_->d_;
	// First define the bounds for the function
	vector<double> lowerBound(2*d, 0.0);
	vector<double> upperBound(2*d, 0.0);
	for(int i = 0; i < d; i++){
		lowerBound[i] = -10;
		upperBound[i] = 3;
		lowerBound[d+i] = 1.0;
		upperBound[d+i] = 2.0;
	}
	ConcentratedLikelihoodFunction* function = new ConcentratedLikelihoodFunction(2*d, lowerBound, upperBound, this);
	auxSolver_->updateProblem(function, false);
	VectorXd hyperparameters = auxSolver_->optimise();
	delete function;
	// Extract two vectors, and store!
	for(int i = 0; i < d; i++){
		theta_[i] = pow(10,hyperparameters(i));
		// theta_[i] = hyperparameters(i);
		pVector_[i] = hyperparameters(d + i);
	}
}

void Kriging::saveMuSigma(){
	auto data = muSigmaCalculator();
	mu_ = get<0>(data);
	sigma_ = get<1>(data);
	rMatrix_ = get<2>(data);
	rMatrixDecomposition_ = get<3>(data);
	return;
}


tuple<double, double, MatrixXd, LDLT<MatrixXd>> Kriging::muSigmaCalculator(){
	int n = (int)sampledPoints_.size();
	int d = (int)sampledPoints_[0].size();

	if(n < 1){
		printf("Trying to train Kriging model with less points than needed! Have %d but need %d. Exiting now...\n", n, 1);
		exit(0);
	}
	MatrixXd rMatrix(n,n);
	// Define matrix
	auto it1 = sampledPoints_.begin();
	for(int i = 0; i < n; i++, it1++){
		auto it2 = it1;
		for(int j = i; j < n; j++, it2++){
			double sum = 0;
			for(int k = 0; k < d; k++){
				sum += theta_[k] * pow(abs((*it1)(k) - (*it2)(k)), pVector_[k]);
			}
			rMatrix(i,j) = exp(-sum);
			rMatrix(j,i) = rMatrix(i,j);	
		}
	}
	VectorXd one(n);
	VectorXd y(n);
	for(int i = 0; i < n; i++){
		one(i) = 1;
		y(i) = sampledPointsValues_[i];
	}
	// Save matrix decomposition
	LDLT<MatrixXd> rMatrixDecomposition = rMatrix.ldlt();
	MatrixXd mu_top = one.transpose() * rMatrixDecomposition.solve(y);
	MatrixXd mu_bottom = one.transpose() * rMatrixDecomposition.solve(one);
	double mu = mu_top(0,0)/ mu_bottom(0,0);
	MatrixXd sigma_m = (y - one*mu).transpose() * rMatrixDecomposition.solve(y - one*mu);
	double sigma = sigma_m(0,0)/n;
	// Deal with special case when n = 1
	if(n == 1){
		mu = y(0);
		sigma = 0;
	}
	// return make_tuple(mu, sigma, rMatrix, rMatrixInverse, rMatrixDecomposition);
	return make_tuple(mu, sigma, rMatrix, rMatrixDecomposition);
}

double Kriging::surfaceValue(VectorXd &x){
	auto data = meanVarianceCalculator(x);
	double s_x = get<0>(data);
	return s_x;
}

double Kriging::uncertainty(VectorXd &x){
	auto data = meanVarianceCalculator(x);
	double var = get<1>(data);
	return var;
}

vector<double> Kriging::multipleSurfaceValues(vector<VectorXd> &points){
	vector<double> values((int)points.size(), 0.0);
	for(int i = 0; i < (int)points.size(); i++){
		values[i] = surfaceValue(points[i]);
	}
	return values;
}

tuple<double, double> Kriging::meanVarianceCalculator(VectorXd &x){
	int n = (int)sampledPoints_.size();
	int d = (int)sampledPoints_[0].size();
	// First want r vector
	VectorXd r(n);
	auto it = sampledPoints_.begin();
	for(int i = 0; i < n; i++, it++){
		double sum = 0;
		for(int k = 0; k < d; k++){
			sum += theta_[k] * pow(abs((*it)(k) - x(k)), pVector_[k]);
		}
		r(i) = exp(-sum);
	}
	VectorXd one(n);
	VectorXd y(n);
	for(int i = 0; i < n; i++){
		one(i) = 1;
		y(i) = sampledPointsValues_[i];
	}
	VectorXd rightHandSide = r.transpose() * rMatrixDecomposition_.solve(y - one*mu_);
	double s_x = mu_ + rightHandSide(0);
	if(n == 1){s_x = mu_;}
	MatrixXd var_mid = r.transpose() * rMatrixDecomposition_.solve(r);
	MatrixXd var_top_right = one.transpose() * rMatrixDecomposition_.solve(r);
	MatrixXd var_bottom_right = one.transpose() * rMatrixDecomposition_.solve(one);
	double variance = sigma_ * (1 - var_mid(0,0) + (1 - var_top_right(0,0))*(1 - var_top_right(0,0))/var_bottom_right(0,0));
	return make_tuple(s_x, variance);
}

double Kriging::concentratedLikelihoodFunction(){
	// Get info needed, data contains mu, sigma, R and R^-1 in that order
	auto data = muSigmaCalculator();
	int n = (int)sampledPoints_.size();
	double logLikelihood = -n * logl(get<1>(data))/2 - logl((get<3>(data)).matrixL().determinant()) - logl((get<3>(data)).vectorD().prod())/2;
	// If cannot calculate the second half, say it is a bad answer! Do so to not use hyperparamters which lead to errors in calculation
	if(isinf(-n * logl(get<1>(data))/2 - logl((get<3>(data)).matrixL().determinant()) - logl((get<3>(data)).vectorD().prod())/2)){
		return -DBL_MAX;
	}
	return logLikelihood;
}

Kriging::ConcentratedLikelihoodFunction::ConcentratedLikelihoodFunction(int d, vector<double> &lowerBound, vector<double> &upperBound, Kriging* krigingModel):
	Function(d, lowerBound, upperBound),
	krigingModel_(krigingModel){}

Kriging::ConcentratedLikelihoodFunction::~ConcentratedLikelihoodFunction(){}

double Kriging::ConcentratedLikelihoodFunction::evaluate(VectorXd &point){
	// First half of the entries of point contain the d theta variables, and the second half the p values
	// Should find out dimension and size, then extract theta and p values
	int d = (int)krigingModel_->sampledPoints_[0].size();
	vector<double> theta(d,0.0);
	vector<double> pVector(d,0.0);
	for(int i = 0; i < d; i++){
		theta[i] = pow(10,point(i));
		// theta[i] = point(i);
		pVector[i] = point(d + i);
	}
	krigingModel_->theta_ = theta;
	krigingModel_->pVector_ = pVector;
	// Get info needed, data contains mu, sigma, R and R^-1 in that order
	return krigingModel_->concentratedLikelihoodFunction();
}







CoKriging::CoKriging(BiFidelityFunction* biFunction, ARSsolver* auxSolver, int highFiSampleBudget, int lowFiSampleBudget, int randomSeed, bool printInfo):
	Kriging(biFunction, auxSolver, 0, randomSeed, printInfo), 
	biFunction_(biFunction),
	highFiSampleBudget_(highFiSampleBudget),
	lowFiSampleBudget_(lowFiSampleBudget){

	lowFiKriging_ = new Kriging(biFunction_, auxSolver_, lowFiSampleBudget_, randomSeed_, printInfo_);

	thetaB_.reserve(ebbFunction_->d_);
	pBVector_.reserve(ebbFunction_->d_);

}

CoKriging::~CoKriging(){}

void CoKriging::generateSample(){

	if(highFiSampleBudget_ <= 0 || lowFiSampleBudget_ <= 0){
		printf("Initial sample budgets specified are non positive! Stopping now...\n");
		return;
	}else if(highFiSampleBudget_ > lowFiSampleBudget_ ){
		printf("Specified more high fidelity samples than low fidelity samples, want to choose a subset! Stoppinf now...\n");
		return;
	}
	if(printInfo_){
		printf("Generate low fi data sample: ");
		sampleGenerator_->prePrint_ = "Generate low fi data sample: ";
	}

	lowFiKriging_->sampledPoints_ = sampleGenerator_->randomLHS(lowFiSampleBudget_);
	lowFiKriging_->sampledPointsValues_ = biFunction_->evaluateManyLow(lowFiKriging_->sampledPoints_);
	// Now get the high fidelity sample.
	if(printInfo_){
		printf("\nGenerate high fi data sample: ");
		sampleGenerator_->prePrint_ = "Generate high fi data sample: ";
	}
	sampledPoints_ = sampleGenerator_->morrisMitchellSubset(lowFiKriging_->sampledPoints_, highFiSampleBudget_);
	sampledPointsValues_ = biFunction_->evaluateMany(sampledPoints_);
	if(printInfo_){printf("\n");}
}

void CoKriging::trainModel(){
	lowFiKriging_->trainModel();
	trainHyperparameters();
	saveMuSigma();	
	return;
}

void CoKriging::trainHyperparameters(){
	int d = biFunction_->d_;
	// First define the bounds for the function
	vector<double> lowerBound(2*d + 1, 0.0);
	vector<double> upperBound(2*d + 1, 0.0);
	for(int i = 0; i < d; i++){
		// Bounds for theta_b
		lowerBound[i] = -10;
		upperBound[i] = 3;
		// Bounds for p_b
		lowerBound[d+i] = 1.0;
		upperBound[d+i] = 2.0;
	}
	// Bounds for rho
	lowerBound[2*d] = -100;
	upperBound[2*d] = 100;

	IntermediateConcentratedLikelihoodFunction* function = new IntermediateConcentratedLikelihoodFunction(2*d + 1, lowerBound, upperBound, this);
	auxSolver_->updateProblem(function, false);
	VectorXd hyperparameters = auxSolver_->optimise();
	delete function;
	// Extract two vectors, and store!
	for(int i = 0; i < d; i++){
		thetaB_[i] = pow(10,hyperparameters(i));
		pBVector_[i] = hyperparameters(d + i);
	}
	// Last value is rho
	rho_ = hyperparameters(2*d);
}

void CoKriging::saveMuSigma(){
	auto data = intermediateMuSigmaCalculator();
	sigmaL_ = lowFiKriging_->sigma_;
	sigmaB_ = get<1>(data);
	auto data1 = combinedMuCmatrixCalculator();
	muCombined_ = get<0>(data1);
	cMatrix_ = get<1>(data1);
	cMatrixDecomposition_ = get<2>(data1);
	return;
}

tuple<double, double, MatrixXd, LDLT<MatrixXd>> CoKriging::intermediateMuSigmaCalculator(){	
	int n = (int)sampledPoints_.size();
	int d = (int)sampledPoints_[0].size();
	if(n < 1){
		printf("Trying to train CoKriging model with less points than needed! Have %d but need %d. Exiting now...\n", n, 1);
		exit(0);
	}

	MatrixXd bExpensiveMatrix(n,n);
	// First of all need to define matrix
	auto it1 = sampledPoints_.begin();
	for(int i = 0; i < n; i++, it1++){
		auto it2 = it1;
		for(int j = i; j < n; j++, it2++){
			double sum = 0;
			for(int k = 0; k < d; k++){
				sum += thetaB_[k] * pow(abs((*it1)(k) - (*it2)(k)), pBVector_[k]);
			}
			bExpensiveMatrix(i,j) = exp(-sum);
			bExpensiveMatrix(j,i) = bExpensiveMatrix(i,j);	
		}
	}
	VectorXd one(n);
	VectorXd b(n);
	for(int i = 0; i < n; i++){
		one(i) = 1;
		b(i) = sampledPointsValues_[i] - rho_ * lowFiKriging_->surfaceValue(sampledPoints_[i]);
	}
	LDLT<MatrixXd> bExpensiveMatrixDecomposition = bExpensiveMatrix.ldlt();
	MatrixXd mu_top = one.transpose() * bExpensiveMatrixDecomposition.solve(b);
	MatrixXd mu_bottom = one.transpose() * bExpensiveMatrixDecomposition.solve(one);
	double mu = mu_top(0,0)/ mu_bottom(0,0);
	MatrixXd sigma_m = (b - one*mu).transpose() * bExpensiveMatrixDecomposition.solve(b - one*mu);
	double sigma = sigma_m(0,0)/n;

	return make_tuple(mu, sigma, bExpensiveMatrix, bExpensiveMatrixDecomposition);
}

tuple<double, MatrixXd, LDLT<MatrixXd>> CoKriging::combinedMuCmatrixCalculator(){

	int nL = (int)lowFiKriging_->sampledPoints_.size();
	int nH = (int)sampledPoints_.size();
	int d = (int)sampledPoints_[0].size();

	MatrixXd cMatrix(nH + nL, nH + nL);
	// Start with Rl(Xl, Xl)
	auto it1 = lowFiKriging_->sampledPoints_.begin();
	for(int i = 0; i < nL; i++, it1++){
		auto it2 = it1;
		for(int j = i; j < nL; j++, it2++){
			double sum = 0;
			for(int k = 0; k < d; k++){
				sum += lowFiKriging_->theta_[k] * pow(abs((*it1)(k) - (*it2)(k)), lowFiKriging_->pVector_[k]);
			}
			sum = exp(-sum);
			sum = sum * sigmaL_;
			cMatrix(i,j) = sum;
			cMatrix(j,i) = cMatrix(i,j);	
		}
	}
	// Now go with Rl(Xl, Xh) and Rl(Xh, Xl)
	it1 = lowFiKriging_->sampledPoints_.begin();
	for(int i = 0; i < nL; i++, it1++){
		auto it2 = sampledPoints_.begin();
		for(int j = 0; j < nH; j++, it2++){
			double sum = 0;
			for(int k = 0; k < d; k++){
				sum += lowFiKriging_->theta_[k] * pow(abs((*it1)(k) - (*it2)(k)), lowFiKriging_->pVector_[k]);
			}
			sum = exp(-sum);
			sum = sum * rho_ * sigmaL_;
			cMatrix(i, nL + j) = sum;
			cMatrix(nL + j, i) = sum;
		}
	}

	// Now go with Rl(Xh, Xh) + Rb(Xh, Xh)
	it1 = sampledPoints_.begin();
	for(int i = 0; i < nH; i++, it1++){
		auto it2 = it1;
		for(int j = i; j < nH; j++, it2++){
			double sumL = 0;
			double sumB = 0;
			for(int k = 0; k < d; k++){
				sumL += lowFiKriging_->theta_[k] * pow(abs((*it1)(k) - (*it2)(k)), lowFiKriging_->pVector_[k]);
				sumB += thetaB_[k] * pow(abs((*it1)(k) - (*it2)(k)), pBVector_[k]);
			}
			sumL = exp(-sumL);
			sumB = exp(-sumB);
			double total = rho_ * rho_ * sigmaL_ * sumL + sigmaB_ * sumB;
			cMatrix(nL + i, nL + j) = total;
			cMatrix(nL + j, nL + i) = total;
		}
	}

	VectorXd one(nL + nH);
	VectorXd y(nL + nH);
	for(int i = 0; i < nL; i++){
		one(i) = 1;
		y(i) = lowFiKriging_->sampledPointsValues_[i];
	}
	for(int i = 0; i < nH; i++){
		one(nL + i) = 1;
		y(nL + i) = sampledPointsValues_[i];
	}

	LDLT<MatrixXd> cMatrixDecomposition = cMatrix.ldlt();
	MatrixXd mu_top = one.transpose() * cMatrixDecomposition.solve(y);
	MatrixXd mu_bottom = one.transpose() * cMatrixDecomposition.solve(one);
	double mu = mu_top(0,0)/ mu_bottom(0,0);
	return make_tuple(mu, cMatrix, cMatrixDecomposition);
}

double CoKriging::intermediateConcentratedLikelihoodFunction(){
	auto data = intermediateMuSigmaCalculator();
	int n = (int)sampledPoints_.size();
	double logLikelihood = -n * logl(get<1>(data))/2 - logl((get<3>(data)).matrixL().determinant()) - logl((get<3>(data)).vectorD().prod())/2;

	if(isinf(logLikelihood)){return -DBL_MAX;}
	return logLikelihood;
}

tuple<double, double> CoKriging::meanVarianceCalculator(VectorXd &x){
	int nL = (int)lowFiKriging_->sampledPoints_.size();
	int nH = (int)sampledPoints_.size();
	int d = (int)sampledPoints_[0].size();
	// First want c vector
	VectorXd c(nL + nH);
	// First deal with Rl(Xl, x)
	auto it = lowFiKriging_->sampledPoints_.begin();
	for(int i = 0; i < nL; i++, it++){
		double sum = 0;
		for(int k = 0; k < d; k++){
			sum += lowFiKriging_->theta_[k] * pow(abs((*it)(k) - x(k)), lowFiKriging_->pVector_[k]);
		}
		c(i) = rho_ * sigmaL_ * exp(-sum);
	}
	// Deal with Rl(Xh, x) + Rb(Xh, x)
	it = sampledPoints_.begin();
	for(int i = 0; i < nH; i++, it++){
		double sumC = 0;
		double sumB = 0;
		for(int k = 0; k < d; k++){
			sumC += lowFiKriging_->theta_[k] * pow(abs((*it)(k) - x(k)), lowFiKriging_->pVector_[k]);
			sumB += thetaB_[k] * pow(abs((*it)(k) - x(k)), pBVector_[k]);
		}
		c(nL + i) = rho_ * rho_ * sigmaL_ * exp(-sumC) + sigmaB_ * exp(-sumB);
	}

	VectorXd one(nL + nH);
	VectorXd y(nL + nH);
	for(int i = 0; i < nL; i++){
		one(i) = 1;
		y(i) = lowFiKriging_->sampledPointsValues_[i];
	}
	for(int i = 0; i < nH; i++){
		one(nL + i) = 1;
		y(nL + i) = sampledPointsValues_[i];
	}

	
	// New way
	VectorXd rightHandSide = c.transpose() * cMatrixDecomposition_.solve(y - one * muCombined_);
	double s_x = muCombined_ + rightHandSide(0);
	rightHandSide = c.transpose() * cMatrixDecomposition_.solve(c);
	double variance = rho_ * rho_ * sigmaL_ + sigmaB_ - rightHandSide(0);
	
	return make_tuple(s_x, variance);
}

CoKriging::IntermediateConcentratedLikelihoodFunction::IntermediateConcentratedLikelihoodFunction(int d, vector<double> &lowerBound, vector<double> &upperBound, CoKriging* cokrigingModel):
	Function(d, lowerBound, upperBound),
	cokrigingModel_(cokrigingModel){}

CoKriging::IntermediateConcentratedLikelihoodFunction::~IntermediateConcentratedLikelihoodFunction(){}

double CoKriging::IntermediateConcentratedLikelihoodFunction::evaluate(VectorXd &point){
	// Ok so here idea is that the first half of the entries of point contain the d theta variables, and the second half the p values
	// Should find out dimension and size, then extract theta and p values
	int d = (int)cokrigingModel_->sampledPoints_[0].size();
	vector<double> theta(d,0.0);
	vector<double> pVector(d,0.0);
	for(int i = 0; i < d; i++){
		theta[i] = pow(10,point(i));
		// theta[i] = point(i);
		pVector[i] = point(d + i);
	}
	cokrigingModel_->thetaB_ = theta;
	cokrigingModel_->pBVector_ = pVector;
	cokrigingModel_->rho_ = point(2*d);
	// Get info needed, data contains mu, sigma, R and R^-1 in that order
	return cokrigingModel_->intermediateConcentratedLikelihoodFunction();
}

#endif