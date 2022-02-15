#ifndef INPUT_OUTPUT_CPP
#define INPUT_OUTPUT_CPP

#include "input_output.hpp"



BiFidelityFunction* processFunctionName(string name){
	if(name.compare("LiuPedagogical") == 0){
		return new LiuPedagogicalFunction();
	
	}else if(name.compare("ShiGramacyLee") == 0){
		return new ShiGramacyLeeFunction();

	}else if(name.compare("ShiCurrinSin") == 0){
		return new ShiCurrinSinFunction();

	}else if(name.compare("ShiHolsclaw") == 0){
		return new ShiHolsclawFunction();

	}else if(name.compare("ShiSantner") == 0){
		return new ShiSantnerFunction();

	}else if(name.compare("LiuBranin") == 0){
		return new LiuBraninFunction();

	}else if(name.compare("ShiBranin") == 0){
		return new ShiBraninFunction();

	}else if(name.compare("ShiNumberSix") == 0){
		return new ShiNumberSixFunction();

	}else if(name.compare("ShiNumberSeven") == 0){
		return new ShiNumberSevenFunction();

	}else if(name.compare("ShiBeale") == 0){
		return new ShiBealeFunction();

	}else if(name.compare("ShiStyblinskiTang") == 0){
		return new ShiStyblinskiTangFunction();

	}else if(name.compare("ShiCurrinExp") == 0){
		return new ShiCurrinExpFunction();

	}else if(name.compare("ShiLim") == 0){
		return new ShiLimFunction();

	}else if(name.compare("ShiGramacy") == 0){
		return new ShiGramacyFunction();

	}else if(name.compare("DongBohachevsky") == 0){
		return new DongBohachevskyFunction();

	}else if(name.compare("DongBooth") == 0){
		return new DongBoothFunction();

	}else if(name.compare("DongBranin") == 0){
		return new DongBraninFunction();

	}else if(name.compare("DongHimmelblau") == 0){
		return new DongHimmelblauFunction();

	}else if(name.compare("DongSixHumpCamelback") == 0){
		return new DongSixHumpCamelbackFunction();

	}else if(name.compare("XiongCurrinExp") == 0){
		return new XiongCurrinExpFunction();

	}else if(name.compare("MarchWillcoxRosenbrock1") == 0){
		return new MarchWillcoxRosenbrockFunction(1);

	}else if(name.compare("MarchWillcoxRosenbrock2") == 0){
		return new MarchWillcoxRosenbrockFunction(2);

	}else if(name.compare("MarchWillcoxRosenbrock3") == 0){
		return new MarchWillcoxRosenbrockFunction(3);

	}else if(name.compare("MarchWillcoxRosenbrock4") == 0){
		return new MarchWillcoxRosenbrockFunction(4);

	}else if(name.compare("MarchWillcoxRosenbrock5") == 0){
		return new MarchWillcoxRosenbrockFunction(5);

	}else if(name.compare("RajnarayanHartmannH3") == 0){
		return new RajnarayanHartmannH3Function();

	}else if(name.compare("ShiHartmannH3") == 0){
		return new ShiHartmannH3Function();

	}else if(name.compare("ShiDettePepelyshevExp") == 0){
		return new ShiDettePepelyshevExpFunction();

	}else if(name.compare("RajnarayanWoods") == 0){
		return new RajnarayanWoodsFunction();

	}else if(name.compare("ShiHartmannH4") == 0){
		return new ShiHartmannH4Function();

	}else if(name.compare("ShiPark") == 0){
		return new ShiParkFunction();

	}else if(name.compare("XiongParkFirst") == 0){
		return new XiongParkFirstFunction();

	}else if(name.compare("XiongParkSecond") == 0){
		return new XiongParkSecondFunction();

	}else if(name.compare("LiuStyblinskiTang") == 0){
		return new LiuStyblinskiTangFunction();

	}else if(name.compare("RajnarayanHartmannH6") == 0){
		return new RajnarayanHartmannH6Function();

	}else if(name.compare("ShiHartmannH6") == 0){
		return new ShiHartmannH6Function();

	}else if(name.compare("ShiRosenbrock") == 0){
		return new ShiRosenbrockFunction();

	}else if(name.compare("ParkHartmannH6") == 0){
		return new ParkHartmannH6Function();

	}else if(name.compare("ShiDettePepelyshev") == 0){
		return new ShiDettePepelyshevFunction();

	}else if(name.compare("LiuAckley10") == 0){
		return new LiuAckley10Function();

	}else if(name.compare("LiuEllipsoid") == 0){
		return new LiuEllipsoidFunction();

	}else if(name.compare("LiuDixonPrice") == 0){
		return new LiuDixonPriceFunction();

	}else if(name.compare("LiuAckley20") == 0){
		return new LiuAckley20Function();

	}else if(name.compare(0, 10, "ToalBranin") == 0){
		double a = stof(name.substr(10, 4));
		return new ToalBraninFunction(a);

	}else if(name.compare(0, 16, "SongToalForretal") == 0){
		double a = stof(name.substr(16, 4));
		return new SongToalForretalFunction(a);

	}else if(name.compare(0, 12, "ToalPaciorek") == 0){
		double a = stof(name.substr(12, 4));
		return new ToalPaciorekFunction(a);

	}else if(name.compare(0, 14, "SongToalBranin") == 0){
		double a = stof(name.substr(14, 4));
		return new SongToalBraninFunction(a);

	}else if(name.compare(0, 14, "ToalHartmannH3") == 0){
		double a = stof(name.substr(14, 4));
		return new ToalHartmannH3Function(a);

	}else if(name.compare(0, 16, "SongToalColville") == 0){
		double a = stof(name.substr(16, 4));
		return new SongToalColvilleFunction(a);

	}else if(name.compare(0, 8, "ToalTrid") == 0){
		double a = stof(name.substr(8, 4));
		return new ToalTridFunction(a);

	}else if(name.compare(0, 13, "WangRastrigin") == 0){
		stringstream ss(name);
		string line;
		// skip name
		getline(ss, line, '-');
		// get error line
		getline(ss, line, '-');
		int dim = stoi(line.substr(1));
		getline(ss, line, '-');
		int error = stoi(line.substr(5));
		getline(ss, line, '-');
		double phi = stof(line.substr(3));
		return new WangRastriginFunction(dim, error, phi);
	
	}else if(name.compare(0, 12, "COCOfunction") == 0){
		return processCOCOFunctionName(name);
	}
						
	return NULL;
}


COCOBiFunction* processCOCOFunctionName(string name){
	// Here should have a lot of information to deal with this in different ways, but for now just deal with the case I am interested in
	// Really, should first specify global noise, followed by parameters pertaining to global noise
	// Then local noise, followed by parameters pertaining to local noise
	// So let's do that
	stringstream ss(name);
	string line;
	// Get function number
	getline(ss, line, '-');
	int func = stoi(line.substr(12));
	// Get dimension
	getline(ss, line, '-');
	int dim = stoi(line.substr(3));
	// Get seed
	getline(ss, line, '-');
	int seed = stoi(line.substr(4));
	// By this point, can initialsie the function
	COCOBiFunction* function = new COCOBiFunction(func, dim, seed);
	// Get disturbance type
	getline(ss, line, '-');
	function->disturbanceType_ = line[4];
	function->disturbanceNum_ = stoi(line.substr(5,1));
	if(function->disturbanceType_ == 'h'){
		// Get height
		getline(ss, line, '-');
		function->disturbanceHeight_ = stof(line.substr(6));
		// Get radius
		getline(ss, line, '-');
		function->disturbanceRadius_ = stof(line.substr(6));

	}else if(function->disturbanceType_ == 's'){
		// Get number of sources and initialise points
		getline(ss, line, '-');
		int centres = stoi(line.substr(7));
		SampleGenerator* generator = new SampleGenerator(function, seed, false);
		function->disturbanceCentres_ = generator->randomLHS(centres);
		delete generator;		
		// Get radius
		getline(ss, line, '-');
		function->disturbanceRadius_ = stof(line.substr(6));	
	}else{
		printf("Disturbance type %c not yet implemented, for now only have height (h) and source (s) based!\n", function->disturbanceType_);
		return NULL;
	}
	// Get basic disturbance parameters, frequency and amplitude
	getline(ss, line, '-');
	function->basicDisturbanceFrequency_ = stoi(line.substr(4));
	getline(ss, line, '-');
	function->basicDisturbanceAmplitude_ = stof(line.substr(3));
	// These disturbances need to know the range of the function.
	// Minimum is chosen, need to find maximum. Approximation of range is still valid.
	ARSsolver* auxSolver = new ARSsolver(function, 10, 5000, false, seed, false);
	VectorXd best = auxSolver->optimise();
	function->fMax_ = function->evaluate(best);

	return function;

}


void processExperiment(string outputFilename, string instructionLine, double r, vector<double> pVals){
	// instructionLine should contain all the desired information, let's extract it
	stringstream ss(instructionLine);
	string token;
	getline(ss, token, ' ');
	string functionName = token;
	getline(ss, token, ' ');
	string technique = token;
	getline(ss, token, ' ');
	int highFiBudget = stoi(token);
	getline(ss, token, ' ');
	int lowFiBudget = stoi(token);
	getline(ss, token, ' ');
	// Last special thing, this should be in the form of seedStart-seedEnd
	stringstream ss2(token);
	getline(ss2, token, '-');
	int seedStart = stoi(token);
	getline(ss2, token, '-');
	int seedEnd = stoi(token);
	printf("Got specified function %s using technique %s, budget high-low %d-%d and seed %d-%d\n", functionName.c_str(), technique.c_str(), highFiBudget, lowFiBudget, seedStart, seedEnd);
	for(int seed = seedStart; seed <= seedEnd; seed++){
		printf("Running on function %s using technique %s, budget high-low %d-%d and seed %d\n", functionName.c_str(), technique.c_str(), highFiBudget, lowFiBudget, seed);
		double err = executeExperiment(outputFilename, functionName, technique, highFiBudget, lowFiBudget, seed, r, pVals);
		if(err == -DBL_MAX){printf("Completed without model analysis!\n");}
		else{printf("Completed with err %.10f\n", err);}
	}
}


double executeExperiment(string filename, string functionName, string technique, int highFiBudget, int lowFiBudget, int seed, double r, vector<double> pVals,
							bool printSolverInfo, bool printAllInfo, int testSampleSize, int auxSolverIterations){

	// First create function
	BiFidelityFunction* function = processFunctionName(functionName);
	// Process surrogate model, call it Kriging as it is a parent class of CoKriging
	ARSsolver* auxSolver = new ARSsolver(function, 10, auxSolverIterations, true, seed, printSolverInfo);
	Kriging* surrogateModel;
	if(technique.compare("kriging") == 0){
		surrogateModel = new Kriging(function, auxSolver, highFiBudget, seed, printAllInfo);
	
	}else if(technique.compare("cokriging") == 0){
		surrogateModel = new CoKriging(function, auxSolver, highFiBudget, lowFiBudget, seed, printAllInfo);

	}else{
		printf("Unkown technique %s! Ending here...\n", technique.c_str()); return -DBL_MAX;
	}
	// Train model and calculate accuracy
	surrogateModel->createSurrogateModel();
	double performance;
	if(!surrogateModel->trainedModel_){performance = -DBL_MAX;}
	else{
		SampleGenerator* sampleGenerator = new SampleGenerator(function, seed, false);
		vector<VectorXd> samples = sampleGenerator->randomLHS(testSampleSize * function->d_);
		delete sampleGenerator;
		vector<double> trueVals = function->evaluateMany(samples);
		vector<double> modelVals = surrogateModel->multipleSurfaceValues(samples);
		performance = relativeRootMeanSquaredError(trueVals, modelVals);
	}
	// Get features
	vector<double> analysis = calculateFunctionFeatures(function, 1000, seed, r, pVals);	

	// Store info
	ofstream outputFile;
	outputFile.open(filename, ios_base::app);
	outputFile << functionName << 
					" " << technique << 
					" " << to_string(highFiBudget) << 
					" " << to_string(lowFiBudget) << 
					" " << to_string(seed) <<
					" " << to_string(function->d_);

	for(int i = 0; i < (int)analysis.size(); i++){
		outputFile << " " << to_string(analysis[i]).substr(0,6);
	}
	if(performance == -DBL_MAX){outputFile << " nan";}
	else{outputFile << " " << to_string(performance).substr(0,10);}
	outputFile << "\n";
	outputFile.close();

	delete auxSolver;
	delete surrogateModel;
	delete function;

	return performance;
}



#endif