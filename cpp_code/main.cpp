#ifndef MAIN_CPP
#define MAIN_CPP

#include "libraries.hpp"
#include "input_output.hpp"

int main(int argc, char *argv[]){
	// Specifying here the LCC values to calculate (i.e. p and r values)
	double r = 0.2;
	vector<double> pVals = {0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95, 0.975};

	if(argc == 4){
		// This should be the spartan specification, i.e. say which file specifies the job, followed by the array job, followed by how many arrays to do
		// Follow formula
		string filename = argv[1];
		int indexNum = stoi(argv[2]);
		int indexMult = stoi(argv[3]);

		int arrayNumStart = (indexNum - 1) * indexMult + 1;
		int arrayNumEnd = indexNum * indexMult;
		string inputFilename = "../data/runScripts/" + filename + ".txt";
		ifstream inputFile(inputFilename, ios::in);
		if(!inputFile.is_open()){printf("Error: Could not open input file %s to read experiment specifications! Stopping now...\n", inputFilename.c_str()); return 0;}
		string line;
		int lineNum = 0;
		while(getline(inputFile, line)){
			if(lineNum >= arrayNumStart && lineNum <= arrayNumEnd){
				// Found them! Define output name
				string outputFilename = "../data/clusterResults/" + filename + "_arrayJob" + to_string(arrayNumStart) + "-" + to_string(arrayNumEnd) + ".txt";
				// If this is the first one, rewrite file and save headers
				if(lineNum == arrayNumStart){
					ofstream outputFile(outputFilename);
					if(!outputFile.is_open()){printf("Error: Could not open output file %s to output experiment results! Stopping now...\n", outputFilename.c_str()); return 0;}
					outputFile << "instance technique highFiBudget lowFiBudget seed dimension CC RRMSE";
					for(int i = 0; i < (int)pVals.size(); i++){
						// Check decimal places
						if(abs(pVals[i] * 10 - (int)(pVals[i]*10)) < TOL){outputFile << " LCC_" << to_string(pVals[i]).substr(0,3);}
						else if(abs(pVals[i] * 100 - (int)(pVals[i]*100)) < TOL){outputFile << " LCC_" << to_string(pVals[i]).substr(0,4);}
						else if(abs(pVals[i] * 1000 - (int)(pVals[i]*1000)) < TOL){outputFile << " LCC_" << to_string(pVals[i]).substr(0,5);}
						else{outputFile << " LCC_" << to_string(pVals[i]).substr(0,6);}
					}
					outputFile << " LCC_sd LCC_coeff performance\n";
					outputFile.close();
				}
				// Now have a line, should call processing of instructions
				processExperiment(outputFilename, line, r, pVals);
			}
			lineNum++;
		}
		inputFile.close();
		// Print warning if did not get all the expected lines
		if(lineNum <= arrayNumStart){printf("Could not find expected array jobs, wanted lines %d-%d but file only had %d lines!\n", arrayNumStart, arrayNumEnd, lineNum - 1);}
		else if(lineNum <= arrayNumEnd){printf("Looks like did not get as many array jobs as expected, wanted lines %d-%d but file only had %d lines!\n", arrayNumStart, arrayNumEnd, lineNum - 1);}

	}else if(argc == 3){
		// This should be the spartan specification, i.e. say which file specifies the job, followed by the array job
		string filename = argv[1];
		int arrayNum = stoi(argv[2]);
		// Check arrayNum is not 0, as this will get the header
		if(arrayNum == 0){printf("Error: Specified first line of file as inputs to run, but this is the header line! Stopping now...\n"); return 0;}
		// Now need to get to the file in read mode, and go to the right row
		string inputFilename = "../data/runScripts/" + filename + ".txt";
		ifstream inputFile(inputFilename, ios::in);
		if(!inputFile.is_open()){printf("Error: Could not open input file %s to read experiment specifications! Stopping now...\n", inputFilename.c_str()); return 0;}
		string line;
		int lineNum = 0;
		bool found = false;
		while(getline(inputFile, line)){
			if(lineNum == arrayNum){found = true; break;}
			lineNum++;
		}
		// Check that for the right line insted of being finished
		if(!found){printf("Error: Did not get to the desired array job, specified job %d but file only has %d specifications! Stopping now...\n", arrayNum, lineNum-1); return 0;}
		inputFile.close();
		// Since found instructions, create file where output will be written
		string outputFilename = "../data/clusterResults/" + filename + "_arrayJob" + to_string(arrayNum) + ".txt";
		ofstream outputFile(outputFilename);
		if(!outputFile.is_open()){printf("Error: Could not open output file %s to output experiment results! Stopping now...\n", outputFilename.c_str()); return 0;}
		outputFile << "instance technique highFiBudget lowFiBudget seed dimension CC RRMSE";
		for(int i = 0; i < (int)pVals.size(); i++){
			if(abs(pVals[i] * 10 - (int)(pVals[i]*10)) < TOL){outputFile << " LCC_" << to_string(pVals[i]).substr(0,3);}
			else if(abs(pVals[i] * 100 - (int)(pVals[i]*100)) < TOL){outputFile << " LCC_" << to_string(pVals[i]).substr(0,4);}
			else if(abs(pVals[i] * 1000 - (int)(pVals[i]*1000)) < TOL){outputFile << " LCC_" << to_string(pVals[i]).substr(0,5);}
			else{outputFile << " LCC_" << to_string(pVals[i]).substr(0,6);}
		}
		outputFile << " LCC_sd LCC_coeff performance\n";
		outputFile.close();

		// Now call function which processes instructions
		processExperiment(outputFilename, line, r, pVals);

	
	}else{
		printf("NEED SOME ERROR HERE");
	}

	return 0;
}



#endif