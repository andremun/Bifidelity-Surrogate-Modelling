#ifndef INPUT_OUTPUT_HPP
#define INPUT_OUTPUT_HPP

#include "libraries.hpp"
#include "aux_solvers.hpp"
#include "functions.hpp"
#include "kriging.hpp"

// Functions used to interpret the information passed as input when calling the project.

// Function which takes a function name and outputs the correct bifidelity source function. For more information on the available functions,
// consult the functions.hpp file.
BiFidelityFunction* processFunctionName(string name);

// Need further processing when specifying a COCO function, this function deals with that.
COCOBiFunction* processCOCOFunctionName(string name);

// Function which takes in an experiment specification as a string and processes the information. It then calls executeExperiment to run the specifications.
// r and pVals are specifications used when calculating the LCC feature. 
void processExperiment(string outputFilename, string instructionLine, double r, vector<double> pVals);

// Executes the experiment as speficied by "processExperiment". 
double executeExperiment(string filename, string functionName, string technique, int highFiBudget, int lowFiBudget, int seed, double r, vector<double> pVals,
							bool printSolverInfo = true, bool printAllInfo = false, int testSampleSize = 1000, int auxSolverIterations = 1000);




#endif