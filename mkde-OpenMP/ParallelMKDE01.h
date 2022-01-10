/*
 * ParallelMKDE01.h
 * MPI/OpenMP task farm to Compute 3D MKDEs
 *
 * Author:  Jeff A. Tracey (jeff.a.tracey@gmail.com)
 * Created: March 3, 2013
 */

/* -------------------------------------------------------------------
 * TO-DO:
 * 1.  Revise data input to include variances.
 * 2.  ** Broadcast data from farmer to workers.
 * 3.  ** Create arrays to store voxel densities.
 * 4.  Finish functions to write output files.
 * 5.  Have workers compute and send voxel densities (maybe use
 *     a dummy function to test).
 * 6.  Have farmer take voxel densities computed by workers and
 *     put them in output array.
 -------------------------------------------------------------------*/

#include <iostream>   // not required by most systems
#include <algorithm>
#include <fstream>
#include <sstream>
#include <iomanip>
#include <stdlib.h>
#include <time.h>
#include <string.h>

#include <math.h>
#include <cmath>
#include <vector>

const double MY_EPS = 0.00000001;
const int TASK_LENGTH = 6;
const int FNAME_LEN = 100;
const int ANIMALTIME_STR_LEN = 25;

inline void eatline(std::ifstream & file_name) {
	while (file_name.get() != '\n')
		continue;
}

inline bool doubleEquals(double a, double b) {
	return fabs(a - b) < MY_EPS;
}

double integrateNormal(double x0, double x1, double mu, double sigma);

int * getGridIndexes(int i, int nC, int nL);

int getLinearIndex(int row, int col, int level, int nC, int nL);

double * getVoxelCoordsFromIndex(int i, int nC, int nL, double xMin, double xW,
		double yMin, double yW, double zMin, double zW);

double * subsetArray(double * arrIn, int sizeIn, int startIndex, int endIndex);

double testVoxelDensity(double voxelX, double voxelY, double voxelZ,
                           double zMinXY, double zMaxXY, double maxT, double obsX[], double obsY[],
                           double obsZ[], double obsT[], int obsN, double mvSig2xy,
                           double obsSig2xy, double mvSig2z, double obsSig2z, double stepT);

double computeVoxelDensity(double voxelX, double voxelY, double voxelZ,
                           double zMinXY, double zMaxXY, double maxT, double obsX[], double obsY[],
                           double obsZ[], double obsT[], int obsN, double mvSig2xy,
                           double obsSig2xy, double mvSig2z, double obsSig2z, double stepT,
			   double *sig2xy, double *sig2z, double *factor, 
			   double *eX, double *eY, double *eZ, double *kZ);

bool readInputData(int rank, char * filename, char ** &animalTimeID, long * &startIndex,
                   long * &finalIndex, int * &locationID, double * &t, double * &x, double * &y,
                   double * &z, int * &num, double * &obsSig2xy, double * &moveSig2xy,
                   double * &obsSig2z, double * &moveSig2z);


void printGroupData(int rank, char ** &animalTimeID, long * &startIndex,
                   long * &finalIndex, int * &num);

void printInputData(int rank, double * &t, double * &x, double * &y,
                   double * &z, int * &num, double * &obsSig2xy, double * &moveSig2xy,
                   double * &obsSig2z, double * &moveSig2z);

void loadConfigFile(int rank, char * filename, char * datFile, double &xMin, int &xN,
		double &xCellSz, double &yMin, int &yN, double &yCellSz, double &zMin, int &zN,
		double &zCellSz);

void writeVTK(int procRank, char * filename);

void writeGRASS(int procRank, char * filename);
