/*
 * ParallelMKDE01.cpp
 * MPI/OpenMP task farm to Compute 3D MKDEs
 *
 * Author:  Jeff A. Tracey (jeff.a.tracey@gmail.com)
 * Created: March 3, 2013
 */


#include "ParallelMKDE01.h"

// Command line arguments
// Input path/file name, xMin, nX, cellX, yMin, nY, cellY, zMin, nZ, cellZ

// Data Input --------------------------------------------------------------

// Assumptions:
//   Records are sorted by (1) animalTimeID and then (2) time
//   The file line of the file consists of column headers
//   Each line of the rest of the file consists of a single record
//   Fields are: (0) animalTimeID, (1) locationID, (2) time, (3) x, (4) y, and (5) z (ADD VARIANCES)
//

// static? i.e. not on heap


int main(int argc, char *argv[]) {
  time_t t;
  t = time(NULL);  // seed the random number
  srand((int) t);  // generator from outside
  
  // set up
  // Read configuration data
  char configFile[] = "ParallelMKDEconfig.txt";
  char filename[] = "../Data/CondorAllDATA-preproc.txt";
  // config vars
  double xMin, yMin, zMin, xSz, ySz, zSz; // broadcast these
  int nX, nY, nZ; // broadcast these
  loadConfigFile(0, configFile, filename, xMin, nX, xSz, yMin, nY, ySz, zMin, nZ, zSz);
  
  // location data vars
  char ** animalTimeID;
  long * startIndex;
  long * finalIndex;
  int * locationID;
  double * obsT; // broadcast this
  double * obsX; // broadcast this
  double * obsY; // broadcast this
  double * obsZ; // broadcast this
  double * obsSig2xy; // broadcast this
  double * moveSig2xy; // broadcast this
  double * obsSig2z; // broadcast this
  double * moveSig2z; // broadcast this
  int * num; // 0: nRec, 1: nGroups
  int nGroups;
  int nRec; // broadcast nRec (msg size)

  double * tmpT = NULL;
  double * tmpX = NULL;
  double * tmpY = NULL;
  double * tmpZ = NULL;
  int nLoc;


  bool ioSuccess = readInputData(0, filename, animalTimeID, startIndex, finalIndex,
				 locationID, obsT, obsX, obsY, obsZ, num,
				 obsSig2xy, moveSig2xy, obsSig2z, moveSig2z);

  std::cout << "After readInputData" << std::endl;

  nRec = num[0];
  nGroups = num[1];
  
  long nVoxels = nX * nY * nZ;

  std::cout << "nVoxels: " << nVoxels << std::endl;
  
  // Create output arrays
  double * mkde  = (double *) malloc(nVoxels * sizeof(double));
  double * zterm = (double *) malloc(nZ * sizeof(double));

  char vtkFname[FNAME_LEN];
  char grassFname[FNAME_LEN];

  for (int i = 0; i < nGroups; i++) { // for each animalTimeID
    std::cout << "Group: " << i << std::endl;
    tmpT = subsetArray(obsT, nRec, startIndex[i], finalIndex[i]); // Observation times
    tmpX = subsetArray(obsX, nRec, startIndex[i], finalIndex[i]); // Observation x-coor
    tmpY = subsetArray(obsY, nRec, startIndex[i], finalIndex[i]); // Observation y-coor
    tmpZ = subsetArray(obsZ, nRec, startIndex[i], finalIndex[i]); // Observation z-coor
    int obsN = finalIndex[i] - startIndex[i] + 1;                 // Observation count

    double zMinXY    = zMin;                         // Minimum Z location
    double zMaxXY    = (zMin + ((double)nZ) * zSz);  // Maximum Z location
    double maxT	     = 200.0;                        // Max allowed time between observations
    double stepT     = 2.0;                          // Timestep for interpolations
    bool exitLoop = false, finalLoop = false;
    double t0, t1, dt, t, tOld, alpha;
    double sig2xy, sig2z, eX, eY, eZ, kZ, factor, sig2xy_inv, sig2z_inv;
    double pXYZ;
    double totalT = 0.0;

    /// Initialize voxels to zero
    for (int j = 0; j < nVoxels; j++) {
      mkde[j] = 0.0;
    }

    for (int k = 0; k < (obsN - 1); k++) {
      //std::cout << "----- New observation -----" << std::endl;
      //std::cout << "Start/End time points " << tmpT[k] << " " << tmpT[k+1] << std::endl; 
      //std::cout << "Start/End x           " << tmpX[k] << " " << tmpX[k+1] << std::endl; 
      //std::cout << "Start/End y           " << tmpY[k] << " " << tmpY[k+1] << std::endl; 
      //std::cout << "Start/End z           " << tmpZ[k] << " " << tmpZ[k+1] << std::endl; 
      //std::cout << "Interpolant information" << std::endl;
      //std::cout << "t  x  y  z  alpha  sig2xy  sig2z  kZ  factor "  << std::endl;
      t0 = tmpT[k];
      t1 = tmpT[k + 1];
      dt = t1 - t0;     // Elapsed time between observations
      if (dt <= maxT) { // Make sure elapsed time is not beyond a reasonable upper limit
	totalT += dt;
	t = t0;
	exitLoop = false;
	finalLoop = false;
	while (!exitLoop) {

	  // Calculate fractional distance between t0 and current time
	  alpha = (t - t0) / dt;

	  // Calculate parameters for bilinear interpolation
	  sig2xy = dt * alpha * (1.0 - alpha) * moveSig2xy[0]
	    + obsSig2xy[0] * (1.0 - alpha) * (1.0 - alpha)
	    + obsSig2xy[0] * alpha * alpha;
	  sig2z = dt * alpha * (1.0 - alpha) * moveSig2z[0]
	    + obsSig2z[0] * (1.0 - alpha) * (1.0 - alpha)
	    + obsSig2z[0] * alpha * alpha;
	  sig2xy_inv = 1.0/sig2xy;
	  sig2z_inv  = 1.0/sig2z;

	  // Get (x,y,z) coordinates of kernel origin using linear interpolation
	  eX = tmpX[k] + alpha * (tmpX[k + 1] - tmpX[k]);
	  eY = tmpY[k] + alpha * (tmpY[k + 1] - tmpY[k]);
	  eZ = tmpZ[k] + alpha * (tmpZ[k + 1] - tmpZ[k]);

	  // Convert to grid indices
	  int i1k = (eX - xMin)/xSz  - 0.5;
	  int i2k = (eY - yMin)/ySz  - 0.5;
	  int i3k = (eZ - zMin)/zSz  - 0.5;

	  // Calculate parameters needed for kernel calculation
	  kZ = integrateNormal(zMinXY, zMaxXY, eZ, sqrt(sig2z));
	  factor = 1.0/((2.0 * M_PI * sig2xy) * sqrt(2.0 * M_PI * sig2z));

	  // Output parameters for this interpolated point
	  //std::cout << t << " " << eX << " " << eY << " " << eZ << " " ;
	  //std::cout << alpha << " " << sig2xy << " " << sig2z << " " ;
	  //std::cout << kZ << " " << factor << std::endl;

	  int halo = 50;

	  // Precompute contribution from z distance from kernel origin
	  for(int i3=0; i3<nZ; i3++) {
	    double voxz = zSz * (0.5 + i3) + zMin;
	    double zDistSq  = (eZ - voxz) * (eZ - voxz);
	    zterm[i3] = exp(-0.5 * zDistSq * sig2z_inv);
	  }

#pragma omp parallel for
	  for (int i1 = std::max(0, i1k-halo); i1 < std::min(nX, i1k+halo); i1++) {
	    double voxx = xSz * (0.5 + i1) + xMin;
	    for (int i2 = std::max(0, i2k-halo); i2 < std::min(nY, i2k+halo); i2++) {
	      double voxy = ySz * (0.5 + i2) + yMin;
	      double xyDistSq = (eX - voxx) * (eX - voxx) + (eY - voxy) * (eY - voxy);
	      double xyterm = exp(-0.5 * xyDistSq * sig2xy_inv);
	      for (int i3 = std::max(0, i3k-halo); i3 < std::min(nZ, i3k+halo); i3++) {
		
		double pXYZ, tmpDens;
		
		// Calculate contribution of kernel to voxel
		pXYZ = xyterm * zterm[i3] * factor;
		if (kZ > 0.0) {
		  pXYZ /= kZ;
		}
		else {
		  pXYZ = 0.0;
		}
		if (doubleEquals(t, t0)) { // first term
		  tmpDens = stepT * pXYZ / 2.0;
		}
		else if (doubleEquals(t, t1)) { // last term
		  tmpDens = (t - tOld) * pXYZ / 2.0;
		}
		else {
		  tmpDens = stepT * pXYZ;
		}
		
		// Add contribution to voxel (removed Kahan summation for now)
		int j = i1*nY*nZ + i2*nZ + i3;
		mkde[j] += tmpDens;
	      }
	    }
	  }

	  // update the eval time (t) and stopping conditions
	  if (finalLoop) {
	    exitLoop = true;
	  }
	  else {
	    tOld = t;
	    t += stepT;
	    if (t >= t1) {
	      t = t1;
	      finalLoop = true;
	    }
	  }

	} // End loop over interpolated steps between observations
      }   // End test on elapsed time between observations
    }     // End loop over observations

    // Scale voxels for totalT
    for (int j = 0; j < nVoxels; j++) {
      mkde[j] /= totalT;
    }
    
    double mkde_max = 0.0;
    double mkde_sum = 0.0;
    for (int j = 0; j < nVoxels; j++) {
      if (mkde[j] > mkde_max) mkde_max = mkde[j];
      mkde_sum += mkde[j];
    }

    double mkde_scl = mkde_sum * xSz * ySz * zSz;
    char fname[80];
    strcpy(fname, animalTimeID[i]);
    strcat(fname, ".dat");
    std::cout << "Max voxel value:  " << mkde_max << std::endl;
    std::cout << "Sum voxel value:  " << mkde_sum << std::endl;
    std::cout << "Sum voxel scaled: " << mkde_scl << std::endl;
    
    // Binary dump
    FILE *fptr = fopen(fname, "wb");
    fwrite(mkde, sizeof(double), nVoxels, fptr);
    fclose(fptr);


    // ASCII dump for debug purposes
    //std::ofstream mkde_str;
    //mkde_str.open("mkde.txt");
    //for (int j = 0; j < nVoxels; j++) {
    //  mkde_str << mkde[j] << std::endl;
    //}
    //mkde_str.close();

    // write output files (AT END OF EACH GROUP)
    strcpy(vtkFname, animalTimeID[i]);
    strcat(vtkFname, "-VTKgridFile.asc");
    strcpy(grassFname, animalTimeID[i]);
    strcat(grassFname, "-GRASSrasterFile.asc");
    writeVTK(0, vtkFname);
    writeGRASS(0, grassFname);

    free((char *)tmpT); tmpT = NULL;
    free((char *)tmpX); tmpX = NULL;
    free((char *)tmpY); tmpY = NULL;
    free((char *)tmpZ); tmpZ = NULL;
  }
  
}



/* ------------------------------------------------------------------------------------
 * MKDE functions
 ------------------------------------------------------------------------------------*/

double integrateNormal(double x0, double x1, double mu, double sigma) {
  double z0 = (mu - x0) / (sigma * sqrt(2.0));
  double z1 = (mu - x1) / (sigma * sqrt(2.0));
  double res = 0.5 * (erfc(z1) - erfc(z0));
  return res;
}

/* Assumptions about voxel data set up:
 *   Relation between array and spatial coordinates:
 *   x ~ row, index 0
 *   y ~ col, index 1
 *   z ~ level, index 2
 * Coordinates:
 *   min and max coordinates (e.g. xMin, xMax, ...) correspond to the edges of the geographic space
 *   voxel coordinates (i.e. voxelX, voxelY, voxelZ) correspond to the center of the voxel
 */

/* Get row, col, level (in that order) from index */
int * getGridIndexes(int i, int nC, int nL) {
  int * res_ptr = new int[3];
  res_ptr[0] = i / (nL * nC); // row index
  int j = i % (nL * nC);
  res_ptr[1] = j / nL; // col index
  res_ptr[2] = j % nL; // level index
  return res_ptr;
}

/* Get index from row, col, level */
int getLinearIndex(int row, int col, int level, int nC, int nL) {
  int i = row * nC * nL + col * nL + level;
  return i;
}

// Get voxelX, voxelY, voxelZ
double * getVoxelCoordsFromIndex(int i, int nC, int nL, double xMin, double xW,
                                 double yMin, double yW, double zMin, double zW) {
  double * res_ptr = new double[3];
  int * indexes = getGridIndexes(i, nC, nL); // row, col, lvl
  res_ptr[0] = xW * (0.5 + indexes[0]) + xMin;
  res_ptr[1] = yW * (0.5 + indexes[1]) + yMin;
  res_ptr[2] = zW * (0.5 + indexes[2]) + zMin;
  return res_ptr;
}

double * subsetArray(double * arrIn, int sizeIn, int startIndex, int endIndex) {
  double * res = NULL;
  if ((sizeIn > 0) && (startIndex >= 0) && (startIndex <= endIndex) && (endIndex < sizeIn)) {
    int newSize = endIndex - startIndex + 1;
    res = (double *)malloc(newSize*sizeof(double));
    for (int i = startIndex; i <= endIndex; i++) {
      res[i - startIndex] = arrIn[i];
    }
  }
  return res;
}


/* ------------------------------------------------------------------------------------
 * I/O functions
 ------------------------------------------------------------------------------------*/

// READ INPUT DATA (each row: char[] animalID, double time, double x, double y, double z)
void loadConfigFile(int rank, char * filename, char * datFile, double &xMin,
                    int &nX, double &xCellSz, double &yMin, int &nY, double &yCellSz,
                    double &zMin, int &nZ, double &zCellSz) { // OTHER ARGS PROBABLY NEEDED
  std::cout << "Process " << rank << " reading configuration file " << filename << std::endl;
  std::ifstream ifl;
  ifl.open(filename);
  if (!ifl.is_open()) {
    std::cerr << "Error in loadConfigFile():" << std::endl;
    std::cerr << "\tUnable to open data file " << filename << "." << std::endl;
    exit(1);
  }
  //
  char tmp_in[20];
  //
  ifl >> tmp_in;
  ifl >> datFile;
  eatline(ifl);
  //
  ifl >> tmp_in;
  ifl >> xMin;
  eatline(ifl);
  //
  ifl >> tmp_in;
  ifl >> nX;
  eatline(ifl);
  //
  ifl >> tmp_in;
  ifl >> xCellSz;
  eatline(ifl);
  //
  ifl >> tmp_in;
  ifl >> yMin;
  eatline(ifl);
  //
  ifl >> tmp_in;
  ifl >> nY;
  eatline(ifl);
  //
  ifl >> tmp_in;
  ifl >> yCellSz;
  eatline(ifl);
  //
  ifl >> tmp_in;
  ifl >> zMin;
  eatline(ifl);
  //
  ifl >> tmp_in;
  ifl >> nZ;
  eatline(ifl);
  //
  ifl >> tmp_in;
  ifl >> zCellSz;
  eatline(ifl);
  //
  ifl.close();
}

/* The idea here is to dynamically allocate the arrays to store the input data
 * using pointers to the arrays */
bool readInputData(int rank, char * filename, char ** &animalTimeID, long * &startIndex,
                   long * &finalIndex, int * &locationID, double * &t, double * &x, double * &y,
                   double * &z, int * &num, double * &obsSig2xy, double * &moveSig2xy,
                   double * &obsSig2z, double * &moveSig2z) {
  // Open file
  std::ifstream ifl;
  ifl.open(filename);
  if (!ifl.is_open()) {
    std::cerr << "Error in readInputData():" << std::endl;
    std::cerr << "\tUnable to open data file " << filename << "." << std::endl;
    exit(1);
  }
  else {
    std::cout << "Reading location data from file " << filename << "." << std::endl;
  }
  
  // Scan file once, et number of records and individual-time groups
  int lnCnt = 0;
  int grpCnt = 0;
  char tmpID[ANIMALTIME_STR_LEN];
  char tmp_in[ANIMALTIME_STR_LEN];
  eatline(ifl);                // eat header line
  while (ifl >> tmp_in) {
    // update group count
    if (lnCnt == 0) {
      strcpy(tmpID, tmp_in);
      grpCnt = 1;
    }
    else if (strcmp(tmpID, tmp_in) != 0) {
      strcpy(tmpID, tmp_in);
      grpCnt++;
    }
    // update record count
    lnCnt++;
    //
    eatline(ifl);
    //
    if (ifl.eof())
      break;
  }
  num = (int *)  malloc(2*sizeof(int));
  num[0] = lnCnt;
  num[1] = grpCnt;
  std::cout << "Number of groups: " << num[1] << std::endl;
  std::cout << "Number of records: " << num[0] << std::endl;
  ifl.close();
  // Allocate arrays (use malloc ??)
  animalTimeID = new char*[grpCnt];
  startIndex = new long[grpCnt];
  finalIndex = new long[grpCnt];
  locationID = new int[lnCnt];
  t = (double *) malloc(lnCnt*sizeof(double));
  x = (double *) malloc(lnCnt*sizeof(double));
  y = (double *) malloc(lnCnt*sizeof(double));
  z = (double *) malloc(lnCnt*sizeof(double));
  obsSig2xy = (double *) malloc(lnCnt*sizeof(double));
  obsSig2z = (double *) malloc(lnCnt*sizeof(double));
  moveSig2xy = (double *) malloc(lnCnt*sizeof(double));
  moveSig2z = (double *) malloc(lnCnt*sizeof(double));

  // Scan file a second time, read data into arrays
  ifl.open(filename);
  if (!ifl.is_open()) {
    std::cerr << "Error in vectorMoveData::readDataFile():" << std::endl;
    std::cerr << "\tUnable to open data file " << filename << "." << std::endl;
    exit(1);
  }
  eatline(ifl);                // eat the header line
  //
  for (int i = 0; i < grpCnt; i++) {
    animalTimeID[i] = new char[ANIMALTIME_STR_LEN];
  }

  std::cout << "After animal: " << std::endl;
  std::cout << "lnCnt: " << lnCnt << std::endl;

  grpCnt = 0;
  for (int i = 0; i < lnCnt; i++) {
    // update group data
    ifl >> tmp_in;
    // std::cout << "Reading group " << grpCnt << " (" << tmp_in << ") record "<< i << std::endl;
    if (i == 0) {
      strcpy(animalTimeID[grpCnt], tmp_in);
      startIndex[grpCnt] = 0;
      finalIndex[grpCnt] = lnCnt - 1;
      grpCnt = 1;
    }
    else if (strcmp(animalTimeID[grpCnt - 1], tmp_in) != 0) {
      strcpy(animalTimeID[grpCnt], tmp_in);
      startIndex[grpCnt] = i;
      finalIndex[grpCnt - 1] = i - 1;
      finalIndex[grpCnt] = lnCnt - 1;
      std::cout << "\tNew group: " << animalTimeID[grpCnt] << std::endl;
      grpCnt++;
    }
    // update location record
    ifl >> locationID[i];
    ifl >> t[i];
    ifl >> x[i];
    ifl >> y[i];
    ifl >> z[i];
    ifl >> obsSig2xy[i];
    ifl >> obsSig2z[i];
    ifl >> moveSig2xy[i];
    ifl >> moveSig2z[i];
    eatline(ifl);
  }
  // Close input file
  ifl.close();
  // Display data

  return true; // return something to indicate the process when well
}

void printGroupData(int rank, char ** &animalTimeID, long * &startIndex,
		    long * &finalIndex, int * &num) {
  std::cout << "Number of groups: " << num[1] << std::endl;
  for (int i = 0; i < num[1]; i++) {
    std::cout << "Group " << animalTimeID[i];
    std::cout << "\t(" << startIndex[i] << ", ";
    std::cout << finalIndex[i] << ")" << std::endl;
  }
}

void printInputData(int rank, double * &t, double * &x, double * &y,
		    double * &z, int * &num, double * &obsSig2xy, double * &moveSig2xy,
		    double * &obsSig2z, double * &moveSig2z) {
  std::cout << "Number of records: " << num[0] << std::endl;
  for (int i = 0; i < num[0]; i++) {
    std::cout << "Rank: " << rank;
    std::cout << "\t "<< i;
    std::cout << "\t" << t[i];
    std::cout << "\t" << x[i];
    std::cout << "\t" << y[i];
    std::cout << "\t" << z[i];
    std::cout << "\t" << obsSig2xy[i];
    std::cout << "\t" << obsSig2z[i];
    std::cout << "\t" << moveSig2xy[i];
    std::cout << "\t" << moveSig2z[i] << std::endl;
  }
}


// WRITE VTK FILE
void writeVTK(int rank, char * filename) { // OTHER ARGS PROBABLY NEEDED
  std::cout << "Process " << rank << " writing VTK file " << filename <<  " (TO BE COMPLETED)" << std::endl;
}

// WRITE OTHER FORMAT (GRASS ASCII RASTER?)
void writeGRASS(int rank, char * filename) { // OTHER ARGS PROBABLY NEEDED
  std::cout << "Process " << rank << " writing GRASS file " << filename << " (TO BE COMPLETED)" << std::endl;
}
