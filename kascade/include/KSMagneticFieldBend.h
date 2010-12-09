//-*-mode:c++; mode:font-lock;-*-
/**
 * Original Author: Glenn H. Sembroski
 * $Author$
 * $Date$
 * $Revision$
 * $Tag$
 *
 **/
#ifndef KSMAGNETICFIELDBEND_H
#define KSMAGNETICFIELDBEND_H


#include "stdint.h"
#include <time.h>
#include <string>
#include <iostream>
#include <fstream>
#include <cmath>
#include <vector>
#include <string>

using namespace std;

const double kDeg2Rad=180./3.1415926;


class KSMagneticFieldBend
{
 public:
  KSMagneticFieldBend(string location);
  ~KSMagneticFieldBend();
  
  void bendTrajectory(double tenergy, double zpath, int ispec,
		      double& dl1, double& dm1, double& dn1,
		      int qz, double xmass);
private:
  void normalize(vector<double>& A);
  void getCrossProduct(vector<double>& A, vector<double>& B,vector<double>& C);

  double fBFieldGauss;
  double fDipAngleDeg;
  double fAngleEastDeg;
  double fCosTheta;

  vector <double > fB;
  vector <double > fV;
  vector <double > fVPerp;
  vector <double > fVxPerp;
  vector <double > fVyPerp;
  vector <double > fVParallel;
};

#endif


