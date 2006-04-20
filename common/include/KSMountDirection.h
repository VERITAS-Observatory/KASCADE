/**
 * Original Author: Glenn H. Sembroski
 * $Author$
 * $Date$
 * $Revision$
 * $Tag$
 *
 **/
// This is a class that holds all the stuff needed by ksTrigger and ksAomega
// for a pixel in a camera


#ifndef KSMOUNTDIRECTION_H
#define KSMOUNTDIRECTION_H
#include "KSTeDataClasses.h"
#include "VASlalib.h" 

#include <iostream>
#include <fstream>

class KSMountDirection
// *******************************************************
// ** MountDirection class for KASACADE
// *******************************************************
{
 public:
  KSMountDirection(KSTeHeadData* pfTeHead, double DriftedGammaStepSizeRad);
  virtual ~KSMountDirection();
  void createMountDirections();
  void readMountDirections(std::ifstream* pMountDirFile);
  void writeMountDirections(std::ofstream* pMountDirFile);
  void getIThetaIPhi(int ithphi,int nsides,int& itheta,int& iphi);

 public:
  KSTeHeadData* pfTeHead;
  bool   fDriftingGammas;
  bool   fMultipleMountDirections;
  double fStepSizeRad;
  KSCameraTypes fCameraType;

  int    fPhiSteps;
  int    fThetaSteps;

  int fNumDirections;
  double* pfDlm;
  double* pfDmm;
  double* pfDnm;

  double* pfXDlm;         //X vectors in focal plane for different mount
  double* pfXDmm;         //directions
  double* pfXDnm;

  double* pfYDlm;         //Y vectors in focal plane for different mount
  double* pfYDmm;         //directions
  double* pfYDnm;

  double* pfSTheta;
  double* pfSPhi;


 private:
  void unitCrossProduct(double* X,double* Y,double* Z);
  void getVector(double* X,double& x1,double& x2,double& x3);
  void vectorRedirect(double theta, double phi, double* X, double* Y,
		      double* Z,double* R);
  float fXDummy;
};
// ***************************************************************************
#endif
