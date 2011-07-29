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


#ifndef KSAREA_H
#define KSAREA_H


#include <iostream>
#include <fstream>
#include <vector>


#include "KSPeFile.h"
#include "KSPeDataClasses.h"
#include "KSTeFile.h"
#include "KSTeDataClasses.h"
#include "KSTriggerDataIn.h"
#include "KSCamera.h"
#include "KSCommon.h"
#include "KSMountDirection.h"

#include "KSTiltAndTrace.h"


#define WHIPLONG          1.935190
#define WHIPLAT           0.552978


const int kNumPesAreaMinimum=3;     //Minimum number of pes that have to be in
                                    // an area before we even look at it.
const float kWhip490RotDeg= -7.306; //Whipple 490 camera is rotated by this 
                                    //much (degrees)

// ** Area class for ksTrigger
// *******************************************************
class KSArea
{
 public:
  KSArea(KSPeFile* pPesFile, KSTeFile* pTeFile, 
	 KSSegmentHeadData* pSegmentHead, KSPeHeadData* pPeHead,
	 KSTriggerDataIn* pDataIn);

  virtual ~KSArea();

 private:
  KSPeFile*        pfPesFile;
  KSPeData         fPe;
  KSTeFile*        pfTeFile;
  KSTeData         fTe;

  KSTriggerDataIn* pfDataIn;
  KSTiltAndTrace* pfTiltAndTrace;
  int icount;
  int debugCount;

  int    fPhiSteps;
  int    fThetaSteps;
  int    fNumDirections;
  bool   fGammas2D;
  bool   fMultipleMountDirections;
  double fStepSizeRad;
  KSCameraTypes fCameraType;

  KSCamera*    pfCamera;
  vector<KSPeData> fPes;

  KSMountDirection*  pfMountDir;
  bool  fFirstRead;
  int   fType;
  int   fAreaNx;
  int   fAreaNy;
  int   fNumAreasProcessed;
  int   fGoodTriggerCount;

  float fXDummy;
 

  void InitMountDirectionVectors();
  void unitCrossProduct(double* X,double* Y,double* Z);
  void getIThetaIPhi(int ithphi,int nsides,int& itheta,int& iphi);
  void getVector(double* X,double& x1,double& x2,double& x3);
  void vectorRedirect(double theta, double phi, double* X, double* Y,
		      double* Z,double* R);
  void InitDirectionArrays();

 public:
  bool ReadPes();
  void ProcessImages();
  void PrintStats();

  double* pfSTheta;
  double* pfSPhi;

};
// ***************************************************************************


#endif
