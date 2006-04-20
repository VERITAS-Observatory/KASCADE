/**
 * Original Author: Glenn H. Sembroski
 * $Author$
 * $Date$
 * $Revision$
 * $Tag$
 *
 **/
// This is a class that holds all the stuff needed by ksTrigger and ksAomega
// for a camera


#ifndef KSCAMERA_H
#define KSCAMERA_H


#include <string>
#include <cmath>
#include <vector>

#include "KSPixel.h"
#include "KSTeDataClasses.h"
#include "KSCommon.h"
#include "KSCameraTrigger.h"
#include "KSCameraGrid.h"
#include "KSPST.h"
#include "KSCFD.h"
#include "KSCellGrid.h"
#include "WhippleCams.h"


class KSCamera
// *******************************************************
// ** Camera class for KASACADE
// *******************************************************
{
 public:
  KSCamera(KSCameraTypes Camera, KSTeHeadData* pTeHead, 
	                                             bool fUsePatternTrigger);
  virtual ~KSCamera();
  bool isFastTriggered(){return pfCameraTrigger->isFastTriggered();};
  int  buildTriggerWaveForms();
  bool isWaveFormTriggered(){return pfCameraTrigger->isWaveFormTriggered();};
  void buildNonTriggerWaveForms();
  void loadNoiseRatesAndPeds();
  double getPSTTriggerTimeNS(){return pfCameraTrigger->fPSTTriggerTimeNS;};

  void InitPixelImageData();
  void Print();

 private:
  double fWaveFormStart;
  double fWaveFormLength;

  void generateCameraPixels();
  void generateCamera();
  void ringBuild(int& fLastGoodPixel, int fRing, int npix);
  void loadPixelCharacteristics();
  bool trywhipple490OuterPixels(double fXDeg, double fYDeg, int&fIPix);
  void findWaveFormLimits(double& fWaveFormStartNS, double& fWaveFormLengthNS);

  KSTeHeadData* pfTeHead;
  KSCameraTrigger* pfCameraTrigger;
  KSCellGrid* pfPixelGrid;

 public:
  int fNumPixels;
  int fNumPixelsTrigger;
  std::vector<KSPixel> fPixel;
  KSCFD* pfCFD;


  KSCameraTypes fCameraType;
  double  fFocalLengthM;
  double  fFacetDiameterM;
  double  fMirrorRadiusSquared;
  double  fMetersPerDeg;
  double  fJitterWidthNorthSouthRad;
  double  fJitterWidthEastWestRad;
  double  fLatitude;
  double  fEastLongitude;
  double  fMinimumDnTight;
  double  fMinimumDnLoose;
  
};
// ***************************************************************************


#endif
