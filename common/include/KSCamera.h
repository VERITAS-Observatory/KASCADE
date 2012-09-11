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
  //  KSCamera(KSCameraTypes Camera, KSTeHeadData* pTeHead, 
  //	                                             bool fUsePatternTrigger);
  //  KSCamera(KSCameraTypes Camera, KSTeHeadData* pTeHead, 
  //	                  bool fUsePatternTrigger, double fDigCntPerPEHiGain);

  KSCamera(KSCameraTypes Camera, KSTeHeadData* pTeHead, 
	   bool fUsePatternTrigger, double fDigCntPerPEHiGain,
	   double fNoiseRateSigma, int numPixelsInTrigger,
	   double PSFNSDeg=-1.0, double PSFEWDeg=-1.0);
 private:
  void InitCamera(KSTeHeadData* pTeHead,bool usePatternTrigger);
 public:
  virtual ~KSCamera();

  bool isFastTriggered(){return pfCameraTrigger->isFastTriggered();};
  int  buildTriggerWaveForms(int nx,int ny);
  bool isWaveFormTriggered(){return pfCameraTrigger->isWaveFormTriggered();};
  void buildNonTriggerWaveForms();
  void loadNoiseRatesAndPeds();
  double getPSTTriggerTimeNS(){return pfCameraTrigger->fPSTTriggerTimeNS;};
  bool getPixelIndex(double WX, double WY, int& pixelIndex);
  void InitPixelImageData();
  void Print();
  void loadAPedestalEventIntoPedPixels();
  bool isCFDTriggered(int fPixelIndex)
              {return pfCameraTrigger->pfPST->fL2TriggerPixels[fPixelIndex];};

 private:
  double fWaveFormStart;
  double fWaveFormLength;
  double fStartTimeOffsetNS;
  double fLastTimeOffsetNS;
  double fLastCFDCrossingNS;


  void generateCameraPixels();
  void loadPixelCharacteristics();
  bool trywhipple490OuterPixels(double XDeg, double YDeg, int&IPix);
  void findWaveFormLimits(double& WaveFormStartNS, double& WaveFormLengthNS);

  KSTeHeadData* pfTeHead;
  KSCameraTrigger* pfCameraTrigger;
  KSCellGrid* pfPixelGrid;

 public:
  int fNumPixels;
  int fNumPixelsTrigger;
  std::vector<KSPixel> fPixel;
  std::vector<KSPixel> fPedPixels;

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
  double  fDigCntsPerPEHiGain;
  double  fNoiseRateSigma;
  
  double  fMaxFOVDeg2;
  float* pfTelescopePixelX;
  float* pfTelescopePixelY;
  float* pfTelescopePixelRadius;

  double  fMinimumDnTight;
  double  fMinimumDnLoose;
  
};
// ***************************************************************************


#endif
