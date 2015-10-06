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


#ifndef KSCAMERATRIGGER_H
#define KSCAMERATRIGGER_H


#include <string>
#include <cmath>
#include <vector>

#include "KSPixel.h"
#include "KSTeDataClasses.h"
#include "KSCommon.h"
#include "KSPST.h"


class KSCameraTrigger
// *******************************************************
// ** CameraTrigger class for KASACADE
// *******************************************************
{
 public:
  KSCameraTrigger(KSTeHeadData* pTeHead, bool UsePatternTrigger,
		  std::vector<KSPixel>* pPixel, int numPixelsInTrigger);
  virtual ~KSCameraTrigger();

  bool isFastTriggered();
  bool isWaveFormTriggered();
  void Print();
  void FindWaveFormLimits(double& fWaveFormStartNS,double& fWaveFormLengthNS);
  double fMultiplicityTriggerTime;
  double fPSTTriggerTimeNS;
  KSPST* pfPST;
  
 private:
  void fillPST();
  int checkThreshold();

  KSTeHeadData* pfTeHead;
  bool fUsePatternTrigger;

  std::vector<KSPixel>* pfPixel;

  std::vector< KSPixelTimes> pfPixelTriggerTime;
  short* pfPSTPatterns;
  KSCameraTypes fCameraType;
  int fNumPixelsTrigger;
  int fPatternTriggerLevel;
  int fTriggerMultiplicity;
  //std::vector< double> pfTimeTrigger;
  std::vector< std::vector< double > > fCFDTriggerTimes;

  float fXDummy;
};
// ***************************************************************************


#endif
