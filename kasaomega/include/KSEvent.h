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


#ifndef KSEVENT_H
#define KSEVENT_H


#include <stdint.h>
#include <vector>


#include "KSSegmentDataClasses.h"
#include "KSPeDataClasses.h"
#include "KSTeDataClasses.h"
#include "KSTeFile.h"
#include "KSAomegaDataIn.h"
#include "KSCamera.h"
#include "KSCommon.h"
#include "KSVDFHelper.h"
#include "KSVBFFile.h"

#include "VAVDF.h"
#include "VAArrayInfo.h"
#include "VAQStatsData.h"
#include "VAPixelStatusData.h"
#include "VASimulationDataClasses.h"
#include "VAKascadeSimulationData.h"
#include "VATime.h"
#include "VADataClasses.h"
#include "VACommon.h"
#include "VAAzElRADecXY.h"

#include "TTree.h"


const double kDefaultPedestal             = 20;
const std::string kFirstValidEventTimeStr ="2005-11-28 02:00:00.000000000 GPS";
const double kDefaultEventRatePerSec      = 100.0; 


// ** Event class for ksAomega. This does all the work in ksAomega
// *******************************************************
class KSEvent
{
 public:
  KSEvent(KSTeFile* pTeFile,KSSegmentHeadData* pSegmentHead, 
	  KSPeHeadData* pPeHead, KSTeHeadData* pTeHead, 
	  KSAomegaDataIn* pDataIn);
  virtual ~KSEvent();

 private:

  KSSegmentHeadData* pfSegmentHead;
  KSPeHeadData*      pfPeHead;
  KSTeFile*          pfTeFile;
  KSTeHeadData*      pfTeHead;
  KSAomegaDataIn*    pfDataIn;

  KSCameraTypes      fCameraType;

  VATime fFirstValidEventTime;
  VATime fEventTime;
  VAVDF* pfVDFOut;
  KSVBFFile* pfVBFOut;

  KSTeData* pfTe;
  KSCamera* pfCamera;

  VARunHeader* pfRunHeader;
  VACalibratedArrayEvent* pfCalEvent;
  VAKascadeSimulationData* pfSimEvent;
  int fCorsikaType;

  int fNumPixels;
  int fEventIndex;
  double fMeanTimeBetweenEventsSec;
  float fXDummy;
  double fAzimuth;
  double fElevation;
  double fSourceRA2000;  //radians
  double fSourceDec2000;  //radians
  double fEastLongitude;
  double fLatitude;
  VAAzElRADecXY* pfAzElRADecXY;

  double fTriggerTimeNS;
  double fFADCStartGateTimeNS;


 public:
  bool BuildImage();
  bool ProcessImage();
  void SaveImage();
  void Close();
  void PrintStats();
};
// ***************************************************************************


#endif
