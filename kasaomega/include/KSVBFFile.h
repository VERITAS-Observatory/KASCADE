//-*-mode:c++; mode:font-lock;-*-
/**
 * Original Author: Glenn H. Sembroski
 * $Author$
 * $Date$
 * $Revision$
 * $Tag$
 * 
 **/
// This is a class that creates and write a VBF file for Kascade

#ifndef KSVBFFILE_H
#define KSVBFFILE_H

/*
 * This is originally from MakeBogusFile.cpp -- makes a bogus VBF file with 
 * all zeroes in all fields.
 * by Filip Pizlo, 2004
 */

// these are the headers you'll need for writing VBF files.
#include <VBF/VBankFileWriter.h>
#include <VBF/VPacket.h>
#include <VBF/VArrayEvent.h>
#include <VBF/VDatum.h>
#include <VBF/VEventType.h>

// include the simulation data structure
#include <VBF/VKascadeSimulationHeader.h>
#include <VBF/VKascadeSimulationData.h>

// include the configuration mask utilities, which give us parseConfigMask()
#include <VBF/VConfigMaskUtil.h>

//KASACADE includes
#include "KSCommon.h"
#include "KSCamera.h"
#include "KSSegmentDataClasses.h"
#include "KSPeDataClasses.h"
#include "KSTeDataClasses.h"
#include "KSAomegaDataIn.h"
#include "KSPixel.h"


//Vegas includes
#include "VATime.h"
#include "VACommon.h"
#include "VAArrayInfo.h"
#include "VAArrayInfoFactoryLite.h"

// some system headers; you may or may not want these depending on how you
// use the library
#include <iostream>
#include <exception>
#include <stdlib.h>
#include <stdio.h>
#include <string>
#include <vector>

// declare that we're using the VConfigMaskUtil namespace, which gives us
// easy access to parseConfigMask().
using namespace VConfigMaskUtil;


class KSVBFFile
{
 public:
  KSVBFFile(KSCameraTypes CameraType, double DigitalCountsPerPE, 
	    int CORSIKAType, KSSegmentHeadData* pSegmentHead,
	    KSPeHeadData* pfPeHead, KSCamera* pCamera);
  virtual ~KSVBFFile();
  bool Create(KSAomegaDataIn* pfDataIn, std::string fConfigMask, 
	      VATime& fEventTime);
  void Close();
  void WriteVBF(int fArrayEventNum, int fTelID,VATime& fEventTime, 
		double fFADCStartGateTimeNS, KSTeData* pfTe, 
		bool fPedestalEvent, float fCoreEastM, float fCoreSouthM); 
  bool foundWriteError(){return fFoundError;};

 private:
  bool loadSimConfigFileFromFile(std::string SimConfigFileName);
  void loadArrayConfiguration();

  VBankFileWriter* pfWriter;
  int fRunNumber;
  KSCameraTypes fCameraType;
  double fDigitalCountsPerPE;

  KSSegmentHeadData* pfSegmentHead;
  uword32 fCORSIKAType;
  float   fEnergyGeV;
  uword32 fShowerID;
  float   fObsAlt;

  KSCamera* pfCamera;

  std::string fSimConfigFile;
  std::vector<VArrayConfiguration>  fArray;
  float       fPrimaryZenithDeg;
  float       fPrimaryAzimuthDeg;
  float       fCoreElevationMASL;
  unsigned int fGrISUTrigger;

  double fXSeg;
  double fYSeg;
  uword32 fNSFlag;

  double fXOffset;
  double fYOffset;


  bool fFoundError;

  //HiLoGains study
  int fSumHiSizeBeforeLoGainConversion;
  int fSumLowGainSizeBeforeClip;
  int fSumLowGainSizeAfterClip;


};
// ***************************************************************************


#endif
