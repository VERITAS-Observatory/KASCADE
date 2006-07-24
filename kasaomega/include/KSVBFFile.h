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

// include the simulation data structure
#include <VBF/VKascadeSimulationData.h>

// include the configuration mask utilities, which give us parseConfigMask()
#include <VBF/VConfigMaskUtil.h>

//KASACADE includes
#include "KSCommon.h"
#include "KSCamera.h"
#include "KSSegmentDataClasses.h"
#include "KSPeDataClasses.h"
#include "KSTeDataClasses.h"


//Vegas includes
#include "VATime.h"
#include "VACommon.h"

// some system headers; you may or may not want these depending on how you
// use the library
#include <iostream>
#include <exception>
#include <stdlib.h>
#include <stdio.h>

// declare that we're using the VConfigMaskUtil namespace, which gives us
// easy access to parseConfigMask().
using namespace VConfigMaskUtil;


class KSVBFFile
{
 public:
  KSVBFFile(KSCameraTypes CameraType, double DigitalCountsPerPE, 
	    int CORSIKAType, KSSegmentHeadData* pSegmentHead,
	    KSPeHeadData* pfPeHead);
  virtual ~KSVBFFile();
  bool Create(std::string VBFFileName,int RunNumber,  
	                                    std::string fConfigMask);
  void Close();
  void WriteVBF(int fArrayEventNum, int fTelID,VATime& fEventTime, 
		KSCamera* pfCamera,double fFADCStartGateTimeNS, 
		KSTeData* pfTe); 

  bool foundWriteError(){return fFoundError;};

 private:
  VBankFileWriter* pfWriter;
  int fRunNumber;
  KSCameraTypes fCameraType;
  double fDigitalCountsPerPE;

  KSSegmentHeadData* pfSegmentHead;
  int fCORSIKAType;
  float fEnergyGeV;
  float fPrimaryZenithDeg;
  float fPrimaryAzimuthDeg;
  float fCoreElevationMASL;
  double fXSeg;
  double fYSeg;
  double fXOffset;
  double fYOffset;


bool fFoundError;

};
// ***************************************************************************


#endif
