/**
 * Original Author: Glenn H. Sembroski
 * $Author$
 * $Date$
 * $Revision$
 * $Tag$
 *
 **/
#ifndef KSVDFHelper_H
#define KSVDFHelper_H


#include <stdlib.h>
#include <iostream>
#include <vector>
#include <string>
#include <cmath>

#include "VAVDF.h"
#include "VAArrayInfo.h"
#include "VARootIO.h"
#include "VAPixelStatusData.h"
#include "VAQStatsData.h"
#include "VARelGainData.h"
#include "VATime.h"

#include "KSCommon.h"

class KSVDFHelper
// *******************************************************
// ** Helper class for creating and filling records for Whipple and VERITAS
// ** VDF files for single telescope runs
// *******************************************************
{
 public:
  KSVDFHelper(int numChannels, VATime& startTime, int TelID,
	      int NumWindowSamples,KSCameraTypes CameraType);
  virtual ~KSVDFHelper();

  void CreateVDFFile(string fFileName,double& fEastLongitude, 
			 double& fLatitude);
  VAVDF* getVDFFilePtr(){return pfOut;};
  void FillRunHeader(int runNumber);
  void FillQStats(const float* ped, const float* pedvar);
  void FillRelGains(const float* gain);
  void FillPixelStatus(int fNumPMT, bool* off);
  
  VAArrayInfo* GetArrayInfoPtr(){return pfArrayInfo;};


 private:
  VAVDF* pfOut;
  VAArrayInfo* pfArrayInfo;
  VAQStatsData* pfQStats;
  VARelGainData* pfRelGainData;
  VAPixelStatusData* pfPixelStatus;
  int fNumPixels;
  VATime fStartTime;
  VATime fEndTime;
  int fTelID;
  int fNumWindowSamples;
  KSCameraTypes fCameraType;

};
// ***************************************************************************


#endif
