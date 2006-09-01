/**
 * Original Author: Glenn H. Sembroski
 * $Author$
 * $Date$
 * $Revision$
 * $Tag$
 *
 **/
#ifndef KSArrayVDFHelper_H
#define KSArrayVDFHelper_H


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

class KSArrayVDFHelper
// *******************************************************
// ** Helper class for creating and filling records for VERITAS
// ** Array VDF files for multi telescope runs
// *******************************************************
{
 public:
  KSArrayVDFHelper(std::vector< KSTelescope* >* pArray,
		   VATime& startTime);
  virtual ~KSArrayVDFHelper();
  void CreateVDFFile(string fFileName,double& fEastLongitude, 
			 double& fLatitude);
  VAVDF* getVDFFilePtr(){return pfOut;};







  void FillRunHeader(int runNumber);
  void FillQStats(const float* ped, const float* pedvar);
  void FillRelGains(const float* gain);
  void FillPixelStatus(int fNumPMT, bool* off);
  
  VAArrayInfo* GetArrayInfoPtr(){return pfArrayInfo;};


 private:
  std::vector< KSTelescope* >* pFArray;
  VATime fStartTime;
  VATime fEndTime;
  VAVDF* pfOut;
  int fNumTels;

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
