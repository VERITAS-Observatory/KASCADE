/**
 * Original Author: Glenn H. Sembroski
 * $Author$
 * $Date$
 * $Revision$
 * $Tag$
 *
 **/
#ifndef KSW10mVDF_H
#define KSW10mVDF_H


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

class KSW10mVDF
// *******************************************************
// ** Utillity class for creating and filling records for Whipple VDF file
// *******************************************************
{
 public:
  KSW10mVDF(VAVDF* pOut, int numChannels, VATime& startTime, int Whipple10MId,
	    int NumWindowSamples);
  virtual ~KSW10mVDF();
  void CreateW10mVDFFile(string fFileName,double& fEastLongitude, 
			 double& fLatitude);
  void CreateRunHeader(int runNumber);
  void CreateW10mQStats(const float* ped, const float* pedvar);
  void CreateW10mRelGains(const float* gain);
  void CreatePixelStatus(int fNumPMT, bool* off);

  VAArrayInfo* GetArrayInfoPtr(){return pfArrayInfo;};


 private:
  VAVDF* pfOut;
  VAArrayInfo* pfArrayInfo;
  VAQStatsData* pfQStats;
  VARelGainData* pfRelGainData;
  VAPixelStatusData* pfPixelStatus;
  int fNumChannels;
  VATime fStartTime;
  VATime fEndTime;
  int fWhipple10MId;
  int fNumWindowSamples;

};
// ***************************************************************************


#endif
