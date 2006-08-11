/**
 * Original Author: Glenn H. Sembroski
 * $Author$
 * $Date$
 * $Revision$
 * $Tag$
 *
 **/
// This is a class that holds all the Telescope stuff needed


#ifndef KSTELESCOPE_H
#define KSTELESCOPE_H


#include <stdint.h>
#include <vector>
#include <bitset>


#include "KSCommon.h"
#include "KSVDFHelper.h"
#include "KSArrayTriggerDataIn.h"


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


#include <stdint.h>


// ** Telescope class for ksArrayTrigger
// *******************************************************
class KSTelescope
{
 public:
  KSTelescope(VATelID TelID, KSAomegaDataIn* pDataIn);
  virtual ~KSTelescope();
  VATime getFirstValidEventTime{return fFirstValidEventTime;};
  void makeGridDirMap();
  int64_t makeGridDirKey( int fNx, int fNy,int fDir);
  void unmakeGridDirKey(int64_t fKey, int,fDir, int fNx, int fNy);
  void getGridDirForIndex(int fBaseIndex, int& fNx, int& fNy, int& fDir);
  int getIndexForGridDirKey(int64_t fKey);
  VAVDF* pfEventFile;
  VARunHeader* pfRunHeader;
  TTree* pfSimTree;
  VAKascadeSimulationData* pfSimData;

 private:
  KSArrayTriggerDataIn* pfDataIn;
  VATime fFirstValidEventTime;



  std::map< int64_t,int32_t> fGridDirMap;
  std::map< int64_t,int32_t>::iterator fMapPos;   
  
 public:
  int fNumEvents;
  std::bitset* pfArrayEventsUsed;
  VATelID fTelID;
  int fNxOffset;
  int fNyOffset;

};
// ***************************************************************************


#endif
