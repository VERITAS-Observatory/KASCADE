/**
 * Original Author: Glenn H. Sembroski
 * $Author$
 * $Date$
 * $Revision$
 * $Tag$
 *
 **/
// This is a class that holds all the stuff needed by ksArrayTrigger 


#ifndef KSARRAYEVENT_H
#define KSARRAYEVENT_H


#include <stdint.h>
#include <vector>
#include <string>
#include <bitset>

#include "KSArrayEventDataIn.h"
#include "KSCommon.h"
#include "KSVDFHelper.h"

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

struct TrigEvent
{
  int fIndex;
  int fTel;
};


// ** ArrayEvent class for ksAomega. This does all the work in ksArrayTrigger
// *******************************************************
class KSASrrayEvent
{
 public:
  KSASrrayEvent(std:;string fRootFileName, KSAomegaDataIn* pDataIn);
  virtual ~KSASrrayEvent();

 private:
  int64_t GetTelescopeGridDirKey(int fBaseTel, int fNx, int fNy,  int fDir,
				 int fTel);

  KSArrayTriggerDataIn* pfDataIn;
  std::vector<KSTelescope*> fArray;

  std::vector<TrigEvent> fTriggerEvents;
  int fBaseTel;
  int fBaseIndex;

 public:
  bool FindTrigger();
  void SaveEvent();
  void Close();
  void PrintStats();
};
// ***************************************************************************


#endif
