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

#include "KSArrayTriggerDataIn.h"
#include "KSCommon.h"
#include "KSArrayVDFFile.h"
#include "KSArrayVBFFile.h"

#include "VAVDF.h"
//#include "VAArrayInfo.h"
//#include "VAQStatsData.h"
//#include "VAPixelStatusData.h"
//#include "VASimulationDataClasses.h"
//#include "VAKascadeSimulationData.h"
//#include "VATime.h"
//#include "VADataClasses.h"
#include "VACommon.h"
#include "VAAzElRADecXY.h"

#include "TTree.h"

const double kMeanEventRateHz=50.0;

struct TrigEvent
{
  int fEventIndex;
  int fTelIndex;
};


// ** ArrayEvent class for ksAomega. This does all the work in ksArrayTrigger
// *******************************************************
class KSArrayEvent
{
 public:
  KSArrayEvent(std::string fRootFileName, KSArrayTriggerDataIn* pDataIn);
  virtual ~KSArrayEvent();

 private:
  int fRunNumber;
  KSArrayTriggerDataIn* pfDataIn;
  KSArrayTriggerDataType fDataType;

  std::vector<KSTelescope*> pfTelsInArray;

  std::vector<TrigEvent> fTriggerEvents;
  int fNumTelsWithData;
  double fMeanTimeBetweenEventsSec;
  int fBaseTelIndex; //Index to base telescope in pfTelsInArray for search. 
  int fBaseIndex;    //Index to event in base telescope file (packet or TTree)

  VATime fEventTime;
  int fOutEventIndex;

  KSArrayVDFFile* pfVDFOut;
  KSArrayVBFFile* pfVBFOut;

  VACalibratedArrayEvent* pfCalEvent;
  VAKascadeSimulationData* pfVDFKSimEvent;

  int fPedEventCount;


 public:
  bool FindTrigger();
  void SaveEvent();
  void SavePedestalEvent();
  void Close();
  void PrintStats();
};
// ***************************************************************************


#endif
