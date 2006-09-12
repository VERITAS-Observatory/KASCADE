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


#include "stdint.h"
#include <vector>
#include <string>

#include "KSCommon.h"
//#include "KSArrayVDFFile.h"
//#include "KSArrayVBFFile.h"
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


#include <VBF/VBankFileReader.h>
#include <VBF/VBankFileWriter.h>
#include <VBF/VPacket.h>
#include <VBF/VArrayEvent.h>
#include <VBF/VDatum.h>
#include <VBF/VKascadeSimulationData.h>
#include <VBF/VKascadeSimulationHeader.h>
#include <VBF/VEventType.h>


#include <stdint.h>


// ** Telescope class for ksArrayTrigger
// *******************************************************
class KSTelescope
{
 public:
  KSTelescope(VATelID TelID, KSArrayTriggerDataIn* pDataIn);
  virtual ~KSTelescope();

  VAVDF* pfVDFEventFile;
  VBankFileReader* pfVBFEventFile;

 private:
  KSArrayTriggerDataIn* pfDataIn;
  KSArrayTriggerDataType fDataType;
  VATime fFirstValidEventTime;
  
  VARunHeader* pfInRunHeader;
  VASimulationHeader* pfVDFSimHead;
  VAKascadeSimulationHead *pfKVDFSimHead;
  TTree* pfVDFSimTree;
  VAKascadeSimulationData* pfVDFSimData;
  VACalibratedArrayEvent* pfVDFCalEvent;
  
  VPacket*         pfHeaderPacket;
  VArrayEvent*     pfAEIn;
  VEvent*          pfEvent;
  VArrayTrigger*   pfAT;
  VSimulationHeader* pfVBFSimHead;
  VKascadeSimulationHeader* pfKVBFSimHead;
  
  std::map< int64_t,int32_t> fGridDirMap;
  std::map< int64_t,int32_t>::iterator fMapPos;   
  
  std::vector< float>fXPositionsM;
  std::vector< float>fYPositionsM;
  std::vector< float>fZPositionsM;
  float fXAreaWidthM;
  float fYAreaWidthM;
  bool  fNorthSouthGrid;
  int fBaseTelIndex;

  //Methods
 public:
  VATime getFirstValidEventTime(){return fFirstValidEventTime;};
  int getRunNumber(){return fRunNumber;};
  void makeGridDirMap();
  int64_t makeGridDirKey( int fNx, int fNy,int fDir);
  void unmakeGridDirKey(int64_t fKey, int fDir, int fNx, int fNy);
  void getGridDirForIndex(int fBaseIndex, int& fNx, int& fNy, int& fDir);
  int getIndexForGridDirKey(int64_t fKey);
  
  void DetermineOffsets(int fBaseTel);
  int  GetNXOffset(int fNy);
  int  GetNYOffset(int fNX);
  
  VPacket* getHeaderPacketPtr(){return pfHeaderPacket;};
  VSimulationHeader* getSimulationHeaderPtr(){return pfVBFSimHead;};
  VKascadeSimulationHeader* getKascadeSimulationHeaderPtr()
    {return pfKVBFSimHead;};
  VPacket* readPacket(int fIndex){return pfVBFEventFile->readPacket(fIndex);};
  
  
  VAKascadeSimulationData* getKascadeSimulationDataPtr()
    {return pfVDFSimData;};
 
  
 public:
  bool fIsInArray;
  int fNumEvents;
  std::vector<bool> pfArrayEventsUsed;
  VATelID fTelID;
  int fNXOffsetEven;
  int fNYOffsetEven;
  int fNXOffsetOdd;
  int fNYOffsetOdd;
  int fRunNumber;
};
// ***************************************************************************


#endif
