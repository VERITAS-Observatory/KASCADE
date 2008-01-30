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
  VAKascadeSimulationHead *pfKVDFSimHead;
  TTree* pfVDFSimTree;
  //  VASimulationData*        pfVDFSimData;
  VAKascadeSimulationData* pfVDFKSimData;
  VACalibratedArrayEvent* pfVDFCalEvent;
  
  VPacket*         pfHeaderPacket;
  VArrayEvent*     pfAEIn;
  VEvent*          pfEvent;
  VArrayTrigger*   pfAT;
  VSimulationHeader* pfVBFSimHead;
  VKascadeSimulationHeader* pfKVBFSimHead;
  
  std::map< int64_t,int32_t> fGridDirMap;
  std::map< int64_t,int32_t>::iterator fMapPos;   
  



  //Methods
 public:
  VATime getFirstValidEventTime(){return fFirstValidEventTime;};
  int getRunNumber(){return fRunNumber;};
  void makeGridDirMap();
  int64_t makeGridDirKey( int fNx, int fNy,int fDir);
  void unmakeGridDirKey(int64_t fKey, int fDir, int fNx, int fNy);
  void  getGridDirForIndex(int fBaseIndex, int& fNx, int& fNy, int& fDir,
			   bool& fTrigger, bool& fPedestal);
  //bool isAPedestalEvent(int fIndex);

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
    {return pfVDFKSimData;};
 
  
 public:
  bool fFileExists;
  int fNumEvents;
  std::vector<bool> pfArrayEventsUsed;
  std::vector<int > pfPedIndexList;
  int fPedListIndex;
 
  VATelID fTelID;
  int fNXOffset;
  int fNXOffsetEven;
  int fNXOffsetOdd;

  int fNYOffset;
  int fNYOffsetEven;
  int fNYOffsetOdd;

  int fRunNumber;
  std::vector< float>fXPositionsM;
  std::vector< float>fYPositionsM;
  std::vector< float>fZPositionsM;
  float fXAreaWidthM;
  float fYAreaWidthM;

  bool  fSquareGrid;
  bool  fNorthSouthGrid;
  bool  fEastWestGrid;
  int fBaseTelIndex;
};
// ***************************************************************************


#endif
