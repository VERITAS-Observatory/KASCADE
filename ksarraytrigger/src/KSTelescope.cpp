//-*-mode:c++; mode:font-lock;-*-
/**
 * \class KSTelescope
 * \brief Class to hold and process an Telescope.
 *
 * Original Author: Glenn H. Sembroski
 * $Author$
 * $Date$
 * $Revision$
 * $Tag$
 *
 **/
#include "KSTelescope.h"
extern "C" float   pran(float* dummy);
extern "C" double  Rexp(double fMeanIntervel);
extern "C" int    KascadeType2CorsikaType(int fKType);

KSTelescope::KSTelescope(VATelID TelID, KSArrayTriggerDataIn* pDataIn)
{
  pfDataIn=pDataIn;
  // ****************************************************************
  // Open the input file
  // ****************************************************************
  fFileExists=false;
  fTelID=TelID;
  fDataType=pfDataIn->fDataType;
  pfVDFEventFile=NULL;
  pfVBFEventFile=NULL;
  if(pfDataIn->fFileName[fTelID]!=" ")
    {
      if(fDataType==ROOTFILE)
	{
	  pfVDFEventFile= new VAVDF();
	  pfVDFEventFile->OpenForStage3(pfDataIn->fFileName[fTelID].c_str());
	  if(pfVDFEventFile==NULL)
	    {
	      std::cout<<"ksArrayTrigger: Failed to Open input file "
		       <<pfDataIn->fFileName[fTelID]<<std::endl;
	      exit(1);
	    }
	  fFileExists=true;
	  fNumEvents=pfVDFEventFile->getNumArrayEvents();
	  //Get start of run so we can get start time
	  pfInRunHeader=pfVDFEventFile->getRunHeaderPtr();
	  if(pfInRunHeader==NULL)
	    {
	      std::cout<<"File: "<<pfDataIn->fFileName[fTelID]
		       <<" has no RunHeader"<<std::endl;
	      exit(1);
	    }
	  fFirstValidEventTime = 
	                    pfInRunHeader->pfRunDetails->fFirstValidEventTime;
	  fRunNumber= pfInRunHeader->pfRunDetails->fRunNum;

	  // *****************************************************************
	  // Pick up the VAKascadeSimulationHead to get:
	  // Array positions(all of them!), xseg/yxeg, N/S flag
	  // *****************************************************************

	  VASimulationHeader* pfVDFSimHead=
	                            pfVDFEventFile->getSimulationHeaderPtr();
	  pfKVDFSimHead = 
	            dynamic_cast< VAKascadeSimulationHead* >(pfVDFSimHead);
	  if(pfKVDFSimHead==NULL)
	    {
	      std::cout<<"ksSumFiles: File "<<pfDataIn->fFileName[fTelID]
		       <<"has no VASimulationHeader record"
		       <<std::endl;
	      exit(1);
	    }
	  int fNumTelPositions=pfKVDFSimHead->fArray.size();
	  fXPositionsM.resize(fNumTelPositions);
	  fYPositionsM.resize(fNumTelPositions);
	  fZPositionsM.resize(fNumTelPositions);
	  for(int i=0;i<fNumTelPositions;i++)
	    {
	      fXPositionsM[i]=pfKVDFSimHead->fArray[i].fRelTelLocEastM;
	      fYPositionsM[i]=pfKVDFSimHead->fArray[i].fRelTelLocSouthM; 
	      fZPositionsM[i]=pfKVDFSimHead->fArray[i].fRelTelLocUpM;
	    }
	  fXAreaWidthM=pfKVDFSimHead->fXAreaWidthM;
	  fYAreaWidthM=pfKVDFSimHead->fYAreaWidthM;
	  fNorthSouthGrid=pfKVDFSimHead->fNorthSouthGrid;

	  pfVDFSimTree = (TTree*)pfVDFEventFile->getSimulationEventTreePtr();
	  pfVDFKSimData = pfVDFEventFile->getKascadeSimulationDataPtr();
	  pfVDFCalEvent= pfVDFEventFile->getCalibratedArrayEventPtr();
	  pfArrayEventsUsed.clear();
	  pfArrayEventsUsed.resize(fNumEvents,false);
	}
      else if(pfDataIn->fDataType==VBFFILE)
	{
	  pfVBFEventFile   = new VBankFileReader(pfDataIn->fFileName[fTelID]);
	  if(pfVBFEventFile==NULL)
	    {
	      return;
	    }
	  fFileExists=true;

	      
	  // *************************************************************
	  //Now get stuff from the Simulation headers
	  // *************************************************************
	  pfHeaderPacket=pfVBFEventFile->readPacket(0); 
	                                                 //0=Location of header

	  pfVBFSimHead  = pfHeaderPacket->get< VSimulationHeader >
	                                   (VGetSimulationHeaderBankName());
	  pfKVBFSimHead = pfHeaderPacket->get< VKascadeSimulationHeader >
	                            (VGetKascadeSimulationHeaderBankName());
	  fRunNumber=pfKVBFSimHead->fRunNumber;
	  //Now get stuff from the header.
	  int fNumTelPositions=pfVBFSimHead->fArray.size();
	  fXPositionsM.resize(fNumTelPositions);
	  fYPositionsM.resize(fNumTelPositions);
	  fZPositionsM.resize(fNumTelPositions);
	  for(int i=0;i<fNumTelPositions;i++)
	    {
	      fXPositionsM[i]=pfVBFSimHead->fArray[i].fRelTelLocEastM;
	      fYPositionsM[i]=pfVBFSimHead->fArray[i].fRelTelLocSouthM; 
	      fZPositionsM[i]=pfVBFSimHead->fArray[i].fRelTelLocUpM;
	    }
	  fXAreaWidthM=pfKVBFSimHead->fXAreaWidthM;
	  fYAreaWidthM=pfKVBFSimHead->fYAreaWidthM;
	  fNorthSouthGrid=pfKVBFSimHead->fNorthSouthGrid;
	  fNumEvents = pfVBFEventFile->numPackets()-1; //
	  if(fNumEvents==0)
	    {
	      return;
	    }

	  VPacket* pfPacket=pfVBFEventFile->readPacket(1);
	  pfAEIn=pfPacket->getArrayEvent();
	  pfAT = pfAEIn->getTrigger();
	  uint8_t  fGPSYear=6;          //Use 2006 as year
	  fFirstValidEventTime.setFromVBF(fGPSYear,
					  pfAT->getGPSTimeNumElements(),
					  pfAT->getGPSTime());
	  delete pfPacket;
	  pfArrayEventsUsed.clear();
	  pfArrayEventsUsed.resize(fNumEvents+1,false); //index starts at 1
	}
      // ****************************************************************
      // Make up a vector<bool> array to keep track of which events have been
      // checked for being in an event
      // ****************************************************************
    }
}
// ************************************************************************


KSTelescope::~KSTelescope()
{
  //Nothing here
}
// ************************************************************************

void KSTelescope::makeGridDirMap()
// *****************************************************************
// Now the tricky and probably inefficient (but clear) way to find the event
// with a particular fNx,fNy,fDirectionIndex. Scheme is to create a std::map 
// where the key is a number built out of fNx,fNy and fDirectionIndex (saved
// in the KascadeSimulationData TTree) and the value is the event index that 
// has those values.
// This is a sperate method since we don't need to create it for the first
// telescope in the array.
// *****************************************************************
// Go through simulation TTree filling map
{
  fGridDirMap.clear();
  //std::cout<<"Max possible map size:"<<fGridDirMap.max_size()<<std::endl;

  bool fTriggerEvent;
  bool fPedestalEvent;

  pfPedIndexList.clear();
  fPedListIndex=-1;

  int fStart=0;
  int fNum=0;
  if(fDataType==ROOTFILE)
    {
      fStart=0;
      fNum=fNumEvents;
    }
  else if (fDataType==VBFFILE)
    {
      fStart=1;    //Skip first packet
      fNum=fNumEvents+1;
    }
  for(int i=fStart;i<fNum;i++) 
    {
      int fNx;
      int fNy;
      int fDir;
      getGridDirForIndex(i, fNx, fNy, fDir,fTriggerEvent,fPedestalEvent);
      if(fTriggerEvent)    //Trigger event?
	{
	  int64_t fKey=makeGridDirKey(fNx,fNy,fDir);
	  fGridDirMap[fKey]=i;
	  pfArrayEventsUsed[i]=false;
	  //std::cout<<"i,nx,ny,dir,key: "<<i<<" "<<fNx<<" "<<fNy<<" "<<fDir
	  //	   <<" "<<fKey<<std::endl;
	}
      else
	{
	  pfArrayEventsUsed[i]=true;   //Disable (for now) non normal events 
	                             //(pedestal events  mostly)
	}
      if(fPedestalEvent)
       	{
      	  pfPedIndexList.push_back(i);
      	}
    }
  return;
}
// *************************************************************************


int64_t KSTelescope::makeGridDirKey( int fNx, int fNy, int fDir)
// *************************************************************************
// Combine fNx,fNy,fDir into a unique 64 bit number for use as a key in the
// GridDirMap. Use the fact that the range of fNX and fNY are given by 
// gAbsNXAbsNYMax (in KSCommon.h)
// **************************************************************************
{
  int fNumNxNy=(2*gAbsNXAbsNYMax)+1;  //the +1 is for 0
  int64_t fKey=fDir*fNumNxNy + fNx;
  fKey=fKey*fNumNxNy+fNy;
  return fKey;
}
// ***************************************************************************

void KSTelescope::unmakeGridDirKey(int64_t fKey,  int fNx, int fNy, int fDir)
// ***************************************************************************
// Decode fKey back into fNx fNy and fDir. This is the reverse process 
// (compliment) of what is done in makeGridDirKey. (see above).
// ***************************************************************************
{
   int fNumNxNy=2*gAbsNXAbsNYMax+1;
   int64_t fTempKey=fKey/fNumNxNy;
   fNy=fKey-(fTempKey*fNumNxNy);
   fDir=fTempKey/fNumNxNy;
   fNx=fTempKey-fDir*fNumNxNy;
   return;
}
// **************************************************************************


void KSTelescope::getGridDirForIndex(int fIndex, int& fNx, int& fNy, 
				     int& fDir, bool& fTrigger,
				     bool& fPedestal)
// ************************************************************************
// Find for true event at fIndex the Nx,ny and dir. Return Flags  for Normal 
// event or pedestal events
// ************************************************************************
{
  fTrigger=false;
  fPedestal=false;
  if(fDataType==ROOTFILE)
    {
      pfVDFEventFile->loadInArrayEvent(fIndex);
      if(pfVDFCalEvent->fEventType==ET_ARRAY_TRIGGER)
	{
	  fTrigger=true;//trigger event;
	  pfVDFSimTree->GetEntry(fIndex);
	  
	  fNx=pfVDFKSimData->fNx;
	  fNy=pfVDFKSimData->fNy;
	  fDir=pfVDFKSimData->fDirectionIndex;
	  return;
	}
      if(pfVDFCalEvent->fEventType==ET_PEDESTAL)
	{
	  fPedestal=true;
	  return;
	}
      return;
    }
  else if(fDataType==VBFFILE)
    {
      if(!pfVBFEventFile->hasPacket(fIndex))
	{
	  std::cout<<"ksArrayTrigger:getGridDirForIndex: Could not find "
	    " packet: "<<fIndex<<" In file: "<<pfDataIn->fFileName[fTelID]
		   <<std::endl;
	  std::cout<<"ksArrayTrigger:getGridDirForIndex: File has: "
		   <<fNumEvents+1<<" packets"<<std::endl;
	  return;
	}
      VPacket* pfPacket   = pfVBFEventFile->readPacket(fIndex);  
      if (!pfPacket->hasArrayEvent())
	{
	  std::cout<<"ksArrayTrigger:getGridDirForIndex: Missing ArrayEvent "
	    "at packet#: "<<fIndex<<std::endl;
	  delete pfPacket;
	  return;
	} 
      VArrayEvent* pfAEIn = pfPacket->getArrayEvent();
      VArrayTrigger* pfAT = pfAEIn->getTrigger();
 
      // ***************************************************************
      // Trigger event
      // ************************************************
      if(pfAT->getEventType().trigger==VEventType::L2_TRIGGER)
	{
	  fTrigger=true;
	  if (!pfPacket->has(VGetKascadeSimulationDataBankName())  )
	    {
	      std::cout<<"ksArrayTrigger:getGridDirForIndex: Missing "
		"KascadeSimulationData bank at packet#: "<<fIndex<<std::endl;
	      fTrigger=false;
	      delete pfPacket;
	      return;
	    }
	  VKascadeSimulationData* pfKVBFSimData =
	    pfPacket->get< VKascadeSimulationData >
	    (VGetKascadeSimulationDataBankName());
	  fNx  = pfKVBFSimData->fNx;
	  fNy  = pfKVBFSimData->fNy;
	  fDir = pfKVBFSimData->fDirectionIndex;
	  delete pfPacket;
	  return;
	}
      if(pfAT->getEventType().trigger==VEventType::PED_TRIGGER)
	{
	  fPedestal=true;
	  delete pfPacket;
	  return;
	}
      delete pfPacket;
      return;
    }
}

int  KSTelescope::getIndexForGridDirKey(int64_t fKey)
// ***********************************************************************
// For the GridDir Key fKey, look in our map for an index. Return the index.
// If the fKey is not in the map return -1. Ped events not in map!
// ***********************************************************************
{
  fMapPos=fGridDirMap.find(fKey);
  if(fMapPos==fGridDirMap.end())  //If fKey doesn't exist as a key return -1
    {
      return -1;
    }
  else
    {
      int fIndex=fMapPos->second;
      return fIndex;
    }
}
// *************************************************************************

//Obsolete
//void KSTelescope::DetermineOffsets(int fBaseTel)
// ************************************************************************
// Determine nx,ny offsets relative to TelID=fBaseTel for this telescope.
// May be different for odd and even, so make both
// Ignore Z for now.  Introduces correction later that may be of interest
//  Note KASCADE convention +x axis is east, +y axis is south
//  Note VEGAS convention +x axis is east, +y axis is north
// ************************************************************************
//{
//  // ************************************************************************
//  // Only North-South Triangular grid is valid
//  //
//  if(!fNorthSouthGrid)
//    {
//      std::cout<<"ksArrayTrigger: Only North-South triangular grid are valid "
//	"presently"<<std::endl;
//      exit(1);
//    }
//  float fBaseX=fXPositionsM[fBaseTel];   //X +east
//  float fBaseY=fYPositionsM[fBaseTel];   //Y +south
//  //float fBaseZ=fZPositionsM[fBaseTel];
//
//  float fXRelativeM=fXPositionsM[fTelID]-fBaseX;
//  float fYRelativeM=fYPositionsM[fTelID]-fBaseY;
//  //float fZRelativeM=fZPositionsM[fTelID]-fBaseZ;
//
//  //For any fNX on N-S grid: no changes
//  fNXOffsetEven= (int)(fabs(fXRelativeM)/fXAreaWidthM+.5);
//  if(fXRelativeM<0)
//    {
//      fNXOffsetEven=-fNXOffsetEven;
//    }
//  fNXOffsetOdd=fNXOffsetEven;
 // // **************************************************************
//  // For NX  Odd even NY are different
//  // **************************************************************
//  // NX even on N-S grid: Ny has no shift 
//  if(fNXOffsetEven%2==0)                  // % is the C++ mod operator
//    {
//      fNYOffsetEven= (int)(fabs(fYRelativeM)/fYAreaWidthM+.5);
//      if(fYRelativeM<0)
//	{
//	  fNYOffsetEven=-fNYOffsetEven;
// 	}
//      fNYOffsetOdd=fNYOffsetEven;      //If fNX origin is even or odd no shift
//     }
//  else
//    {                                // NY Odd on N-S grid:
//      fNYOffsetEven= (int)(fabs(fYRelativeM)/fYAreaWidthM);
//      if(fYRelativeM<0)
//	{
//	  fNYOffsetEven=-fNYOffsetEven-1;
//	}
//      fNYOffsetOdd=fNYOffsetEven+1; //If Nx origen odd we shift.
//    }
//  return;
//}
// **************************************************************************

int KSTelescope::GetNXOffset(int fNY)
// **************************************************************************
// Get offset fro this telescope. WSince we use triangular grid will depend on
// odd or even of fNY
// **************************************************************************
{
  if(fNY%2==0)
    {
      return fNXOffsetEven;
    }
  else
    {
      return fNXOffsetOdd;
    }
}
// **************************************************************************

int KSTelescope::GetNYOffset(int fNX)
// **************************************************************************
// Get offset for this telescope. Since we use triangular grid will depend on
// odd or even of fNX
// **************************************************************************
{
  if(fNX%2==0)
    {
      return fNYOffsetEven;
    }
  else
    {
      return fNYOffsetOdd;
    }
}
// **************************************************************************


/*
bool KSTelescope::isAPedestalEvent(int fIndex)
// ************************************************************************
// See if  event at fIndex is a pedestal event. Return false if not.
// ************************************************************************
{
  if(fDataType==ROOTFILE)
    {
      pfVDFEventFile->loadInArrayEvent(fIndex);
      if(pfVDFCalEvent->fEventType!=ET_PEDESTAL)
	{
	  return false;    //Not a trigger event;
	}
    }
  else if(fDataType==VBFFILE)
    {
      if(!pfVBFEventFile->hasPacket(fIndex))
	{
	  std::cout<<"ksArrayTrigger:isAPedestalEvent: Could not find "
	    " packet: "<<fIndex<<" In file: "<<pfDataIn->fFileName[fTelID]
		   <<std::endl;
	  std::cout<<"ksArrayTrigger:isAPedestalEvent: File has: "
		   <<fNumEvents+1<<" packets"<<std::endl;
	  return false;
	}
      VPacket* pfPacket   = pfVBFEventFile->readPacket(fIndex);  
      if (!pfPacket->hasArrayEvent())
	{
	  std::cout<<"ksArrayTrigger:getGridDirForIndex: Missing ArrayEvent "
	    "at packet#: "<<fIndex<<std::endl;
	  return false;
	} 
      VArrayEvent* pfAEIn = pfPacket->getArrayEvent();
      VArrayTrigger* pfAT = pfAEIn->getTrigger();
      if(pfAT->getEventType().trigger!=VEventType::PED_TRIGGER)
	{
	  return false;
	}
    }
    return true;
}
*/

// *********************************************************************



      
