//-*-mode:c++; mode:font-lock;-*-
/**
 * \class238 KSArrayEvent
 * \brief Class to hold and process an ArrayEvent.
 *
 * Original Author: Glenn H. Sembroski
 * $Author$
 * $Date$
 * $Revision$
 * $Tag$
 *
 **/
#include "KSArrayEvent.h"

extern "C" float   pran(float* dummy);
extern "C" double  Rexp(double fMeanIntervel);
extern "C" int     KascadeType2CorsikaType(int fKType);
extern "C" void    GetAzElevFromVec(double* X, double& fAzimuth, 
				    double& fElevation);

KSArrayEvent::KSArrayEvent(std::string fOutputFileName,
			   KSArrayTriggerDataIn* pDataIn)
{
  pfDataIn=pDataIn;
  fDataType=pfDataIn->fDataType;
  fRunNumber=pfDataIn->fRunNumber;

  // ****************************************************************
  // Open the input files, check we have enough telescopes for multiplicity
  // ****************************************************************
  // Note that while these files all share the same mount direction vectors
  // they may not have the same triggers. We thus make no assumptions about
  // correlations of eventIndex between input files.
  // ****************************************************************
  // Note that at this level we don't care if these are root or vbf files
  // ****************************************************************
  // Try each telescope
  
  KSTelescope* pfFirstTelFile=NULL;  //Used for building ouput file when 
                                     //inputs give no events.
                                     //Possible this tel has no events
  KSTelescope* pfT1 = new KSTelescope(E_T1,pfDataIn);
  if(pfT1->fFileExists)
    {
      if(pfFirstTelFile==NULL)
	{
	  pfFirstTelFile=pfT1;
	}
      if(pfT1->fNumEvents>0)
	{
	  pfTelsInArray.push_back(pfT1);
	}
    }

  KSTelescope* pfT2 = new KSTelescope(E_T2,pfDataIn);
  if(pfT2->fFileExists)
    {
      if(pfFirstTelFile==NULL)
	{
	  pfFirstTelFile=pfT2;
	}
      if(pfT2->fNumEvents>0)
	{
	  pfTelsInArray.push_back(pfT2);
	}
    }

  KSTelescope* pfT3 = new KSTelescope(E_T3,pfDataIn);
  if(pfT3->fFileExists)
    {
      if(pfFirstTelFile==NULL)
	{
	  pfFirstTelFile=pfT3;
	}
      if(pfT3->fNumEvents>0)
	{
	  pfTelsInArray.push_back(pfT3);
	}
    }

  KSTelescope* pfT4 = new KSTelescope(E_T4,pfDataIn);
  if(pfT4->fFileExists)
    {
      if(pfFirstTelFile==NULL)
	{
	  pfFirstTelFile=pfT4;
	}
      if(pfT4->fNumEvents>0)
	{
	  pfTelsInArray.push_back(pfT4);
	}
    }


  VATime fFirstValidEventTime("2006-08-23 22:00:00 UTC"); //Default if we can't
                                                          //find a time.
  if(pfFirstTelFile!=NULL)
    {
      fFirstValidEventTime = pfFirstTelFile->getFirstValidEventTime();
    }
  else
    {
      // *********************************
      //None of the input files exists.
      // *******************************
      std::cout<<"ksArrayTrigger: Found no existing input files."<<std::endl;
      exit(1);
    }

  // *****************************************************************
  // If we do not have telescopes with data we will not loop through things
  // but we do need to create an 'empty' output file so that we can 'count' 
  // the shower in our weighting scheme.
  // *****************************************************************
  fNumTelsWithData=pfTelsInArray.size();
  if(fNumTelsWithData==0)
    {
      pfTelsInArray.push_back(pfFirstTelFile);// Just so we can get stuff to 
                                              // create empty output file
    } 

  // **************************************************************************
  // Open the output file. 
  // *************************************************************************
  if(fDataType==ROOTFILE)
    {
      // **************
      // DataType is root
      // **************
      // Create and write the Run Header,ArrayInfo,
      // QStatsData. PixelsStatusData RelGainData and SimulationHeader
      // ******************************************************************  
      pfVDFOut= new KSArrayVDFFile(pfTelsInArray,fFirstValidEventTime);
      pfVDFOut->CreateVDFFile(fOutputFileName);
      pfVDFOut->FillRunHeader(fRunNumber);  //Write this at end of run when 
                                            // we add EndRunTime
      pfVDFOut->FillAndWriteQStatsData();
      pfVDFOut->FillAndWritePixelStatusData();
      pfVDFOut->FillAndWriteRelGainsData();
      pfVDFOut->FillAndWriteSimulationHeader();
      pfCalEvent= pfVDFOut->getCalibratedArrayEventPtr();
      pfVDFOut->CreateKascadeSimulationDataEventTree();
      pfVDFKSimEvent= pfVDFOut->getKascadeSimulationDataPtr();
    }
  else   if(fDataType==VBFFILE)
    {
      pfVBFOut=new KSArrayVBFFile(pfTelsInArray);
      pfVBFOut->CreateVBFFile(fOutputFileName);
      pfVBFOut->CopyOutHeaderPacket();
    }

  // *********************************************************************
  // Check we have enough for multiplicity
  // *********************************************************************
  if(fNumTelsWithData<pfDataIn->fArrayTriggerMultiplicity)
    {
      std::cout<<"ksArrayTrigger: Too few telescopes specified or have events "
	" to make multiplicty requirements"<<std::endl;
      if(fDataType==ROOTFILE)
	{
	  for(int i=0;i<fNumTelsWithData;i++)
	    {
	      pfTelsInArray[i]->pfVDFEventFile->Close();
	    }
	  VAVDF* pfOut=pfVDFOut->getVDFFilePtr();
	  VARunHeader* pfVDFRunHeader=pfOut->getRunHeaderPtr();
	  pfVDFRunHeader->pfRunDetails->fLastValidEventTime=
	                                                fFirstValidEventTime;
	  pfOut->writeRunHeader();
	  pfOut->Close();
	}
      if(fDataType==VBFFILE)
	{
	  //this creates the index and writes the checksum.
	  VBankFileWriter* pfWriter=pfVBFOut->getVBFFilePtr();
	  pfWriter->finish();
	}  
      std::cout<<"ksArrayTrigger: Normal exit for empty file"<<std::endl;
      exit(0);
    }
  
  // **********************************************************************
  // We need to make fGridDirMap's for all the telescopes with data 
  // These maps associate a particular nx,ny,direction index combined 
  // togeteher as a key,  with the event's index (VBF: packet ID, 
  // VDF: EventIndex)
  // We don't really need one for the first one but we do need to set up each
  // telescope pfArrayInEvent vectors. Init to ingore pedestal events.
  // Also copy over the nx,ny offset info
  // Also makes a vector of all the Pedestal events.
  // **********************************************************************
  for(int i=0;i<fNumTelsWithData;i++)
    {
      pfTelsInArray[i]->makeGridDirMap();
    }
  
  fBaseTelIndex=0; //Index to base telescope in pfTelsInArray for search.
  
  // **********************************************************************
  // Determine all nx,ny offsets using each telescope as Base tel.
  for(int i=1;i<fNumTelsWithData;i++)
    {
      //This sets tells each telescope what its offset is from the base tel
      pfTelsInArray[i]->DetermineOffsets(pfTelsInArray[fBaseTelIndex]->fTelID);
    }
  
  // ***********************************************************************
  // Set Index -1 in Base (TTree index or Packet index) of next to look at for
  //  a trigger
  // ***********************************************************************
  if(pfDataIn->fDataType==VBFFILE)
    {
      fBaseIndex=0;  //VBF data packets start a at index 1.
      fOutEventIndex=0;
    }
  else if(pfDataIn->fDataType==ROOTFILE)
    {
      fBaseIndex=-1; //Root TTree data index startas at 0.
      fOutEventIndex=-1;
    }
  
  // ************************************************************************
  // Init event time.
  // ************************************************************************
  fEventTime=fFirstValidEventTime;
  fMeanTimeBetweenEventsSec=1./kMeanEventRateHz;

  fPedEventCount=0;
}
// ***************************************************************************

KSArrayEvent::~KSArrayEvent()
{
  // Nothing to do yet
}
// **************************************************************************


bool KSArrayEvent::FindTrigger()
// ***************************************************************************
// Find the next trigger in the array. Return true if shower has ended
// ***************************************************************************
{
  while(1)   //Search for trigger
    {
      // ********************************************************************
      // First test that we havent run out of events in this base telescope
      // ********************************************************************
      if(
	 (fDataType==ROOTFILE&&
	  fBaseIndex==pfTelsInArray[fBaseTelIndex]->fNumEvents-1) ||
	 (fDataType==VBFFILE&&
	  fBaseIndex==pfTelsInArray[fBaseTelIndex]->fNumEvents)    )
	{
	  std::cout<<"fBaseIndex,fNumEvents :"<<fBaseIndex<<" "
		   <<pfTelsInArray[fBaseTelIndex]->fNumEvents<<std::endl;
	  // *********************************************************
	  // We have finished looking for array triggers were this
	  // telscope (fBaseTelIndex) is in the trigger. If our required
	  // multiplicity allows us, switch to the next telescope as 
	  // the next telescope to use as our Base Telescope.
	  // *********************************************************
	  fBaseTelIndex++;

	  if((fNumTelsWithData-fBaseTelIndex)<pfDataIn->fArrayTriggerMultiplicity)
	    {
	      // *********************************************
	      // Were done. Shower is finished
	      // *********************************************
	      return true;
	    }
	  if(pfDataIn->fDataType==VBFFILE)
	    {
	      fBaseIndex=0;  //VBF data packets start a at index 1.
	    }
	  else if(pfDataIn->fDataType==ROOTFILE)
	    {
	      fBaseIndex=-1; //Root TTree data index startas at 0.
	    }
	  // Determine all nx,ny offsets using each telescope as Base tel.
	  for(int i=fBaseTelIndex+1;i<fNumTelsWithData;i++)
	    {
	      //This sets tells each telescope what its offset is from the 
	      //base tel
	      pfTelsInArray[i]->
		     DetermineOffsets(pfTelsInArray[fBaseTelIndex]->fTelID);
	    }
	  continue;
	}
      // ************************************************************
      // Now look for a trigger
      // *********************************************************
      fBaseIndex++;

      // ****************************************************************
      // Test that the next event in this telescope has not been 
      // used yet
      // *****************************************************************
      if(pfTelsInArray[fBaseTelIndex]->pfArrayEventsUsed[fBaseIndex])
	{
	  continue;
	}

      // ****************************************************************
      // Try using this event this telescope as a base event
      // ****************************************************************
      // Flag that we are using it
      pfTelsInArray[fBaseTelIndex]->pfArrayEventsUsed[fBaseIndex]=true;
      
      // Now test the other telescopes to see if they triggered.
      // Find nx,ny,fdir this telescope this index.
      int fNx;
      int fNy;
      int fDir;
      bool fTrigEvt;
      bool fPedEvt;
      
      pfTelsInArray[fBaseTelIndex]->
	getGridDirForIndex(fBaseIndex, fNx, fNy, fDir, fTrigEvt, fPedEvt);
      fTriggerEvents.clear();
      TrigEvent fTEvent;
      fTEvent.fEventIndex=fBaseIndex;
      fTEvent.fTelIndex=fBaseTelIndex;
      fTriggerEvents.push_back(fTEvent);
      
      for(int i=fBaseTelIndex+1;i<fNumTelsWithData;i++)
	{
	  // ***********************************************************
	  // Find the GridDir Key for the next telescope relative to this 
	  // telescope this event.
	  // ***********************************************************
	  int fTelNx=fNx+pfTelsInArray[i]->GetNXOffset(fNy);
	  int fTelNy=fNy+pfTelsInArray[i]->GetNYOffset(fNx);

	  int64_t fKey=pfTelsInArray[fBaseIndex]->
	                                   makeGridDirKey(fTelNx,fTelNy,fDir);
	  
	  // **********************************************************
	  // See if this telescope has a nomal event with this GridKey
	  // This checks the map which has no ped events in it.
	  // **********************************************************
	  int fEventIndex=pfTelsInArray[i]->getIndexForGridDirKey(fKey);
	  // Returns -1 if event doesn't exist.
	  //std::cout<<"fBaseIndex,fNx,fNy,fDir,fTelNx,fTelNy,fEventIndex: "
	  //	   <<fBaseIndex<<" "<<fNx<<" "<<fNy<<" "<<fDir<<" "<<fTelNx
	  //	   <<" "<<fTelNy<<" "<<fEventIndex<<std::endl;
	  if(fEventIndex>-1)  
	    {  // good event!
	      //put the event into Trigger events vector.
	      fTEvent.fEventIndex=fEventIndex;
	      fTEvent.fTelIndex=i;
	      fTriggerEvents.push_back(fTEvent);
	      // Flag that we are using it
	      pfTelsInArray[i]->pfArrayEventsUsed[fEventIndex]=true;
	    }
	}
      // ************************************************************
      // Now see if we have enought telescopes in out trigger vector to
      // meet the multiplicity requirment. This could be more complicated
      // by using the event trigger times, dynamoic delays etc. But we know
      // these are events are from the same shower and so should be in time 
      // so we don't really have to make that test unless we are studying 
      // how small an coincidence window we need.
      // ************************************************************
      if((int)fTriggerEvents.size()<(int)pfDataIn->fArrayTriggerMultiplicity)
	{
	  // *********************************************************
	  // Try again
	  continue;
	}
      else
	{
	  return false;   
	}
    }
}
// ***************************************************************************

void KSArrayEvent::SaveEvent()
// ***************************************************************************
// Build the found array event and save to the output file;
// ***************************************************************************
{
  int fNumTrigTel=fTriggerEvents.size();
  int fFirstTrigTelIndex=fTriggerEvents[0].fTelIndex;
  int fFirstTrigTelEventIndex=fTriggerEvents[0].fEventIndex;
  fOutEventIndex++;

  VATime fOriginalEventTime=fEventTime;
  double fEventTimeMJD=fEventTime.getMJDDbl();
  fEventTimeMJD+=Rexp(fMeanTimeBetweenEventsSec)/(60.*60.*24.);
  fEventTime.setFromMJDDbl(fEventTimeMJD);
  
  // *********************************************************************
  // Check if its time to make a pedestal event. Pedestal events come once 
  // per second. See if we cross a second boundary. Actually we may cross 
  // multiple second boundrys but only generate one pedestal even in that case
  // *********************************************************************
  int fOriginalSeconds=fOriginalEventTime.getSec();
  int fNewSeconds=fEventTime.getSec();
  // *********************************************************************
  // See if we need to write a Pedestal event out
  // *********************************************************************
  if(fNewSeconds>fOriginalSeconds)
    {
      SavePedestalEvent();
    }

  // ****************************************************************
  // Init and load the calibrated event class
  // ****************************************************************
  if(fDataType==ROOTFILE)
    {
      pfCalEvent->Reset();//pfCalEvent has # telescopes preset.
      pfCalEvent->fTels=fNumTrigTel;
      pfCalEvent->fArrayEventNum=fOutEventIndex+1;
      pfCalEvent->fEventType=ET_ARRAY_TRIGGER;//normal event
      pfCalEvent->fArrayTime=fEventTime;
      pfCalEvent->fTelEvents.resize(fNumTrigTel);
      pfCalEvent->fPresentTels.clear();
      pfCalEvent->fPresentTels.resize(4,false);
      
      for(int i=0;i<fNumTrigTel;i++)
	{
	  int fTrigTelIndex=fTriggerEvents[i].fTelIndex;
	  int fTrigTelEventIndex=fTriggerEvents[i].fEventIndex;
	  int fTelID=pfTelsInArray[fTrigTelIndex]->fTelID;
	  pfCalEvent->fPresentTels[fTelID]=true;
	  
	  // ****************************************************************
	  //Fill the Calibrated Telescope event
	  // ****************************************************************
	  VACalibratedArrayEvent* pfInCalEvent = pfTelsInArray[fTrigTelIndex]->
	    pfVDFEventFile->getCalibratedArrayEventPtr();
	  pfTelsInArray[fTrigTelIndex]->pfVDFEventFile->
	                                  loadInArrayEvent(fTrigTelEventIndex);
	  if(pfInCalEvent==NULL)
	    {
	      std::cout<<"ksSumFiles: Failed to "
		"getCalibratedArrayEventPtr"
		       <<std::endl;
	      exit(1);
	    }
	  
	  pfCalEvent->fTelEvents[i].fTelTime=fEventTime;
	  pfCalEvent->fTelEvents[i].fPointingData=pfInCalEvent->
	                                          fTelEvents[0].fPointingData;
	  pfCalEvent->fTelEvents[i].fTelID=pfTelsInArray[fTrigTelIndex]->
	                                                               fTelID;
	  pfCalEvent->fTelEvents[i].fChanData=
	                                 pfInCalEvent->fTelEvents[0].fChanData;
	}
      pfVDFOut->writeCalibratedArrayEvent(fNumTrigTel);

      // ************************************************
      // Now sim data: Just copy over the one from the first triggered tel.
      // ************************************************
      pfTelsInArray[fFirstTrigTelIndex]->pfVDFEventFile->
	                          readSimulationData(fFirstTrigTelEventIndex); 
     VAKascadeSimulationData*  pfKInSimEvent=
             pfTelsInArray[fFirstTrigTelIndex]->getKascadeSimulationDataPtr();
      *pfVDFKSimEvent=*pfKInSimEvent;
      pfVDFOut->writeSimulationData();
    }
  else if(fDataType==VBFFILE)
    {
      VPacket* pfWritePacket = new VPacket();
      // *********************************************************************
      // First fill in simulation packets. Use first triggered telescope
      // *********************************************************************
      VPacket* pfReadPacket=pfTelsInArray[fFirstTrigTelIndex]->
	                                   readPacket(fFirstTrigTelEventIndex);
      // *************************************************
      // Update event numbers here and maybe times
      // *************************************************
      
      // *************************************************
      // Fix up simulation data bank in this packet
      // *************************************************
      VSimulationData *pfSimData = pfReadPacket->get< VSimulationData >
	                                      (VGetSimulationDataBankName());
      VSimulationData* pfWriteSimData = pfSimData->copySimData();
      pfWriteSimData->fEventNumber=fOutEventIndex;
      //pfWriteSimData->fRunNumber=fRunNumber;
      pfWritePacket->put(VGetSimulationDataBankName(), pfWriteSimData);  

      // *************************************************
      // Fix up Kascade simulation data bank in this packet
      // *************************************************
      VKascadeSimulationData *pfKSimData = 
	                     pfReadPacket->get< VKascadeSimulationData >
			    (VGetKascadeSimulationDataBankName());
      VKascadeSimulationData *pfWriteKSimData = 
	                                    pfKSimData->copyKascadeSimData();
      pfWriteKSimData->fEventNumber=fOutEventIndex;
      //pfWriteKSimData->fRunNumber=fRunNumber;
      pfWritePacket->put(VGetKascadeSimulationDataBankName(),
				pfWriteKSimData);  
    
      // *************************************************
      // Now the ArrayEvents
      // First fix times and event number in array Trigger
      // *********************************************
      VArrayEvent* pfAEOut  = new VArrayEvent();
			  
      VArrayEvent* pfAEIn = pfReadPacket->getArrayEvent();
      VArrayTrigger* pfAT = pfAEIn->getTrigger();
      float fAlt=pfAT->getAltitude(0);  //get stuff we need to propagate
      float fAz=pfAT->getAzimuth(0);

      // ****************************************************************
      //Resize things. Array trigger will contains fNumTrigTels telescopes
      // ****************************************************************
      pfAT->resizeSubarrayTelescopes(fNumTelsWithData);
      pfAT->resizeTriggerTelescopes(fNumTrigTel);
      
      // ********************************************************************
      // have to set the telescope ID's that this record corresponds to.
      // in this case, the record number and telescope ID happen to
      // be the same
      // Fill up the subarrytels vector
      for(int i=0;i<fNumTelsWithData;i++)
	{
	  int fTelID=pfTelsInArray[i]->fTelID;
	  VEventType fEvType;
	  fEvType.trigger=VEventType::L2_TRIGGER;

	  pfAT->setSpecificEventType(i,fEvType);
	  pfAT->setSubarrayTelescopeId(i,fTelID);
	  pfAT->setShowerDelay(i,0);
	  pfAT->setCompDelay(i,0);
	  pfAT->setAltitude(i,fAlt);
	  pfAT->setAzimuth(i,fAz);
	  pfAT->setTDCTime(i,0);
	}

      std::string fConfigMask;
      for(int i=0;i<fNumTrigTel;i++)
	{
	  int fTrigTelIndex=fTriggerEvents[i].fTelIndex;
	  int fTelID=pfTelsInArray[fTrigTelIndex]->fTelID;
	  pfAT->setTriggerTelescopeId(i,fTelID);

	  if(fTelID==E_T1)fConfigMask+="0";
	  else if(fTelID==E_T2)fConfigMask+="1";
	  else if(fTelID==E_T3)fConfigMask+="2";
	  else if(fTelID==E_T4)fConfigMask+="3";
	  fConfigMask+=",";
  	}
      // Original AT from first triggered tel already has: node number=255,
      // run number(but may want to do this explictly later)
      //  pfAT->setRunNumber(fRunNumber).  flags=0;

      unsigned short fCMask=toDAQMask(parseConfigMask(fConfigMask.c_str()) ); 
      pfAT->setConfigMask(fCMask);

      // set the event number
      pfAT->setEventNumber(fOutEventIndex);
      //pfAT->setRunNumber(fRunNumber);
      uint16_t fGPSWords[5];
      uint8_t  fGPSYear=6;   
      fEventTime.getForVBF(fGPSYear,5,fGPSWords);
			  // SEtGPS Time, I know it looks like a get but its 
			  //not
      pfAT->getGPSTime()[0]=fGPSWords[0];
      pfAT->getGPSTime()[1]=fGPSWords[1];
      pfAT->getGPSTime()[2]=fGPSWords[2];
      pfAT->getGPSTime()[3]=fGPSWords[3];
      pfAT->getGPSTime()[4]=fGPSWords[4];
			  
      pfAT->setGPSYear(fGPSYear);

      // **************************************************
      // Now copy over and fix telescope events.
      // *************************************************
      VPacket** pfTelReadPacket = new VPacket*[fNumTrigTel]; 
      for(int i=0;i<fNumTrigTel;i++)
	{
	  int fTrigTelIndex=fTriggerEvents[i].fTelIndex;
	  int fTrigTelEventIndex=fTriggerEvents[i].fEventIndex;
	  int fTelID=pfTelsInArray[fTrigTelIndex]->fTelID;
	  // set the event number
	  pfTelReadPacket[i]=pfTelsInArray[fTrigTelIndex]->
	                                   readPacket(fTrigTelEventIndex);
	  VArrayEvent* pfAEIn=pfTelReadPacket[i]->getArrayEvent();
 	  VEvent* pfEvent=pfAEIn->getEvent(0);
	  pfEvent->setEventNumber(fOutEventIndex);
	  pfEvent->setNodeNumber(fTelID);
	  pfAT->setTriggerTelescopeId(i,fTelID);
	  
	  pfEvent->getGPSTime()[0]=fGPSWords[0];
	  pfEvent->getGPSTime()[1]=fGPSWords[1];
	  pfEvent->getGPSTime()[2]=fGPSWords[2];
	  pfEvent->getGPSTime()[3]=fGPSWords[3];
	  pfEvent->getGPSTime()[4]=fGPSWords[4];
			      
	  pfEvent->setGPSYear(fGPSYear);
	  // add the event to the array event!
	  VEvent* pfWriteEvent= pfEvent->copyEvent();
	  pfAEOut->addEvent(pfWriteEvent);
	}
      // now put array trigger back into the array event
      VArrayTrigger* pfWriteAT = pfAT->copyAT();
      pfAEOut->setTrigger(pfWriteAT);
			  



      // put the array event back into the packet
      // I'm told this will be a replacement
      pfWritePacket->putArrayEvent(pfAEOut);
      
      // finally, write the packet into the file
      VBankFileWriter* pfWriter=pfVBFOut->getVBFFilePtr();
      if (!pfWritePacket->
	  has(VGetKascadeSimulationDataBankName())  )
	{
	  std::cout<<"ksArrayTrigger:SaveEvent: Missing "
	    "SimulationDataBank at packet#: "<<fOutEventIndex<<std::endl;
	} 
      if (!pfWritePacket->hasArrayEvent())
	{
	  std::cout<<"ksArrayTrigger:SaveEvent: Missing ArrayEvent at "
	    "packet#: "<<fOutEventIndex<<std::endl;
	} 
      //std::cout<<"Event Packet at: "<<fOutEventIndex<<std::endl;
      pfWriter->writePacket(fOutEventIndex,pfWritePacket);
      delete pfWritePacket;
      delete []pfTelReadPacket;
      delete pfReadPacket;
    }
  return;
}
// *********************************************************************  

void KSArrayEvent::Close()
// **********************************************************************
// Close files.
// **********************************************************************
{
  // ****************************************************************
  // First check that we have written at least one event
  // ****************************************************************
  if(fOutEventIndex>0)
    {
      // ****************************************************************
      // First that we have written at least one pedestal event
      // ****************************************************************
      int fPedListIndex= pfTelsInArray[0]->fPedListIndex;
      if(fPedListIndex==0)
	{
	  // **************************************************************
	  // Cross a second, will be rest by SavePedEvent to second tick.
	  // **************************************************************
	  VATime fOriginalEventTime=fEventTime;
	  double fEventTimeMJD=fEventTime.getMJDDbl();
	  fEventTimeMJD+=1.0/(60.*60.*24.); //Bump a second
	  fEventTime.setFromMJDDbl(fEventTimeMJD);
	  
	  fOutEventIndex++;
	  SavePedestalEvent();  //If there are no ped events in the list this
                                //should just return
	}
    }
  
  if(fDataType==ROOTFILE)
    {
      for(int i=0;i<fNumTelsWithData;i++)
	{
	  pfTelsInArray[i]->pfVDFEventFile->Close();
	}
      VAVDF* pfOut=pfVDFOut->getVDFFilePtr();
      VARunHeader* pfVDFRunHeader=pfOut->getRunHeaderPtr();
      pfVDFRunHeader->pfRunDetails->fLastValidEventTime=fEventTime;
      pfOut->writeRunHeader();
      pfOut->writeCalibratedEventTree();
      pfOut->writeSimulationEventTree();
      pfOut->Close();
    }
  if(fDataType==VBFFILE)
    {
      //this creates the index and writes the checksum.
      VBankFileWriter* pfWriter=pfVBFOut->getVBFFilePtr();
      pfWriter->finish();
    }  
  return;
}
// **********************************************************************

void KSArrayEvent::PrintStats()
{
  std::cout<<"ksArrayTrigger: End of Run Stats"<<std::endl;
  if(fDataType==ROOTFILE)
    {
      std::cout<<"   Number of Normal events writen to output file: "
	       <<fOutEventIndex+1<<std::endl;
    }
  if(fDataType==VBFFILE)
    {
      std::cout<<"   Number of Normal events writen to output file: "
	       <<fOutEventIndex<<std::endl;
    }
  std::cout<<" Number of Pedestal events writen to output file: "
	   <<fPedEventCount<<std::endl;
  return;
}
// **********************************************************************

void KSArrayEvent::SavePedestalEvent()
// **********************************************************************
// Write a pedestal event to the output file
// **********************************************************************
{
  //Get time onf the event (on the tick)
  uint32_t fYear,fMonth,fDay,H,M,S,NS;
  fEventTime.getCalendarDate(fYear,fMonth,fDay);
  fEventTime.getTime(H,M,S,NS);
  NS=0;   //On the tick!
  VATime fPedestalEventTime;
  fPedestalEventTime.setFromCalendarDateAndTime(fYear,fMonth,fDay,
						H,M,S,NS);
  int fPedListIndex=pfTelsInArray[0]->fPedListIndex;
  if(fPedListIndex>=(int)pfTelsInArray[0]->pfPedIndexList.size()-1)
    {
      return;          //No more ped events to use.
    }
  pfTelsInArray[0]->fPedListIndex++;
  fPedListIndex=pfTelsInArray[0]->fPedListIndex;

  int fPedIndex= pfTelsInArray[0]->pfPedIndexList[fPedListIndex]; 

  if(fDataType==ROOTFILE)
    {
      pfCalEvent->Reset();//pfCalEvent has # telescopes preset.
      pfCalEvent->fTels=fNumTelsWithData;
      pfCalEvent->fArrayEventNum=fOutEventIndex;
      pfCalEvent->fEventType=ET_PEDESTAL;//Pedestal event
      pfCalEvent->fArrayTime=fEventTime;
      pfCalEvent->fTelEvents.resize(fNumTelsWithData);
      pfCalEvent->fPresentTels.clear();
      pfCalEvent->fPresentTels.resize(4,false);
      
      for(int i=0;i<fNumTelsWithData;i++)
	{
	  int fTelID=pfTelsInArray[i]->fTelID;
	  pfCalEvent->fPresentTels[i]=true;
	  
	  // ****************************************************************
	  //Fill the Calibrated Telescope event
	  // ****************************************************************
	  VACalibratedArrayEvent* pfInCalEvent = pfTelsInArray[i]->
	    pfVDFEventFile->getCalibratedArrayEventPtr();
	  pfTelsInArray[i]->pfVDFEventFile->
	                      loadInArrayEvent(fPedIndex);
	  if(pfInCalEvent==NULL)
	    {
	      std::cout<<"ksSumFiles: Failed to "
		"getCalibratedArrayEventPtr for Pedestal Event at index:"
		       <<fPedIndex<<std::endl;
	      exit(1);
	    }
	  
	  pfCalEvent->fTelEvents[i].fTelTime=fEventTime;
	  pfCalEvent->fTelEvents[i].fPointingData=pfInCalEvent->
	                                          fTelEvents[0].fPointingData;
	  pfCalEvent->fTelEvents[i].fTelID=fTelID;
	  pfCalEvent->fTelEvents[i].fChanData=
	                                 pfInCalEvent->fTelEvents[0].fChanData;
	}
      pfVDFOut->writeCalibratedArrayEvent(fNumTelsWithData);

      // ************************************************
      // Now sim data: Just copy it over from first telescope
      // ************************************************
      pfTelsInArray[0]->pfVDFEventFile->readSimulationData(fPedIndex); 
      VAKascadeSimulationData*  pfKInSimEvent=
             pfTelsInArray[0]->getKascadeSimulationDataPtr();
      *pfVDFKSimEvent=*pfKInSimEvent;
      pfVDFOut->writeSimulationData();
      fOutEventIndex++;

    }
  else if(fDataType==VBFFILE)
    {
      VPacket* pfWritePacket = new VPacket();
      // *********************************************************************
      // First fill in simulation packets. Use first telescope's
      // *********************************************************************

      VPacket* pfReadPacket=pfTelsInArray[0]->
	                                   readPacket(fPedIndex);
      // *************************************************
      // Update event numbers here and maybe times
      // *************************************************
      
      // *************************************************
      // Fix up simulation data bank in this packet.
      // *************************************************
      VSimulationData *pfSimData = pfReadPacket->get< VSimulationData >
	                                      (VGetSimulationDataBankName());
      VSimulationData* pfWriteSimData=pfSimData->copySimData();
      pfWriteSimData->fEventNumber=fOutEventIndex;
      //pfWriteSimData->fRunNumber=fRunNumber;
      pfWritePacket->put(VGetSimulationDataBankName(), pfWriteSimData);  
      // *************************************************
      // Fix up Kascade simulation data bank in this packet
      // *************************************************
      VKascadeSimulationData *pfKSimData = 
	                     pfReadPacket->get< VKascadeSimulationData >
			    (VGetKascadeSimulationDataBankName());
      VKascadeSimulationData* pfWriteKSimData = 
	                                     pfKSimData->copyKascadeSimData();
      pfWriteKSimData->fEventNumber=fOutEventIndex;
      //pfWriteKSimData->fRunNumber=fRunNumber;
      pfWritePacket->put(VGetKascadeSimulationDataBankName(),
				pfWriteKSimData);  
      // *************************************************
      // Now the ArrayEvents
      // First fix times and event number in array Trigger
      // *********************************************
      VArrayEvent* pfAEOut  = new VArrayEvent();
			  
      VArrayEvent* pfAEIn = pfReadPacket->getArrayEvent();
      VArrayTrigger* pfAT = pfAEIn->getTrigger();
      float fAlt=pfAT->getAltitude(0);  //get stuff we need to propagate
      float fAz=pfAT->getAzimuth(0);

      // ****************************************************************
      //Resize things. Array trigger will contains fNumTrigTels telescopes
      // ****************************************************************
      pfAT->resizeSubarrayTelescopes(fNumTelsWithData);
      pfAT->resizeTriggerTelescopes(fNumTelsWithData);
      
      // ********************************************************************
      // have to set the telescope ID's that this record corresponds to.
      // in this case, the record number and telescope ID happen to
      // be the same
      std::string fConfigMask;
      for(int i=0;i<fNumTelsWithData;i++)
	{
	  int fTelID=pfTelsInArray[i]->fTelID;
	  pfAT->setSubarrayTelescopeId(i,fTelID);
	  pfAT->setTriggerTelescopeId(i,fTelID);
	  VEventType fEvType;
	  fEvType.trigger=VEventType::PED_TRIGGER;
	  pfAT->setSpecificEventType(i,fEvType);
	  pfAT->setShowerDelay(i,0);
	  pfAT->setCompDelay(i,0);
	  pfAT->setAltitude(i,fAlt);
	  pfAT->setAzimuth(i,fAz);
	  pfAT->setTDCTime(i,0);


	  if(fTelID==E_T1)fConfigMask+="0";
	  else if(fTelID==E_T2)fConfigMask+="1";
	  else if(fTelID==E_T3)fConfigMask+="2";
	  else if(fTelID==E_T4)fConfigMask+="3";
	  fConfigMask+=",";
	}
      // Original AT from first triggered tel already has: node number=255,
      // run number(but may want to do this explictly later)
      //pfAT->setRunNumber(fRunNumber);
   
      // set the event number
      pfAT->setEventNumber(fOutEventIndex);

      unsigned short fCMask=toDAQMask(parseConfigMask(fConfigMask.c_str()) ); 
      pfAT->setConfigMask(fCMask);


      uint16_t fGPSWords[5];
      uint8_t  fGPSYear=6;   
      fEventTime.getForVBF(fGPSYear,5,fGPSWords);
			  // SEtGPS Time, I know it looks like a get but its 
			  //not
      pfAT->getGPSTime()[0]=fGPSWords[0];
      pfAT->getGPSTime()[1]=fGPSWords[1];
      pfAT->getGPSTime()[2]=fGPSWords[2];
      pfAT->getGPSTime()[3]=fGPSWords[3];
      pfAT->getGPSTime()[4]=fGPSWords[4];
			  
      pfAT->setGPSYear(fGPSYear);
			  
      // **************************************************
      // Now copy over and fix telescope events.
      // *************************************************
      VPacket** pfTelReadPacket = new VPacket*[fNumTelsWithData]; 
      for(int i=0;i<fNumTelsWithData;i++)
	{
	  int fTelID=pfTelsInArray[i]->fTelID;
	  // set the event number
	  pfTelReadPacket[i]=pfTelsInArray[i]->
	                                   readPacket(fPedIndex);
	  VArrayEvent* pfAEIn=pfTelReadPacket[i]->getArrayEvent();
 	  VEvent* pfEvent=pfAEIn->getEvent(0);  //Only one telescope in input 
	                                        //file, its a 0;
	  pfEvent->setEventNumber(fOutEventIndex);

	  pfEvent->setNodeNumber(fTelID);
	  pfAT->setTriggerTelescopeId(i,fTelID);

	  pfEvent->getGPSTime()[0]=fGPSWords[0];
	  pfEvent->getGPSTime()[1]=fGPSWords[1];
	  pfEvent->getGPSTime()[2]=fGPSWords[2];
	  pfEvent->getGPSTime()[3]=fGPSWords[3];
	  pfEvent->getGPSTime()[4]=fGPSWords[4];
			      
	  pfEvent->setGPSYear(fGPSYear);
	  // add the event to the array event!
	  VEvent* pfWriteEvent=pfEvent->copyEvent();
	  pfAEOut->addEvent(pfWriteEvent);
	}

      // now put array trigger back into the array event
      VArrayTrigger* pfWriteAT = pfAT->copyAT();
      pfAEOut->setTrigger(pfWriteAT);

      // put the array event back into the packet
      // I'm told this will be a replacement
      pfWritePacket->putArrayEvent(pfAEOut);
      
      // finally, write the packet into the file
      VBankFileWriter* pfWriter=pfVBFOut->getVBFFilePtr();
      if (!pfWritePacket->
	  has(VGetKascadeSimulationDataBankName())  )
	{
	  std::cout<<"ksArrayTrigger:SavePedestalEvent: Missing "
	    "SimulationDataBank at packet#: "<<fOutEventIndex<<std::endl;
	} 
      if (!pfWritePacket->hasArrayEvent())
	{
	  std::cout<<"ksArrayTrigger:SavePedestalEvent: Missing ArrayEvent "
	    " at packet#: "<<fOutEventIndex<<std::endl;
	} 
      //std::cout<<"Ped Packet at: "<<fOutEventIndex<<std::endl;
      pfWriter->writePacket(fOutEventIndex,pfWritePacket);
      fOutEventIndex++;
      delete pfReadPacket;
      delete pfWritePacket;
      delete []pfTelReadPacket;
    }
  fPedEventCount++;
  return;
}
// *************************************************************************

