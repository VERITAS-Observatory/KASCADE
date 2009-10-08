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

KSArrayEvent::KSArrayEvent(std::string fOutputFileName,
			   KSArrayTriggerDataIn* pDataIn)
{
  pfDataIn=pDataIn;
  fDataType=pfDataIn->fDataType;
  fRunNumber=pfDataIn->fRunNumber;
  fNoMorePedEvents=false;

  // ****************************************************************
  // Open the input files, check we have enough telescopes for multiplicity
  // ****************************************************************
  // Note that while these files all share the same mount direction vectors
  // they may not have the same triggers. We thus make no assumptions about
  // correlations of eventIndex between input files.
  // ****************************************************************
  // For VBF files build the configMask.
  // ****************************************************************
  // Try each telescope
  pfTelsInArray.clear();
  pfTelsWithData.clear();

  std::cout<<"KSArrayEvent: Open Input Single Tel files"<<std::endl;

  KSTelescope* pfT1 = new KSTelescope(E_T1,pfDataIn);
  std::string fConfigMask;
  if(pfT1->fFileExists)
    {
      pfTelsInArray.push_back(pfT1);
      if(pfT1->fNumEvents>0)
	{
	  pfTelsWithData.push_back(pfT1);
	  fConfigMask="0,";
	}
    }

  KSTelescope* pfT2 = new KSTelescope(E_T2,pfDataIn);
  if(pfT2->fFileExists)
    {
      pfTelsInArray.push_back(pfT2);
      if(pfT2->fNumEvents>0)
	{
	  pfTelsWithData.push_back(pfT2);
	  fConfigMask+="1,";
	}
    }

  KSTelescope* pfT3 = new KSTelescope(E_T3,pfDataIn);
  if(pfT3->fFileExists)
    {
      pfTelsInArray.push_back(pfT3);
      if(pfT3->fNumEvents>0)
	{
	  pfTelsWithData.push_back(pfT3);
	  fConfigMask+="2,";
	}
    }

  KSTelescope* pfT4 = new KSTelescope(E_T4,pfDataIn);
  if(pfT4->fFileExists)
    {
      pfTelsInArray.push_back(pfT4);
      if(pfT4->fNumEvents>0)
	{
	  pfTelsWithData.push_back(pfT4);
	  fConfigMask+="3,";
	}
    }

  fNumTelsWithData=pfTelsWithData.size();
  fNumTelsInArray=pfTelsInArray.size();
  if(fNumTelsInArray==0)
    {
      // *********************************
      //None of the input files exists.
      // *******************************
      std::cout<<"ksArrayTrigger: Found no existing input files."
	       <<std::endl;
      exit(1);
    }

  // ************************************************************************
  // Build the VBF ConfigMask. Common to all VBF events
  // For 123-M2 it would be 007
  // ************************************************************************
  std::cout<<"KAArrayEvent: Build configMask"<<std::endl;
  fCMask=toDAQMask(parseConfigMask(fConfigMask.c_str()) ); 

  // ***********************************************************************
  // If a file with an external set of telescope array positons has been
  // specified, replace the telescope positons with the new ones and load a
  // new FirstValidEventTime
  // ***********************************************************************
  VATime fFirstValidEventTime(gDefaultStartOfRunTime.c_str());
  if(pfDataIn->fTelescopePositionListFile!=" ")
    {
      LoadTelescopePositionsFromList(pfDataIn->fTelescopePositionListFile,
				     pfTelsInArray,fFirstValidEventTime);
    }
  else 
    {
                                     //Default if we can't find a time.
      if(fNumTelsWithData>0)
	{
	  fFirstValidEventTime = 
	    pfTelsWithData.at(0)->getFirstValidEventTime();
	}
    }
  fGPSYear=(uint8_t)(fFirstValidEventTime.getYear()-2000);



  // *****************************************************************
  // If we do not have telescopes with data we will not loop through things
  // but we do need to create an 'empty' output file so that we can 'count' 
  // the shower in our weighting scheme.
  // *****************************************************************

  // ***********************************************************************
  // Open the output file. 
  // *************************************************************************
  std::cout<<"KAArrayEvent: Open the OutPut file "<<fOutputFileName
	   <<std::endl;

  if(fDataType==ROOTFILE)
    {
      // **************
      // DataType is root
      // **************
      // Create and write the Run Header,ArrayInfo,
      // QStatsData. PixelsStatusData RelGainData and SimulationHeader
      // ******************************************************************  
      pfVDFOut= new KSArrayVDFFile(pfTelsWithData,fFirstValidEventTime);
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
      pfVBFOut->CreateVBFFile(fOutputFileName,(uint)fRunNumber);
    
      // Delay writing out header packet until we determine the new telescope
      // Array positions and fill the SimulationHeader with them. 
      // pfVBFOut->CopyOutHeaderPacket();
    }

  // **********************************************************************
  // Determine all nx,ny locations.
  DetermineTelescopeNxNy();

  if(fDataType==VBFFILE)
    {
      LoadInputSimHeaderWithTelescopePositions();
      pfVBFOut->CopyOutHeaderPacket();
    }

  PrintRelativeTelescopePositions();


  fBaseTelIndex=0; //Index to base telescope in pfTelsWithData for search.
  for(int i=1;i<fNumTelsWithData;i++)
    {
      //This sets each telescope Nx,ny offsets from the base tel
      SetTelescopeOffsetFromBaseTel(fBaseTelIndex,i);
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
	  for(int i=0;i<fNumTelsInArray;i++)
	    {
	      pfTelsInArray.at(i)->pfVDFEventFile->Close();
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
      pfTelsWithData.at(i)->makeGridDirMap();
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
	  fBaseIndex==pfTelsWithData.at(fBaseTelIndex)->fNumEvents-1) ||
	 (fDataType==VBFFILE&&
	  fBaseIndex==pfTelsWithData.at(fBaseTelIndex)->fNumEvents)    )
	{
	  std::cout<<"fBaseIndex,fNumEvents :"<<fBaseIndex<<" "
		   <<pfTelsWithData.at(fBaseTelIndex)->fNumEvents<<std::endl;
	  // *********************************************************
	  // We have finished looking for array triggers were this
	  // telscope (fBaseTelIndex) is in the trigger. If our required
	  // multiplicity allows us, switch to the next telescope as 
	  // the next telescope to use as our Base Telescope.
	  // *********************************************************
	  fBaseTelIndex++;

	  if((fNumTelsWithData-fBaseTelIndex)<
	                                  pfDataIn->fArrayTriggerMultiplicity)
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
	      SetTelescopeOffsetFromBaseTel(fBaseTelIndex,i);
	    }
	  continue;  //This allows for check on number events up above on 
	             //this tel
	}
      // ************************************************************
      // Now look for a trigger
      // *********************************************************
      fBaseIndex++;

      // ****************************************************************
      // Test that the next event in this telescope has not been 
      // used yet
      // *****************************************************************
      if(pfTelsWithData.at(fBaseTelIndex)->pfArrayEventsUsed.at(fBaseIndex))
	{
	  continue;  //go to next event
	}

      // ****************************************************************
      // Try using this event this telescope as a base event
      // ****************************************************************
      // Flag that we are using it (and so we don't try to use it again)
      pfTelsWithData.at(fBaseTelIndex)->pfArrayEventsUsed.at(fBaseIndex)=true;
      
      // Now test the other telescopes to see if they triggered.
      // Find nx,ny,fdir this telescope this index.
      int fNx;
      int fNy;
      int fDir;
      bool fTrigEvt;
      bool fPedEvt;
      
      pfTelsWithData.at(fBaseTelIndex)->
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
	  int fTelNx=fNx+pfTelsWithData.at(i)->GetNXOffset(fNy);
	  int fTelNy=fNy+pfTelsWithData.at(i)->GetNYOffset(fNx);

	  int64_t fKey=(int64_t)pfTelsWithData.at(0)->
	                                   makeGridDirKey(fTelNx,fTelNy,fDir);
	  
	  // **********************************************************
	  // See if this telescope has a nomal event with this GridKey
	  // This checks the map which has no ped events in it.
	  // **********************************************************
	  int fEventIndex=pfTelsWithData.at(i)->getIndexForGridDirKey(fKey);
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
	      pfTelsWithData.at(i)->pfArrayEventsUsed.at(fEventIndex)=true;
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
  int fFirstTrigTelIndex=fTriggerEvents.at(0).fTelIndex;
  int fFirstTrigTelEventIndex=fTriggerEvents.at(0).fEventIndex;
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
	  int fTrigTelIndex=fTriggerEvents.at(i).fTelIndex;
	  int fTrigTelEventIndex=fTriggerEvents.at(i).fEventIndex;
	  int fTelID=pfTelsWithData.at(fTrigTelIndex)->fTelID;
	  pfCalEvent->fPresentTels.at(fTelID)=true;
	  
	  // ****************************************************************
	  //Fill the Calibrated Telescope event
	  // ****************************************************************
	  VACalibratedArrayEvent* pfInCalEvent = pfTelsWithData.at(fTrigTelIndex)->
	    pfVDFEventFile->getCalibratedArrayEventPtr();
	  pfTelsWithData.at(fTrigTelIndex)->pfVDFEventFile->
	                                  loadInArrayEvent(fTrigTelEventIndex);
	  if(pfInCalEvent==NULL)
	    {
	      std::cout<<"ksSumFiles: Failed to "
		"getCalibratedArrayEventPtr"
		       <<std::endl;
	      exit(1);
	    }
	  
	  pfCalEvent->fTelEvents.at(i).fTelTime=fEventTime;
	  pfCalEvent->fTelEvents.at(i).fPointingData=pfInCalEvent->
	                                          fTelEvents.at(0).fPointingData;
	  pfCalEvent->fTelEvents.at(i).fTelID=pfTelsWithData.at(fTrigTelIndex)->
	                                                               fTelID;
	  pfCalEvent->fTelEvents.at(i).fChanData=
	                                 pfInCalEvent->fTelEvents.at(0).fChanData;
	}
      pfVDFOut->writeCalibratedArrayEvent(fNumTrigTel);

      // ************************************************
      // Now sim data: Just copy over the one from the first triggered tel.
      // ************************************************
      pfTelsWithData.at(fFirstTrigTelIndex)->pfVDFEventFile->
	                          readSimulationData(fFirstTrigTelEventIndex); 
     VAKascadeSimulationData*  pfKInSimEvent=
             pfTelsWithData.at(fFirstTrigTelIndex)->getKascadeSimulationDataPtr();
      *pfVDFKSimEvent=*pfKInSimEvent;
      pfVDFOut->writeSimulationData();
    }
  else if(fDataType==VBFFILE)
    {
      VPacket* pfWritePacket = new VPacket();
      // *********************************************************************
      // First fill in simulation packets. Use first triggered telescope
      // *********************************************************************
      VPacket* pfReadPacket=pfTelsWithData.at(fFirstTrigTelIndex)->
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




      // *************************************************
      // Fix up Kascade simulation data bank in this packet
      // *************************************************
      VKascadeSimulationData *pfKSimData = 
	                     pfReadPacket->get< VKascadeSimulationData >
			    (VGetKascadeSimulationDataBankName());
      VKascadeSimulationData *pfWriteKSimData = 
	                                    pfKSimData->copyKascadeSimData();
      pfWriteKSimData->fEventNumber=fOutEventIndex;
      pfWritePacket->put(VGetKascadeSimulationDataBankName(),
				pfWriteKSimData);  
    

      // **************************************************************
      // Find position of shower core relative to position relative to 0,0 of 
      // array.  Note that this must be relative to the positions of the array
      // we have just put into the sim header.
      // We do this in 2 steps:
      // 1: Find relative location of shower core relative to First Telescope
      //    in this event. Include any shower core offset here.
      //    This was set up orignally in the single file processing(ksAomega)
      //    VSimulationData::fCoreEastM and  VSimulationData::fCoreSouthM
      //    Core offset is already included.
      // 2: Take this relative location and modify it by the location given 
      //    in the sim header for this telescope to find the location of the 
      //    shower core relative to the center of the array.
      // *********************************************************************
      //  What is nx,ny of the first telescope in  this event?
      // *********************************************************************
      //int fNx;
      //int fNy;
      //int fDir;
      //bool fTrigEvt;
      //bool fPedEvt;
      //
      //pfTelsWithData.at(fFirstTrigTelIndex)->
      //getGridDirForIndex(fFirstTrigTelEventIndex, fNx, fNy, fDir, fTrigEvt,
      //		   fPedEvt);
     
      double fXM;
      double fYM;

      fXM = pfSimData->fCoreEastM;
      fYM = pfSimData->fCoreSouthM;

      // Need position of telescope 
      int telID=pfTelsWithData.at(fFirstTrigTelIndex)->fTelID;
      VSimulationHeader* pVBFSimHeader=  
	                        pfTelsWithData.at(0)->getSimulationHeaderPtr();
      fXM=fXM + pVBFSimHeader->fArray.at(telID).fRelTelLocEastM; //+ east
      fYM=fYM + pVBFSimHeader->fArray.at(telID).fRelTelLocSouthM;// + south

      //Refill this new position.
      pfWriteSimData->fCoreEastM  = fXM;
      pfWriteSimData->fCoreSouthM = fYM;
      pfWritePacket->put(VGetSimulationDataBankName(), pfWriteSimData);
      
      // *************************************************
      // Now the ArrayEvents
      // *************************************************
      // First fix times and event number in array Trigger
      // *********************************************
      VArrayEvent* pfAEOut  = new VArrayEvent();
			  
      VArrayEvent* pfAEIn = pfReadPacket->getArrayEvent();
      VArrayTrigger* pfAT = pfAEIn->getTrigger();
      float fAlt=pfAT->getAltitude(0);  //get stuff we need to propagate
      float fAz=pfAT->getAzimuth(0);

      // ****************************************************************
      //Resize things. Array trigger will contains fNumTrigTels telescopes
      // fNumTelsInArray is the number of telescopes included in the 
      // subarray. So fNumTrigTels will vary event to event. Example for
      // a 123-M2 trigger condition fNumTelsInArray will always be 3 and 
      // fNumTrigTels will be 2 or 3 changine event by event.
      // ****************************************************************
      pfAT->resizeSubarrayTelescopes(fNumTelsInArray);
      pfAT->resizeTriggerTelescopes(fNumTrigTel);
      
      // ********************************************************************
      // have to set the telescope ID's that this record corresponds to.
      // in this case, the record number and telescope ID happen to
      // be the same
      // *********************************************************************
      // Fill up the subarrytels vector: Goes over the subarray configuratrion
      // For 123-M2 this would be over T1 and T2 and T3. This, except for fALT 
      // and fAz, is identical event to event.
      // ********************************************************************
      VEventType fEvType;
      fEvType.trigger=VEventType::L2_TRIGGER;
      pfAT->setEventType(fEvType);

      for(int i=0;i<fNumTelsInArray;i++)
	{
	  int fTelID=pfTelsInArray.at(i)->fTelID;
	  pfAT->setSpecificEventType(i,fEvType);
	  pfAT->setSubarrayTelescopeId(i,fTelID);
	  pfAT->setShowerDelay(i,0);
	  pfAT->setCompDelay(i,0);
	  pfAT->setAltitude(i,fAlt);
	  pfAT->setAzimuth(i,fAz);
	  pfAT->setTDCTime(i,0);
	}
      // *******************************************************************
      // Set the config Mask. This also is constant event to event
      // For 123-M2 it will be 007
      // Determine here and set the triggerMask. This is a VDatum value and is
      // set in all AT and Event objects.
      // *******************************************************************
      std::string fTriggerMask;
      for(int i=0;i<fNumTrigTel;i++)
	{
	  int fTrigTelIndex=fTriggerEvents.at(i).fTelIndex;
	  int fTelID=pfTelsWithData.at(fTrigTelIndex)->fTelID;
	  pfAT->setTriggerTelescopeId(i,fTelID);

	  if(fTelID==E_T1)fTriggerMask+="0";
	  else if(fTelID==E_T2)fTriggerMask+="1";
	  else if(fTelID==E_T3)fTriggerMask+="2";
	  else if(fTelID==E_T4)fTriggerMask+="3";
	  fTriggerMask+=",";
  	}
      // Original AT from first triggered tel already has: node number=255,
      // run number(but may want to do this explictly later)

      unsigned short fTMask=toDAQMask(parseConfigMask(fTriggerMask.c_str()) ); 
      pfAT->setTriggerMask(fTMask);
      pfAT->setConfigMask(fCMask);

      // *********************************************************************
      // set the event number
      // ********************************************************************* 
      pfAT->setEventNumber(fOutEventIndex);
      pfAT->setRunNumber(fRunNumber);
      uint16_t fGPSWords[5];
      //uint8_t  fGPSYear=6;   
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
      for(int i=0;i<fNumTrigTel;i++)
	{
	  int fTrigTelIndex=fTriggerEvents.at(i).fTelIndex;
	  int fTrigTelEventIndex=fTriggerEvents.at(i).fEventIndex;
	  int fTelID=pfTelsWithData.at(fTrigTelIndex)->fTelID;
	  // set the event number
	  VPacket* pfTelReadPacket = pfTelsWithData.at(fTrigTelIndex)->
	                                   readPacket(fTrigTelEventIndex);
	  VArrayEvent* pfAEIn=pfTelReadPacket->getArrayEvent();
 	  VEvent* pfEvent=pfAEIn->getEvent(0);
	  pfEvent->setEventNumber(fOutEventIndex);
	  pfEvent->setNodeNumber(fTelID);
	  pfEvent->setTriggerMask(fTMask);
	  
	  pfEvent->getGPSTime()[0]=fGPSWords[0];
	  pfEvent->getGPSTime()[1]=fGPSWords[1];
	  pfEvent->getGPSTime()[2]=fGPSWords[2];
	  pfEvent->getGPSTime()[3]=fGPSWords[3];
	  pfEvent->getGPSTime()[4]=fGPSWords[4];
			      
	  pfEvent->setGPSYear(fGPSYear);

	  pfEvent->setEventType(fEvType);

	  // add the event to the array event!
	  VEvent* pfWriteEvent= pfEvent->copyEvent();
	  pfAEOut->addEvent(pfWriteEvent);
	  delete pfTelReadPacket;
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
      int fPedListIndex= pfTelsWithData.at(0)->fPedListIndex;
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
	  pfTelsWithData.at(i)->pfVDFEventFile->Close();
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
  if(fNoMorePedEvents)
    {
      return;
    }
  //Get time onf the event (on the tick)
  uint32_t fYear,fMonth,fDay,H,M,S,NS;
  fEventTime.getCalendarDate(fYear,fMonth,fDay);
  fEventTime.getTime(H,M,S,NS);
  NS=0;   //On the tick!
  VATime fPedestalEventTime;
  fPedestalEventTime.setFromCalendarDateAndTime(fYear,fMonth,fDay,
						H,M,S,NS);
  // *******************************************************************
  // We have to make sure we have a pedestal event avaiable from each 
  // telescope's list of ped events.
  // *******************************************************************
  pfTelsWithData.at(0)->fPedListIndex++;  //Bump to the next one to use.
  int fPedListIndex=pfTelsWithData.at(0)->fPedListIndex;
  for(int i=0;i<fNumTelsWithData;i++)
    {
      if(fPedListIndex>=(int)pfTelsWithData.at(i)->pfPedIndexList.size()-1)
	{
	  std::cout<<"ksArrayTrigger: No More Pedestal events avaiable from T"
		   <<i+1<<" at SavePedestalEvent request: "<<fPedListIndex
		   <<std::endl;
          fNoMorePedEvents=true;
	  return;          //No more ped events to use.
	}
    }

  if(fDataType==ROOTFILE)
    {
      int fPedIndex= pfTelsWithData.at(0)->pfPedIndexList.at(fPedListIndex); 

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
	  int fTelID=pfTelsWithData.at(i)->fTelID;
	  pfCalEvent->fPresentTels.at(i)=true;
	  
	  // ****************************************************************
	  //Fill the Calibrated Telescope event
	  // ****************************************************************
	  VACalibratedArrayEvent* pfInCalEvent = pfTelsWithData.at(i)->
	    pfVDFEventFile->getCalibratedArrayEventPtr();
	  pfTelsWithData.at(i)->pfVDFEventFile->
	                      loadInArrayEvent(fPedIndex);
	  if(pfInCalEvent==NULL)
	    {
	      std::cout<<"ksSumFiles: Failed to "
		"getCalibratedArrayEventPtr for Pedestal Event at index:"
		       <<fPedIndex<<std::endl;
	      exit(1);
	    }
	  
	  pfCalEvent->fTelEvents.at(i).fTelTime=fEventTime;
	  pfCalEvent->fTelEvents.at(i).fPointingData=pfInCalEvent->
	                                      fTelEvents.at(0).fPointingData;
	  pfCalEvent->fTelEvents.at(i).fTelID=fTelID;
	  pfCalEvent->fTelEvents.at(i).fChanData=
	                             pfInCalEvent->fTelEvents.at(0).fChanData;
	}
      pfVDFOut->writeCalibratedArrayEvent(fNumTelsWithData);

      // ************************************************
      // Now sim data: Just copy it over from first telescope
      // ************************************************
      pfTelsWithData.at(0)->pfVDFEventFile->readSimulationData(fPedIndex); 
      VAKascadeSimulationData*  pfKInSimEvent=
             pfTelsWithData.at(0)->getKascadeSimulationDataPtr();
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
      int fPedIndex= pfTelsWithData.at(0)->pfPedIndexList.at(fPedListIndex); 
      VPacket* pfReadPacket=pfTelsWithData.at(0)->
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

      VEventType fEvType;
      fEvType.trigger=VEventType::PED_TRIGGER;
      pfAT->setEventType(fEvType);

      std::string fTriggerMask;
      for(int i=0;i<fNumTelsWithData;i++)
	{
	  int fTelID=pfTelsWithData.at(i)->fTelID;
	  pfAT->setSubarrayTelescopeId(i,fTelID);
	  pfAT->setTriggerTelescopeId(i,fTelID);
	  pfAT->setSpecificEventType(i,fEvType);
	  pfAT->setShowerDelay(i,0);
	  pfAT->setCompDelay(i,0);
	  pfAT->setAltitude(i,fAlt);
	  pfAT->setAzimuth(i,fAz);
	  pfAT->setTDCTime(i,0);


	  if(fTelID==E_T1)fTriggerMask+="0";
	  else if(fTelID==E_T2)fTriggerMask+="1";
	  else if(fTelID==E_T3)fTriggerMask+="2";
	  else if(fTelID==E_T4)fTriggerMask+="3";
	  fTriggerMask+=",";
	}
      // Original AT from first triggered tel already has: node number=255,
      // run number(but may want to do this explictly later)
      pfAT->setRunNumber(fRunNumber);
   
      // set the event number
      pfAT->setEventNumber(fOutEventIndex);

      unsigned short fTMask=toDAQMask(parseConfigMask(fTriggerMask.c_str()) ); 
      pfAT->setTriggerMask(fTMask);
      pfAT->setConfigMask(fCMask);


      uint16_t fGPSWords[5];
      //uint8_t  fGPSYear=6;   
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
      for(int i=0;i<fNumTelsWithData;i++)
	{
	  int fTelID=pfTelsWithData.at(i)->fTelID;
	  // set the event number
	  int fPedIndex= 
	               pfTelsWithData.at(i)->pfPedIndexList.at(fPedListIndex); 

	  VPacket* pfTelReadPacket=pfTelsWithData.at(i)->
	                                   readPacket(fPedIndex);
	  VArrayEvent* pfAEIn=pfTelReadPacket->getArrayEvent();
 	  VEvent* pfEvent=pfAEIn->getEvent(0);  //Only one telescope in input 
	                                        //file, its a 0;
	  pfEvent->setEventNumber(fOutEventIndex);
	  pfEvent->setNodeNumber(fTelID);
	  pfEvent->setTriggerMask(fTMask);

	  pfEvent->getGPSTime()[0]=fGPSWords[0];
	  pfEvent->getGPSTime()[1]=fGPSWords[1];
	  pfEvent->getGPSTime()[2]=fGPSWords[2];
	  pfEvent->getGPSTime()[3]=fGPSWords[3];
	  pfEvent->getGPSTime()[4]=fGPSWords[4];
			      
	  pfEvent->setGPSYear(fGPSYear);

	  pfEvent->setEventType(fEvType);

	  // add the event to the array event!
	  VEvent* pfWriteEvent=pfEvent->copyEvent();
	  pfAEOut->addEvent(pfWriteEvent);
	  delete pfTelReadPacket;
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
    }
  fPedEventCount++;
  return;
}
// *************************************************************************

void KSArrayEvent::DetermineTelescopeNxNy()
// ************************************************************************
// Determine the Nx, Ny values of the telescopes to the best of our ability
// This is a weak point of this method as its hard to fit them to a great fit 
// especially when we have low zenith pointings (makes for large xseg or 
// ysegs) or when we have telescopes large (agasin gives big xseg and yseg)
// or if the telescopes are close to each other. This could easily have 2 
// telescopes (t1 and t4) end up on top of each other. Good reason to point 
// to the east when looking at low zenith angles.
// ************************************************************************
{
  // ***********************************************************************
  // We have the actual telescope locations from the sim header bank and the
  // xseg and yseg from the KascadeSimHeader bank. These were read in when we 
  // created the telescope object. Use values from first telescope.
  // ***********************************************************************
  // We should do a minimization here. The quatity we want to minimize is the 
  // the error in aproximating the telescope positions to a praticular set of 
  // Nx,Ny values. What we can vary is the origin of the array. Nominally its 
  // at arrayX,arrayY=0,0 but we are free to vary this by +/- xseg/2 and 
  // +/-yseg/2.
  // Note: If this is not a square array  we have to worry about both odd and 
  //       even sets.
  // ************************************************************************
  // Brute force way to search is to try all values of arrayX, arrayY over 
  // the range specified above. Stepping maybe 30 times each for a total of 900
  // steps. This should take that much time.

  // *************************************************************************
  // For SquareGrid use Set1
  // *************************************************************************

  pfTelNXSet1.resize(4);
  pfTelNXSet2.resize(4);
  pfTelNYSet1.resize(4);
  pfTelNYSet2.resize(4);

  std::vector<int> pfNXSet1(4);
  std::vector<int> pfNYSet1(4);
  std::vector<int> pfNXSet2(4);
  std::vector<int> pfNYSet2(4);

  double fBestError=-1;
  int fNumSteps=30;
  double fXSeg=pfTelsInArray.at(0)->fXAreaWidthM;
  double fYSeg=pfTelsInArray.at(0)->fYAreaWidthM;

  // ********************************************************************
  std::cout<<"ksArrayTrigger: Z differences of Telescope positions ignored "
	   <<std::endl;
 
  for(int i=0;i<fNumSteps;i++)
    {
      double fArrayX=i*fXSeg/fNumSteps-fXSeg/2.0;
      for(int j=0;j<fNumSteps;j++)
	{
	  double fArrayY=j*fYSeg/fNumSteps-fYSeg/2.0;
	  // ***********************************************************
	  // GetTelsNxNy fills only Set1 for SquareGrid. Fills both sets for
	  // Triangular grid
	  // ***********************************************************
	  double fArrayError=GetTelsNxNy(fArrayX,fArrayY,pfNXSet1,pfNYSet1,
					 pfNXSet2,pfNYSet2);
	  if(fArrayError<fBestError || fBestError<0)
	    {
	      fBestError=fArrayError;
	      pfTelNXSet1=pfNXSet1;
	      pfTelNYSet1=pfNYSet1;
	      pfTelNXSet2=pfNXSet2;
	      pfTelNYSet2=pfNYSet2;
	      fBestArrayX=fArrayX;
	      fBestArrayY=fArrayY;
	    }
	}
    }
  return;
}
// ***********************************************************************

double KSArrayEvent::GetTelsNxNy(double fArrayX,double fArrayY,
				 std::vector<int>& pfNXSet1,
				 std::vector<int>& pfNYSet1,
				 std::vector<int>& pfNXSet2,
				 std::vector<int>& pfNYSet2)
// *************************************************************************
// For the array origen cenetered at fArrayX,fArrayY, find the set of nx,ny 
// values for the telescopes.This is whats done for the SquareGrid.  For the 
// triangular grid, to later (DetermineOffsets) have 
// the ability to use even vs odd, recenter the origen at fArrayY+fYSeg and 
// get the nx,ny values for that configuration. Then caculate the 'binning' 
// error (squared) (for existing tels in array only) and return that.
// *************************************************************************
// Ignore Z for now.  Introduces correction later that may be of interest
//  Note KASCADE convention +x axis is east, +y axis is south
//  Note VEGAS convention +x axis is east, +y axis is north
// ************************************************************************
{

  bool fSquareGrid     = pfTelsInArray.at(0)->fSquareGrid;
  bool fNorthSouthGrid = pfTelsInArray.at(0)->fNorthSouthGrid;
  //bool fEastWestGrid   = pfTelsInArray.at(0)->fEastWestGrid;
  if(!fNorthSouthGrid && !fSquareGrid)
    {
      std::cout<<"ksArrayTrigger: Only Square orNorth-South triangular grid "
	"are valid presently"<<std::endl;
      exit(1);
    }
  // ************************************************************************
  // Only SquareGrid or North-South Triangular grid are valid
  // Set 1 first:
  // ************************************************************************
  //Find nx,ny for all tels.
  double fXSeg=pfTelsInArray.at(0)->fXAreaWidthM;
  double fYSeg=pfTelsInArray.at(0)->fYAreaWidthM;
  int fNX=0;
  int fNY=0;
  for(int i=0;i<4;i++)
    {
      double fXM=pfTelsInArray.at(0)->fXPositionsM.at(i)+fArrayX;
      double fYM=pfTelsInArray.at(0)->fYPositionsM.at(i)+fArrayY;
      //For any fNX on N-S grid: no changes
      fNX= (int)(fabs(fXM)/fXSeg+.5);  //.5 is for the round down.
      if(fXM<0)
	{
	  fNX=-fNX;
	}

      // **************************************************************
      // Now find fNY.
      // **************************************************************
      if(pfTelsInArray.at(0)->fSquareGrid)
	{
	  fNY= (int)(fabs(fYM)/fYSeg+.5); //.5 is for the round down.
	  if(fYM<0)
	    {
	      fNY=-fNY;
	    }
	}

      // **************************************************************
      // For NorthSouth Triangular grid NX  Odd even NY are different
      // **************************************************************
      if(pfTelsInArray.at(0)->fNorthSouthGrid)
	{

	  // NX even on N-S grid: Ny has no shift 
	  if(fNX%2==0)                  // % is the C++ mod operator
	    {
	      fNY= (int)(fabs(fYM)/fYSeg+.5); //.5 is for the round down.
	      if(fYM<0)
		{
		  fNY=-fNY;
		}
	    }
	  else
	    {                                // NY Odd on N-S grid:
	      fNY= (int)(fabs(fYM)/fYSeg);
	      if(fYM<0)
		{
		  fNY=-fNY-1;
		}
	    }
	}
      pfNXSet1.at(i)=fNX;
      pfNYSet1.at(i)=fNY;
    }

  // **********************************************************************
  // Now Set2. for NorthSouth Triangular grid:Shift over in NX.
  // **********************************************************************
  //  For a Square grid, defualt to set1. Don't expect set2 to be used when 
  // its a square grid but might as well put something there.
  // **********************************************************************
  for(int i=0;i<4;i++)
    {
      if(pfTelsInArray.at(0)->fSquareGrid)   
	{
	  fNX=pfNXSet1.at(i);
	  fNY=pfNYSet1.at(i);
	}
      else
	{
	  double fXM=pfTelsInArray.at(0)->fXPositionsM.at(i)+(fArrayX+fXSeg);
	  double fYM=pfTelsInArray.at(0)->fYPositionsM.at(i)+fArrayY;
	  //For any fNX on N-S grid: no changes
	  fNX= (int)(fabs(fXM)/fXSeg+.5); //.5 is for the round down.
	  if(fXM<0)
	    {
	      fNX=-fNX;
	    }
	  // **************************************************************
	  // For NX  Odd even NY are different
	  // **************************************************************
	  // NX even on N-S grid: Ny has no shift 
	  if(fNX%2==0)                  // % is the C++ mod operator
	    {
	      fNY= (int)(fabs(fYM)/fYSeg+.5); //.5 is for the round down.
	      if(fYM<0)
		{
		  fNY=-fNY;
		}
	    }
	  else
	    {                                // NY Odd on N-S grid:
	      fNY= (int)(fabs(fYM)/fYSeg);
	      if(fYM<0)
		{
		  fNY=-fNY-1;
		}
	    }
	}

      pfNXSet2.at(i)=fNX;
      pfNYSet2.at(i)=fNY;
    }

  // **********************************************************************
  // Now determine error of these. Only look at all tels so that we get the
  // same result for all showers.
  // **********************************************************************
  // First find CM of present confiugurations
  // **********************************************************************
  // Actual array
  double fCMX=0;
  double fCMY=0;
  int fCount=0;
  for(int j=0;j<4;j++)
    {
       fCMX+=pfTelsInArray.at(0)->fXPositionsM.at(j);
      fCMY+=pfTelsInArray.at(0)->fYPositionsM.at(j);
      fCount++;
    }
  //This shold be 0,0
  fCMX=fCMX/fCount;
  fCMY=fCMY/fCount;

  // Set1
  double fSet1CMX=0;
  double fSet1CMY=0;
  fCount=0;
  for(int j=0;j<4;j++)
    {
      double fXFromNX;
      double fYFromNY;
      GetXYFromNXNY(pfNXSet1.at(j), pfNYSet1.at(j), fXFromNX, fYFromNY);
      fSet1CMX+=fXFromNX;
      fSet1CMY+=fYFromNY;
      fCount++;
    }
  fSet1CMX=fSet1CMX/fCount;
  fSet1CMY=fSet1CMY/fCount;

  double fSet2CMX=0;
  double fSet2CMY=0;
  // ********************************************************************
  // Set 2 only if triangular grid
  // *******************************************************************
  if(pfTelsInArray.at(0)->fNorthSouthGrid)
    {
      fCount=0;
      for(int j=0;j<4;j++)
	{
	  double fXFromNX;
	  double fYFromNY;
	  GetXYFromNXNY(pfNXSet2.at(j), pfNYSet2.at(j), fXFromNX, fYFromNY);
	  fSet2CMX+=fXFromNX;
	  fSet2CMY+=fYFromNY;
	  fCount++;
	}
      fSet2CMX=fSet2CMX/fCount;
      fSet2CMY=fSet2CMY/fCount;
    }
  // **********************************************************************
  // Not clear what is the best way to find the difference between these 2 
  // arrays (actual and grid). So try looking at CM=0 positions differences
  // Probably something better to do but this is the best I can think of this 
  // morning.
  // ***********************************************************************
  double fSumError2=0;
  fCount=0;
  for(int j=0;j<4;j++)
    {
      double fXM=pfTelsInArray.at(0)->fXPositionsM.at(j);
      double fYM=pfTelsInArray.at(0)->fYPositionsM.at(j);
      double fXFromNX;
      double fYFromNY;
      GetXYFromNXNY(pfNXSet1.at(j), pfNYSet1.at(j), fXFromNX, fYFromNY);

      double xDifference=((fXM-fCMX)-(fXFromNX-fSet1CMX));
      double yDifference=((fYM-fCMY)-(fYFromNY-fSet2CMY));
      fSumError2+=sqrt((xDifference*xDifference)+(yDifference*yDifference));
      fCount++;

      // ******************************************************************
      // For NorthSouth triangular gird add in error for set2. Ignore for 
      // Square Grid
      // *******************************************************************
      if(pfTelsInArray.at(0)->fNorthSouthGrid)
	{
	  fXM=pfTelsInArray.at(0)->fXPositionsM.at(j);
	  fYM=pfTelsInArray.at(0)->fYPositionsM.at(j)-fCMY;
	  GetXYFromNXNY(pfNXSet2.at(j), pfNYSet2.at(j),fXFromNX, fYFromNY);
	  double xDifference=((fXM-fCMX)-(fXFromNX-fSet2CMX));
	  double yDifference=((fYM-fCMY)-(fYFromNY-fSet2CMY));
	  fSumError2+=sqrt((xDifference*xDifference)+
			                          (yDifference*yDifference));
	  fCount++;
	}
    }
  return fSumError2/fCount;
}
// **************************************************************************

void KSArrayEvent::GetXYFromNXNY(int fNx, int fNy, double& fX, double& fY)
// ************************************************************************
// Get X,Y coords of center of NX,NY grid areas. This is for Square Grid or
// North-South triangular arrays only.
// ************************************************************************
{
  double fXSeg=pfTelsInArray.at(0)->fXAreaWidthM;
  double fYSeg=pfTelsInArray.at(0)->fYAreaWidthM;

  fX=fXSeg*fNx;

  if(pfTelsInArray.at(0)->fSquareGrid)
    {
      fY=fYSeg*fNy;
    }
  else
    {
      // **************************************************************
      // Check to see if we are on odd or even column 
      // **************************************************************
      if(fNx%2==0) 
	{                   //Nx even
	  fY=fYSeg*fNy;
	}
      else
	{
	  fY=fYSeg*(fNy+.5);
	}
    }
  return;
}
// ***************************************************************************

void KSArrayEvent::SetTelescopeOffsetFromBaseTel(int fBaseTelIndex, 
						 int fTelIndex)
 // **************************************************************************
 // Determine and set the Nx,Ny (odd and Even ) offsets for telescope
 // fTelId from  the Base Telescope
 // **************************************************************************
{   
  int fBaseID=pfTelsWithData.at(fBaseTelIndex)->fTelID;
  int fBaseSet1NX=pfTelNXSet1.at(fBaseID);

  int fTelID=pfTelsWithData.at(fTelIndex)->fTelID;

  pfTelsWithData.at(fTelIndex)->fNXOffset= pfTelNXSet1.at(fTelID)-fBaseSet1NX;
  pfTelsWithData.at(fTelIndex)->fNXOffsetEven= 
                                      pfTelsWithData.at(fTelIndex)->fNXOffset;
  pfTelsWithData.at(fTelIndex)->fNXOffsetOdd= 
                                      pfTelsWithData.at(fTelIndex)->fNXOffset;

  // ************************************************************************
  // Check Odd and Evenness on the base telescope position
  // ************************************************************************
  int fNYOffsetEven = 0;
  int fNYOffsetOdd  = 0;
  if(pfTelsInArray.at(0)->fSquareGrid)
    {
      pfTelsWithData.at(fTelIndex)->fNYOffset= 
	                     pfTelNYSet1.at(fTelID)-pfTelNYSet1.at(fBaseID);
      pfTelsWithData.at(fTelIndex)->fNYOffsetEven=
	                             pfTelsWithData.at(fTelIndex)->fNYOffset;
      pfTelsWithData.at(fTelIndex)->fNYOffsetOdd=
                                     pfTelsWithData.at(fTelIndex)->fNYOffset;
    }
  else 
    {
      if(pfTelsInArray.at(0)->fNorthSouthGrid)
	{
	  if(fBaseSet1NX%2==0)
	    {
	      // *************************************************************
	      // Set1 has the fBaseID telescope on an even NX
	      // *************************************************************
	      fNYOffsetEven= pfTelNYSet1.at(fTelID)-pfTelNYSet1.at(fBaseID);
	      fNYOffsetOdd= pfTelNYSet2.at(fTelID)-pfTelNYSet2.at(fBaseID);
	    }
	  else
	    {
	      // ************************************************************
	      // Set1 has the fBaseID telescope on an Odd NX
	      // ************************************************************
	      fNYOffsetOdd= pfTelNYSet1.at(fTelID)-pfTelNYSet1.at(fBaseID);
	      fNYOffsetEven= pfTelNYSet2.at(fTelID)-pfTelNYSet2.at(fBaseID);
	    }
	  pfTelsWithData.at(fTelIndex)->fNYOffsetEven=fNYOffsetEven;
	  pfTelsWithData.at(fTelIndex)->fNYOffsetOdd=fNYOffsetOdd;
	}
    }
  return;
}

// **************************************************************************

void KSArrayEvent::PrintRelativeTelescopePositions()
// ************************************************************************
// As a check to see how far off we are print out the actual and set1 and set2
// telescope positons.  Use Vegas coord system.
// *************************************************************************
{
  // ***********************************************************************

  double fXMA=0;
  double fYMA=0;
  for(int j=0;j<4;j++)
    {
      fXMA=fXMA+pfTelsInArray.at(0)->fXPositionsM.at(j);
      fYMA=fYMA+pfTelsInArray.at(0)->fYPositionsM.at(j);
    }
  fXMA=fXMA/4;
  fYMA=fYMA/4;
  std::cout<<"Actual ArrayCenter: X,Y: "<<fXMA<<","<<fYMA<<std::endl;

  double fXFromNX;
  double fYFromNY;
  double fXM1=0;
  double fYM1=0;
  for(int j=0;j<4;j++)
    {
      GetXYFromNXNY(pfTelNXSet1.at(j), pfTelNYSet1.at(j), fXFromNX, fYFromNY);
      fXM1=fXM1+fXFromNX;
      fYM1=fYM1+fYFromNY;
    }
  fXM1=fXM1/4;
  fYM1=fYM1/4;
  std::cout<<"Set1 ArrayCenter: X,Y: "<<fXM1<<","<<fYM1<<std::endl;

  double fXM2=0;
  double fYM2=0;
  for(int j=0;j<4;j++)
    {
      GetXYFromNXNY(pfTelNXSet2.at(j), pfTelNYSet2.at(j), fXFromNX, fYFromNY);
      fXM2=fXM2+fXFromNX;
      fYM2=fYM2+fYFromNY;
    }
  fXM2=fXM2/4;
  fYM2=fYM2/4;
  std::cout<<"Set2 ArrayCenter: X,Y: "<<fXM2<<","<<fYM2<<std::endl;

  std::cout<<"Telescope posistions:"<<std::endl;
  std::cout<<"Tel:Actual:SimHeader:EvenSet:OddSet"<<std::endl;
  VSimulationHeader* pVBFSimHeader=  
	                        pfTelsInArray.at(0)->getSimulationHeaderPtr();
  for(int j=0;j<4;j++)
    {
      double fXM=pfTelsInArray.at(0)->fXPositionsM.at(j)-fXMA;
      double fYM=pfTelsInArray.at(0)->fYPositionsM.at(j)-fYMA;
      std::cout<<j<<": "<<fXM<<" "<<fYM;

      double fXS=pVBFSimHeader->fArray.at(j).fRelTelLocEastM;
      double fYS=pVBFSimHeader->fArray.at(j).fRelTelLocSouthM;
      std::cout<<j<<": "<<fXS<<" "<<fYS;

      GetXYFromNXNY(pfTelNXSet1.at(j), pfTelNYSet1.at(j), fXFromNX, fYFromNY);
      std::cout<<" :   "<<fXFromNX-fXM1<<" "<<fYFromNY-fYM1;

      GetXYFromNXNY(pfTelNXSet2.at(j), pfTelNYSet2.at(j),fXFromNX, fYFromNY);
      std::cout<<" :   "<<fXFromNX-fXM2<<" "<<fYFromNY-fYM2
	       <<std::endl;
    }
  return;
}
// ***************************************************************************

void KSArrayEvent::LoadInputSimHeaderWithTelescopePositions()
// ***************************************************************************
// Load the telescope positions into of the Simulation input VBF file Header 
// packets (Tel[0] wiil be used as output packet) using offsets Set1. Ignore 
// Set2 for North-South(thats the breaks bro!). These positions will be used 
// by vegas (so better just be SquareGrid for now). 
// ***************************************************************************
{
  // ***********************************************************************
  // Iterate through all Tels In array.
  // ***********************************************************************
  double fXFromNX;
  double fYFromNY;
  double fXM1=0;
  double fYM1=0;
  // ****************************************************
  // First find CM.
  // ****************************************************
  for(int j=0;j<4;j++)
    {
      GetXYFromNXNY(pfTelNXSet1.at(j), pfTelNYSet1.at(j), fXFromNX, 
		    fYFromNY);
      fXM1=fXM1+fXFromNX;
      fYM1=fYM1+fYFromNY;
    }
  fXM1=fXM1/4;
  fYM1=fYM1/4;

  // ******************************************************
  // Now fill each tlescopes simulation header
  // Get positio of each telescope. then put that position in all simheaders
  for(int i=0;i<4;i++)
    {
      GetXYFromNXNY(pfTelNXSet1.at(i), pfTelNYSet1.at(i), fXFromNX, fYFromNY);
      fXFromNX=fXFromNX-fXM1;   //Move CM to 0,0
      fYFromNY=fYFromNY-fYM1;
      for(int j=0;j<(int)pfTelsInArray.size();j++)
	{
	  VSimulationHeader* pVBFSimHeader=  
	                        pfTelsInArray.at(j)->getSimulationHeaderPtr();
	  pVBFSimHeader->fArray.at(i).fRelTelLocEastM=fXFromNX;
	  pVBFSimHeader->fArray.at(i).fRelTelLocSouthM=fYFromNY;
	  pVBFSimHeader->fArray.at(i).fRelTelLocUpM=0.0;
	}
    }
  return;
}
// ***************************************************************************
void KSArrayEvent::LoadTelescopePositionsFromList(
				  std::string fTelescopePositionListFile,
				  std::vector<KSTelescope*>& pfTelsInArray,
				  VATime& FirstValidEventTime)
// ************************************************************************
{
  ifstream in(pfDataIn->fTelescopePositionListFile.c_str());
  if(!in)
    {
      std::cout<<"ksArrayTrigger::LoadTelescopePositionsFromList - Can't "
	"open the specified Telescope Array positions file: "
	       <<pfDataIn->fTelescopePositionListFile<<std::endl;
      std::cout<<"ksArrayTrigger::LoadTelescopePositionsFromList - File "
	"may not exist"<<std::endl;
      exit(EXIT_FAILURE);
    }

  // ********************************************************************
  // Read in the time. Use this as first valid event time of simulation run
  // ********************************************************************
  std::string baseDate;
  std::string baseTime;
  std::string base;

  in>>baseDate>>baseTime>>base;
  std::string eventTime=baseDate + " " + baseTime + " " + base;
  FirstValidEventTime.setFromString(eventTime);


  // ********************************************************************
  // Read in the positions
  // ********************************************************************
  // For values in a VAArrayInfo positionEW:East is positive,
  //  positionNS: North is positive, positionUD: Up is positive
  // ********************************************************************
  for(int iTel=0;iTel<4;iTel++)  // In list file: East is positive
    {                            // North is positive
      double XEast;
      double YNorth;
      double ZUp;
      in>>XEast>>YNorth>>ZUp;
      if(in.eof())
	{
	  std::cout<<"ksArrayTrigger::LoadTelescopePositionsFromList - "
	    "Did not find the positions of 4 telescopes "
	    "specified in "
		   <<pfDataIn->fTelescopePositionListFile.c_str()
		   <<std::endl;
	  std::cout<<"ksArrayTrigger::LoadTelescopePositionsFromList - "
	    "This is required"<<std::endl;
	  exit(EXIT_FAILURE);
	}
      // ****************************************************************
      // We load all telescopes with all telescope positons
      // *****************************************************************
      for(int i=0;i<(int)pfTelsInArray.size();i++)
	{
	  pfTelsInArray.at(i)->fXPositionsM.at(iTel)=XEast;
	  pfTelsInArray.at(i)->fYPositionsM.at(iTel)=-YNorth;
	  pfTelsInArray.at(i)->fZPositionsM.at(iTel)=ZUp;
	}
    }

  std::cout<<"ksArrayTrigger::LoadTelescopePositionsFromList - Array "
    "Telescope Positions loaded from file: "
	   <<pfDataIn->fTelescopePositionListFile<<std::endl;
  std::cout<<"ksArrayTrigger::LoadTelescopePositionsFromList - First Event "
    "time form file is: "<<FirstValidEventTime<<std::endl;
  return;
}
