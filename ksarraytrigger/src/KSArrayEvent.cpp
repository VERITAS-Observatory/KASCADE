//-*-mode:c++; mode:font-lock;-*-
/**
 * \class KSArrayEvent
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
  
  KSTelescope* pfT1 = new KSTelescope(E_T1,pfDataIn);
  if(pfT1->fIsInArray)
    {
      pfTelsInArray.push_back(pfT1);
    }
  KSTelescope* pfT2 = new KSTelescope(E_T2,pfDataIn);
  if(pfT2->fIsInArray)
    {
      pfTelsInArray.push_back(pfT2);
    }
  KSTelescope* pfT3 = new KSTelescope(E_T3,pfDataIn);
  if(pfT3->fIsInArray)
    {
      pfTelsInArray.push_back(pfT3);
    }
  KSTelescope* pfT4 = new KSTelescope(E_T4,pfDataIn);
  if(pfT4->fIsInArray)
    {
      pfTelsInArray.push_back(pfT4);
    }
  
  
  //Check we have enough for multiplicity
  fNumTels=pfTelsInArray.size();
  if(fNumTels<pfDataIn->fArrayTriggerMultiplicity)
    {
      std::cout<<"ksArrayTrigger: Too few telescopes specified to make "
	"multiplicty requirements"<<std::endl;
      exit(1);
    }
  if(fNumTels<2)
    {
      std::cout<<"ksArrayTrigger: Need at least 2 telescopes to make an array"
	       <<std::endl;
      exit(1);
    }
  
  VATime fFirstValidEventTime = pfTelsInArray[0]->getFirstValidEventTime();
  
  // **************************************************************************
  // Open the ouput file. 
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
      //pfVDFOut->WriteArrayInfo(); //This was created in the 
      //                            //CreateArrayVDFFile method
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
  
  // **********************************************************************
  // We need to make fGridDirMap's for all the telescopes except the first one
  // These maps associate a particular nx,ny,direction index combined 
  // togeteher as a key,  with the event's index (VBF: packet ID, 
  // VDF: EventIndex)
  // Also copy over the nx,ny offset info
  // **********************************************************************
  for(int i=1;i<fNumTels;i++)
    {
      pfTelsInArray[i]->makeGridDirMap();
    }
  
  fBaseTelIndex=0; //Index to base telescope in pfTelsInArray for search.
  
  // **********************************************************************
  // Determine all nx,ny offsets relative to this Base tel.
  for(int i=1;i<fNumTels;i++)
    {
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
}
// ***************************************************************************

KSArrayEvent::~KSArrayEvent()
{
  // Nothing to do yet
}
// **************************************************************************


bool KSArrayEvent::FindTrigger()
// ***************************************************************************
// Find the next trigger in the array. Return false if shower has ended
// ***************************************************************************
{
  while(1)   //Search for trigger
    {
      // ********************************************************************
      // First test that  this event in this telescope has not been used yet
      // ********************************************************************
      while(1)
	{
	   fBaseIndex++;
	   if(pfTelsInArray[fBaseTelIndex]->pfArrayEventsUsed[fBaseIndex])
	    {
	      fBaseIndex++;
	      if(fBaseIndex==pfTelsInArray[fBaseTelIndex]->fNumEvents)
		{
		  // *********************************************************
		  // We have finished looking for array triggers were this
		  // telscope (fBaseTelIndex) is in the trigger. If out 
		  // required
		  // multiplicity allows us switch to the next telescope as 
		  // the next telescope to use as our Base Telescope. Be 
		  // careful to not use events that have already been tested. 
		  // *********************************************************
		  fBaseTelIndex++;

		  if((fNumTels-fBaseTelIndex)
		     <pfDataIn->fArrayTriggerMultiplicity)
		    {
		      // *********************************************
		      // Were done. Shower is finished
		      // *********************************************
		      return false;
		    }
		  if(pfDataIn->fDataType==VBFFILE)
		    {
		      fBaseIndex=0;  //VBF data packets start a at index 1.
		    }
		  else if(pfDataIn->fDataType==ROOTFILE)
		    {
		      fBaseIndex=-1; //Root TTree data index startas at 0.
		    }
		  // *******************************************************
		  // Determine all nx,ny offsets relative to this new Base 
		  //tel.
		  // ********************************************************
		  for(int i=fBaseTelIndex+1;i<fNumTels;i++)
		    {
		      pfTelsInArray[i]->
			DetermineOffsets(pfTelsInArray[fBaseTelIndex]->fTelID);
		    }
		  continue;
		}
	    }
	
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
      
      pfTelsInArray[fBaseTelIndex]->
	getGridDirForIndex(fBaseIndex,fNx,fNy,fDir);

      fTriggerEvents.clear();
      TrigEvent fTEvent;
      fTEvent.fEventIndex=fBaseIndex;
      fTEvent.fTelIndex=fBaseTelIndex;
      fTriggerEvents.push_back(fTEvent);
      
      for(int i=fBaseTelIndex+1;i<fNumTels;i++)
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
	  // See if this telescope has an event with this GridKey
	  // **********************************************************
	  int fEventIndex=pfTelsInArray[i]->getIndexForGridDirKey(fKey);
	  // Returns -1 if event doesn't exist.
	  if(fEventIndex>-1)
	    {
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
	  return true;   
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
      // Nothing here yet but there will be
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
      //pfSimData->fRunNumber=fRunNumber;
      pfSimData->fEventNumber=fOutEventIndex;
      pfWritePacket->put(VGetSimulationDataBankName(), pfSimData);  
      // *************************************************
      // Fix up Kascade simulation data bank in this packet
      // *************************************************
      VKascadeSimulationData *pfKSimData = 
	                     pfReadPacket->get< VKascadeSimulationData >
			    (VGetKascadeSimulationDataBankName());
      //pfKSimData->fRunNumber=fRunNumber;
      pfKSimData->fEventNumber=fOutEventIndex;
      pfWritePacket->put(VGetKascadeSimulationDataBankName(),
				pfKSimData);  
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
      pfAT->resizeSubarrayTelescopes(fNumTrigTel);
      pfAT->resizeTriggerTelescopes(fNumTrigTel);
      
      // ********************************************************************
      // have to set the telescope ID's that this record corresponds to.
      // in this case, the record number and telescope ID happen to
      // be the same
      for(int i=0;i<fNumTrigTel;i++)
	{
	  int fTrigTelIndex=fTriggerEvents[i].fTelIndex;
	  int fTel=pfTelsInArray[fTrigTelIndex]->fTelID;
	  pfAT->setSubarrayTelescopeId(i,fTel);
	  VEventType fEvType;
	  fEvType.trigger=VEventType::L2_TRIGGER;
	  pfAT->setSpecificEventType(i,fEvType);
	  pfAT->setShowerDelay(i,0);
	  pfAT->setCompDelay(i,0);
	  pfAT->setAltitude(i,fAlt);
	  pfAT->setAzimuth(i,fAz);
	  pfAT->setTDCTime(i,0);
	}
      // Original AT from first triggered tel already has: node number=255,
      // run number(but may want to do this explictly later)
      //  pfAT->setRunNumber(fRunNumber).  flags=0;
   
      // set the event number
      pfAT->setEventNumber(fOutEventIndex);
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
      // now put array trigger back into the array event
      pfAEOut->setTrigger(pfAT);
			  
      // **************************************************
      // Now copy over and fix telescope events.
      // *************************************************
      for(int i=0;i<fNumTrigTel;i++)
	{
	  int fTrigTelIndex=fTriggerEvents[i].fTelIndex;
	  int fTrigTelEventIndex=fTriggerEvents[i].fEventIndex;
	  int fTelID=pfTelsInArray[fTrigTelIndex]->fTelID;
	  // set the event number
	  VPacket* pfReadPacket=pfTelsInArray[fTrigTelIndex]->
	                                   readPacket(fTrigTelEventIndex);
	  VArrayEvent* pfAEIn=pfReadPacket->getArrayEvent();
 	  VEvent* pfEvent=pfAEIn->getEvent(0);
	  pfEvent->setEventNumber(fOutEventIndex);
	  pfEvent->setNodeNumber(fTelID);
	  
	  pfEvent->getGPSTime()[0]=fGPSWords[0];
	  pfEvent->getGPSTime()[1]=fGPSWords[1];
	  pfEvent->getGPSTime()[2]=fGPSWords[2];
	  pfEvent->getGPSTime()[3]=fGPSWords[3];
	  pfEvent->getGPSTime()[4]=fGPSWords[4];
			      
	  pfEvent->setGPSYear(fGPSYear);
	  // add the event to the array event!
	  pfAEOut->addEvent(pfEvent);
	}
      // put the array event back into the packet
      // I'm told this will be a replacement
      pfWritePacket->putArrayEvent(pfAEOut);
      
      // finally, write the packet into the file
      VBankFileWriter* pfWriter=pfVBFOut->getVBFFilePtr();
      pfWriter->writePacket(fOutEventIndex,pfWritePacket);
      delete pfWritePacket;
    }
  return;
}
// *********************************************************************  

void KSArrayEvent::Close()
// **********************************************************************
// Close files.
// **********************************************************************
{
  if(fDataType==ROOTFILE)
    {
      for(int i=0;i<fNumTels;i++)
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
