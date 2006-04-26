//-*-mode:c++; mode:font-lock;-*-
/**
 * \class KSEvent
 * \brief Class to hold and process an Event.
 *
 * Original Author: Glenn H. Sembroski
 * $Author$
 * $Date$
 * $Revision$
 * $Tag$
 *
 **/
#include "KSEvent.h"

extern "C" float   pran(float* dummy);
extern "C" double  Rexp(double fMeanIntervel);

KSEvent::KSEvent(KSTeFile* pTeFile, KSSegmentHeadData* pSegmentHead, 
		 KSPeHeadData* pPeHead, KSTeHeadData* pTeHead, 
		 KSAomegaDataIn* pDataIn)
{
  pfDataIn=pDataIn;
  pfTeFile=pTeFile;
  pfTeHead=pTeHead;
  pfTe= new KSTeData();

  fCameraType= pfTeHead->fCameraType;
  fNumPixels = gNumPixelsCamera[fCameraType];
  fMeanTimeBetweenEventsSec=1./kDefaultEventRatePerSec;

  // ------------------------------------------------------------------------
  //  Initalize the camera (Lots and Lots of work done in this constructor)
  // ----------------------------------------------------------------------
  bool fUsePatternTrigger=true;
  
  // ************************************************************************
  // Change the values of various things in the pTeHead record using the 
  // values specified in the config file read by the KDAomegaDataIn class
  // Basically this is a trick to allow us to try telescope parameters from
  // those used to create the pfTeFile (in ksTrigger). These new values should
  // be more strick than those used in ksTrigger.
  // We do this since the KSCameras constructor uses pfTeHead yo get these
  // parameters
  // ************************************************************************
  pfTeHead->fNoiseRate                =pfDataIn->fNewNoiseRate;
  pfTeHead->fDiscCoincidenceWidthNS   =pfDataIn->fNewDiscCoincidenceWidthNS;
  pfTeHead->fDiscriminatorThresholdPes=pfDataIn->fNewDiscriminatorThresholdPes;
  pfTeHead->fEfficiency               =pfDataIn->fNewEfficiency;
  pfTeHead->fPatternTriggerLevel      =pfDataIn->fNewPatternTriggerLevel;
  pfTeHead->fTriggerMultiplicity      =pfDataIn->fNewTriggerMultiplicity; 
  pfTeHead->fLightConeConcentration   =pfDataIn->fNewLightConeConcentration;

  pfCamera=new KSCamera(fCameraType, pfTeHead, fUsePatternTrigger);
  pfCamera->Print();

  // ************************************************************************
  // We need to open, create and fill the non-event records in the ouput files.  // ************************************************************************
  // First, set up the Pixels status stuff( pedvars,badpixels and relative 
  // gains.
  // *************************************************************************
  fEastLongitude= gEastLongitude[fCameraType];
  fLatitude     = gLatitude[fCameraType];
  pfAzElRADecXY = new VAAzElRADecXY(fEastLongitude,fLatitude);

  VAVDF* pfVDFStats=NULL;
  if(pfDataIn->fPixelStatsRootFileName==string())
    {
      std::cout<<"ksAOMEGA: No Pixel Status Root input file specified"
	       <<std::endl;
      for(int i=0;i<fNumPixels;i++)
	{
	  pfCamera->fPixel[i].fRelativeGain=1;//Used in 1 place:FADC TRaces
	                                    //affects effective CFD thresholds
	  pfCamera->fPixel[i].fPedVarRel=1;//This is relative pedvars at this 
	                                //point.Used to model night sky.
	  pfCamera->fPixel[i].fPed=kDefaultPedestal;// Pedestal to use for 
	                                            //ouput file.
	  pfCamera->fPixel[i].fBadPixel=false;      //Set all pixels ON.
	}
      
      fFirstValidEventTime.setFromString(kFirstValidEventTimeStr.c_str());
      

    }
  else
    {
      // ******************************************************************
      // We are to model this simulation run on an actual run. We do this by
      // reading from the VEGAS stage2 (or reduced Stage3) root file the
      // relative gains, pedvars and bad pixels.  We will ignore variations 
      // during the run for now and just use the values at the start of the 
      // run.
      // *******************************************************************
      std::cout<<"ksAomega: Reference Pixel status VDF root file name:"
	                        <<pfDataIn->fPixelStatsRootFileName<<std::endl;
      pfVDFStats->Open(pfDataIn->fPixelStatsRootFileName);
      pfRunHeader      = pfVDFStats->getRunHeaderPtr();

      std::cout<<"ksAOMEGA: Reference Run Number: "
	       <<pfRunHeader->getRunNumber()<<std::endl;
                                        //Has pedvars for model run;
      VAQStatsData* pfPeds          = pfVDFStats->getQStatsDataPtr(); 
                                        //Has pixel  on/off status for on run;
      VAPixelStatusData* pfPixOnOff = pfVDFStats->getPixelStatusPtr();
      VARelGainData* pfRelGain      = pfVDFStats->getRelGainDataPtr();

      fFirstValidEventTime = pfRunHeader->fRunDetails.fFirstValidEventTime;
      int fNumChannels=pfRunHeader->fRunDetails.fNumOfChans.at(0);  
      if(fNumChannels!=fNumPixels)
	{
	  std::cout<<"ksAomega: KSEvent: NumChannels ("
		   <<fNumChannels<<") different from NumPixels("
		   <<fNumPixels<<")"<<std::endl;
	  exit(1);
	}
      //  ******************************************************************
      // Now load up things
      // Note we only need pedvars to get relative rates of night sky in pixels
      // Thus use a standard window width of 10 samples( 20 ns). This is 
      // default for wipple data and is big enough to cause minimum ststistical
      // prob,ems for VERITAS (I Hope)
      // The base rate oif the night sky is determined from 
      // pfDataIn->fNewNoiseRate but that is done below somewhere.
      // ********************************************************************
      bool ifLO=false;  //Always want high gain. This is appropriate for
      //Gain corrected Charge,SignalToNoise from VATraceData
      // and Pedvars from VAQSatausData.
      for(int i=0;i<fNumPixels;i++)
	{
	  double gain=pfRelGain->getRelGainMean(E_T1,i,ifLO);
	  pfCamera->fPixel[i].fRelativeGain=gain;
	  pfCamera->fPixel[i].fPed=pfPeds->getTraceMean(fFirstValidEventTime,
					    E_T1, 1,kDefaultNumWindowSamples);
	  pfCamera->fPixel[i].fPedVarRel = pfPeds->getTraceVar(
		 fFirstValidEventTime, E_T1, 1,kDefaultNumWindowSamples)/gain;
	  bool fPixelSuppressed;
	  pfPixOnOff->getSuppressed(E_T1, i,fFirstValidEventTime, 
				    fPixelSuppressed);
	  pfCamera->fPixel[i].fBadPixel=fPixelSuppressed;
	}	  
    }

  fEventTime=fFirstValidEventTime;

  // ************************************************************************
  // Useing the config files specified noise rate and the relative ped vars
  // Determine noiseRates for pixels
  // Also determine Pixel WaveForm Night Sky peds 
  // ************************************************************************
  pfCamera->loadNoiseRatesAndPeds();


  // ********************************************************************
  // If defined setup ouput VDF root file
  // ********************************************************************
  if(pfDataIn->fRootFileName!=string())
    {
      int fNumTels=1;
      pfVDFOut= new VAVDF();
      if(fCameraType==WHIPPLE490)
	{
	  std::string lCam("WC490");
	  pfVDFOut->createFileWithCamera(pfDataIn->fRootFileName,1,lCam);
	  //Creates and Opens the output file
	  //(But does not write them).
	  //Also creates all the objects to be written to the file;
	  //Creates a VAArrrayInfo with a whipple 490 pixel camara
	}
      else
	{
	  pfVDFOut->createFile(pfDataIn->fRootFileName,fNumTels,
			       fEventTime);
	}
      
      pfRunHeader=pfVDFOut->getRunHeaderPtr();
      pfRunHeader->fRunDetails.fFirstEventTime=fFirstValidEventTime;
      pfRunHeader->fRunDetails.fRunNum=pfDataIn->fRunNumber;
      pfRunHeader->fRunDetails.fTels=fNumTels;
      pfRunHeader->fRunDetails.fExpectedTels.resize(fNumTels);
      pfRunHeader->fRunDetails.fExpectedTels.at(0)=true;
      pfRunHeader->fRunDetails.fNumOfChans.resize(fNumTels);
      pfRunHeader->fRunDetails.fNumOfChans.at(0)=fNumPixels;
      pfRunHeader->fRunDetails.fFirstValidEventTime=fFirstValidEventTime;
	
      // ****************************************************************
      // Create the VAQStatsData 
      // ****************************************************************
      VATelQStats tempTelQStats;
      tempTelQStats.fTelId=E_T1;//Only one telescope
      
      for(int i=0;i<fNumPixels;i++)
	{                         //We have only 1 window size per channel
	  VASumWinQStats tempSumWinQStats;
	  tempSumWinQStats.fSumWinSize = kDefaultNumWindowSamples;//arbitrary
	  tempSumWinQStats.fNumEvtsAcc = 1000;                //arbitrary
	  tempSumWinQStats.fChargeMean = pfCamera->fPixel[i].fPed;
	  tempSumWinQStats.fChargeVar  = pfCamera->fPixel[i].fChargeVar;
	  
	  VAChanQStats tempChanQStats;   //Only this entry in our "sum"
	  tempChanQStats.fChanNum=i;
	  for(int j=0;j<kDefaultNumWindowSamples;j++)
	    {
	      tempChanQStats.fSumWinColl.push_back(tempSumWinQStats);
	    }
	  //Add this Channel to this telescope data
	  tempTelQStats.fChanColl.push_back(tempChanQStats); 
	}
      //At this point we have a VATelQStats for 1 telescope for one time 
      // slice in tempTelQStats
      
      //Set up the time slice.
      VATimeSliceQStats tempTimeSliceQStats;
      tempTimeSliceQStats.fTelColl.push_back(tempTelQStats);
      tempTimeSliceQStats.fStartTime=fFirstValidEventTime;
      tempTimeSliceQStats.fEndTime.setFromMJDDbl(
		       fFirstValidEventTime.getMJDDbl() + 1.0 ); //A day later
      
      //Put this time slice (onl y one we've got) into QStats Data.
      VAQStatsData* pfQStats=pfVDFOut->getQStatsDataPtr();
      pfQStats->fTimeSliceColl.push_back(tempTimeSliceQStats);
      pfVDFOut->writeQStatsData();

      // ****************************************************************
      // Create the VARelGainsData 
      // ****************************************************************
      VATelRelGains tempTelRelGains;
      tempTelRelGains.fTelId=E_T1;  
      tempTelRelGains.fLowGainRefVarRelGain=1.0;//arbitrary
      tempTelRelGains.fHighGainRefVarRelGain=1.0;//arbitrary
      tempTelRelGains.fLowGainRefRelGain=1.0;
      tempTelRelGains.fHighGainRefRelGain=1.0;
      //Gains are not time variable.
      for(int i=0;i<fNumPixels;i++)
	{
	  //Use same gain for hi and low. Only one channel here.
	  VAChanRelGains tempChanRelGains;
	  tempChanRelGains.fChanNum=i;
	  tempChanRelGains.fNumEvtsAcc=1000;   //arbitrary
	  tempChanRelGains.fMean=pfCamera->fPixel[i].fRelativeGain;
	  tempChanRelGains.fVar=sqrt(pfCamera->fPixel[i].fRelativeGain);
	  tempTelRelGains.fLowGainChanColl.push_back(tempChanRelGains);
	  tempTelRelGains.fHighGainChanColl.push_back(tempChanRelGains);
	}
      
      VARelGainData* pfRelGainData=pfVDFOut->getRelGainDataPtr();
      pfRelGainData->fTelColl.push_back(tempTelRelGains);
      pfVDFOut->writeRelGainData();
      
      // ****************************************************************
      // Create the Pixel Status
      // ****************************************************************
      TelOnOffLogType tempTelOnOffLog;
      tempTelOnOffLog.fTelId=E_T1;
      
      TelSuppressedLogType tempTelSuppressedLog;
      tempTelSuppressedLog.fTelId=E_T1;
      
      for(int i=0;i<fNumPixels;i++)
	{
	  OnOffStatus tempOnOffStatus;
	  tempOnOffStatus.isOn = !pfCamera->fPixel[i].fBadPixel;
	  tempOnOffStatus.startTime=fFirstValidEventTime;
	  tempOnOffStatus.stopTime.setFromMJDDbl(
		       fFirstValidEventTime.getMJDDbl() + 1.0 ); //A day later
	  ChOnOffLogType tempChanOnOffLog;
	  tempChanOnOffLog.push_back(tempOnOffStatus);
	  tempTelOnOffLog.fChColl.push_back(tempChanOnOffLog); 
	  
	  SuppressedStatus tempSuppressedStatus;
	  tempSuppressedStatus.isSuppressed=pfCamera->fPixel[i].fBadPixel;
	  tempSuppressedStatus.startTime=fFirstValidEventTime;
	  //A day later
	  tempSuppressedStatus.stopTime.setFromMJDDbl(
		    fFirstValidEventTime.getMJDDbl() + 1.0 ); //A day later
	  ChSuppressedLogType tempChanSuppressedLog;
	  tempChanSuppressedLog.push_back(tempSuppressedStatus);
	  tempTelSuppressedLog.fChColl.push_back(tempChanSuppressedLog);
	}
      VAPixelStatusData* pfPixelStatus=pfVDFOut->getPixelStatusPtr();
      pfPixelStatus->fOnOffLogs.push_back(tempTelOnOffLog);
      pfPixelStatus->fSuppressedLogs.push_back(tempTelSuppressedLog);
      pfVDFOut->writePixelStatusData();
  // **************************************************************
  // Now generate Calibrated event tree for output root file.
  // **************************************************************
      pfVDFOut->createTheCalibratedArrayEventTree();
      pfCalEvent= pfVDFOut->getCalibratedArrayEventPtr();
      if(pfCalEvent==NULL)
	{
	  std::cout<<"ksAomega: KSEvent:Failed to getCalibratedArrayEvent "
	    "non-NULL pointer"<<std::endl;
	  exit(1);
	}
    }
  // **************************************************************
  // Still need stuff to init VBF file ???????
  // **************************************************************
  
}
// *************************************************************************


KSEvent::~KSEvent()
{ 
  // nothing to do
}
// *************************************************************************


bool KSEvent::BuildImage()
// *************************************************************************
// Read in the next event. Build an image (make traces from the pe hit times)
// *************************************************************************
{
  // *******************************************************************
  // Read in an event
  // *******************************************************************
  bool fShowerEnd=pfTeFile->ReadTe(pfTe);
  if(fShowerEnd)
    {
      if(pfTeFile->foundEOF())
	{
	  return true;
	}
      else
	{
	  std::cout<<"ksAomega: Abnormal error reading in Te file"<<std::endl;
	  exit(1);
	}
    }
  pfCamera->InitPixelImageData();// zeros's and clears things
  fShowerEnd=pfTeFile->ReadTePixelData(pfCamera->fPixel);
  if(fShowerEnd)
    {
      std::cout<<"ksAomega: Abnormal error reading in Te Pixel data"
	       <<std::endl;
      exit(1);
    }
  return false;
}
// **************************************************************************


bool KSEvent::ProcessImage()
// **************************************************************************
// Convert to waveforms and check for triggers
// **************************************************************************
{
  //At this point all the pe times are in fPixel.

  // *************************************************************************
  // Check to see if we are cutting by sapectrum weights. This test is done
  // here to maximize performance.
  // *************************************************************************
  if(pfDataIn->fWeightBySpectrum)
    {
      if(pran(&fXDummy)>pfTe->fWeight)
	{
	  return false;
	}
    }
  
  bool fGoodEvent=false;
  
  int fCFDTriggers=pfCamera->buildTriggerWaveForms();//Finds triggered trigger
                                           // pixels and the times they do it.
  if(fCFDTriggers>0)
    {
      fGoodEvent=pfCamera->isWaveFormTriggered();
      if(fGoodEvent)
	{
	  fTriggerTimeNS=pfCamera->getPSTTriggerTimeNS(); //Time of PST trigger
          // **************************************************************
	  // Now determine the start time for our FADC gate to get the pulse.
	  // fTriggerTimeNS is time we reached enough overlap to trigger PST
	  fFADCStartGateTimeNS=fTriggerTimeNS-gFADCTOffsetNS[fCameraType]-
	    gCFDDelayNS[fCameraType]-gCFDTriggerDelayNS[fCameraType];
	}
    }
  return fGoodEvent;
}  
// **************************************************************************
      
void KSEvent::SaveImage()
// **************************************************************************
// Save event to ouput file(s)
// **************************************************************************
{
  // ********************************************************************
  // Note: We have not put pedestals in here yet.
  // ********************************************************************
  double fEventTimeMJD=fEventTime.getMJDDbl();
  fEventTimeMJD+=Rexp(fMeanTimeBetweenEventsSec);
  fEventTime.setFromMJDDbl(fEventTimeMJD);

  if(pfDataIn->fRootFileName!=string())
    {
      // ********************************************************************
      //Create, Fille and Write out a VACalibratedEvent
      // ********************************************************************
      
      // ****************************************************************
      // Init and load the calibrated event class
      // ****************************************************************
      pfCalEvent->Reset();//pfCalEvent has # telescopes preset.
      pfCalEvent->fTels=1;
      pfCalEvent->fArrayEventNum=fEventIndex;
      pfCalEvent->fEventType=ET_ARRAY_TRIGGER;//normal event
      pfCalEvent->fPresentTels.push_back(true);
      pfCalEvent->fArrayTime=fEventTime;

      // ****************************************************************
      //First fill the Calibrated Telescope event
      // ****************************************************************
      pfCalEvent->fTelEvents.resize(1);
      pfCalEvent->fTelEvents[0].fTelTime=fEventTime;
      VAPointingData fPointing;

      double X[3];
      X[0]=pfTe->fMountDl;
      X[1]=pfTe->fMountDm;
      X[2]=sqrt(1.-X[0]*X[0]-X[1]*X[1]);   //Elevation positive
      GetAzElevFromVec(X,fAzimuth,fElevation);
      pfAzElRADecXY->AzEl2RADec2000(fAzimuth, fElevation, fEventTime,
				    fSourceRA2000,fSourceDec2000);
      fPointing.fCorRA  = fSourceRA2000;  //radians
      fPointing.fCorDec = fSourceDec2000;  //radians
      pfCalEvent->fTelEvents[0].fPointingData=fPointing;
      pfCalEvent->fTelEvents[0].fTelID=E_T1;

      for(uint16_t i=0;i<fNumPixels;i++)  // No zero supression yet
	{
	  VATraceData chanData;
	  chanData.fChanID=i;
	  chanData.fCharge=
	    pfCamera->fPixel[i].GetCharge(fFADCStartGateTimeNS);
	  chanData.fSignalToNoise=chanData.fCharge/
	                                    pfCamera->fPixel[i].fChargeVar;
	  chanData.fHiLo=false;  //We assume hi gain mode for now.
	  chanData.fWindowWidth=kDefaultNumWindowSamples;
	  pfCalEvent->fTelEvents[0].fChanData.push_back(chanData);
	}
      // ****************************************************************
      pfVDFOut->writeCalibratedArrayEvent(1);//Only one telescope to write
    }
  //  ******************************************************************
  // VBF stuff would go here
  // *******************************************************************

  fEventIndex++;
  return;
}
// ************************************************************************

void KSEvent::Close()
  
// ************************************************************************
// Finish up and close up any ouput files.
// ************************************************************************
{
  pfRunHeader->fRunDetails.fNumArrayEvents=(int)pfVDFOut->getNumArrayEvents();
  pfRunHeader->fRunDetails.fLastEventTime=fEventTime;
  pfVDFOut->writeRunHeader(); //This needed regardless of rest 
                                           //of file contents
  pfVDFOut->writeCalibratedEventTree();

  pfVDFOut->Close(); //write out trees
  std::cout<<"ksAomega: Root Output file closed ok!"<<std::endl;
 
  return;
}



// ************************************************************************

void KSEvent::PrintStats()
// ************************************************************************
// Finish up and close up any ouput files.
// ************************************************************************
{
  //std::cout<<"Total Number of Events Written to Root file: "<<
  //  numOfEvents<<std::endl;
  //std::cout<<"Number of Pedestal Events Written to Root file: "<<
  //  numPedestalEvents<<std::endl;
  std::cout<<"Number of Normal Events Written to Root file: "<<
    fEventIndex<<std::endl;
  return;
}
void KSEvent::GetAzElevFromVec(double* X, double& fAzimuth, double& fElevation)
  // **************************************************************************
  //   Get the Az and Elevation of a vector X 
  // **************************************************************************
{
  fElevation=M_PI/2-(acos(fabs(X[2])));
  fAzimuth=0.0;
  if(X[1]==0 && X[0]==0)
    {      //At zenith
      fAzimuth=0.0;
    }
  else if(X[1]==0 && X[0]>0)    //along + x axis  (270 deg)
    {
      fAzimuth=3*M_PI/2;
    }
  else if(X[1]==0 && X[0]<0)    //along - x axis (90 deg)
    {
      fAzimuth=M_PI/2;
    }
  else if(X[1]>0 && X[0]<=0.0)     //Quadrant 1 (0 to 90 deg)
    {
      fAzimuth=-atan(X[0]/X[1]);
    }
  else if(X[1]<0 && X[0]<=0.0)     //Quadrant 2 (90 to 180 deg)
    {
      fAzimuth=M_PI/2+atan(X[0]/X[1]);
    }
  else if(X[1]<0 && X[0]>=.0)      //Quadrant 3 (180 to 270 deg)
    {
      fAzimuth=M_PI-atan(X[0]/X[1]);
    }
  else if(X[1]>0 && X[0]>0.0)       //Quadrant 4 (270 to 360 deg)
    {
      fAzimuth=2*M_PI-atan(X[0]/X[1]);
    }
  return;
}
