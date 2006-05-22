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
extern "C" int    KascadeType2CorsikaType(int fKType);
KSEvent::KSEvent(KSTeFile* pTeFile, KSSegmentHeadData* pSegmentHead, 
		 KSPeHeadData* pPeHead, KSTeHeadData* pTeHead, 
		 KSAomegaDataIn* pDataIn)
{
  fEventIndex=0;
  pfDataIn      = pDataIn;
  pfSegmentHead = pSegmentHead;
  pfPeHead      = pPeHead;
  pfTeFile      = pTeFile;
  pfTeHead      = pTeHead;
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
  if(pfDataIn->fPixelStatsRootFileName==" ")
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
  if(pfDataIn->fRootFileName!=" ")
    {
      int fNumTels=1;
      int fNumChannels=492;
      pfVDFOut= new VAVDF();
      VAArrayInfo* pfArrayInfo;
      KSW10mVDF* pfW10mVDF=NULL;
      if(fCameraType==WHIPPLE490)
	{
	  pfW10mVDF = new KSW10mVDF(pfVDFOut,fNumChannels,fEventTime, 
				    kWhipple10MId,kNumWindowSamples);

	  double fEastLongitude;
	  double fLatitude;
	  pfW10mVDF->CreateW10mVDFFile(pfDataIn->fRootFileName,fEastLongitude,
				       fLatitude);
	  pfArrayInfo=pfVDFOut->getArrayInfoPtr();

	}
      else
	{
	  pfVDFOut->createFile(pfDataIn->fRootFileName,fNumTels,
			       fEventTime);
	}
      
 // *********************************************************************
 //Fill over Run Header
 // *********************************************************************
    
      pfW10mVDF->FillRunHeader(pfDataIn->fRunNumber);
      pfRunHeader=pfVDFOut->getRunHeaderPtr();
      
 // *********************************************************************

      float ped[fNumChannels];
      float pedvar[fNumChannels];
      float gain[fNumChannels];
      bool  off[fNumChannels];
      for(int i=0;i<fNumPixels;i++)
	{
	  ped[i]  = (float)pfCamera->fPixel[i].fPed;
	  gain[i] = (float)pfCamera->fPixel[i].fRelativeGain;
	  off[i]  = pfCamera->fPixel[i].fBadPixel;
	  if(fCameraType==WHIPPLE490)
	    {
	      pedvar[i]=(float)pfCamera->fPixel[i].fChargeVarPE*
		pfDataIn->fDigitalCountsPerPE;
	    }
	  else
	    {
	      pedvar[i]=(float)pfCamera->fPixel[i].fChargeVarPE;
	    }
	}
	
      // ****************************************************************
      // Fill the VAQStatsData 
      // ****************************************************************
       
      pfW10mVDF->FillW10mQStats((const float*) ped, (const float*) pedvar);


 
      pfVDFOut->writeQStatsData();


      // ****************************************************************
      // Fill the VARelGainsData 
      // ****************************************************************

      pfW10mVDF->FillW10mRelGains((const float*)gain);
       
      pfVDFOut->writeRelGainData();
      
      // ****************************************************************
      // Fill the Pixel Status
      // ****************************************************************
      
      pfW10mVDF->FillPixelStatus(gNumImagePixels[fCameraType],off);
      
      pfVDFOut->writePixelStatusData();

      // ****************************************************************
      // Fill the Simulation Head object
      // ****************************************************************
      VAKascadeSimulationHead fSimHeader(pfVDFOut);
      // Set up Header: (see VASimulationData.h and VAKascadeSimulationData.h
      // files for definitions.)
      fSimHeader.pfKascadeSimHead->fSimulationPackage   = 3;  //Purdue Kascade
      fSimHeader.pfKascadeSimHead->fExtensionFormat     = 3;  //Purdue ksAomega
      fSimHeader.pfKascadeSimHead->fDocumentationFileName = "ToBeDetermined";
      fSimHeader.setEnergyGeV(pfSegmentHead->fGeVEnergyPrimary);
 
      pfVDFOut->writeSimulationHeader();
  
      // **************************************************************
      // Generate Calibrated event tree for output root file.
      // **************************************************************
      pfVDFOut->createTheCalibratedArrayEventTree();
      pfCalEvent= pfVDFOut->getCalibratedArrayEventPtr();
      if(pfCalEvent==NULL)
	{
	  std::cout<<"ksAomega: KSEvent:Failed to getCalibratedArrayEvent "
	    "non-NULL pointer"<<std::endl;
	  exit(1);
	}
      // **************************************************************
      // Generate Simulation event tree for output root file.
      // **************************************************************
      pfVDFOut->createTheSimulationEventTree();
      pfSimEvent= new VAKascadeSimulationData(pfVDFOut);

      // Fill in what doesn't change in the shower.
      pfSimEvent->setEnergyGeV(pfSegmentHead->fGeVEnergyPrimary);

      fCorsikaType=KascadeType2CorsikaType(pfSegmentHead->fType);
      std::cout<<"ksAomega: Kascade Primary Type: "<<pfSegmentHead->fType
	       <<"Corsika Primary Type: "<<fCorsikaType<<std::endl;
      pfSimEvent->setCORSIKAParticleID(fCorsikaType);

      double X[3];
      X[0]=pfSegmentHead->fDlInitial;
      X[1]=pfSegmentHead->fDmInitial;
      X[2]=sqrt(1.-X[0]*X[0]-X[1]*X[1]);   //Elevation positive
      GetAzElevFromVec(X,fAzimuth,fElevation);
      pfSimEvent->setPrimaryZenithDeg(((M_PI/2)-fElevation)*gRad2Deg);
      pfSimEvent->setPrimaryAzimuthDeg(fAzimuth*gRad2Deg);
      pfSimEvent->setCoreElevationMASL(pfSegmentHead->fObservationAltitudeM);
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
  bool fGoodRead=pfTeFile->ReadTe(pfTe);
  if(!fGoodRead)
    {
      if(pfTeFile->foundEOF())
	{
	  return true;    //True is flag we hit end of shower file.
	}
      else
	{
	  std::cout<<"ksAomega: Abnormal error reading in Te data"<<std::endl;
	  exit(1);
	}
    }
  pfCamera->InitPixelImageData();// zeros's and clears things
  fGoodRead=pfTeFile->ReadTePixelData(pfCamera->fPixel);
  if(!fGoodRead)
    {
      std::cout<<"ksAomega: Abnormal error reading in Te Pixel data"
	       <<std::endl;
      exit(1);
    }
  return false;        //False indicates event was read in ok and we are not at
                       //end of file
}
// **************************************************************************


bool KSEvent::ProcessImage()
// **************************************************************************
// Convert to waveforms and check for triggers
// **************************************************************************
{
  //At this point all the pe times are in fPixel.
  // ************************************************************************
  // Create pixel waveforms (as they would be as they enter CFD's and FADC's)
  // ************************************************************************  
  int fCFDTriggers=pfCamera->buildTriggerWaveForms(pfTe->fNx,pfTe->fNy); 

  bool fGoodEvent=false;
  if(fCFDTriggers>0)
    {
      fGoodEvent=pfCamera->isWaveFormTriggered();//Finds 
                                           //triggered trigger
                                           // pixels and the times they do it.
      if(fGoodEvent)
	{
	  fTriggerTimeNS=pfCamera->getPSTTriggerTimeNS(); //Time of PST trigger
          // **************************************************************
	  // Now determine the start time for our FADC gate to get the pulse.
	  // fTriggerTimeNS is time we reached enough overlap to trigger PST
	  fFADCStartGateTimeNS=fTriggerTimeNS-gFADCTOffsetNS[fCameraType];
	  pfCamera->buildNonTriggerWaveForms();


	}
    }
  return fGoodEvent;
}  
// **************************************************************************
      
void KSEvent::SaveImage()
// **************************************************************************
// Save event to output file(s)
// **************************************************************************/
// *&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
// We still have to do the simulation ttree!!!!!!!!!!!!!!!!!!!!!!!!!!!!{
// *&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
{
  // ********************************************************************
  // Note: We have not put pedestals in here yet.
  // ********************************************************************
  double fEventTimeMJD=fEventTime.getMJDDbl();
  fEventTimeMJD+=Rexp(fMeanTimeBetweenEventsSec)/(60.*60.*24.);
  fEventTime.setFromMJDDbl(fEventTimeMJD);

  if(pfDataIn->fRootFileName!=" ")
    {
      // ********************************************************************
      //Create, Fill and Write out a VACalibratedEvent
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
      if(fCameraType==WHIPPLE490)
	{
	  pfCalEvent->fTelEvents[0].fTelID=kWhipple10MId;
	}
      else
	{
	  pfCalEvent->fTelEvents[0].fTelID=E_T1;
	}

      for(uint16_t i=0;i<fNumPixels;i++)  // No zero supression yet
	{
	  VATraceData chanData;
	  chanData.fChanID=i;
	  if(fCameraType==WHIPPLE490)
	    {
	      chanData.fCharge=
		pfCamera->fPixel[i].GetCharge(fFADCStartGateTimeNS);
	      chanData.fSignalToNoise=chanData.fCharge/
	                                    pfCamera->fPixel[i].fChargeVarPE;
	      chanData.fCharge=chanData.fCharge*pfDataIn->fDigitalCountsPerPE;
	    }
	  else if(fCameraType==VERITAS499) 
	    {
	      chanData.fCharge=
		pfCamera->fPixel[i].GetCharge(fFADCStartGateTimeNS);
	      chanData.fSignalToNoise=chanData.fCharge/
	                                    pfCamera->fPixel[i].fChargeVarPE;
	    }
	  chanData.fHiLo=false;  //We assume hi gain mode for now.
	  chanData.fWindowWidth=kNumWindowSamples;
	  pfCalEvent->fTelEvents[0].fChanData.push_back(chanData);
	}
      // ****************************************************************
      pfVDFOut->writeCalibratedArrayEvent(1);//Only one telescope to write
      
      // ****************************************************************
      // Next Save  the Simulation event MC tags
      // ****************************************************************
      pfSimEvent->setEventNumber((int)pfVDFOut->getNumArrayEvents()-1);
      pfSimEvent->setCORSIKAParticleID(fCorsikaType);
      pfSimEvent->setEnergyGeV(pfSegmentHead->fGeVEnergyPrimary);

      pfSimEvent->setObservationZenithDeg(((M_PI/2)-fElevation)*gRad2Deg);
      pfSimEvent->setObservationAzimuthDeg(fAzimuth*gRad2Deg);


      double fMountXLocationM=pfPeHead->fXAreaWidthM*pfTe->fNx +
	                                              pfPeHead->fXCoreOffsetM;
      double fMountYLocationM=pfPeHead->fYAreaWidthM*pfTe->fNy +
	                                              pfPeHead->fYCoreOffsetM;
      pfSimEvent->setCoreEastM(-fMountXLocationM);
      pfSimEvent->setCoreSouthM(-fMountYLocationM);

      // ********************************************************************
      // Now tags for KASCADE puposes
      // ********************************************************************
      pfSimEvent->setNXIndex((float)pfTe->fNx);         //used for shower sort
      pfSimEvent->setNYIndex((float)pfTe->fNy);         //           '      '
      pfSimEvent->setDirectionIndex((float)pfTe->fDirectionIndex);// '      '

      pfSimEvent->setEmissionAltitudeM((float)pfTe->fEmissionAltitude);
      pfSimEvent->
	        setEmissionAltitudeSigma((float)pfTe->fEmissionAltitudeSigma);
      pfSimEvent->setMuonRatio((float)pfTe->fMuonRatio);
      pfSimEvent->setAomega((float)pfTe->fAomega);
      pfVDFOut->writeSimulationData();

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
  pfVDFOut->writeCalibratedEventTree();
  pfVDFOut->writeSimulationEventTree();

  pfRunHeader->fRunDetails.fNumArrayEvents=(int)pfVDFOut->getNumArrayEvents();
  pfRunHeader->fRunDetails.fLastEventTime=fEventTime;
  pfVDFOut->writeRunHeader(); //This needed regardless of rest 
                                           //of file contents
  pfVDFOut->writeArrayInfo();
// ********************************************************************

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
  //   Get the Az and Elevation(radians) of a vector X 
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
// *************************************************************************

