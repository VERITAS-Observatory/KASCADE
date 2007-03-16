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
extern "C" void   GetAzElevFromVec(double* X, double& fAzimuth, 
				   double& fElevation);

KSEvent::KSEvent(KSTeFile* pTeFile, KSSegmentHeadData* pSegmentHead, 
		 KSPeHeadData* pPeHead, KSTeHeadData* pTeHead, 
		 KSAomegaDataIn* pDataIn)
{
  fEventIndex=0;
  fNumPedestalEvents=0;
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
  double fFADCDigCntsPerPEHiGain      =pfDataIn->fDigitalCountsPerPE;
  double fNoiseRateSigma              =pfDataIn->fNoiseRateSigma;
  //pfCamera=new KSCamera(fCameraType, pfTeHead, fUsePatternTrigger,
  //			                              fFADCDigCntsPerPEHiGain);
  pfCamera=new KSCamera(fCameraType, pfTeHead, fUsePatternTrigger,
			fFADCDigCntsPerPEHiGain,fNoiseRateSigma);
  pfCamera->Print();

  // ************************************************************************
  // We need to open, create and fill the non-event records in the output 
  // files.  
  // ************************************************************************
  // First, set up the Pixels status stuff( pedvars,badpixels and relative 
  // gains.
  // *************************************************************************
  fEastLongitude= gEastLongitude[fCameraType];
  fLatitude     = gLatitude[fCameraType];
  pfAzElRADecXY = new VAAzElRADecXY(fEastLongitude,fLatitude);

  if(pfDataIn->fPixelStatsRootFileName==" ")
    {
      std::cout<<"ksAomega: No Pixel Status Root input file specified"
	       <<std::endl;
      for(int i=0;i<fNumPixels;i++)
	{
	  pfCamera->fPixel.at(i).fRelativeGain=1;//Used in 1 place:FADC TRaces
	                                    //affects effective CFD thresholds
	  pfCamera->fPixel.at(i).fPedVarRel=1;//This is relative pedvars at this 
	                                //point.Used to model night sky.
	  pfCamera->fPixel.at(i).fBadPixel=false;      //Set all pixels ON.
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
      // We don't use VAVDF here since it uses VARootIO which opens the file 
      // as "UPDATE". Since we may have many concurrent programs opening this 
      // file we need it to be opened "READ", ie readonly. So we do this 
      // ourselves directly.
      // *******************************************************************
      TFile* pfVDFStats =
	new TFile(pfDataIn->fPixelStatsRootFileName.c_str(),"READ");
      if(pfVDFStats==NULL)
	{
	  std::cout<<"ksAomega: Failure to Open Reference Pixel status VDF "
	    "root file"<<std::endl;
	  exit(1);
	}
      std::cout<<"ksAomega: Reference Pixel status VDF root file name:"
	       <<pfDataIn->fPixelStatsRootFileName<<std::endl; 
      // *****************************************************************
      // Read the run header: Get run header directory, read run header
      // *****************************************************************
      TDirectory* pfRunHeaderDir = 
	               (TDirectory*)pfVDFStats->Get(gRunHeaderDirName.c_str());
      if(pfRunHeaderDir==NULL)
	{
	  pfVDFStats->ls();
	  std::cout<<"ksAomega: Failed to find Run header directory:"
		   <<gRunHeaderDirName
		   <<" in Reference File"<<std::endl;
	  exit(1);
	}
      VARunHeader* pfStatsRunHeader = 
                     (VARunHeader*)pfRunHeaderDir->Get(gRunHeaderName.c_str());
      if(pfRunHeader==NULL)
	{
	  std::cout<<"ksAomega: No Run header in Reference File"
		   <<std::endl;
	  exit(1);
	}
      std::cout<<"ksAOMEGA: Reference Run Number: "
	       <<pfStatsRunHeader->getRunNumber()<<std::endl;
      fFirstValidEventTime = 
	                pfStatsRunHeader->pfRunDetails->fFirstValidEventTime;


      // *****************************************************************
      // Get the VAQStatsData. has pedvars
      // *****************************************************************
      VAQStatsData* pfPeds=NULL;
      if(pfDataIn->fUseRelativePedVars)
	{
	  TDirectory* pfQStatsDir = 
	    (TDirectory*)pfVDFStats->Get(gQStatsDirName.c_str());
	  if(pfQStatsDir==NULL)
	    {
	      std::cout<<"ksAomega: No QStats directory in Reference File"
		       <<std::endl;
	      exit(1);
	    }
	  pfPeds = (VAQStatsData*)pfQStatsDir->Get(gQStatsDataName.c_str());
	  if(pfPeds==NULL)
	    {
	      std::cout<<"ksAomega: No QStats  in Reference File"
		       <<std::endl;
	      exit(1);
	    }
	}

      // *****************************************************************
      // Get the Pixel Status Data
      // *****************************************************************
      VAPixelStatusData* pfPixOnOff=NULL;
      if(pfDataIn->fUseBadPixelSupression)
	{
	  pfPixOnOff = 
	(VAPixelStatusData*) pfRunHeaderDir->Get(gPixelStatusDataName.c_str());
	  if(pfPixOnOff==NULL)
	    {
	      std::cout<<"ksAomega: No PixelStatusData  in Reference File"
		       <<std::endl;
	      exit(1);
	    }
	}


      // *****************************************************************
      // Get the Relative Gain Data
      // *****************************************************************
      VARelGainData* pfRelGain=NULL;
      if(pfDataIn->fUseRelativeGains)
	{
	  TDirectory* pfRelGainDir = 
	    (TDirectory*)pfVDFStats->Get(gRelGainDirName.c_str()); 
	  if(pfRelGainDir==NULL)
	    {
	      std::cout<<"ksAomega: No Rel Gain  directory in Reference File"
		       <<std::endl;
	      exit(1);
	    }
	  pfRelGain = 
	    (VARelGainData*)pfRelGainDir->Get(gRelGainDataName.c_str());
	  if(pfRelGain==NULL)
	    {
	      std::cout<<"ksAomega: No Relative Gain Data  in Reference File"
		       <<std::endl;
	      exit(1);
	    }
	}

      // *****************************************************************
      // Get the ArrayInfo
      // *****************************************************************
      VAArrayInfo* pfArrayInfo = 
	          (VAArrayInfo*)pfRunHeaderDir->Get(gArrayInfoName.c_str());
      if(pfArrayInfo==NULL)
	    {
	      std::cout<<"ksAomega: No ArrayInfo  in Reference File"
		       <<std::endl;
	      exit(1);
	    }
      int fNumChannels= pfArrayInfo->telescope(0)->numChannels();
      if(fNumChannels!=gNumChannelsCamera[fCameraType])     //A sanity check.
	{
	  if(fCameraType==WHIPPLE490)
	    {
	      std::cout<<"ksAomega: KSEvent: NumChannels ("
		       <<fNumChannels<<") wrong for Whipple490 camera"
		"Should be 492."<<std::endl;
	      exit(1);
	    }
	  else if(fCameraType==VERITAS499)
	    {
	      std::cout<<"ksAomega: KSEvent: NumChannels ("
		       <<fNumChannels<<") different from NumPixels("
		       <<fNumPixels<<")"<<std::endl;
	      exit(1);
	    }
	}



      //  ******************************************************************
      // Now load up things
      // Note we only need pedvars to get relative rates of night sky in pixels
      // Thus use a the standard window size from gFADCWinSize. This is 
      // default for whipple data and is big enough to cause minimum 
      // statistical problems for VERITAS (I Hope)
      // The base rate of the night sky is determined from 
      // pfDataIn->fNewNoiseRate but that is done below somewhere.
      // ********************************************************************
      bool ifLO=false;  //Always want high gain. This is appropriate for
                        //Gain corrected Charge,SignalToNoise from VATraceData
                        //and Pedvars from VAQSatausData.
      uint16_t fTel=(uint16_t)pfDataIn->fTelescope;
      uint16_t winSize=(uint16_t)gFADCWinSize[fCameraType];
      int fNumBadPixels=0;
      for(uint16_t chan=0;chan<(uint16_t)fNumPixels;chan++)
	{
	  double gain=1;
	  if(pfDataIn->fUseRelativeGains)
	    {
	     gain=pfRelGain->getRelGainMean(fTel,chan, ifLO);
	    }
	  pfCamera->fPixel.at(chan).fRelativeGain=gain;

	  double pedvar=1;
	  if(pfDataIn->fUseRelativePedVars)
	    {
	      pedvar= pfPeds->getTraceVar(fFirstValidEventTime,fTel,chan,
			       winSize)/gain;
	    }
	  pfCamera->fPixel.at(chan).fPedVarRel=pedvar; 

	  bool fPixelSuppressed=false;
	  bool fChannelIsPMT=true;
	  if(pfDataIn->fUseBadPixelSupression)
	    {
	      fChannelIsPMT=
		pfArrayInfo->telescope(fTel)->channel(chan)->hasPMT();
	      pfPixOnOff->getSuppressed(fTel,chan,fFirstValidEventTime,
					fPixelSuppressed);
	    }
	  if(fPixelSuppressed || !fChannelIsPMT)
	    {
	      pfCamera->fPixel.at(chan).fBadPixel=true;
	      if(fNumBadPixels%10==0)
		{
		  std::cout<<std::endl;
		  std::cout<<"BadPixels: ";
		}
	      std::cout<<" "<<chan;
	      fNumBadPixels++;
	    }
	  else
	    {
	      pfCamera->fPixel.at(chan).fBadPixel=false;
	    }
	}
      if(fNumBadPixels>0)
	{
	  std::cout<<std::endl;
	}
    }
  fEventTime=fFirstValidEventTime;

  // ************************************************************************
  // Useing the config files specified noise rate and the relative ped vars
  // Determine noiseRates for pixels
  // Also determine Pixel WaveForm Night Sky peds 
  // ************************************************************************
  pfCamera->loadNoiseRatesAndPeds();

  fCorsikaType=KascadeType2CorsikaType(pfSegmentHead->fType);

  // ********************************************************************
  // If defined setup ouput VDF root file
  // ********************************************************************
  pfVDFOut=NULL;
  if(pfDataIn->fRootFileName!=" ")
    {
      VAArrayInfo* pfArrayInfo;
      KSVDFHelper* pfVDFHelper = new KSVDFHelper(fNumPixels, fEventTime, 
			    E_T1, gFADCWinSize[fCameraType], fCameraType);
      double fEastLongitude;
      double fLatitude;

      pfVDFHelper->CreateVDFFile(pfDataIn->fRootFileName,fEastLongitude,
				       fLatitude);
      pfVDFOut=pfVDFHelper->getVDFFilePtr();
      pfArrayInfo=pfVDFOut->getArrayInfoPtr();
      
      std::cout<<"KSAomega: Output VDF Root File: "<<pfDataIn->fRootFileName
	       <<std::endl;
      //std::cout<<"fSinglePeMeanFADCArea[0]:"
      //	       <<pfCamera->fPixel[0].fSinglePeMeanFADCArea<<std::endl;

      
 // *********************************************************************
 //Fill over Run Header
 // *********************************************************************
    
      pfVDFHelper->FillRunHeader(pfDataIn->fRunNumber);
      pfRunHeader=pfVDFOut->getRunHeaderPtr();
      
 // *********************************************************************

      float ped[fNumPixels];
      float pedvar[fNumPixels];
      float gain[fNumPixels];
      bool  off[fNumPixels];
      for(int i=0;i<fNumPixels;i++)
	{
	  gain[i] = (float)pfCamera->fPixel.at(i).fRelativeGain;
	  off[i]  = pfCamera->fPixel.at(i).fBadPixel;
	  if(fCameraType==WHIPPLE490)
	    {
	      // *********************************************************
	      // Now we apply the DigitalCountsPerPE for whipple only here. 
	      // For VERITAS499 this is applied within the KSFADC function
	      // *********************************************************
	      ped[i]  = (float)pfCamera->fPixel.at(i).fPedPE*
		pfDataIn->fDigitalCountsPerPE;
	      pedvar[i]=(float)pfCamera->fPixel.at(i).fChargeVarPE*
		pfDataIn->fDigitalCountsPerPE;
	    }
	  else if(fCameraType==VERITAS499)
	    {
	      ped[i]  = (float)pfCamera->fPixel.at(i).fPedDC;
	      pedvar[i]=(float)pfCamera->fPixel.at(i).fChargeVarDC;
	    }
	}
	
      // ****************************************************************
      // Fill the VAQStatsData 
      // ****************************************************************
       
      pfVDFHelper->FillQStats((const float*) ped, (const float*) pedvar);
      pfVDFOut->writeQStatsData();


      // ****************************************************************
      // Fill the VARelGainsData 
      // ****************************************************************

      pfVDFHelper->FillRelGains((const float*)gain);
       
      pfVDFOut->writeRelGainData();
      
      // ****************************************************************
      // Fill the Pixel Status
      // ****************************************************************
      
      pfVDFHelper->FillPixelStatus(gNumImagePixels[fCameraType],off);
      
      pfVDFOut->writePixelStatusData();

      // ****************************************************************
      // Fill the Simulation Head object
      // ****************************************************************
      VAKascadeSimulationHead fSimHeader;

      // Set up Header: (see VASimulationData.h and VAKascadeSimulationData.h
      // files for definitions.)
      //Get present time
      VATime fTimeNow;
      fTimeNow.setFromSystemTime();
      uint32_t y,m,d;
      fTimeNow.getCalendarDate(y,m,d);
      fSimHeader.fDateOfSimsUTC= y*10000ULL+ m*100ULL+ d;
      fSimHeader.fSimulationPackage   = VASimulationHeader::E_KASCADE;  //Purdue Kascade
      fSimHeader.fSimulator           = VASimulationHeader::E_SEMBROSKI;  //Purdue Sembroski
      if(pfDataIn->fPixelStatsRootFileName!=" ")
	{
	  fEventTime.getCalendarDate(y,m,d);
	  fSimHeader.fDateOfArrayForSims= y*10000ULL + m*100ULL + d;
	}
      else
	{
	  fSimHeader.fDateOfArrayForSims=0;
	} 
      if(pfDataIn->fSimulationConfigFileName!=" ")
	{
	  fSimHeader.loadSimConfigFileFromFile( pfDataIn->
						fSimulationConfigFileName);
	} 
      else
	{
	  fSimHeader.fSimConfigFile= "Not Specified";
	}
      //Kascade specific header stuff
      fSimHeader.fCORSIKAParticleID=fCorsikaType;
      fSimHeader.fShowerID=pfSegmentHead->fShowerID;
      fSimHeader.fEnergyGeV=pfSegmentHead->fGeVEnergyPrimary;

      // ******************************************************************
      // The following works for anything inheritaed from VASimulationHeader 
      // since VASimulationHeader is a TObject and thus VARootIO can write it 
      // out.
      // ******************************************************************
      pfVDFOut->setSimulationHeaderPtr(&fSimHeader); 
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
      // Now here we have to be a little more specific. By default VAVDF 
      // creates a TTree with branches of VASimulationData objects. We want 
      // the TTree to be of VAKascadeSimulationData objects so we have to
      // do this ourselves. We basically copy the code from VAVDF for
      // createTheSimulationEventTree() and make some changes
      // ***************************************************************
      pfSimEvent= new VAKascadeSimulationData();
      TTree* pfSimulationEventTree= new TTree(gSimulatedEventsTreeName.c_str(),
				       "Simulation Parameters");
      if(pfSimulationEventTree==NULL)
	{
	  std::cout<<"KSEvent: Problem creating  pfSimulationEventTree"
		   <<std::endl;
	  exit(1);
	}
      pfSimulationEventTree->Branch(gSimulatedEventsBranchName.c_str(),
			   "VAKascadeSimulationData",&pfSimEvent,16000,0);
      pfVDFOut->setSimulationPtr(pfSimEvent);
      pfVDFOut->setSimulationEventTree(pfSimulationEventTree);


      //*******************************************************************
      // Fill in what doesn't change in the shower.
      // *****************************************************************
      // VASimulationData stuff
      // *****************************************************************
      pfSimEvent->fEnergyGeV=pfSegmentHead->fGeVEnergyPrimary;
      std::cout<<"ksAomega: Kascade Primary Type: "<<pfSegmentHead->fType
	       <<" ->Corsika Primary Type: "<<fCorsikaType<<std::endl;
      pfSimEvent->fCORSIKAParticleID=fCorsikaType;
      pfSimEvent->fTriggeredArray=true;

      // *******************************************************************
      //Get direction shower is COMING from! Reverse direction.
      //Vegas definition x+ east, y + north z+ up
      //Kascade definition x+ east y + south z + down
      //Convert to vegas definition
      // *******************************************************************
      double X[3];
      X[0]=-pfSegmentHead->fDlInitial; //Negative for reverse   
      X[1]=pfSegmentHead->fDmInitial;  //double negative. once for reverse, 
                                       //once for converting to vegas
      X[2]=sqrt(1.-X[0]*X[0]-X[1]*X[1]);//Elevation positive(reverse)

      GetAzElevFromVec(X,fAzimuth,fElevation);
      pfSimEvent->fPrimaryZenithDeg=((M_PI/2)-fElevation)*gRad2Deg;
      pfSimEvent->fPrimaryAzimuthDeg=fAzimuth*gRad2Deg;
      pfSimEvent->fCoreElevationMASL=pfSegmentHead->fObservationAltitudeM;

    }
  // **************************************************************
  // Init VBF file if one is specified
  // **************************************************************
  pfVBFOut=NULL;
  if(pfDataIn->fVBFFileName!=" ")
    {
      std::cout<<"ksAomega: Creating Output VBF file: "
		   <<pfDataIn->fVBFFileName<<std::endl;
      pfVBFOut = new KSVBFFile(fCameraType, pfDataIn->fDigitalCountsPerPE,
			       fCorsikaType,pfSegmentHead,pfPeHead,pfCamera); 
      std::string fConfigMask("0");
      bool fCreatedVBFFile=pfVBFOut->Create(pfDataIn,fConfigMask,fEventTime);
      if(!fCreatedVBFFile)
	{
	  std::cout<<"ksAomega: Got error while trying to open VBF file: "
		   <<pfDataIn->fVBFFileName<<std::endl;
	  exit(1);
	}
    }
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
  pfCamera->InitPixelImageData();// zeros's and clears things, set overflow 
                                 // cfd time . especially needed for bad pixels
                                 // It does not however reset fBadPixel.
  fGoodRead=pfTeFile->ReadTePixelData(pfCamera->fPixel);
  if(!fGoodRead)
    {
      std::cout<<"ksAomega: Abnormal error reading in Te Pixel data"
	       <<std::endl;
      exit(1);
    }
  // *************************************************************************
  // As a safety check. clear out the BadPixel times
  // *************************************************************************
  for(uint16_t chan=0;chan<(uint16_t)fNumPixels;chan++)
    {
      if(pfCamera->fPixel.at(chan).fBadPixel)
	{
	  pfCamera->fPixel.at(chan).fTimePe.clear();
	}
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
{
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
	  uint32_t fYear,fMonth,fDay,H,M,S,NS;
	  fEventTime.getCalendarDate(fYear,fMonth,fDay);
	  fEventTime.getTime(H,M,S,NS);
	  NS=0;   //On the tick!
	  VATime fPedestalEventTime;
	  fPedestalEventTime.setFromCalendarDateAndTime(fYear,fMonth,fDay,
							H,M,S,NS);
	  pfCamera->loadAPedestalEventIntoPedPixels();

	  //Pedestal event to VBF file
	  if(pfDataIn->fVBFFileName!=" ")
	    {
	      bool fPedestalEvent=true;
	      pfVBFOut->WriteVBF(fEventIndex+1, pfDataIn->fTelescope, 
			     fPedestalEventTime, 
				 fFADCStartGateTimeNS, pfTe,fPedestalEvent,
				 0.0,0.0);
	    }
	  if(pfDataIn->fRootFileName!=" ")
	    {
	      // *************************************************************
	      //Create, Fill and Write out a Pedestal VACalibratedEvent and 
	      // Save  the VASimulation event MC tags
	      // *************************************************************
	      bool fPedestalEvent=true;
	      CreateRootEvent(fPedestalEvent,fPedestalEventTime);
	      pfVDFOut->writeCalibratedArrayEvent(1);
	      pfVDFOut->writeSimulationData();
	    }
	  fNumPedestalEvents++;

	  if(pfDataIn->fVBFFileName!=" " ||pfDataIn->fRootFileName!=" ")
	    {
	      fEventIndex++; 
	    }
    }

  if(pfDataIn->fRootFileName!=" ")
    {
      
      // ********************************************************************
      //Create, Fill and Write out a VACalibratedEvent and Save  the 
      //VASimulation event MC tags
      // ********************************************************************
      bool fPedestalEvent=false;
      CreateRootEvent(fPedestalEvent,fEventTime);

      pfVDFOut->writeCalibratedArrayEvent(1);//Only one telescope to write

      pfVDFOut->writeSimulationData();  //This write works because we already 
      //set with setSimulationPtr pfSimEvent as our simulation pointer within 
      //createSimulationEventTree.
    }

  //  ******************************************************************
  // Write out event to VBF file
  // *******************************************************************
  if(pfDataIn->fVBFFileName!=" ")
    {
      // ********************************************************************
      // Write out a VAArrayEvent
      // ********************************************************************
      float fCoreEastM  = -(float)findTelescopeXM();
      float fCoreSouthM = -(float)findTelescopeYM();
      pfVBFOut->WriteVBF(fEventIndex+1, pfDataIn->fTelescope, fEventTime, 
			 fFADCStartGateTimeNS, pfTe,false,fCoreEastM,
			 fCoreSouthM);
    } 
  fEventIndex++;
  return;
}
// ************************************************************************

void KSEvent::Close()
  
// ************************************************************************
// Finish up and close up any ouput files.
// ************************************************************************
// If we have any events, make sure we have at least one pedestal event.
{
  if(fEventIndex>0 && fNumPedestalEvents==0)
    {
      double fEventTimeMJD=fEventTime.getMJDDbl();
      fEventTimeMJD+=1./(60.*60.*24.);
      fEventTime.setFromMJDDbl(fEventTimeMJD);

      uint32_t fYear,fMonth,fDay,H,M,S,NS;
      fEventTime.getCalendarDate(fYear,fMonth,fDay);
      fEventTime.getTime(H,M,S,NS);
      NS=0;   //On the tick!
      fEventTime.setFromCalendarDateAndTime(fYear,fMonth,fDay,
							H,M,S,NS);
      pfCamera->loadAPedestalEventIntoPedPixels();

      //Pedestal event to VBF file
      if(pfDataIn->fVBFFileName!=" ")
	{
	  bool fPedestalEvent=true;
	  pfVBFOut->WriteVBF(fEventIndex+1, pfDataIn->fTelescope, 
			     fEventTime, 
			     fFADCStartGateTimeNS, pfTe,fPedestalEvent,
			     0.0,0.0);
	}
      if(pfDataIn->fRootFileName!=" ")
	{
	  // *************************************************************
	  //Create, Fill and Write out a Pedestal VACalibratedEvent and 
	  // Save  the VASimulation event MC tags
	  // *************************************************************
	  bool fPedestalEvent=true;
	  CreateRootEvent(fPedestalEvent,fEventTime);
	  pfVDFOut->writeCalibratedArrayEvent(1);
	  pfVDFOut->writeSimulationData();
	}
      fNumPedestalEvents++;
      fEventIndex++; 
    }

  if(pfVDFOut!=NULL)
    {
      pfVDFOut->writeCalibratedEventTree();
      pfVDFOut->writeSimulationEventTree();

      pfRunHeader->pfRunDetails->fNumArrayEvents=
	(int)pfVDFOut->getNumArrayEvents();
      pfRunHeader->pfRunDetails->fLastEventTime=fEventTime;
      pfVDFOut->writeRunHeader(); //This needed regardless of rest 
                                  //of file contents
      pfVDFOut->writeArrayInfo();

      pfVDFOut->Close(); //write out trees
      std::cout<<"ksAomega: Root Output file closed ok!"<<std::endl;
    }

  if(pfVBFOut!=NULL)
    {
      pfVBFOut->Close();
    }
  return;
}



// ************************************************************************

void KSEvent::PrintStats()
// ************************************************************************
// Finish up and close up any ouput files.
// ************************************************************************
{
  //std::cout<<"ksSAomega:Total Number of Events Written to Output file(s): "
  //<<numOfEvents<<std::endl;
  std::cout<<"ksSAomega:Normal Events Written to Output file(s): "<<
    fEventIndex-fNumPedestalEvents<<std::endl;
  std::cout<<"ksSAomega:Number of Pedestal Events Written to Output "
  "file(s): "<<fNumPedestalEvents<<std::endl;
  return;
}
// **************************************************************************

void KSEvent::CreateRootEvent(bool fPedestalEvent, VATime& EventTime)
// ********************************************************************
//Create and fill a VACalibratedEvent and the VAKascadeSimulationData
// ********************************************************************
{
  // ****************************************************************
  // Init and load the calibrated event class
  // ****************************************************************
  pfCalEvent->Reset();//pfCalEvent has # telescopes preset.
  pfCalEvent->fTels=1;
  pfCalEvent->fArrayEventNum=fEventIndex+1;

  if(!fPedestalEvent)
    {
      pfCalEvent->fEventType=ET_ARRAY_TRIGGER;//normal event
    }
  else
    {
      pfCalEvent->fEventType=ET_PEDESTAL; //pedestal event
    }
  pfCalEvent->fPresentTels.push_back(true);
  pfCalEvent->fArrayTime=EventTime;

  // ****************************************************************
  //First fill the Calibrated Telescope event
  // ****************************************************************
  pfCalEvent->fTelEvents.resize(1);
  pfCalEvent->fTelEvents.at(0).fTelTime=EventTime;
  VAPointingData fPointing;

  // *******************************************************************
  //Get direction mount is pointing to.
  //Vegas definition x+ east, y + north z+ up
  //Kascade definition x+ east y + south z + down
  //Convert to vegas definition
  // *******************************************************************
  double X[3];
  X[0]=pfTe->fMountDl;
  X[1]=-pfTe->fMountDm;   //Kascade to vegas flip
  X[2]=sqrt(1.-X[0]*X[0]-X[1]*X[1]);   //Elevation positive

  GetAzElevFromVec(X,fAzimuth,fElevation);
  pfAzElRADecXY->AzEl2RADec2000(fAzimuth, fElevation, EventTime,
				fSourceRA2000,fSourceDec2000);
  fPointing.fCorRA  = fSourceRA2000;  //radians
  fPointing.fCorDec = fSourceDec2000;  //radians
  pfCalEvent->fTelEvents.at(0).fPointingData=fPointing;
      
  pfCalEvent->fTelEvents.at(0).fTelID=pfDataIn->fTelescope;

  for(uint16_t i=0;i<fNumPixels;i++)  // No zero supression yet
    {
      // *************************************************************
      // ONly good, 'Live' pixels go out to fTelEvents
      // *******************************************************
      if(!pfCamera->fPixel.at(i).fBadPixel)
	{
	  VATraceData chanData;
	  chanData.fChanID=i;
	  if(fCameraType==WHIPPLE490)
	    {
	      if(!fPedestalEvent)
		{
		  chanData.fCharge=
		    pfCamera->fPixel.at(i).GetCharge(fFADCStartGateTimeNS,
						  fPedestalEvent);
		  chanData.fSignalToNoise=chanData.fCharge/
		    pfCamera->fPixel.at(i).fChargeVarPE;
		}
	      else
		{
		  chanData.fCharge=
		    pfCamera->fPedPixels.at(i).GetCharge(0,fPedestalEvent);
		  chanData.fSignalToNoise=chanData.fCharge/
		    pfCamera->fPixel.at(i).fChargeVarPE;
		}
	      // *********************************************************
	      // Now we apply the DigitalCountsPerPE for whipple onlyhere. 
	      // For VERITAS499 this is applied within the KSFADC function
	      // *********************************************************
	      chanData.fCharge=
		chanData.fCharge*pfDataIn->fDigitalCountsPerPE;
	    }
	  else if(fCameraType==VERITAS499) 
	    {
		if(!fPedestalEvent)
		{
		  //GetCharge for VERITAS499 returns FADC sum in dc.
		  double fChargeStartTimeNS=fFADCStartGateTimeNS-
		                            gFADCWindowOffsetNS[VERITAS499]+
		                            gFADCChargeOffsetNS[VERITAS499];
		  chanData.fCharge=pfCamera->fPixel.at(i).GetCharge(
					fChargeStartTimeNS, fPedestalEvent);
		  chanData.fSignalToNoise=chanData.fCharge/
		    pfCamera->fPixel.at(i).fChargeVarDC;
		}
	      else
		{
		  chanData.fCharge=pfCamera->fPedPixels.at(i).GetCharge(
			       gFADCChargeOffsetNS[VERITAS499],fPedestalEvent);
		  chanData.fSignalToNoise=chanData.fCharge/
		    pfCamera->fPixel.at(i).fChargeVarDC;
		}
	    }
	  chanData.fHiLo=false;  //We assume always hi gain mode for now.
	  chanData.fWindowWidth=gFADCWinSize[fCameraType];
	  pfCalEvent->fTelEvents.at(0).fChanData.push_back(chanData);
	}
    }
  // ****************************************************************
  
  // ****************************************************************
  // Next Save  the VASimulation event MC tags
  // ****************************************************************
  pfSimEvent->fEventNumber=pfVDFOut->getNumArrayEvents()-1;
  pfSimEvent->fObservationZenithDeg=((M_PI/2)-fElevation)*gRad2Deg;
  pfSimEvent->fObservationAzimuthDeg=fAzimuth*gRad2Deg;

  pfSimEvent->fCoreEastM  = -findTelescopeXM();
  pfSimEvent->fCoreSouthM = -findTelescopeYM();
  

  // ********************************************************************
  // Now tags for KASCADE puposes
  // ********************************************************************
  pfSimEvent->fNx=pfTe->fNx;         //used for shower sort
  pfSimEvent->fNy=pfTe->fNy;         //           '      '
  pfSimEvent->fDirectionIndex=pfTe->fDirectionIndex;// '      '

  pfSimEvent->fEmissionAltitudeM=(float)pfTe->fEmissionAltitude;
  pfSimEvent->fEmissionAltitudeSigma=(float)pfTe->fEmissionAltitudeSigma;
  pfSimEvent->fMuonRatio=(float)pfTe->fMuonRatio;
  pfSimEvent->fAomega=(float)pfTe->fAomega;
  pfSimEvent->fRelTriggerTimeNS=(float)fFADCStartGateTimeNS;
  
  return;
}
// **********************************************************************

double  KSEvent::findTelescopeXM()
// *********************************************************************
// convert from triangular array indicess.
// This supposedly works for both NS and EW arrays. We always want to round 
// more negative.)
//Kascade x + east, y + south
// ********************************************************************
{  
  if(pfPeHead->fNorthSouthGrid)
    {
      double fX=pfPeHead->fXAreaWidthM*pfTe->fNx - pfPeHead->fXCoreOffsetM;
      return fX;
    }
  else
    {
      // **************************************************************
      // Check to see if we are on odd or even row
      // **************************************************************
      if(pfTe->fNy%2==0) 
	{                   //Ny even
	  double fX=pfPeHead->fXAreaWidthM*pfTe->fNx - pfPeHead->fXCoreOffsetM;
	  return fX;
	}
      else
	{
	  double fX=pfPeHead->fXAreaWidthM*(pfTe->fNx+.5) - 
	                                            pfPeHead->fXCoreOffsetM;
	  return fX;

	}
    }
}

double  KSEvent::findTelescopeYM()
// *********************************************************************
// convert from triangular array indicess.
// This supposedly works for both NS and EW arrays. We alwyas want to round 
// more negative.)
// ********************************************************************
{  
  if(!pfPeHead->fNorthSouthGrid)
    {
      double fY=pfPeHead->fYAreaWidthM*pfTe->fNy - pfPeHead->fYCoreOffsetM;
      return fY;
    }
  else
    {
      // **************************************************************
      // Check to see if we are on odd or even column
      // **************************************************************
      if(pfTe->fNx%2==0) 
	{                   //Nx even
	  double fY=pfPeHead->fYAreaWidthM*pfTe->fNy - pfPeHead->fYCoreOffsetM;
	  return fY;
	}
      else
	{
	  double fY=pfPeHead->fYAreaWidthM*(pfTe->fNy+.5) - 
	                                            pfPeHead->fYCoreOffsetM;
	  return fY;

	}
    }
}
// *************************************************************************
