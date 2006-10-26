//-*-mode:c++; mode:font-lock;-*-
/**
 * \class KSVBFFile
 * \ingroup common
 * \brief Methods for Kascade VBF file.
 
 * Original Author: Glenn H. Sembroski
 * $Author$
 * $Date$
 * $Revision$
 * $Tag$
 *
 **/

#include "KSVBFFile.h"

extern "C" int    KascadeType2CorsikaType(int fKType);
extern "C" void   GetAzElevFromVec(double* X, double& fAzimuth, 
				   double& fElevation);

KSVBFFile::KSVBFFile(KSCameraTypes CameraType, double DigitalCountsPerPE, 
		     int CORSIKAType, KSSegmentHeadData* pSegmentHead,
		     KSPeHeadData* pfPeHead, KSCamera* pCamera)
{
  fCameraType=CameraType;
  fDigitalCountsPerPE=DigitalCountsPerPE;

  // ************************************************************************
  //Constant Stuff for simulation banks (VSimulationHeader and 
  // VKascadeSimulationHeader)
  // ************************************************************************
  pfSegmentHead = pSegmentHead;
  fCORSIKAType  = (uword32) KascadeType2CorsikaType(pfSegmentHead->fType);
  fEnergyGeV    = (float)   pfSegmentHead->fGeVEnergyPrimary;
  fShowerID     = (uword32) pfSegmentHead->fShowerID;
  fObsAlt       = (float)   pfSegmentHead->fObservationAltitudeM; 

  // *************************************************
  // Get mount directions az.,elev
  // *************************************************

  double X[3];
  X[0]=pfSegmentHead->fDlInitial;
  X[1]=pfSegmentHead->fDmInitial;
  X[2]=sqrt(1.-X[0]*X[0]-X[1]*X[1]);   //Elevation positive
  double fAzimuth;
  double fElevation;
  GetAzElevFromVec(X,fAzimuth,fElevation);
  fPrimaryZenithDeg  = ((M_PI/2)-fElevation)*gRad2Deg;
  fPrimaryAzimuthDeg = fAzimuth*gRad2Deg;
  fCoreElevationMASL=pfSegmentHead->fObservationAltitudeM;
  //fGrISUTrigger=1;

  fXSeg=pfPeHead->fXAreaWidthM;
  fYSeg=pfPeHead->fYAreaWidthM;
  fNSFlag=0;
  if(pfPeHead->fNorthSouthGrid)
    {
      fNSFlag=1;
    }
  fXOffset=pfPeHead->fXCoreOffsetM;
  fYOffset=pfPeHead->fYCoreOffsetM;
  
  pfCamera=pCamera;


  pfWriter=NULL;
}
// *************************************************************************

KSVBFFile::~KSVBFFile()
{
  //Nothing yet
}

// ***************************************************************************


bool KSVBFFile::Create(KSAomegaDataIn* pfDataIn,
		       std::string fConfigMask,VATime& fEventTime)
// ***************************************************************************
//  Create an output VBF file. Write out the header event (event 0) with a
// VSimulationHeader bank and a VKascadeSimulationHeader bank.
// ***************************************************************************
{
   fFoundError=false;
   if(pfWriter!=NULL)
    { 
      std::cout<<"KSVBFFile-- Output VBF file already created"
	       <<std::endl;
      fFoundError=true;
      return false;
    }
  // open a file for writing, specifying that the filename is 'bogus.vbf'
  // that the run number is 400, and that the configuration mask
  // includes the first two telescopes.  note that you'll need to
  // provide the run number to the array event and array trigger below as
  // well.
  fRunNumber=pfDataIn->fRunNumber;

  pfWriter = new VBankFileWriter(pfDataIn->fVBFFileName.c_str(),fRunNumber,
				 parseConfigMask(fConfigMask.c_str()));
  if(pfWriter==NULL)
    {
      std::cout<<"KSVBFFile--Output VBF file failed to open"
	       <<std::endl;
      return false;
      //throw exception
    }

  VPacket *packet=new VPacket();

  // **********************************************************************
  // Set up Simulation Header variables (see VSimulationHeader.h and 
  // VKascadeSimulationHeader.h files for definitions.)
  // **********************************************************************
  // Set up Simulation Header variables
  // **********************************************************************
  //Get present time
  VATime fTimeNow;
  fTimeNow.setFromSystemTime();
  uint32_t y,m,d;
  fTimeNow.getCalendarDate(y,m,d);
  uword32 fDateOfSimsUTC= y*10000ULL+ m*100ULL+ d;
  uword32 fSimulationPackage   = KASCADE;  //Purdue Kascade
  std::string fSimulator           = "SEMBROSKI";  //Purdue Sembroski
  uword32 fDateOfArrayForSims  = 0;
  if(pfDataIn->fPixelStatsRootFileName!=" ")
    {
      fEventTime.getCalendarDate(y,m,d);
      fDateOfArrayForSims= y*10000ULL + m*100ULL + d;
    }
  uword32 fAtmosphericModel    = 0;  //no codes as of yet
    
  loadArrayConfiguration();
  
  fSimConfigFile = "Not Specified";
  if(pfDataIn->fSimulationConfigFileName!=" ")
    {
      loadSimConfigFileFromFile(pfDataIn->fSimulationConfigFileName);
    } 
  
  // ********************************************************************
  // We need to create and write a vSimulationHeader bank and a 
  // VKascadeSimulationBank
  // Kascade specific header stuff already filled in constructor.
  // ********************************************************************
  
  VSimulationHeader* pfSimHead = 
    new VSimulationHeader(fDateOfSimsUTC, fSimulationPackage,
			  fSimulator, fDateOfArrayForSims,
			  fAtmosphericModel, fObsAlt, fArray,
			  fSimConfigFile);
  
  VKascadeSimulationHeader* pfKSimHead = 
    new VKascadeSimulationHeader(fCORSIKAType, fEnergyGeV, fShowerID,
				      (float)fXSeg, (float)fYSeg, fNSFlag);
    
  // ***********************************************************************
  // Put these header banks into packet(0)
  // ***********************************************************************
  // Put the simulation header data into the packet
  packet->put(VGetSimulationHeaderBankName(), pfSimHead);
  if (!packet->has(VGetSimulationHeaderBankName())  )
    {
      std::cout<<"KSVBFFile: No SimulationHeader bank in packet when "
	"we just put one in"<<std::endl;
      exit(1);
    }
  //else
  // {
  //   std::cout<<"KSVBFFile: Packet has VSimulationHeader"<<std::endl; 
  // }
 
  // and put the KascadeSimulation header data into the packet
  packet->put(VGetKascadeSimulationHeaderBankName(), pfKSimHead);  if (!packet->has(VGetKascadeSimulationHeaderBankName())  )
    {
      std::cout<<"KSVBFFile: No KascadeSimulationHeader bank in packet when "
  	"we just put one in"<<std::endl;
      // exit(1);
    }
  // finally, write the packet into the file
  uword32 fArrayEventNum=0;
  pfWriter->writePacket(fArrayEventNum, packet);
  //std::cout<<"Wrote Packet: 0"<<std::endl;

  // dispose of the packet, so that we don't leak memory
  delete packet;
  return true;
}
// ***************************************************************************

void KSVBFFile::Close()
// ***************************************************************************
//  Close the opened file.
// ***************************************************************************
{
  if(pfWriter!=NULL)
    {
      // finish up.  this creates the index and writes the checksum.
      pfWriter->finish();
      std::cout<<"KSVBFFile: Finished (kind of a close) Output VBF file!"
	       <<std::endl;
    }
  return;
}
// ***************************************************************************

void KSVBFFile::WriteVBF(int fArrayEventNum, int fTelID, VATime& fEventTime, 
			 double fFADCStartGateTimeNS, KSTeData* pfTe,
			 bool fPedestalEvent, float fCoreEastM,
			 float fCoreSouthM) 

// ***************************************************************************
// Write the VBF event(single telescope for now) to the output file.
// ***************************************************************************
{

  // ******************************************************************
  // Now proceed in writing out event (ped or normal)
  // ******************************************************************
  if(pfWriter==NULL)
    {
      std::cout<<"KSVBFFile--Output VBF file is not yet opened"
	       <<std::endl;
      fFoundError=true;
      //throw exception
    }

  VPacket *packet=new VPacket();
            
  // ************************************************************************
  // first create the array event.  notice that the array event also
  // has to know about the run number.
  // ************************************************************************
  VArrayEvent *ae=new VArrayEvent(fRunNumber);
  VEvent *event=new VEvent();
                

  // **********************************************************************
  // Each event gets compressed using VBF compression
  // **********************************************************************
  event->setCompressedBit( true );

  // ************************************************************************
  // resize channel data. Use fCameraType for num channels, but use 
  // VERITAS499 type for Num samples
  // ************************************************************************
  event->resizeChannelData(gFADCNumSamples[VERITAS499],
			                    gNumChannelsCamera[fCameraType]);
		
  // ************************************************************************
  // by default, the max num channels is 0.  it must be set to the
  // actual number of channels recognized by the DACQ.  here we
  // set it to 500(for VERITAS499, 492 for WHIPPLE490)
  // ************************************************************************
  event->resizeChannelBits(gNumChannelsCamera[fCameraType]);

  // ************************************************************************
  // by default, the event contains 0 clock trigger boards.  here
  // I make it 1
  event->resizeClockTrigData(1);
 
  // ************************************************************************
  // note that you call call those resize methods more than once.
  // in particular, you can keep adding channels, samples, or clock
  // trigger boards with repeated calls to the resize methods.
  // this may not give you great performance, however.
                
  // set the event number
  event->setEventNumber(fArrayEventNum);
                
  // set the node number (telescope number)
  event->setNodeNumber(fTelID);
                
  // ************************************************************************
  // now I will set all of the fields to 0, except for the hit pattern,
  // which will be set so all of the bits are 1 (this means that no channels
  // got zero-suppressed).  if you're creating events from
  // a simulation, you can use this code as a template!
  // ************************************************************************
  uint8_t  fGPSYear;
  uint16_t fGPSWords[5];
  fEventTime.getForVBF(fGPSYear,5,fGPSWords);
  event->getGPSTime()[0]=fGPSWords[0];
  event->getGPSTime()[1]=fGPSWords[1];
  event->getGPSTime()[2]=fGPSWords[2];
  event->getGPSTime()[3]=fGPSWords[3];
  event->getGPSTime()[4]=fGPSWords[4];
                
  event->setGPSYear(fGPSYear);
  
  event->setFlags(1); // enable compression

  if(!fPedestalEvent)
    {
      VEventType fEvType;
      fEvType.trigger=VEventType::L2_TRIGGER;
      event->setEventType(fEvType);
      //event->setEventTypeCode(0); //0 is an acceptable L2_TRIGGER
      //event->setEventTypeCode(VEventType::L2_TRIGGER);
                                       // specify which channels triggered.
      for (unsigned k=0;k<(unsigned)gNumPixelsCamera[fCameraType];++k) 
	{
	  event->setTriggerBit(k,pfCamera->isCFDTriggered(k));
	}
      if(gNumChannelsCamera[fCameraType]>gNumPixelsCamera[fCameraType])
	{
	  for (unsigned k=gNumPixelsCamera[fCameraType];
	       k<(unsigned)gNumChannelsCamera[fCameraType];++k) 
	    {
	      event->setTriggerBit(k,false);
	    }
	}
    }
  else
    {         //Pedestal event
      VEventType fEvType;
      fEvType.trigger=VEventType::PED_TRIGGER;
      event->setEventType(fEvType);

      //event->setEventTypeCode(VEventType::PED_TRIGGER);
                                  // specify no channels triggered.
      for (unsigned k=0;k<(unsigned)gNumChannelsCamera[fCameraType];++k) 
	{
	  event->setTriggerBit(k,false);
	}
    }


  // *********************************************************************
  // specify that all channels passed zero suppression
  // Set trigger bits as needed
  // *********************************************************************
  for (unsigned k=0;k<event->getMaxNumChannels();++k)
    {
      event->setHitBit(k,true);
    }

  // *************************************************************
  // Since this VBF file is supposed to look like a VERITAS file 
  // Use VERITAS499 for num of samples. Also use VERITAS499 
  // pedestal
  // *************************************************************
  int fADCNumBins = (int)((gFADCNumSamples[VERITAS499]*gFADCBinSizeNS) /
			  gWaveFormBinSizeNS);

  for (unsigned k=0;k<(unsigned)gNumPixelsCamera[fCameraType]; ++k)
    {
      // *****************************************************************
      // Charge and ped always set to zero in VBF file
      // *****************************************************************
      event->setCharge(k,0);          //Always set to 0.
      event->setPedestal(k,0);
      if(pfCamera->fPixel[k].fBadPixel)
	{
	  event->setHiLo(k,false); //set hi?
	  for (unsigned l=0; l<event->getNumSamples(); ++l)
	    {
	      short unsigned int fSam = 
		                  (short unsigned int)gPedestal[fCameraType];
	      event->setSample(k,l,fSam);
	    }
	}
      else
	{
	  if(!fPedestalEvent)
	    {
	      int fStartGateBin=(int)((fFADCStartGateTimeNS-
				       gFADCWindowOffsetNS[fCameraType]-
				       pfCamera->fPixel[k].fWaveFormStartNS)/
				      gWaveFormBinSizeNS);
	      if(fStartGateBin<0)
		{
		  std::cout<<"KSVBFFile: Start Gate bin was <0"<<std::endl;
		  exit(1);
		}
	      // *************************************************************
	      // Convert wave form to FADC trace.  Pedestal(use VERITAS 
	      // pedestal)
	      // Added in MakeTrace
	      pfCamera->fPixel[k].fFADC.makeFADCTrace(
					       pfCamera->fPixel[k].fWaveForm,
					       fStartGateBin, fADCNumBins,true,
					       gPedestal[VERITAS499]);
	      // **************************************************************
	      // Now we are ready to load up the VBF samples.
	      // ************************************************************* 
	      event->setHiLo(k,pfCamera->fPixel[k].fFADC.fFADCLowGain);
	      for (unsigned l=0; l<event->getNumSamples(); ++l)
		{
		  short unsigned int fTrc=
		  (short unsigned int)pfCamera->fPixel[k].fFADC.fFADCTrace[l];
		  event->setSample(k,l,fTrc);
		}
	    }
	  else
	    {   //Pedestal event, use fPedPixels for waveform source
	      // *************************************************************
	      // Convert wave form to FADC trace.  Pedestal(use VERITAS 
	      // pedestal)
	      // Added in MakeTrace
	      pfCamera->fPedPixels[k].fFADC.makeFADCTrace(
					  pfCamera->fPedPixels[k].fWaveForm,0,
					  fADCNumBins,true,
					  gPedestal[VERITAS499]);
	      event->setHiLo(k,pfCamera->fPedPixels[k].fFADC.fFADCLowGain);
	      for (unsigned l=0; l<event->getNumSamples(); ++l)
		{
		  short unsigned int fTrc=
	      (short unsigned int)pfCamera->fPedPixels[k].fFADC.fFADCTrace[l];
		  event->setSample(k,l,fTrc);
		}
	    }
	}
      // *******************************************************
      // Fake Clock Trigger Boards 
      // *******************************************************
      for (unsigned k=0; k<event->getNumClockTrigBoards(); ++k)
	{
	  for (unsigned l=0; l<7;  ++l) 
	    {
	      event->getClockTrigData(k)[l]=0;
	    }
	}
      
    }
  // add the event to the array event!
  ae->addEvent(event);

            
  // ************************************************************************
  // now create an array trigger
  // ************************************************************************
  VArrayTrigger *at=new VArrayTrigger();
            
  //array trigger will contains 1 subarray telescope
  at->resizeSubarrayTelescopes(1);

  // array trigger will contains 1 triggered telescope
  at->resizeTriggerTelescopes(1);
            
  // set the event number
  at->setEventNumber(fArrayEventNum);
            
  // set the node number to 255.
  at->setNodeNumber(255);
            
  // the array trigger also needs to know about the run number.
  at->setRunNumber(fRunNumber);

  // SEtGPS Time
  at->getGPSTime()[0]=fGPSWords[0];
  at->getGPSTime()[1]=fGPSWords[1];
  at->getGPSTime()[2]=fGPSWords[2];
  at->getGPSTime()[3]=fGPSWords[3];
  at->getGPSTime()[4]=fGPSWords[4];
            
  at->setGPSYear(fGPSYear);
            
  if(!fPedestalEvent)
    {
      VEventType fEvType;
      fEvType.trigger=VEventType::L2_TRIGGER;
      at->setEventType(fEvType);
      //at->setEventTypeCode(VEventType::L2_TRIGGER);
    }
  else
    {
      VEventType fEvType;
      fEvType.trigger=VEventType::PED_TRIGGER;
      at->setEventType(fEvType);
      //at->setEventTypeCode(VEventType::PED_TRIGGER);
    }
  at->setFlags(0);
            
  at->setATFlags(0);

  // ************************************************************************
  // have to set the telescope ID that this record corresponds to.
  // in this case, the record number and telescope ID happen to
  // be the same
  at->setSubarrayTelescopeId(0,fTelID);
  // ********************************************************************
  // now construct the simulation data. Common data first
  // ********************************************************************
  double X[3];
  double fAzimuth;
  double fElevation;
  X[0]=pfTe->fMountDl;
  X[1]=pfTe->fMountDm;
  X[2]=sqrt(1.-X[0]*X[0]-X[1]*X[1]);   //Elevation positive
  GetAzElevFromVec(X,fAzimuth,fElevation);

  float fObservationZenithDeg  = ((M_PI/2)-fElevation)*gRad2Deg;
  float fObservationAzimuthDeg = fAzimuth*gRad2Deg;

  at->setAltitude(0,(float)fObservationZenithDeg);
  at->setAzimuth(0,(float)fObservationAzimuthDeg);
  at->setTDCTime(0,0);
  VEventType fEvType;
  fEvType.trigger=VEventType::L2_TRIGGER;
  at->setSpecificEventType(0,fEvType);

  //  at->setSpecificEventTypeCode(0,1);
  at->setShowerDelay(0,0);
  at->setCompDelay(0,0);
   
            
  // now add the array trigger to the array event
  ae->setTrigger(at);
            
  // put the array event into the packet
  packet->putArrayEvent(ae);
            

  uint32_t fNx=pfTe->fNx;
  uint32_t fNy=pfTe->fNy;

  VSimulationData *pfSimdata=
        new VSimulationData(fCORSIKAType,fEnergyGeV, 
			    fObservationZenithDeg,
			    fObservationAzimuthDeg, fPrimaryZenithDeg,
			    fPrimaryAzimuthDeg, fCoreEastM,
			    fCoreSouthM, fCoreElevationMASL);
  //,fGrISUTrigger);

  float fIntergralRatePerEventHz=0;
  float fDifferentialRatePerEventHz=0;

  VKascadeSimulationData *pfKSimdata=
    new VKascadeSimulationData(fNx, fNy,
			       (uint32_t)pfTe->fDirectionIndex, 
			       (float)pfTe->fEmissionAltitude,
			       (float)pfTe->fEmissionAltitudeSigma,
			       (float)pfTe->fMuonRatio,
			       (float)pfTe->fAomega,
			       (float)fFADCStartGateTimeNS,
			       fIntergralRatePerEventHz,
			       fDifferentialRatePerEventHz
			       );


  // and put the simulation data into the packet
  packet->put(VGetSimulationDataBankName(),pfSimdata);
  // and put the Kascade Simulation data into the packet
  packet->put(VGetKascadeSimulationDataBankName(),pfKSimdata);
            
  // finally, write the packet into the file
  //if(fPedestalEvent)
  // {
  //   std::cout<<"Wrote Ped Packet: "<<fArrayEventNum<<std::endl;
  // }
  //else
  //  {
  //    std::cout<<"Wrote packet: "<<fArrayEventNum<<std::endl;
  //  }
  pfWriter->writePacket(fArrayEventNum, packet);
            
  // dispose of the packet, so that we don't leak memory
  delete packet;
  return;
}
// ***********************************************************************

 
bool KSVBFFile::loadSimConfigFileFromFile(std::string SimConfigFileName)
// **********************************************************************
// Load the fSimConfigFile from a file.
// **********************************************************************
{
// read the file
  int fFile=open(SimConfigFileName.c_str(),O_RDONLY);
  if (!fFile)
    {
      //Throw exception
      std::cout<<"VASimulationHeader--Failed to open input Simulation "
	"configuration text file "<<SimConfigFileName<<std::endl;
      return false;
    }
  int fLength=lseek(fFile,0,SEEK_END);
  if (fLength<0)
    {
      //Throw exception
      std::cout<<"VASimulationHeader--Failed to get length of Input "
	"Simulation configuration text file "<<SimConfigFileName<<std::endl;
      return false;
	}

  lseek(fFile,0,SEEK_SET);
  char* fBuf=new char[fLength];
  int fResult=read(fFile,fBuf,fLength);
  if (fResult!=fLength) 
    {
      //Throw exception
      std::cout<<"VASimulationHeader--Read length fails to match length of "
	"input Simulation configuration text file "<<SimConfigFileName
	       <<std::endl;
      return false;
    }
  close(fFile);
  fSimConfigFile=std::string(fBuf,fLength);
  return true;
}
// **************************************************************************

void  KSVBFFile::loadArrayConfiguration()
// **********************************************************************
// Load the ArrayConfiguration.
// **********************************************************************
{
// **************************************************************************
// This code is for a Multiple telescopes. Uses VAArrayInfo factory
// if desired.
// **************************************************************************
  //This will load the default VERITAS arrayInfo at fTime.
  VATime fTime("2006-08-23 22:00:00 UTC");
  VAArrayInfo* pfArrayInfo=
    VAArrayInfoFactoryLite::instance()->getArrayInfo(fTime); 
  // ***********************************************************************
  // For values in a VAArrayInfo positionEW:East is positive,
  //  positionNS: North is positive, positionUD: Up is positive

  for(int iTel=0;iTel<4;iTel++)  //East is positive,North positive
    {
      VArrayConfiguration fArConfig;
      fArConfig.fRelTelLocSouthM = -pfArrayInfo->telescope(iTel)->positionNS();
      fArConfig.fRelTelLocEastM  = pfArrayInfo->telescope(iTel)->positionEW();
      fArConfig.fRelTelLocUpM    = pfArrayInfo->telescope(iTel)->positionUD();
      for(int i=0;i<gNumPixelsCamera[fCameraType];i++)
	{
	  VPixelLocation fPixLoc;
	  fPixLoc.fPixLocEastAtStowDeg = pfCamera->fPixel[i].fXDeg;
	  fPixLoc.fPixLocUpAtStowDeg   = pfCamera->fPixel[i].fYDeg;
	  fPixLoc.fPixRadiusDeg        = pfCamera->fPixel[i].fRadiusDeg;
	  fArConfig.fCamera.push_back(fPixLoc);
	}
      fArray.push_back(fArConfig);
    }
  return;
}
