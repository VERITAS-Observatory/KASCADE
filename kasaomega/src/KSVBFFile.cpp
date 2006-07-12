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

KSVBFFile::KSVBFFile(KSCameraTypes CameraType, double DigitalCountsPerPE)
{
  fCameraType=CameraType;
  fDigitalCountsPerPE=DigitalCountsPerPE;
  pfWriter=NULL;
}

KSVBFFile::~KSVBFFile()
{
  //Nothing yet
}

// ***************************************************************************


  bool KSVBFFile::Create(std::string fVBFFileName, int RunNumber, 
			                     std::vector< bool >& fConfigMask)
// ***************************************************************************
//  Create an output VBF file.
// ***************************************************************************

{
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
 fRunNumber=RunNumber;

 pfWriter = new VBankFileWriter(fVBFFileName.c_str(),fRunNumber,
				fConfigMask);
 
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
    }
  return;
}
// ***************************************************************************

void KSVBFFile::WriteVBF(int fArrayEventNum, int fTelID, 
			 VATime& fEventTime, KSCamera* pfCamera,
			 double fFADCStartGateTimeNS) 

// ***************************************************************************
// Write the VBF event(single telescope for now) to the output file.
// ***************************************************************************
{
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
                
  // ************************************************************************
  // resize channel data. Use fCameraType for num channels, but use 
  // VERITAS499 type for Num samples
  // ************************************************************************
  event->resizeChannelData(gFADCNumSamples[VERITAS499],
			                       gNumPixelsCamera[fCameraType]);
		
  // ************************************************************************
  // by default, the max num channels is 0.  it must be set to the
  // actual number of channels recognized by the DACQ.  here we
  // set it to 499
  // ************************************************************************
  event->resizeChannelBits(gNumPixelsCamera[fCameraType]);

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
  
  event->setEventTypeCode(ET_ARRAY_TRIGGER);
                
  event->setFlags(1); // enable compression

  // specify which channels triggered.
  for (unsigned k=0;k<event->getMaxNumChannels();++k) 
    {
      if(pfCamera->fPixel[k].fCFDTriggerTimeNS<gOverflowTime)
	{
	  event->setTriggerBit(k,true);
	}
      else
	{
	  event->setTriggerBit(k,false);
	}
		
    }
  // specify that all channels passed zero suppression
  for (unsigned k=0;k<event->getMaxNumChannels();++k)
    {
      event->setHitBit(k,true);
    }
                
  for (unsigned k=0;k<event->getNumChannels(); ++k)
    {
      // *****************************************************************
      // Charge and ped always set to zero in VBF file
      // *****************************************************************
      event->setCharge(k,0);          //Always set to 0.
      event->setPedestal(k,0);
      if(!pfCamera->fPixel[k].fBadPixel)
	{
	  event->setHiLo(k,false); //set hi?
	  for (unsigned l=0; l<event->getNumSamples(); ++l)
	    {
	      event->setSample(k,l,0);
	    }
	}
      else
	{
	  // ***********************************************************
	  // We have to add a pedestal here and then remove it because the
	  //FADC doesn't allow values below 0, so we need to offset up
	  // ************************************************************
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
	  // Since this VBF file is supposed to look like a VERITAS file 
	  // Use VERITAS499 for num of samples. Also use VERITAS499 
	  // pedestal
	  // *************************************************************
	  int fADCNumBins = (int)((gFADCWinSize[VERITAS499]*
				   gFADCBinSizeNS) /
				  gWaveFormBinSizeNS);
	  // *************************************************************
	  // Convert wave form to FADC trace. First add in the 
	  // pedestal(use VERITAS pedestal)
	  pfCamera->fPixel[k].
		AddPedestalToWaveForm(gWaveFormPedestalPE[VERITAS499]);
	  pfCamera->fPixel[k].fFADC.makeFADCTrace(
	      pfCamera->fPixel[k].fWaveForm,fStartGateBin, fADCNumBins,false);
	  
	  // **************************************************************
	  // Now we are read y to load up the VBF samples.
	  // We will need to convert to hi/low
	  // First see if we need to
	  // ************************************************************* 
	  bool fLowGain=false;
	  double fGain=1.0;  //default is no gain change
	  for (unsigned l=0; l<event->getNumSamples(); ++l)
	    {
	      int fTraceValue=pfCamera->fPixel[k].fFADC.fFADCTrace[l];
	      if(fTraceValue>gFADCHiLoGainThreshold)
		{
		  fLowGain=true;
		  fGain=1./gFADCHiLoGainRatio;
		  break;
		}
	    }
	  event->setHiLo(k,fLowGain);
	  for (unsigned l=0; l<event->getNumSamples(); ++l)
	    {
	      double fTraceValue=
		              (double)pfCamera->fPixel[k].fFADC.fFADCTrace[l];
	      short unsigned int fTrc=(short unsigned int)(fTraceValue*fGain);
	      if(fTrc>gFADCHiLoGainThreshold)
		{
		  fTrc=gFADCHiLoGainThreshold;
		}
	      event->setSample(k,l,fTrc);
	    }
	}
	  
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
            
  // set the node number to 1.
  at->setNodeNumber(1);
            
  // the array trigger also needs to know about the run number.
  at->setRunNumber(fRunNumber);

  // SEtGPS Time
  at->getGPSTime()[0]=fGPSWords[0];
  at->getGPSTime()[1]=fGPSWords[1];
  at->getGPSTime()[2]=fGPSWords[2];
  at->getGPSTime()[3]=fGPSWords[3];
  at->getGPSTime()[4]=fGPSWords[4];
            
  at->setGPSYear(fGPSYear);
            
  at->setEventTypeCode(ET_ARRAY_TRIGGER);
             
  at->setFlags(0);
            
  at->setATFlags(0);
            
  for (unsigned j=0; j<at->getNumSubarrayTelescopes(); ++j)
    {
      // have to set the telescope ID that this record corresponds to.
      // in this case, the record number and telescope ID happen to
      // be the same
      at->setSubarrayTelescopeId(j,j);
                
      at->setAltitude(j,0.0);
      at->setAzimuth(j,0.0);
      at->setTDCTime(j,0);
      at->setSpecificEventTypeCode(j,0);
      at->setShowerDelay(j,0);
      at->setCompDelay(j,0);
    }
            
  // now add the array trigger to the array event
  ae->setTrigger(at);
            
  // put the array event into the packet
  packet->putArrayEvent(ae);
            
  // ********************************************************************
  // now construct the simulation data.  I'll set the fields to 0, but
  // you can set them to whatever you like!
  // ********************************************************************

  float r[3];
  r[0]=0.0;
  r[1]=0.0;
  r[2]=0.0;
            
  VSimulationData *simu_data=
    new VSimulationData(0, // id
			0.0,   // e
			0.0,   // theta
			0.0,   // phi
			r);
            
  // and put the simulation data into the packet
  packet->putSimulationData(simu_data);
            
  // finally, write the packet into the file
  pfWriter->writePacket(fArrayEventNum, packet);
            
  // dispose of the packet, so that we don't leak memory
  delete packet;
  return;
}
