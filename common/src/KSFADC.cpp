/**
 * \class KSFADC
 * \ingroup common
 * \brief File of methods for KSFADC.
 * Original Author: Glenn H. Sembroski * $Author$Date$
 * $Revision$
 * $Tag$
 *
 **/

#include <iostream>
#include "KSFADC.h"


KSFADC::KSFADC()
{
  //Nothing to do here

}
// ************************************************************************

KSFADC::~KSFADC()
{
  // Nothing here
}
// ************************************************************************

  void KSFADC::SetCameraType(KSCameraTypes CameraType)
{
  fCameraType=CameraType;
  fDigCntsPerPEHiGain=gFADCDigCntsPerPEHiGain[fCameraType];
  fDigCntsPerPELoGain=gFADCDigCntsPerPEHiGain[fCameraType]/gFADCHiLoGainRatio;

  return;
}
// *************************************************************************

void KSFADC::SetDigCntsPerPEGains(double DigCntsPerPEHiGain)
{
  fDigCntsPerPEHiGain=DigCntsPerPEHiGain;
  fDigCntsPerPELoGain=DigCntsPerPEHiGain/gFADCHiLoGainRatio;
  return;
}
// ************************************************************************

void KSFADC::makeFADCTrace(std::vector<double>& fWaveForm, 
			   int fWaveFormStartIndex, int& fNumSamplesTrace, 
			   bool fEnableHiLoGainProcessing, 
			   double fFADCTracePed)
// *********************************************************************
// Convert the fWaveForm to a FADC Trace
// *********************************************************************
// The number of samples in the fFADCTrace is set by fNumSamplesTrace
// **********************************************************************
// Note: If fEnableHiLoGainProcessing= true and fWaveForm causes us to flip
// into LOwGain mode then flage fFADCLowGain will be set true 
// Unlike the real FADC we don't need to do a fancy delay for Low gain,
// just start over reducing wave form by the gain difference
// ***********************************************************
// We have to add a pedestal here because the FADC
// doesn't allow values below 0, so we need to offset up
// ************************************************************
{
  fFADCTrace.clear();
  fFADCTrace.resize(fNumSamplesTrace);

  int fWaveFormBinsPerFADCSample=(int)(gFADCBinSizeNS/gWaveFormBinSizeNS);
  int fNumWaveFormBins1NS=(int)(1./gWaveFormBinSizeNS);

  fFADCLowGain=false;             //We start at High Gain
  for( int i=0; i<fNumSamplesTrace; i++)  //
    {
// ********************************************************************
//  Fadc electronics averages over 1 nsec.
// ********************************************************************
      int fStartBin=fWaveFormStartIndex+i*fWaveFormBinsPerFADCSample;
      double fSum=0;
      for(int j=fStartBin;j<fStartBin+fNumWaveFormBins1NS;j++)
	{
	  fSum+=fWaveForm.at(j);
	}
      // **********************************************************
      // This is where we add the constant ped.
      // **********************************************************
      fSum=((fSum/fNumWaveFormBins1NS)*fDigCntsPerPEHiGain)+fFADCTracePed;  
      if(fSum<0)
	{
	  fSum=0;                 //Event though we added a pedestal
                                  //this may be negative. If so,
                                  //set it to 0, as it would be in the 
                                  //electronics.
	}
                           //Amplify to digital counts Hi Gain
      fFADCTrace.at(i)=(int)fSum;  

// *****************************************************************
// Hi/low: If we have a bin that goes over the HiLow threshold (usually about
//  250 ) The we need to swith to low gain mode.
//  We stop the hi gain trace right here.
//  Then after a few blank bins we will repeat the waveform but at a lower 
//  gain.  This end up increasing the #of Trace Bins
// *****************************************************************
      if(fEnableHiLoGainProcessing)
	{                
	  if((int)fFADCTrace.at(i)>gFADCHiLoGainThreshold)   //Look for high/low
	    {
	      fFADCLowGain=true;  //set Low gain flag.
	      break;   //Go repeat pulse but at lower gain after a delay.
	    }
	}
    }
  // *************************************************************************
  // If we are low gain. Repeat at low gain.
  // *************************************************************************
  if(fEnableHiLoGainProcessing && fFADCLowGain)
    {
      for( int i=0; i<fNumSamplesTrace; i++)  //
	{
	  // ***************************************************************
	  //  Fadc electronics averages over 1 nsec.
	  // ***************************************************************
	  int fStartBin=fWaveFormStartIndex+i*fWaveFormBinsPerFADCSample;
	  double fSum=0;
	  for(int j=fStartBin;j<fStartBin+fNumWaveFormBins1NS;j++)
	    {
	      fSum+=fWaveForm.at(j);
	    }
	  // **********************************************************
	  // This is where we add the constant ped, and mmplify to digital 
	  //counts Lo Gain
	  // **********************************************************
	  fSum=((fSum/fNumWaveFormBins1NS)*fDigCntsPerPELoGain)+fFADCTracePed;
	  if(fSum<0)
	    {
	      fSum=0;             //Event though we added a pedestal
	                          //this may be negative. If so,
	                          //set it to 0, as it would be in the 
                                  //electronics.
	    }
	  fFADCTrace.at(i)=(int)fSum;  
	  if((int)fFADCTrace.at(i)>gFADCHiLoGainThreshold) //Look for high/low
	    {
	      fFADCTrace.at(i)=gFADCHiLoGainThreshold;       //Saturated
	    }
	}
    }
  return;
}
// ***************************************************************************

double KSFADC::getWindowArea(int fStartTraceIndex, int fNumBinsToSum)
// ***************************************************************************
// Sum bins in trace statring at fStartTraceIndex. Result is in Digital Counts
// Since the makeFADCTrace creates a fFADCTrace in DC.
// ***************************************************************************
{
  int fTraceLength=fFADCTrace.size();
  if(fTraceLength<fStartTraceIndex+fNumBinsToSum)
    {
      std::cout<<"KSFADC: Request for non-existant fFADCTrace bins in "
	"KSFADC::getWindowArea"<<std::endl;
      return 0;
    }
  double fSum=0;
  for(int i=0;i<fNumBinsToSum;i++)
    {
      int fIndex=fStartTraceIndex+i;
      fSum+=fFADCTrace.at(fIndex);
    }
  return fSum;
}
// ************************************************************************	
void  KSFADC::Print(int fStartTraceIndex, int fNumBinsToPrint)
{

  int fTraceLength=fFADCTrace.size();
  if(fTraceLength<fStartTraceIndex+fNumBinsToPrint)
    {
      std::cout<<"KSFADC: Request for non-existant fFADCTrace bins in "
	"KSFADC::Print"<<std::endl;
      return;
    }
  for(int i=0;i<fNumBinsToPrint;i++)
    {
      int fIndex=fStartTraceIndex+i;
      std::cout<<i<<" "<<fIndex<<" "<<fFADCTrace.at(fIndex)<<std::endl;
    }
  return;
}







