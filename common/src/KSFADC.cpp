/**
 * \class KSFADC
 * \ingroup common
 * \brief File of methods for KSFADC.
 * Original Author: Glenn H. Sembroski
 * $Author$
 * $Date$
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

void KSFADC::makeFADCTrace(std::vector<double>& fWaveForm, 
			   int fWaveFormStartIndex, int& fTraceLengthBins, 
			   bool fEnableHiLoGainProcessing)
// *********************************************************************
// Convert the fWaveForm to a FADC Trace
// *********************************************************************
// Note: If fEnableHiLoGainProcessing= true and fWaveForm cauuses us to flip
// into LOwGain mode then flage fFADCLowGain will be set true and 
// fTraceLengthBins WILL BE INCREASED!!!
{
  fFADCTrace.clear();
  fFADCTrace.resize(fTraceLengthBins);

  int fWaveFormBinsPerFADCBin=(int)(gFADCBinSizeNS/gWaveFormBinSizeNS);
  int fNumWaveFormBins1NS=(int)(1./gWaveFormBinSizeNS);

  int fLowGainThresholdIndex=0;
  fFADCLowGain=false;             //We start at High Gain
  for( int i=0; i<fTraceLengthBins; i++)  //
    {
// ********************************************************************
//  Fadc electronics averages over 1 nsec.
// ********************************************************************
      int fStartBin=fWaveFormStartIndex+i*fWaveFormBinsPerFADCBin;
      double fSum=0;
      for(int j=fStartBin;j<fStartBin+fNumWaveFormBins1NS;j++)
	{
	  fSum+=fWaveForm[j];
	}
      fSum=fSum/fNumWaveFormBins1NS;  
      if(fSum<0)
	{
	  fSum=0;                 //Since we may have subtracted a 
                                  //pedistal this may be negative. If so,
                                  //set it to 0, as it would be in the 
                                  //electronics.
	}
                           //Amplify to digital counts Hi Gain
      fFADCTrace[i]=(int)(fSum*gFADCDigCntsPerPEHiGain[fCameraType]);  

// *****************************************************************
// Hi/low: If we have a bin that goes over the HiLow threshold (usually about
//  250 ) The we need to swith to low gain mode.
//  We stop the hi gain trace right here.
//  Then after a few blank bins we will repeat the waveform but at a lower 
//  gain.  This end up increasing the #of Trace Bins
// *****************************************************************
      if(fEnableHiLoGainProcessing)
	{                
	  if((int)fFADCTrace[i]>gFADCHiLoGainThreshold)   //Look for high/low
	    {
	      fFADCLowGain=true;  //set Low gain flag.
	      fFADCTrace[i]=0; //We don't want any bins overHiLoGainThreshold
	      fLowGainThresholdIndex=i;
	      break;   //Go repeat pulse but at lower gain after a delay.
	    }
	}
    }
  // *************************************************************************
  // If we are low gain. delay the pulse and repeat at low gain.
  // *************************************************************************
  if(fEnableHiLoGainProcessing)
    {
      if(fFADCLowGain)
	{
	  int fLoTraceStart=fLowGainThresholdIndex+gFADCLowGainDelayBins;
	  fTraceLengthBins=fTraceLengthBins+fLoTraceStart;
	  fFADCTrace.resize(fTraceLengthBins);

	  for( int i=fLoTraceStart; i<fTraceLengthBins; i++)  //
	    {
// ********************************************************************
//  Fadc electronics averages over 1 nsec.
// ********************************************************************
	      int fStartBin=fWaveFormStartIndex+i*fWaveFormBinsPerFADCBin;
	      double fSum=0.0;
	      for(int j=fStartBin;j<fStartBin+fNumWaveFormBins1NS;j++)
		{
		  fSum+=fWaveForm[j];
		}
	      fSum=fSum/fNumWaveFormBins1NS;  
	      if(fSum<0)
		{
		  fSum=0;               //Since we may have subtracted a 
                                        //pedistal this may be negative. If so,
                                        //set it to 0, as it would be in the 
                                        //electronics.
		}
                                    //Amplify to digital counts:Low Gain
	      fFADCTrace[i]=(int)(fSum*gFADCDigCntsPerPELowGain[fCameraType]);

	      if((int)fFADCTrace[i]>gFADCHiLoGainThreshold) //Look for high/low
		{
		  fFADCTrace[i]=gFADCHiLoGainThreshold;       //Saturated
		}
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
      fSum+=fFADCTrace[fIndex];
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
      std::cout<<i<<" "<<fIndex<<" "<<fFADCTrace[fIndex]<<std::endl;
    }
  return;
}







