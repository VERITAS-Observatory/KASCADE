/**
 * \class KSPixel 
 * \ingroup common
 * \brief File of methods for KSPixel.
 *  
 
 * Original Author: Glenn H. Sembroski
 * $Author$
 * $Date$
 * $Revision$
 * $Tag$
 *
 **/
// This is where the code starts.

#include "KSPixel.h"

extern "C" float pran(float* fXDummy);
extern "C" double Rexp(double fRate);

KSPixel::KSPixel()
{
  InitPixel();
}
KSPixel::KSPixel(KSCameraTypes CameraType)
{
  fCameraType=CameraType;
  fFADC.SetCameraType(CameraType);
  InitPixel();
}

void KSPixel::InitPixel()
{
 //This is where we could reset the rise and fall times of the single pe 
  //pulse
  fSinglePeSizeNS      = fSinglePe.getLengthNS();
  fSinglePeSizeNumBins = fSinglePe.fNumBinsInPulse;
  fSinglePeArea        = fSinglePe.getArea();
  fSinglePeMeanFADCArea= fSinglePe.getMeanFADCArea(fCameraType,fFADC);
}
// **************************************************************

KSPixel::~KSPixel()
{
  //nothing to do
}
 
// *************************************************************************

void KSPixel::InitWaveForm(double WaveFormStartNS,double fWaveFormLengthNS)
// ********************************************************************
// Allocate the wave form
// ********************************************************************
{
  fWaveFormStartNS=WaveFormStartNS;
  fNumWaveFormBins=(int)(fWaveFormLengthNS/gWaveFormBinSizeNS + 1.0);
  fWaveForm.clear();
  fWaveForm.resize(fNumWaveFormBins);
  return;
}
// ********************************************************************

void KSPixel::BuildPeWaveForm()
// **************************************************************************
// In fWaveForm, build the wave form we expect from the real pe's 
// **************************************************************************
{
  int fNumPes=fTimePe.size();
  fDisc=0;
  bool fAfterPulse=false;
  if(fNumPes>0)
    {
      for(int i=0;i<fNumPes;i++)
	{
	  if(pran(&fXDummy)<fEfficiency)
	    {
	      fDisc++;
	      double fPeTime=fTimePe[i]-fWaveFormStartNS;
	      addPe(fPeTime,fAfterPulse);
	    }
	}
    }
  return;
}

void KSPixel::AddNoiseToWaveForm(bool fAfterPulse)
// **************************************************************************
// In fWaveForm, Add the noise pulses to it.
// **************************************************************************
// Add pe at random times using Rexp for time intervels
{
  double fMeanTimeGap=1./fNoiseRatePerNS;

  double fNoiseTimeNS=-fSinglePeSizeNS;
  while(fNoiseTimeNS<gWaveFormBinSizeNS*fNumWaveFormBins)
    {
      addPe(fNoiseTimeNS,fAfterPulse);
      fNoiseTimeNS+=Rexp(fMeanTimeGap);
    }
  return;
}

void KSPixel::addPe(double fPeTime,bool fAfterPulse)
// **************************************************************************
// Add a single pe to the waveForm array.
// **************************************************************************
{
  if((-(fSinglePeSizeNS+gWaveFormBinSizeNS)-fPeTime)>0)
    {
      int fStartBin=(int)(fPeTime/gWaveFormBinSizeNS);//Noise can start at or 
                                                    //before 0.
      int fPeStartIndex=0;
      int fPeEndIndex=fSinglePeSizeNumBins-1;
      if(fStartBin<0)              //We only have the tail end of the pe pulse
	{
	  fPeStartIndex=-fStartBin;
	  fStartBin=0;
	}
      else if((fStartBin+fSinglePeSizeNumBins-1)>fNumWaveFormBins)
	{
	  fPeEndIndex=fSinglePeSizeNumBins-fStartBin-1;
	}
      double fPulseHeight=fSinglePe.getPulseHeight(fAfterPulse);
      // Now load in the single pe
      int fWaveFormIndex=fStartBin;     
      for(int i=fPeStartIndex;i<=fPeEndIndex;i++)
	{
	  fWaveForm[fWaveFormIndex]+=fSinglePe.pfSinglePulse[i]*fPulseHeight;
	  fWaveFormIndex++;
	}
    }
  return;
} 
// ***************************************************************************

void KSPixel::DetermineNoisePedestals()
// ***************************************************************************
// Using the noise rate, model a very long wave form of night sky noise Pe.s
// Determine average value value of WaveForm bins. This will be our 
// fWaveFormNightSkyPedestal. Models the fact that we are capacitivly coupled.
// Both WHIPPLE490 and VERITAS499 PMT bases.
// ***************************************************************************
// From this same waveform determine ther FADC window Charge Pedvar
{
  InitWaveForm(0.0,gNightSkyWaveFormNS);
  double fPulseTimeNS=-fSinglePeSizeNS;  // start early to get partial
  double fMeanTimeBetweenNoisePesNS= 1./fNoiseRatePerNS;
  fPulseTimeNS += Rexp(fMeanTimeBetweenNoisePesNS);
  bool fAfterPulse=true;

  while(fPulseTimeNS<gNightSkyWaveFormNS)
    {
      addPe(fPulseTimeNS,fAfterPulse);
      fPulseTimeNS += Rexp(fMeanTimeBetweenNoisePesNS);
    }
  double fWaveFormSum=0;
  for(int i=0;i<(int)fWaveForm.size();i++)
    {
      fWaveFormSum+=fWaveForm[i];
    }
  fWaveFormNightSkyPedestal=fWaveFormSum/fWaveForm.size();
  // *************************************************************************
  // Now the PedVar for ythe FADC(or ADC for Whipple490) window(we have only 1
  // as yet)
  // *************************************************************************
  int fNumFADCWindowBins=gFADCNumSamples[fCameraType]*
                                    (int)(gFADCBinSizeNS/gWaveFormBinSizeNS);
  double fNumFADCWindows=(double)fWaveForm.size()/(double)fNumFADCWindowBins;

  if(fCameraType==WHIPPLE490)
    {    
      //Simple for Charge Summing ADC
      // *******************************************************************
      // Since we are counting pe's then that number is poission distributed
      // and the varience is the mean so the sigma (what we misname pedvar)
      // will be the sqrt of the mean # of Pe's
      // *******************************************************************
      double fMeanADCPedPe=(fWaveFormSum/fNumFADCWindows)/fSinglePeArea;
      fChargeVar=sqrt(fMeanADCPedPe);//Misnomer. This is sqrt of varience
      // For WHIPPLE490 convert to digCnts using pedc.
      fChargeVar=fChargeVar*gFADCDigCntsPerPEHiGain[fCameraType];
    }

  else if(fCameraType==VERITAS499)   
    {    
         //This is much tougher since we need to make FADC wave forms.
         //Note integer round down of fNumADCWindows
      int fTraceLengthBins=((int)fNumFADCWindows)*gFADCNumSamples[fCameraType];

      // *****************************************************************
      // Determine fFADCTrace from almost entire fWaveForm (Note: no use of 
      // HiLowGain)
      // *****************************************************************
      fFADC.makeFADCTrace(fWaveForm,0,fTraceLengthBins,false);
                                     
      double fChargeSum=0;
      for(int i=0;i<(int)fFADC.fFADCTrace.size();
	                               i=i+gFADCNumSamples[fCameraType])
	{
	  fChargeSum+= fFADC.getWindowArea(i,gFADCNumSamples[fCameraType]);
	                                       // Start next window at i
	}
      // Since we are counting pe's then that number is poission distributed
      // and the varience is the mean so the sigma (what we misname pedvar)
      // will be the sqrt of the mean # of Pe's
      double fMeanFADCPedPe=(fChargeSum/((int)fNumFADCWindows))/
	                                               fSinglePeMeanFADCArea;
       fChargeVar=sqrt(fMeanFADCPedPe)*fSinglePeMeanFADCArea; //Convert back to
                                                          //FADC digital counts
    }
  return;
}
// ****************************************************************************

void KSPixel::RemoveNightSkyPedestalFromWaveForm()
 // *************************************************************************
 // REmove precalculated fWaveFromNightSkyPedestal from fWaveForm
 // *************************************************************************
{
  for(int i=0;i<(int)fWaveForm.size();i++)
    {
      fWaveForm[i]=fWaveForm[i]-fWaveFormNightSkyPedestal;
    }
  return;
}
// **************************************************************************

double KSPixel::GetCharge(double fFADCStartStartGateTimeNS)

// *************************************************************************
// Get the charge in this pixel
// **************************************************************************
{
  int fStartGateBin=(int)((fFADCStartStartGateTimeNS-fWaveFormStartNS)/
				                      gWaveFormBinSizeNS);
  int fADCNumBins = (int)((gFADCNumSamples[fCameraType]*gFADCBinSizeNS) /
				                      gWaveFormBinSizeNS);
  double fCharge=0;
  // ************************************************************************
  // Whipple is easy.Just sum waveform over ADCGate
  // ************************************************************************
  if(fCameraType==WHIPPLE490)
    {
      for(int i=0;i<fADCNumBins;i++)
	{
	  int j=i+fStartGateBin;
	  fCharge+=fWaveForm[j];
	}
      fCharge=fCharge*gFADCDigCntsPerPEHiGain[fCameraType];//Pedc
    }
  if(fCameraType==VERITAS499)
    {
      fFADC.makeFADCTrace(fWaveForm,fStartGateBin,fADCNumBins,false);
      fCharge=fFADC.getWindowArea(0,gFADCNumSamples[fCameraType]);
    }
  return fCharge;
}
