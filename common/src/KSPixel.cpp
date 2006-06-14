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
  //Debug:fSinglePe.setRiseFallTimes(4.0,15.0);

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

void KSPixel::addPe(double fPeTimeNS,bool fAfterPulse)
// **************************************************************************
// Add a single pe to the waveForm array.
// **************************************************************************
{
  // **********************************************************************
  // See if any of out pe will be in the waveform window. 
  // fPeTimeNS could be negative
  // **********************************************************************
  if((fPeTimeNS+fSinglePeSizeNS+gWaveFormBinSizeNS)>0)
    {
      int fStartBin=(int)(fPeTimeNS/gWaveFormBinSizeNS);//Noise can start at 
                                                        // or before 0.
      if(fStartBin>fNumWaveFormBins-1)
	{
	  return;
	}
      int fPeStartIndex=0;
      int fPeEndIndex=fSinglePeSizeNumBins-1;
      if(fStartBin<0)              //We only have the tail end of the pe pulse
	{
	  fPeStartIndex=-fStartBin;
	  fStartBin=0;
	}
      
      else if((fStartBin+fSinglePeSizeNumBins-1)>fNumWaveFormBins)
	{
	  fPeEndIndex=fNumWaveFormBins-fStartBin-1;
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
// Using the noise rate, model a very long wave form of night sky noise Pe's
// For Whipple, determine a pedvar the same way cpeds does:rms of ped events.
// Fow Veritas, do the same thing vaStage1 does, but only for one window size,
// again RMS but of FADC trace ped values.
// ***************************************************************************
// From this same waveform determine ther FADC window Charge Pedvar
{
  InitWaveForm(0.0,gNightSkyWaveFormNS);

  bool fAfterPulse=true;
  AddNoiseToWaveForm(fAfterPulse);

  double fWaveFormSum=0;
  for(int i=0;i<(int)fWaveForm.size();i++)
    {
      fWaveFormSum+=fWaveForm[i];
    }

  fWaveFormNightSkyPedestal=fWaveFormSum/fWaveForm.size();

  // ********************************************************************
  // Note that we haven't removed the night sky pedestal. This is because 
  // there is actuallay a pedestal added for both whipple and veritas so we 
  // don't go below 0. Since we are only interesed in a pedvar calc in this 
  // method we leave the ped in (its actually required so we don't go below 0)
  // *************************************************************************
  // Now the PedVar for the FADC(or ADC for Whipple490) window(we have only 1
  // as yet)
  // *************************************************************************
  //Get number of Waveform bins in the FADC (or ADC for Whipple) window
  int fNumBinsFADCWindow =
    (int)((double)gFADCNumSamples[fCameraType]*gFADCBinSizeNS/
                                                           gWaveFormBinSizeNS);
  //Get the integer whoule number of FADC (or ADC for Whipple) windows in the 
  //waveform
  int fNumFADCWindows=(int)(fWaveForm.size()/fNumBinsFADCWindow);

  if(fCameraType==WHIPPLE490)
    {    
      //Simple for Charge Summing ADC
      // *******************************************************************
      // Get rms of area of all the ADC windows.
      // *******************************************************************
      double fPedSum=0;
      double fPed2Sum=0;
      int fCount=0;//Just being careful here.
      for(int i=0;i<fNumFADCWindows;i++)
	{
	  int k=i*fNumBinsFADCWindow;
	  fWaveFormSum=0;
	  for(int j=0;j<fNumBinsFADCWindow;j++)
	    {
	      fWaveFormSum+=fWaveForm[k+j];
	    }
	  fWaveFormSum=fWaveFormSum/fSinglePeArea;//convert to pe's
	  fPedSum+=fWaveFormSum;
	  fPed2Sum+=fWaveFormSum*fWaveFormSum;
	  fCount++; //Just being careful here.
	}
      double fPedMean  =  fPedSum/(double)fCount;
      double fPedMean2 =  fPed2Sum/(double)fCount;
      fChargeVarPE=sqrt(fPedMean2-fPedMean*fPedMean);
    }
  else if(fCameraType==VERITAS499)   
    {    
      //This is much tougher since we need to make FADC wave forms.
      //Get number of trace FADC bins that fit into the waveform
      int fTraceLengthBins=(fNumFADCWindows)*gFADCNumSamples[fCameraType];

      // *****************************************************************
      // Determine fFADCTrace from almost entire fWaveForm (Note: no use of 
      // HiLowGain)Note also that the conversion to dc from pe's is done within
      // the makeFADCTrace class.
      // *****************************************************************
      fFADC.makeFADCTrace(fWaveForm,0,fTraceLengthBins,false);
      double fPedSum=0;
      double fPed2Sum=0;
      int fCount=0;  //Just being careful here.
      for(int i=0;i<(int)fFADC.fFADCTrace.size();
	                               i=i+gFADCNumSamples[fCameraType])
	{
	  int fChargeSum =                    // Start next window at i
	             (int)fFADC.getWindowArea(i,gFADCNumSamples[fCameraType]);
	  fPedSum+=fChargeSum;
	  fPed2Sum+=fChargeSum*fChargeSum;
	  fCount++;//Just being careful here.
	}
      double fPedMean  =  fPedSum/(double)fCount;
      double fPedMean2 =  fPed2Sum/(double)fCount;
      fChargeVarDC=sqrt(fPedMean2-fPedMean*fPedMean);
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
// Get the charge in this pixel in pe's for whipple and DC for veritas
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
      fCharge=fCharge/fSinglePeArea;//Pe's
    }
  if(fCameraType==VERITAS499)
    {
      fFADC.makeFADCTrace(fWaveForm,fStartGateBin,fADCNumBins,false);
      fCharge=fFADC.getWindowArea(0,gFADCNumSamples[fCameraType]);
    }
  return fCharge;
}
// ***************************************************************************

void KSPixel::PrintWaveForm(int nx, int ny, int seqNum, 
			    double time)
// **************************************************************************
// Dump wave form to ouput in form easy to plot with root. Used for debugging.
// **************************************************************************
{
  int fNumBins=fWaveForm.size();
  for(int i=0;i<fNumBins;i++)
    {
      double fBinTime=fWaveFormStartNS+i*gWaveFormBinSizeNS;
      std::cout<<nx<<" "<<ny<<" "<<seqNum<<" "<<fID<<" "<<fBinTime<<" "
	       <<fWaveForm[i]<<" "<<time<<std::endl;
    }
  return;
}
// ***************************************************************************
