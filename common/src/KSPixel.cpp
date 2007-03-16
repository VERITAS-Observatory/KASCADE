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
#include <iostream>
#include <iomanip>

extern "C" float pran(float* fXDummy);
extern "C" double Rexp(double fRate);

KSPixel::KSPixel()
{
  InitPixel();
}
KSPixel::KSPixel(KSCameraTypes CameraType, double DigCntsPerPEHiGain)
{
  fCameraType=CameraType;
  fFADC.SetCameraType(CameraType);
  fFADC.SetDigCntsPerPEGains(DigCntsPerPEHiGain);
  InitPixel();
}

void KSPixel::InitPixel()
{
 //This is where we could reset the rise and fall times of the single pe 
  //pulse
  //Debug:fSinglePe.setRiseFallTimes(4.0,15.0);

  pfSinglePe=NULL;
  if(fCameraType==WHIPPLE490)
    {
      pfSinglePe= new KSSinglePe();  //use base rise and fall times
    }
  if(fCameraType==VERITAS499)
    {
      pfSinglePe= new KSSinglePe(gSinglePeRiseTimeNS[fCameraType],
				 gSinglePeFallTimeNS[fCameraType]); 
    }
  fSinglePeSizeNS      = pfSinglePe->getLengthNS();
  fSinglePeSizeNumBins = pfSinglePe->fNumBinsInPulse;
  fSinglePeArea        = pfSinglePe->getArea();
  fSinglePeMeanFADCArea= pfSinglePe->getMeanFADCArea(fCameraType,fFADC);
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
	      double fPeTime=fTimePe.at(i)-fWaveFormStartNS;
	      addPe(fPeTime,fAfterPulse);
	    }
	}
    }
  return;
}

int KSPixel::AddNoiseToWaveForm(bool fAfterPulse)
// **************************************************************************
// In fWaveForm, Add the noise pulses to it.
// Note that this noise rate is not affected by the base efficiency!!!
// **************************************************************************
// Add pe at random times using Rexp for time intervels
{
  // *********************************************************************
  // The Base noise rate was  modified by the light cone collection area 
  // efficiency to account for different PMT areas/etc and the pixel hexagon
  // area.
  // *********************************************************************
  double fMeanTimeGapNS=1./fNoiseRatePerNS;

  double fNoiseTimeNS=-fSinglePeSizeNS + Rexp(fMeanTimeGapNS);
  int icount=0;
  while(fNoiseTimeNS<gWaveFormBinSizeNS*fNumWaveFormBins)
    {
      addPe(fNoiseTimeNS,fAfterPulse);
      fNoiseTimeNS+=Rexp(fMeanTimeGapNS);
      icount++;
    }
  //std::cout<<"Num noise pe's:"<<icount<<std::endl;
  return icount;
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
      double fPulseHeight=pfSinglePe->getPulseHeight(fAfterPulse);
      //double fPulseHeight=1.0;
      // Now load in the single pe
      int fWaveFormIndex=fStartBin;     
      for(int i=fPeStartIndex;i<=fPeEndIndex;i++)
	{
	  fWaveForm.at(fWaveFormIndex)+=pfSinglePe->pfSinglePulse.at(i)*
	                                                    fPulseHeight;
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
  InitWaveForm(0.0,gNightSkyWaveFormNS[fCameraType]);

  //bool fAfterPulse=true;
  bool fAfterPulse=false;
  //int fICount= AddNoiseToWaveForm(fAfterPulse);  //Note that this noise has not been 
  AddNoiseToWaveForm(fAfterPulse);  //Note that this noise has not been 
                                    //modified by overall efficiency but has 
                                    //been modified by light cone efficiency

  double fWaveFormSum=0;
  for(int i=0;i<(int)fWaveForm.size();i++)
    {
      fWaveFormSum+=fWaveForm.at(i);
    }

  fWaveFormNightSkyPedestal=fWaveFormSum/fWaveForm.size();

  // ********************************************************************
  // Note: For both WHIPPLE and VERITAS the wave form fed to the ADC/FADC is
  // ac coupled(0 mean) which in our case means the night sky pedestal has 
  // been removed. 
  // *************************************************************************
 
  RemovePedestalFromWaveForm(fWaveFormNightSkyPedestal);
 //PrintWaveForm(0,0,fID,0);

  // Now the Ped and PedVar for the FADC(or ADC for Whipple490) window(we 
  // have only 1 as yet)
  // *************************************************************************
  //Get number of Waveform bins in the FADC (or ADC for Whipple) window
  int fNumBinsFADCWindow =
    (int)((double)gFADCWinSize[fCameraType]*gFADCBinSizeNS/
                                                           gWaveFormBinSizeNS);
  //Get the integer whoule number of FADC (or ADC for Whipple) windows in the 
  //waveform
  int fNumFADCWindows=(int)(fWaveForm.size()/fNumBinsFADCWindow);

  if(fCameraType==WHIPPLE490)
    {    
      // ********************************************************************
      // The Whipple ADC's add  a pedestal before digitizing. This 
      // is necessary so the we don't go below 0. We will do the same here.
      // *********************************************************************
      AddPedestalToWaveForm(gPedestal[WHIPPLE490]);  
 
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
	      fWaveFormSum+=fWaveForm.at(k+j);
	    }
	  fWaveFormSum=fWaveFormSum/fSinglePeArea;//convert to pe's
	  fPedSum+=fWaveFormSum;
	  fPed2Sum+=fWaveFormSum*fWaveFormSum;
	  fCount++; //Just being careful here.
	}
      fPedPE  =  fPedSum/(double)fCount;
      double fPedMean2 =  fPed2Sum/(double)fCount;
      fChargeVarPE=sqrt(fPedMean2-fPedPE*fPedPE);
    }
  else if(fCameraType==VERITAS499)   
    {    
      //This is much tougher since we need to make FADC wave forms.
      //Get number of trace FADC bins that fit into the waveform
      int fTraceLengthBins=(fNumFADCWindows)*gFADCWinSize[fCameraType];

      // *****************************************************************
      // Determine fFADCTrace from almost entire fWaveForm
      //  Note We do not need the  use of HiLow Gain stuff here
      // Note also that the conversion to dc from pe's is done within
      // the makeFADCTrace class. (Note Pedestal added there also)
      // *****************************************************************
      fFADC.makeFADCTrace(fWaveForm,0,fTraceLengthBins,false,
			                                gPedestal[VERITAS499]);
      double fPedSum=0;
      double fPed2Sum=0;
      int fCount=0;  //Just being careful here.
      for(int i=0;i<(int)fFADC.fFADCTrace.size();
	                               i=i+gFADCWinSize[fCameraType])
	{
	  int fChargeSum =                    // Start next window at i
	             (int)fFADC.getWindowArea(i,gFADCWinSize[fCameraType]);
	  //std::cout<<fChargeSum<<std::endl;
	  fPedSum+=fChargeSum;
	  fPed2Sum+=fChargeSum*fChargeSum;
	  fCount++;//Just being careful here.
	}
      fPedDC  =  fPedSum/(double)fCount; //This is Charge pedestal in FADC's
      double fPedMean2 =  fPed2Sum/(double)fCount;
      fChargeVarDC=sqrt(fPedMean2-fPedDC*fPedDC);
      //      std::cout<<"fID,fCount,fPedSum,fPedDC,fChargeVarDC, "
      //	"fWaveFormNightSkyPedestal: "
      //std::cout<<fID<<" "<<fICount<<" "<<fNoiseRatePerNS
      //	       <<" "<<fCount
      //	       <<" "<<fPedSum<<" "<<fPedDC<<" "<<fChargeVarDC
      //       <<" "<<fWaveFormNightSkyPedestal<<" "<<fSinglePeMeanFADCArea
      //	       <<std::endl;
    }
  return;
}
// ****************************************************************************

void KSPixel::RemovePedestalFromWaveForm(double fWaveFormPedestal)
 // *************************************************************************
 // Remove Specified Pedestal from fWaveForm
 // *************************************************************************
{
  for(int i=0;i<(int)fWaveForm.size();i++)
    {
      fWaveForm.at(i)=fWaveForm.at(i)-fWaveFormPedestal;
    }
  return;
}
// **************************************************************************

void KSPixel::AddPedestalToWaveForm(double fWaveFormPedestal)
 // *************************************************************************
 // Add specified Pedestal to fWaveForm
 // *************************************************************************
{
  for(int i=0;i<(int)fWaveForm.size();i++)
    {
      fWaveForm.at(i)+=fWaveFormPedestal;
    }
  return;
}
// **************************************************************************


double KSPixel::GetCharge(double fFADCGateStartTimeNS, bool fPedestalEvent)

// *************************************************************************
// Get the charge in this pixel in pe's for whipple and DC for veritas
// **************************************************************************
{
  int fStartGateBin=0;  //default for pedestal event
    {
      fStartGateBin=(int)((fFADCGateStartTimeNS-fWaveFormStartNS)/
			  gWaveFormBinSizeNS);
    }
  int fADCNumBins = (int)((gFADCWinSize[fCameraType]*gFADCBinSizeNS) /
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
	  fCharge+=fWaveForm.at(j);
	}
      fCharge=fCharge/fSinglePeArea;//Pe's
    }
  if(fCameraType==VERITAS499)
    {
       fFADC.makeFADCTrace(fWaveForm,fStartGateBin,fADCNumBins,true,
			  gPedestal[VERITAS499]);
      // ******************************************************************
      // When building the FADC trace (both hi and low gain) a FADC pedestal 
      // was added to the wave form before digitizing. It was the same 
      // pedestal both for hi and low gains. remove it here to get charge.
      // ******************************************************************
      fCharge=fFADC.getWindowArea(0,gFADCWinSize[fCameraType])-fPedDC;
	
      //Amplify for lo gain.
      if(fFADC.fFADCLowGain)
	{
	  fCharge=fCharge*gFADCHiLoGainRatio;
	}
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
	       <<fWaveForm.at(i)<<" "<<time<<std::endl;
    }
  return;
}
// ***************************************************************************


void KSPixel::PrintPulseHeightsOfLightPulse()
// ***********************************************************************
// This is a debug method to find how the pulse height is related to the
// Number of single pe's summed but spread in time.
// **********************************************************************
{
  // Make 5000 6 pe light pulses.
  int fNumPes[6]={4,5,6,8,10,12};
  std::cout<<"N/I:P1/F:A1/F:P2/F:A2/F:P3/F:A3/F:P4/F:A4/F:P5/F:A5/F"
	   <<std::endl;
  double fTimeSpreadNS[5]= {0.0, 2.0, 4.5, 6.0, 8.0};
  for(int n=0;n<6;n++)  //Over Number pes
    {
      for(int i=0;i<5000;i++)  //over trials
	{
      
	  std::cout<<fNumPes[n]<<" ";
	  for(int k=0;k<5;k++)  //Over Time spread
	    {
	      
	      //Make a clean WaveForm 
	      InitWaveForm(0,fSinglePeSizeNS +fTimeSpreadNS[k]);
	      //Add in 6 pes'
	      bool fAfterPulse=false;
	      for(int j=0;j<fNumPes[n];j++)
		{
		  double fPeTimeNS=pran(&fXDummy)*fTimeSpreadNS[k];
		  addPe(fPeTimeNS,fAfterPulse);    //This uses pulse height dist
		}
	      //Find max of this wave form and print it out.
	      double fWaveFormMax=0.0;
	      int fNumBins=fWaveForm.size();
	      for(int j=0;j<fNumBins;j++)
		{
		  if(fWaveForm.at(j)>fWaveFormMax)
		    {
		      fWaveFormMax=fWaveForm.at(j);
		    }
		}
	      // *************************************************************
	      // Also get mean size in FADC DC of single pe.
	      // *************************************************************
	      //Number of Bins in FADC wave form.
	      int fNumTraceBins=(int)(fNumBins*gWaveFormBinSizeNS/
				      gFADCBinSizeNS)+1+1;  
	                                      //Extra 1 is just for insruance.
	      fFADC.makeFADCTrace(fWaveForm,0,fNumTraceBins,false,
				  gPedestal[VERITAS499]);
	      double fSinglePeFADCArea=(fFADC.getWindowArea(0,fNumTraceBins)-
			      fNumTraceBins*gPedestal[VERITAS499])/fNumPes[n];


	      std::cout<<std::fixed<<std::setprecision(10)
		       <<fWaveFormMax<<" "
		       <<fSinglePeFADCArea<<" ";
	    }
	  std::cout<<std::endl;
	}
    }

  return;
}

 
