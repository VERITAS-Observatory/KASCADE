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


extern "C" double Rexp(double rate);

KSPixel::KSPixel()
{
  pfRandom=new TRandom3(0);
  InitPixel();
}
KSPixel::KSPixel(KSCameraTypes cameraType, double digCntsPerPEHiGain)
{
  fCameraType=cameraType;
  fFADC.SetCameraType(fCameraType);
  fFADC.SetDigCntsPerPEGains(digCntsPerPEHiGain);
  pfRandom=new TRandom3(0);
  InitPixel();
}

void KSPixel::InitPixel()
{
  // ****************************************************************************
  //This is where we could reset the rise and fall times of the single pe pulse
  // ****************************************************************************

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

void KSPixel::InitWaveForm(double waveFormStartNS,double waveFormLengthNS)
// ********************************************************************
// Allocate the wave form
// ********************************************************************
{
  fWaveFormStartNS=waveFormStartNS;
  fWaveFormLengthNS=waveFormLengthNS;
  fNumWaveFormBins=(int)(fWaveFormLengthNS/gWaveFormBinSizeNS + 1.0);
  fWaveForm.clear();
  fWaveForm.resize(fNumWaveFormBins,0.0);
  return;
}
// ********************************************************************

void KSPixel::BuildPeWaveForm()
// **************************************************************************
// In fWaveForm, build the wave form we expect from the real pe's 
// **************************************************************************
{
  int numPes=fTimePe.size();
  fDisc=0;
  bool afterPulse=false;
  if(numPes>0){
    for(int i=0;i<numPes;i++){
      if(pfRandom->Rndm()<fEfficiency){
	fDisc++;
	double peTime=fTimePe.at(i)-fWaveFormStartNS;
	addPe(peTime,afterPulse);
      }
    }
  }
  return;
}
// ***************************************************************************

int KSPixel::AddNoiseToWaveForm(bool afterPulse)
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
  double meanTimeGapNS=1./fNoiseRatePerNS;

  double noiseTimeNS=-fSinglePeSizeNS + Rexp(meanTimeGapNS);
  int icount=0;
  while(noiseTimeNS<gWaveFormBinSizeNS*fNumWaveFormBins)
    {
      addPe(noiseTimeNS,afterPulse);
      noiseTimeNS+=Rexp(meanTimeGapNS);
      icount++;
    }
  

  //cout<<"Num noise pe's:"<<icount<<endl;
  return icount;
}

void KSPixel::addPe(double peTimeNS,bool afterPulse)
// **************************************************************************
// Add a single pe to the waveForm array.
// **************************************************************************
{
  // **********************************************************************
  // See if any of out pe will be in the waveform window. 
  // PeTimeNS could be negative
  // **********************************************************************
  int EndInWaveFormThisPeNS=peTimeNS+fSinglePeSizeNS+gWaveFormBinSizeNS;
  if(EndInWaveFormThisPeNS<=0){ //End of pe before waveform start.
    return;
  }

  // ************************************************************************
  // The single pe we are adding ends after the start of the wave form.
  // See if it starts after the end of the waveform.
  // ************************************************************************
  int startBin=(int)(peTimeNS/gWaveFormBinSizeNS);//Noise can start at 
  // or before 0.
  if(startBin>fNumWaveFormBins-1){
    return;
  }
  
  
  // **************************************************************************
  // Find where it starts. If before the wave form find place in the single pe
  // that goes into first bin of wave form
  // **************************************************************************
  
  int peStartIndex=0;       //Nominal atart and end index of single pe
  int peEndIndex=fSinglePeSizeNumBins-1;
  if(startBin<0)              //We only have the tail end of the pe pulse
    {
      peStartIndex=-startBin;   //So we skip the first startBin's of the single pe 
      startBin=0;               //and we put that in first wave form bin
    }
  
  // *******************************************************************************
  // If pulse ends after the end of the wave form reduce to that.
  // *****************************************************************************
  if((startBin+fSinglePeSizeNumBins-1)>=fNumWaveFormBins){
    peEndIndex=fNumWaveFormBins-startBin-1;
  }
  
  double pulseHeight=pfSinglePe->getPulseHeight(afterPulse);
  
  // Now load in the single pe
  int waveFormIndex=startBin;     
  for(int i=peStartIndex;i<=peEndIndex;i++){
    //    if( i<0 || i>(int)pfSinglePe->pfSinglePulse.size()-1){
    // cout<<"Out of range SinglePe: i: "<<i<<endl;
    // }
    //if(waveFormIndex<0 || waveFormIndex >(int)fWaveForm.size()-1 ) {
    //  cout<<"Out of range waveformindex: "<<waveFormIndex<<endl;
    // }
    fWaveForm.at(waveFormIndex)+=pfSinglePe->pfSinglePulse.at(i)*pulseHeight;
    waveFormIndex++;
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

  //bool afterPulse=true;
  bool afterPulse=false;

  AddNoiseToWaveForm(afterPulse);  //Note that this noise has not been 
                                    //modified by overall efficiency but has 
                                    //been modified by light cone efficiency

  double waveFormSum=0;
  for(int i=0;i<(int)fWaveForm.size();i++)
    {
      waveFormSum+=fWaveForm.at(i);
    }

  fWaveFormNightSkyPedestal=waveFormSum/fWaveForm.size();

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
  int numBinsFADCWindow =
    (int)((double)gFADCWinSize[fCameraType]*gFADCBinSizeNS/gWaveFormBinSizeNS);

  //Get the integer whoule number of FADC (or ADC for Whipple) windows in the 
  //waveform
  int numFADCWindows=(int)(fWaveForm.size()/numBinsFADCWindow);

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
      double pedSum=0;
      double ped2Sum=0;
      int iCount=0;//Just being careful here.
      for(int i=0;i<numFADCWindows;i++)
	{
	  int k=i*numBinsFADCWindow;
	  waveFormSum=0;
	  for(int j=0;j<numBinsFADCWindow;j++)
	    {
	      waveFormSum+=fWaveForm.at(k+j);
	    }
	  waveFormSum=waveFormSum/fSinglePeArea;//convert to pe's
	  pedSum+=waveFormSum;
	  ped2Sum+=waveFormSum*waveFormSum;
	  iCount++; //Just being careful here.
	}
      fPedPE  =  pedSum/(double)iCount;
      double pedMean2 =  ped2Sum/(double)iCount;
      fChargeVarPE=sqrt(pedMean2-fPedPE*fPedPE);
    }
  else if(fCameraType==VERITAS499)   
    {    
      //This is much tougher since we need to make FADC wave forms.
      //Get number of trace FADC bins that fit into the waveform
      int numSamplesTrace=(numFADCWindows)*gFADCWinSize[fCameraType];

      // *****************************************************************
      // Determine fFADCTrace from almost entire fWaveForm
      //  Note We do not need the  use of HiLow Gain stuff here
      // Note also that the conversion to dc from pe's is done within
      // the makeFADCTrace class. (Note Pedestal added there also)
      // *****************************************************************
      fFADC.makeFADCTrace(fWaveForm,0,numSamplesTrace,false,
			                                gPedestal[VERITAS499]);
      double pedSum=0;
      double ped2Sum=0;
      int iCount=0;  //Just being careful here.
      for(int i=0;i<(int)fFADC.fFADCTrace.size();
	                               i=i+gFADCWinSize[fCameraType])
	{
	  int chargeSum =                    // Start next window at i
	             (int)fFADC.getWindowArea(i,gFADCWinSize[fCameraType]);
	  //cout<<chargeSum<<endl;
	  pedSum+=chargeSum;
	  ped2Sum+=chargeSum*chargeSum;
	  iCount++;//Just being careful here.
	}
      fPedDC  =  pedSum/(double)iCount; //This is Charge pedestal in FADC's
      double pedMean2 =  ped2Sum/(double)iCount;
      fChargeVarDC=sqrt(pedMean2-fPedDC*fPedDC);
      //      cout<<"fID,iCount,pedSum,pedDC,fChargeVarDC, "
      //	"fWaveFormNightSkyPedestal: "
      //cout<<fID<<" "<<fICount<<" "<<fNoiseRatePerNS
      //	       <<" "<<iCount
      //	       <<" "<<pedSum<<" "<<fPedDC<<" "<<fChargeVarDC
      //       <<" "<<fWaveFormNightSkyPedestal<<" "<<fSinglePeMeanFADCArea
      //	       <<endl;
    }
  return;
}
// ****************************************************************************

void KSPixel::RemovePedestalFromWaveForm(double waveFormPedestal)
 // *************************************************************************
 // Remove Specified Pedestal from fWaveForm
 // *************************************************************************
{
  for(int i=0;i<(int)fWaveForm.size();i++)
    {
      fWaveForm.at(i)=fWaveForm.at(i)-waveFormPedestal;
    }
  return;
}
// **************************************************************************

void KSPixel::AddPedestalToWaveForm(double waveFormPedestal)
 // *************************************************************************
 // Add specified Pedestal to fWaveForm
 // *************************************************************************
{
  for(int i=0;i<(int)fWaveForm.size();i++)
    {
      fWaveForm.at(i)+=waveFormPedestal;
    }
  return;
}
// **************************************************************************


double KSPixel::GetCharge(double FADCGateStartTimeNS, bool pedestalEvent)

// *************************************************************************
// Get the charge in this pixel in pe's for whipple and DC for veritas
// **************************************************************************
{
  int startGateBin=0;  //default for pedestal event
    {
      startGateBin=(int)((FADCGateStartTimeNS-fWaveFormStartNS)/
			  gWaveFormBinSizeNS);
    }

  double charge=0;

  // ************************************************************************
  // Whipple is easy.Just sum waveform over ADCGate
  // ************************************************************************
  if(fCameraType==WHIPPLE490)
    {
      int ADCNumBins = (int)((gFADCWinSize[fCameraType]*gFADCBinSizeNS) /
			      gWaveFormBinSizeNS);
      for(int i=0;i<ADCNumBins;i++)
	{
	  int j=i+startGateBin;
	  charge+=fWaveForm.at(j);
	}
      charge=charge/fSinglePeArea;//Pe's
    }
  if(fCameraType==VERITAS499)
    {

      int numSamplesTrace = gFADCWinSize[fCameraType];

      fFADC.makeFADCTrace(fWaveForm,startGateBin,numSamplesTrace,true,
			  gPedestal[VERITAS499]);
      // ******************************************************************
      // When building the FADC trace (both hi and low gain) a FADC pedestal 
      // was added to the wave form before digitizing. It was the same 
      // pedestal both for hi and low gains. remove it here to get charge.
      // ******************************************************************
      charge=fFADC.getWindowArea(0,gFADCWinSize[fCameraType])-fPedDC;
	
      //Amplify for lo gain.
      if(fFADC.fFADCLowGain)
	{
	  charge=charge*gFADCHiLoGainRatio;
	}
    }
  return charge;
}
// ***************************************************************************

void KSPixel::PrintWaveForm(int nx, int ny, int seqNum, 
			    double time)
// **************************************************************************
// Dump wave form to ouput in form easy to plot with root. Used for debugging.
// **************************************************************************
{
  int numBins=fWaveForm.size();
  for(int i=0;i<numBins;i++)
    {
      double binTime=fWaveFormStartNS+i*gWaveFormBinSizeNS;
      cout<<nx<<" "<<ny<<" "<<seqNum<<" "<<fID<<" "<<binTime<<" "
	       <<fWaveForm.at(i)<<" "<<time<<endl;
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
  int numPes[6]={4,5,6,8,10,12};
  cout<<"N/I:P1/F:A1/F:P2/F:A2/F:P3/F:A3/F:P4/F:A4/F:P5/F:A5/F"
	   <<endl;
  double timeSpreadNS[5]= {0.0, 2.0, 4.5, 6.0, 8.0};
  for(int n=0;n<6;n++)  //Over Number pes
    {
      for(int i=0;i<5000;i++)  //over trials
	{
      
	  cout<<numPes[n]<<" ";
	  for(int k=0;k<5;k++)  //Over Time spread
	    {
	      
	      //Make a clean WaveForm 
	      InitWaveForm(0,fSinglePeSizeNS +timeSpreadNS[k]);
	      //Add in 6 pes'
	      bool afterPulse=false;
	      for(int j=0;j<numPes[n];j++)
		{
		  double peTimeNS=pfRandom->Rndm()*timeSpreadNS[k];
		  addPe(peTimeNS,afterPulse);   //This uses pulse height dist
		}
	      //Find max of this wave form and print it out.
	      double waveFormMax=0.0;
	      int numBins=fWaveForm.size();
	      for(int j=0;j<numBins;j++)
		{
		  if(fWaveForm.at(j)>waveFormMax)
		    {
		      waveFormMax=fWaveForm.at(j);
		    }
		}
	      // *************************************************************
	      // Also get mean size in FADC DC of single pe.
	      // *************************************************************
	      //Number of Bins in FADC wave form.
	      int numTraceBins=(int)(numBins*gWaveFormBinSizeNS/
				      gFADCBinSizeNS)+1+1;  
	                                      //Extra 1 is just for insruance.
	      fFADC.makeFADCTrace(fWaveForm,0,numTraceBins,false,
				  gPedestal[VERITAS499]);
	      double singlePeFADCArea=(fFADC.getWindowArea(0,numTraceBins)-
			      numTraceBins*gPedestal[VERITAS499])/numPes[n];


	      cout<<fixed<<setprecision(10)
		       <<waveFormMax<<" "
		       <<singlePeFADCArea<<" ";
	    }
	  cout<<endl;
	}
    }

  return;
}

 
