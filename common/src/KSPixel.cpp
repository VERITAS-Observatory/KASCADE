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


KSPixel::KSPixel(KSCameraTypes CameraType, double DigCntsPerPEHiGain, 
				 double SinglePeRiseTimeNS, double SinglePeFallTimeNS,
				 double lowGainToHighGainPeakRatio)
{
  fCameraType=CameraType;
  fFADC.SetCameraType(fCameraType);
  fFADC.SetDigCntsPerPEGains(DigCntsPerPEHiGain);
  fFADC.SetLowGainToHighGainPeakRatio(lowGainToHighGainPeakRatio);
  pfRandom=new TRandom3(0);
  fLowGainToHighGainPeakRatio = lowGainToHighGainPeakRatio;

  if(fCameraType==WHIPPLE490) {
    pfSinglePe= new KSSinglePe();  //use base rise and fall times
  }
  if(fCameraType==VERITAS499){
    pfSinglePe= new KSSinglePe(SinglePeRiseTimeNS,SinglePeFallTimeNS);
  }
 
  // *******************************************************
  // We create the pfWaveform here at this time so that we can run the 
  // getMeanFADCArea and getMeanFADCArea here only when we create the pixel
  // and not when we init the pixels which happen many times. See KSCamera
  // *******************************************************
  pfWaveForm=new KSWaveForm(fCameraType,pfSinglePe);
  fSinglePeSizeNS      = pfSinglePe->getLengthNS();
  fSinglePeSizeNumBins = pfSinglePe->fNumBinsInPulse;
  fSinglePeArea        = pfSinglePe->getArea();//Not in DC. Arbitray units

  // ********************************************************************
  //Next set of calls uses pfWaveForm: This is for info on hi/lo gains etc.
  // -1 for endbin indicates use full trace 
  // *********************************************************************
  //double timeSpreadNS = gFADCBinSizeNS;
  double timeSpreadNS = 0.0;
  fSinglePeMeanFADCArea = 
	getMeanFADCArea(fCameraType, gPulseHeightForMeanFADCArea, 1, timeSpreadNS,
					                                                   0, -1);
  fSinglePeMeanFADCArea16Bins = 
	getMeanFADCArea(fCameraType, gPulseHeightForMeanFADCArea, 1, timeSpreadNS,
					                                                   0, 15);
  fSinglePeMeanFADCArea7Bins = 
	getMeanFADCArea(fCameraType, gPulseHeightForMeanFADCArea, 1, timeSpreadNS,
					                                                    1, 7);
  
  // *********************************************************
  // Use crazy large HVGain for no rounding. It gets divided out at the end.
  // *********************************************************
  timeSpreadNS = gFADCBinSizeNS;
  //timeSpreadNS = 5.0;          //Normal shower/mirror induced time spread.(ns)
  std::cout << " Shower/Mirror Time Spread(ns) used in Hi/lo ratio "
	           "calculation: " << timeSpreadNS << std::endl;
  fSinglePeMeanFADCAreaNoRounding = 
	              getMeanFADCArea(fCameraType,  1.0, 1000,timeSpreadNS, 0,-1);
  fSinglePeMeanFADCAreaNoRounding16Bins = 
	             getMeanFADCArea(fCameraType, 1.0, 1000, timeSpreadNS, 0, 15);
  fSinglePeMeanFADCAreaNoRounding7Bins = 
	            getMeanFADCArea(fCameraType, 1.0, 1000.0, timeSpreadNS, 1, 7);
  

  //Next set of calls uses the low gain templates from pfSinglePe
  int templateIndex = 0;
  fLowGainTmplt0FADCAreaNoRounding = 
	getLowGainMeanFADCArea( fCameraType, templateIndex, 200,timeSpreadNS,
							                                           0, -1);
  fLowGainTmplt0FADCAreaNoRounding7Bins = 
	getLowGainMeanFADCArea( fCameraType, templateIndex, 200, timeSpreadNS,
						                                               3, 9);
  fLowGainTmplt0FADCAreaNoRounding16Bins = 
	getLowGainMeanFADCArea( fCameraType, templateIndex, 200, timeSpreadNS,
						                                               0 , 15);

  InitPixelWaveForm();
}

void KSPixel::InitPixelWaveForm()
// ****************************************************************************
//This is where the individiual pixel gets an individual KSWAveForm
// ****************************************************************************
{
  pfWaveForm = new KSWaveForm(fCameraType,pfSinglePe); 
}
// **************************************************************

KSPixel::~KSPixel()
{
  //nothing to do
}
 
// *************************************************************************

void KSPixel::InitWaveForm(double WaveFormStartNS,double WaveFormLengthNS)
{
  // **************************************************************************
  // In pfWaveForm, initalize wave form and get number of elements
  // **************************************************************************
  fNumWaveFormBins = 
            pfWaveForm->InitWaveForm(WaveFormStartNS, WaveFormLengthNS);
  return;
}
// ****************************************************************************

double KSPixel::GetWaveFormElement(int Index)
{
  double element = pfWaveForm->GetWaveFormElement(Index);
  return element;
}
// ****************************************************************************


void KSPixel::BuildPeWaveForm()
// **************************************************************************
// In pfWaveForm, build the wave form we expect from the real pe's 
// Return number of pe we actually added
// **************************************************************************
{
  fDisc=pfWaveForm->BuildPeWaveForm(fTimePe,fEfficiency);
  return;

}
// ***************************************************************************

int KSPixel::AddNoiseToWaveForm(bool AfterPulse)
// **************************************************************************
// In fWaveForm, Add the noise pulses to it.
// Note that this noise rate is not affected by the base efficiency!!!
// **************************************************************************
{
  int iCount = pfWaveForm->AddNoiseToWaveForm(AfterPulse, fNoiseRatePerNS);

  return iCount;
}
 
// ***************************************************************************
void KSPixel::RemovePedestalFromWaveForm(double WaveFormPedestal)
 // *************************************************************************
 // Remove Specified Pedestal from fWaveForm
 // *************************************************************************
{
  pfWaveForm->RemovePedestalFromWaveForm(WaveFormPedestal);

  return;
}
// **************************************************************************

void KSPixel::AddPedestalToWaveForm(double WaveFormPedestal)
 // *************************************************************************
 // Add specified Pedestal to fWaveForm
 // *************************************************************************
{
  pfWaveForm->AddPedestalToWaveForm(WaveFormPedestal);
  return;
}
// **************************************************************************


void KSPixel::DetermineNoisePedestals()
// ***************************************************************************
// Using the noise rate, model a very long wave form of night sky noise Pe's
// For Whipple, determine a pedvar the same way cpeds does:rms of ped events.
// Fow Veritas, do the same thing vaStage1 does, but only for one window size,
// again RMS but of FADC trace ped values.
// ***************************************************************************
// From this same waveform determine ther FADC window Charge Pedvar
{
  pfWaveForm->InitWaveForm(0.0,gNightSkyWaveFormNS[fCameraType]);

  //bool afterPulse=true;
  bool afterPulse=false;

  pfWaveForm->AddNoiseToWaveForm(afterPulse,fNoiseRatePerNS); //Note that this noise 
                                             // (fNoiseRatePerNS) has not been 
                                             //modified by overall efficiency but has 
                                             //been modified by light cone efficiency

  double waveFormSum        = pfWaveForm->GetWaveFormSum();
  fWaveFormNightSkyPedestal = waveFormSum / pfWaveForm->GetNumWaveFormBins();

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
  int numFADCWindows=(int)(pfWaveForm->GetNumWaveFormBins()/numBinsFADCWindow);

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
	  waveFormSum=pfWaveForm->GetWaveFormSum(k,k+numBinsFADCWindow);
	  //for(int j=0;j<numBinsFADCWindow;j++)
	  // {
	  //   waveFormSum+=fWaveForm.at(k+j);
	  // }
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
      // the KSFADC::makeFADCTrace method. (Note Pedestal added there also)
      // *****************************************************************
      fFADC.makeFADCTrace(pfWaveForm,0,numSamplesTrace,false,
			  gPedestal[VERITAS499], gLowGainPedestal[VERITAS499]);
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

      charge=pfWaveForm->GetWaveFormSum(startGateBin,startGateBin+ADCNumBins-1);
      //for(int i=0;i<ADCNumBins;i++)
      //	{
      //	  int j=i+startGateBin;
      //	  charge+=fWaveForm.at(j);
      //	}
      
      charge=charge/fSinglePeArea;//Pe's
    }
  if(fCameraType==VERITAS499)
    {

      int numSamplesTrace = gFADCWinSize[fCameraType];

      fFADC.makeFADCTrace(pfWaveForm,startGateBin,numSamplesTrace,true,
			  gPedestal[VERITAS499], gLowGainPedestal[VERITAS499]);
      // ******************************************************************
      // When building the FADC trace (both hi and low gain) a FADC pedestal 
      // was added to the wave form before digitizing. It was the same 
      // pedestal both for hi and low gains. remove it here to get charge.
      // ******************************************************************
      charge=fFADC.getWindowArea(0,gFADCWinSize[fCameraType])-fPedDC;
	
      //Atenuate for lo gain.
      if(fFADC.fIsLowGainTrace)
	{
	  charge=charge*gFADCHiLoGainRatio;
	}
    }
  return charge;
}
// ***************************************************************************

void KSPixel::PrintWaveForm(int Nx, int Ny, int SeqNum, 
			    double Time)
// **************************************************************************
// Dump wave form to ouput in form easy to plot with root. Used for debugging.
// **************************************************************************
{
  int numBins=pfWaveForm->GetNumWaveFormBins();
  for(int i=0;i<numBins;i++)
    {
      double binTime=fWaveFormStartNS+i*gWaveFormBinSizeNS;
      cout<<Nx<<" "<<Ny<<" "<<SeqNum<<" "<<fID<<" "<<binTime<<" "
	       <<pfWaveForm->GetWaveFormElement(i)<<" "<<Time<<endl;
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
	      pfWaveForm->InitWaveForm(0,fSinglePeSizeNS +timeSpreadNS[k]);
	      //Add in 6 pes'
	      bool afterPulse=false;
	      vector<double> TimePe;
	      for(int j=0;j<numPes[n];j++)
		{
		  double peTimeNS=pfRandom->Rndm()*timeSpreadNS[k];
		  TimePe.push_back(peTimeNS);
		}
	      pfWaveForm->BuildPeWaveForm(TimePe,afterPulse); 


	      //Find max of this wave form and print it out.
	      double waveFormMax=0.0;
	      int numBins=pfWaveForm->GetNumWaveFormBins();
	      for(int j=0;j<numBins;j++)
		{
		  if(pfWaveForm->GetWaveFormElement(j)>waveFormMax)
		    {
		      waveFormMax=pfWaveForm->GetWaveFormElement(j);
		    }
		}
	      // *************************************************************
	      // Also get mean size in FADC DC of single pe.
	      // *************************************************************
	      //Number of Bins in FADC wave form.
	      int numTraceBins=(int)(numBins*gWaveFormBinSizeNS/
				      gFADCBinSizeNS)+1+1;  
	                                      //Extra 1 is just for insruance.
	      fFADC.makeFADCTrace(pfWaveForm,0,numTraceBins,false,
				  gPedestal[VERITAS499], 
				  gLowGainPedestal[VERITAS499]);

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
// *************************************************************************

double KSPixel::getMeanFADCArea(KSCameraTypes fCameraType,  
								double HVGain, int numPesInPulse, 
								double timeSpreadNS, int startFADCBin, 
								int endFADCBin)
// *************************************************************************
// Get mean area for a single pe after its WaveFormn is converted to a FADC
// trace. Do this in two parts just like single pe analyis does.
//
// &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
// And remember dumb ass, with a holey plate there is no night sky nouse!
// &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
// A: Get mean area of trace with no pulse, just night sky and pedestal
// 1. Init a singlePe wave form length of waveform vector.
// 2: make FADC Trace (adds FADC pedestal=14.5, adds uncorreltated FADC noise 
//    with .35dc sigma, rounddown to make trace)
// 3: Find area
// 4: Do above some number of time (100000 times ) and find mean.
// 
// B:Get mean area of trace with added single Pe Pulse
// Same as A but:
// 1.5: Add single Pe pulse multiplied by product of gains (HV and normal) 
//      and random Pulse height. If timeSpreadNS>0.0 Add in the pes with the 
//      time spread.(time spread slows things down)
//
// C:
// 6: Find difference with non-pulse mean
// 7: Divide out HVgain and number of pe's.
// Note:If we use a very large HV scale factor (say 1000) we would have a very
// small rounddown effect, and if we use a small value (like 1.75 (typical for
// 10% HV increase) or so) then we get the round down that the singePe area 
// calculation from singlePe runs get.
// *************************************************************************
{
  bool fDebugPrint=false;
  if ( startFADCBin < 0 ) {
	startFADCBin = 0;
  }
  

  int numWaveFormBinsPerFADCBin=(int)(gFADCBinSizeNS/gWaveFormBinSizeNS);

  // ***************************************************************
  // Number of NS in SinglePe wave form. Extra 3 ns is just for insurance (1 
  // would probably be snough)
  // ***************************************************************
  int singlePeSizeNS=  
	       (int)fSinglePeSizeNumBins*gWaveFormBinSizeNS + timeSpreadNS +3.0;  

  int numSamplesTrace= endFADCBin-startFADCBin+1;  
  // Negative or 0 implies use full trace
  if(numSamplesTrace<1) {
	 numSamplesTrace= (int)( (double)singlePeSizeNS/gFADCBinSizeNS); 
  }

  double singlePeAreaSum = 0;  
  double pedAreaSum      = 0;

  int numTraces= 500;         //Number of traces to generate and average over.
  if (timeSpreadNS > 0.0) {
	if (numPesInPulse > 50 ) {
	  numTraces=1;              //probably one trace would be enough as long 
	}                           //as numPesInPulse is big (>>18)
  }
  else{
	  numTraces=500;           // For no timeSpreadNS we need this for Pulse 
                               // height jitter
  }

  int startWaveFormBin   = (double)startFADCBin / gFADCBinSizeNS;

  if(fDebugPrint) {
	std::cout<< "HiGain number of Traces to average over: " <<  numTraces 
			 << std::endl;
	std::cout<<"  startFADCBin, endFADCBin,gFADCBinSizeNS,startWaveFormBin,"
             "numSamplesTrace: " <<  startFADCBin << " " <<  endFADCBin 
		   << " " <<gFADCBinSizeNS << " " <<  startWaveFormBin << " " 
		   << numSamplesTrace << std::endl;
  }
  for (int i=0;i< numTraces;i++){
    double cleanPed=gPedestal[VERITAS499];

    //A:Step1:
    pfWaveForm->InitWaveForm(0.0,singlePeSizeNS);
    // ************************************************************************
    //A:Step2:make FADC Trace (adds FADC pedestal,Adds FADC noise)
    // ************************************************************************
    fFADC.makeFADCTrace(pfWaveForm, startWaveFormBin, numSamplesTrace, false,
						cleanPed, cleanPed);
  
    double tracePedArea=fFADC.getWindowArea(0, numSamplesTrace);  

    // ************************************************************************
    //   Now do same for singlePe pulse
	// *********************************************************************
    pfWaveForm->InitWaveForm(0.0,singlePeSizeNS);

    // ********************************************************************
    //A: Step 1: 
    //B: Step 1.5: Add single pe: start pulse at least one sample in then 
    //             dirft it
	bool afterPulse=false;
	// ***********************************************************************
    // Jitter Start time of pulse (over 1 sample)
    // **************************
    int j;

	if (timeSpreadNS == 0.0) {
	  j = (int) ( pfRandom->Rndm() * numWaveFormBinsPerFADCBin);
	  if( j == numWaveFormBinsPerFADCBin ) {
		j = 0;
	  }

	  // **************************
	  // Jitter PePulse Hieght
	  // **************************
	  double PEpulseHeight = pfSinglePe->getPulseHeight( afterPulse);
	  for( int k = 0; k < fSinglePeSizeNumBins; k++ ) {
		double newHeight = pfWaveForm->GetWaveFormElement(k+j) + 
		  PEpulseHeight * HVGain *
		  ( numPesInPulse * pfSinglePe->fSinglePulse.at( k));
		pfWaveForm->SetWaveFormElement( k + j, newHeight);
	  }
	  
	}
	else{

	  // *********************************************************************
	  // With a timeSpreadNS we have to do individual pe's. Takes time.
	  // *********************************************************************
	  for (int iCount = 1; iCount <= numPesInPulse ; iCount++ ) {
		int j = (int) (pfRandom->Rndm() * timeSpreadNS / gWaveFormBinSizeNS );
		
		// **************************
		// Jitter PePulse Hieght and add in a pe.
		// **************************
		double PEpulseHeight = pfSinglePe->getPulseHeight( afterPulse);

		for(int k=0;k<fSinglePeSizeNumBins;k++) {
		  double newHeight = pfWaveForm->GetWaveFormElement(k+j) + 
			PEpulseHeight*HVGain*pfSinglePe->fSinglePulse.at(k);
		  pfWaveForm->SetWaveFormElement(k+j, newHeight);
		}
	  }

	}
	
	// *******************************
	//Adds FADC Ped, adds FADC noise, rounddowns.
	// *******************************
	fFADC.makeFADCTrace(pfWaveForm, startWaveFormBin, numSamplesTrace, false,
						cleanPed, cleanPed);
	
	double traceSinglePeArea=fFADC.getWindowArea(0, numSamplesTrace);  
	  

	if ( fDebugPrint ) {
	  cout<< "0 " <<numPesInPulse << " " << j << " " << cleanPed << " "
		  << traceSinglePeArea << endl;
	  cout<< "1 " <<numPesInPulse << " " << j << " " << cleanPed <<" " 
		  << traceSinglePeArea << endl;
	}

	singlePeAreaSum+=traceSinglePeArea;
	pedAreaSum+=tracePedArea;
  }
  double pedAreaMean      = pedAreaSum/(numTraces*numPesInPulse);
  double singlePeAreaMean = singlePeAreaSum/(numTraces*numPesInPulse);
  double singlePeArea     = (singlePeAreaMean-pedAreaMean)/HVGain;
  return singlePeArea;
}
// *************************************************************************
 
double KSPixel::getLowGainMeanFADCArea(KSCameraTypes fCameraType, 
									   int templateIndex, int numPesInPulse,
									   double timeSpreadNS, int startFADCBin, 
									   int endFADCBin)
// *************************************************************************
// Get mean area for a single pe after its WaveFormn is converted to a FADC
// trace.  
// Note:If we use a large numPesInPulse(>> 18) we would have a 
// very small rounddown effect.  
// *************************************************************************
{
  if ( startFADCBin < 0 ) {
	startFADCBin = 0;
  }
  
  // *******************************************************************
  // Get a pointer to the wave form source
  // *******************************************************************
  vector < double >*  pLowGainTemplateWaveForm = 
    &pfSinglePe->fLowGainWaveForm.at(templateIndex).fWaveForm;

  //Number of NS in SinglePe wave form. Extra 1.0 is just for insurance
  
  int lowGainTemplateNumBins = 
    pfSinglePe->fLowGainWaveForm.at(templateIndex).GetSize();
  //int numWaveFormBinsPerFADCBin=(int)(gFADCBinSizeNS/gWaveFormBinSizeNS);

  // ***************************************************************
  // Number of NS in SinglePe wave form. Extra 3 ns is just for insurance (1 
  // would probably be enough)
  // ***************************************************************
  int singlePeSizeNS   = 
	           lowGainTemplateNumBins*gWaveFormBinSizeNS + timeSpreadNS + 3.0;

  int numSamplesTrace  = (endFADCBin-startFADCBin+1);  
  // Negative or 0 implies use full trace
  if( numSamplesTrace < 1) {
	 numSamplesTrace= (int) ( (double)singlePeSizeNS / gFADCBinSizeNS); 
  }

  int startWaveFormBin = (double)startFADCBin / gWaveFormBinSizeNS;


  double singlePeAreaSum=0;  

  int numTraces = 50;

  double lowGainPedestal = gLowGainPedestal[ VERITAS499];
  double linearity  = 
    pfSinglePe->fLowGainWaveForm.at( templateIndex).GetLinearity();
  for ( int i=0; i < numTraces; i++ ) {
    pfWaveForm->InitWaveForm( 0.0, singlePeSizeNS);

    // *****************************************************************
	// Loop over pes. shift each within time spread
	// ********************************************************************
	for( int iCount=1; iCount <= numPesInPulse; iCount++ ) {

	  // ********************************************************************
	  // Add single pe: start pulse at least one sample in then 
	  // dirft it
	  // ********************************************************************
	  // Jitter Start time of pulse
	  // **************************
	  int j = (int) (pfRandom->Rndm() * timeSpreadNS / gWaveFormBinSizeNS);
	  for( int k=0; k < fSinglePeSizeNumBins; k++ ) {
		double newHeight = pfWaveForm->GetWaveFormElement( k + j) + 
		  ( pLowGainTemplateWaveForm->at( k));
		pfWaveForm->SetWaveFormElement( k + j, newHeight);
	  }
	}
	//  CARE's scaling(which I think is the correct one to use)
	// (~.09756=1/10.25 for Hamamatsu)
	double scaleFactor = fLowGainToHighGainPeakRatio * linearity;
	// **************************
	// we will mutilpy by fDigCntsPerPEHiGain (DC/PE)(2.05 for hamamatsu) in 
	// the call to generateTraceSamples() 
	// *****************************************
	pfWaveForm->ScaleWaveForm(scaleFactor);

	//Adds FADC Ped, rounddowns, mutilpy by fDigCntsPerPEHiGain (DC/PE).
	double lowGainChargeDC;
	
	fFADC.generateTraceSamples(lowGainChargeDC, pfWaveForm, startWaveFormBin, 
							   numSamplesTrace, lowGainPedestal);
	
    lowGainChargeDC  = lowGainChargeDC - (numSamplesTrace * lowGainPedestal); 
    singlePeAreaSum += lowGainChargeDC/numPesInPulse;
  }
  double singlePeAreaMean = singlePeAreaSum/numTraces;
  return singlePeAreaMean;
}
// *************************************************************************
 
