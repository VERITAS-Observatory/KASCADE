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
  pRandom=new TRandom3(0);
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

void KSFADC::makeFADCTrace(KSWaveForm* pWaveForm,int waveFormStartIndex, 
			   int numSamplesTrace, bool enableHiLoGainProcessing, 
			   double FADCTracePed, double lowGainPedestal)
// *********************************************************************
// Convert the  pWaveForm->fWaveForm to a FADC Trace
// *********************************************************************
// The number of samples in the fFADCTrace is set by numSamplesTrace
// **********************************************************************
// Note: If enableHiLoGainProcessing= true and fWaveForm causes us to go
// into LOwGain mode then flage fIsLowGainTrace will be set true.
// We then generate the equivalent Low Gain trace using the 
// KSWaveForm::BuildLowGainWaveForm method.
// Unlike the real FADC we don't need to do a fancy delay for Low gain,
// just start over either:
//    (old way)reducing wave form by the gain difference
//  or
//    (new way:first attempt) Use the Low Gain templates method (pick shape, 
//     build waveform from scratch using this shape and its linearity)
//  Then clip the low gain trace as necessary.
// ***********************************************************
// We have to add a pedestal here because the FADC
// doesn't allow values below 0, so we need to offset up
// ************************************************************
{
  fIsLowGainTrace = false;             //We start at High Gain

  fFADCTracePed = FADCTracePed;
  fLowGainPedestal = lowGainPedestal;

  fPixelHiSizeBeforeLoGainConversion = 0;
  fPixelLoSizeBeforeLoGainClip = 0;
  fPixelLoSizeAfterLoGainClip = 0;

  double highGainChargeDC;
  fIsLowGainTrace=generateTraceSamples(highGainChargeDC, pWaveForm,
				       waveFormStartIndex, numSamplesTrace,
				       fFADCTracePed);

  // *************************************************************************
  // If we are low gain. Repeat at low gain.
  // *************************************************************************
  if(enableHiLoGainProcessing && fIsLowGainTrace)
    {
      //create low gain in pWaveForm
      // ***************************************************************
      //To decide which Low gain template to use , use one of:
      // High Gain Charge (FADCTrace sum- minus trace pedestal)
      // High Gain AmplitudeDC (pWaveForm Peak)
      // High Gain AmplitudeMV (pWaveForm Peak)
      // Kust insert correct value in the GenerateTraceSamples call. Linked in
      // template file should have correct parameter(on the linearity line).
      // ******************************************************************
      
      //highGainChargeDC = highGainChargeDC  - numSamplesTrace*fFADCTracePed;

      // Find the max of elements in the vector

      double highGainAmplitudePE =  pWaveForm->GetWaveFormMax();
      double highGainAmplitudeDC = highGainAmplitudePE * fDigCntsPerPEHiGain;
      double highGainAmplitudeMV = highGainAmplitudeDC * gHighGainMVPerDC;


       // **********************************************************
      // Build the low gain wave form using the appropriate template and 
      // linearity 
      // *********************************************************
      //pWaveForm->BuildLowGainWaveForm(highGainChargeDC); 
      //pWaveForm->BuildLowGainWaveForm(highGainAmplitudeDC); 
      pWaveForm->BuildLowGainWaveForm(highGainAmplitudeMV); 
 
      // ******************************************************************
      //  The pulse is now built(includes noise) and is in units of pe's. 
      //
      // ******************************************************************
      // Scale the low gain pulse according to:
      // https://veritas.sao.arizona.edu/wiki/index.php/Pulse_Shapes_and_
      // Amplitude_Response#Data_Download_Links
      //
      //  From Greg Richards: The low gain never gets clipped just broader.
      //  This implies we use the following peak determination
      //   1:Normalize the new low gain pulse to have peak 1.0 (after pedestal
      //     addition)
      //  3:To find the low gain waveform height:Reduce the high Gain
      //    waveform by the hi/lo gain factor gLowGainToHighGainPeakRatio 
      //    (Nepomuks .099) (includes 1/6 factor and the 
      //    heightSingleHGpe/heightSingleLGPe for template 0 ~ .594
      //  4 Furthor reduce the waveform height by  by the selected template 
      //    liniarity factor (pWaveForm->fLinearity), which includes the 
      //    relataive template pulse height for equal area single pes between 
      //    this template and template0)
      //  5. Scale the nomalized LowGain pulse to have this resultant height. 
      // *******************************************************************
      double lowGainAmplitudePE=pWaveForm->GetWaveFormMax();

      double scaleFactor = highGainAmplitudePE * pWaveForm->GetLinearity() * 
	                   gLowGainToHighGainPeakRatio / lowGainAmplitudePE; 

      // **************************
      // we will mutilpy by fDigCntsPerPEHiGain (DC/PE) in the 
      // generateTraceSamples()   call
      // *****************************************
      

      pWaveForm->ScaleWaveForm(scaleFactor);
 

      // *******************************************************************
      // Now make the trace, and get resulting "size/charge"
      // *******************************************************************
      double lowGainChargeDC;
      int    lowGainStartIndex = waveFormStartIndex + gFADCLowGainLookBackBins;
      
      //  ****************************************
      // Generate the FADC trace, applying the DC/PE conversion
      // *****************************************
      generateTraceSamples(lowGainChargeDC, pWaveForm,  lowGainStartIndex, 
			   numSamplesTrace, fLowGainPedestal);
      
      lowGainChargeDC = lowGainChargeDC  - numSamplesTrace * fFADCTracePed;

      int cl=0;
      if(clipTrace()) {//For really big pulses this might be needed.
	cl=1;
      }
      
      /*
	std::cout << "n,hiQ,hiDC,hiMV,i,loQ,cl,Lin,fwhm: " 
		<< pWaveForm->GetPECount() 
		<< " " << highGainChargeDC << " " << highGainAmplitudeDC 
		<< " " << highGainAmplitudeMV << " " 
		<< pWaveForm->GetLowGainIndex() 
	        << " " << lowGainChargeDC << " " << cl << " " 
		<< pWaveForm->GetLinearity() <<" "
		<< pWaveForm->GetWaveFormFWHMns() << " ";
      
      // *******************************************************************
      // Add on the fadc trace
      // *******************************************************************
      for (int m=0; m< (int) fFADCTrace.size(); m++) {
	std::cout<<fFADCTrace.at(m)<<" ";
      }
      std::cout<<std::endl;
      */
 
    }
  // ***********************************************************************
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
// ************************************************************************

bool KSFADC::generateTraceSamples(double& traceArea, KSWaveForm* pWaveForm, 
	     int waveFormStartIndex,int numSamplesTrace, double pedestalFADC) 
// ********************************************************************
// Generate the FADC trace samples from the pWaveForm. Flag if we exceed 
// Maximum allowed trace value
// Sum the area (in dc)
// ********************************************************************
{
  int fWaveFormBinsPerFADCSample= (int) (gFADCBinSizeNS / gWaveFormBinSizeNS);
  int fNumWaveFormBins1NS       = (int) (1. / gWaveFormBinSizeNS);

  traceArea=0;
  fFADCTrace.clear();
  fFADCTrace.resize(numSamplesTrace,0);
  bool shouldBeClipped=false;
  for( int i=0; i<numSamplesTrace; i++)
    {
      // ********************************************************************
      //  Fadc electronics averages over 1 nsec.
      // ********************************************************************
      int fStartBin=waveFormStartIndex+i*fWaveFormBinsPerFADCSample;
      double fSum = pWaveForm->GetWaveFormSum(fStartBin,
				       fStartBin+fNumWaveFormBins1NS-1);
      // **********************************************************
      // This is where we add the constant ped.
      // Add uncorelated electronic noise
      // **********************************************************
      fSum=((fSum/fNumWaveFormBins1NS)*fDigCntsPerPEHiGain)+pedestalFADC + 
	                 pRandom->Gaus()*gElectFADCNoiseSigmaDC[fCameraType];
      if(fSum<0)
	{
	  fSum=0;  //Event though we added a pedestal this may be negative. If
	           // so,set it to 0, as it would be in theelectronics.
	}

      fFADCTrace.at(i)=(int)fSum;  
      traceArea+= fSum;
      fPixelHiSizeBeforeLoGainConversion += fSum-pedestalFADC; 
      // *****************************************************************
      // Hi/low: If we have a bin that goes over the HiLow threshold (usually 
      // about 250 ) The we need to swith to low gain mode.
      // *****************************************************************
      if(fFADCTrace.at(i)>gFADCHiLoGainThreshold)   //Look for high/low
	{
	  shouldBeClipped=true;  //set Low gain flag.
	}
    }
  return shouldBeClipped;
}
// ***********************************************************************

bool KSFADC::clipTrace()
// ***********************************************************************
// Check trace to see if any samples are to be clipped.
// This is almost only happening for lowGainPulsdes.
// For now just limit to 255. Actual may be different (wrap around?)
// ***********************************************************************
{
  bool isClipped=false;
  for (int i =0 ; i < (int) fFADCTrace.size(); i++) {
    if(fFADCTrace.at(i) > 255) {
      fFADCTrace.at(i)=255;
      isClipped=true;
    }
  }
  return isClipped;
}

      




