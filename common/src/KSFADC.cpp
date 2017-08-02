/**
 * \class KSFADC
 * \ingroup common
 * \brief File of methods for KSFADC.
 * Original Author: Glenn H. Sembroski 
 * $Author$Date$
 * $Revision$
 * $Tag$
 *
 **/

#include <iostream>
#include "KSFADC.h"


KSFADC::KSFADC()
{
  pRandom=new TRandom3(0);
  fTraceID = 0;
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
  //fDigCntsPerPELoGain=gFADCDigCntsPerPEHiGain[fCameraType]/gFADCHiLoGainRatio;

  return;
}
// *************************************************************************

void KSFADC::SetDigCntsPerPEGains(double DigCntsPerPEHiGain)
{
  fDigCntsPerPEHiGain=DigCntsPerPEHiGain;
  //fDigCntsPerPELoGain=DigCntsPerPEHiGain/gFADCHiLoGainRatio;
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
  bool fDebugPrint=false;
  //bool fDebugPrint=true;
  bool  fLowGainDiagnosticPrint=false;
#bool fLowGainDiagnosticPrint=true;

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
      bool goodAreas = true;

      double highGainCharge7BinsDC;
      int    startTracePulseHG     = getFADCTracePulseStart(fFADCTracePed);
      // ****************************
      // Make sure we have at least 7 bins to sum
      // ***************************
      if(fFADCTrace.size() - startTracePulseHG < 7 ) {
	goodAreas = false;
      }
      else {
	highGainCharge7BinsDC = getWindowArea(startTracePulseHG,7);
      }
      //if(fDebugPrint ) {
      //	std::cout << "**Hi Gain Pulse: " 
      //			  << startTracePulseHG << " " 
      //			  << (int)fFADCTrace.size() << " ";
      //	for (int m=0; m< (int) fFADCTrace.size(); m++) {
      //	  std::cout<<fFADCTrace.at(m)<<" ";
      //	}
      //	std::cout<<std::endl;
      //}   
      // ***************************************************************
      // To decide which Low gain template to use , use one of:
      // High Gain Charge (FADCTrace sum- minus trace pedestal)
      // High Gain AmplitudeDC (pWaveForm Peak)
      // High Gain AmplitudeMV (pWaveForm Peak)
      // Just insert correct value in the GenerateTraceSamples call. Linked in
      // template file should have correct parameter(on the linearity line).
      // ******************************************************************
      
  
      // Find the max of elements in the vector

      //double highGainAmplitudePE = pWaveForm->GetWaveFormMax();
      //double highGainAmplitudeDC = highGainAmplitudePE * fDigCntsPerPEHiGain;
      //double highGainAmplitudeMV = highGainAmplitudeDC * gHighGainMVPerDC;
      //double highGainTraceAmplitudeDC = getFADCTraceMax();

      double highGainNumPES      = pWaveForm->GetNumPES();
      double highGainIsochronicAmplitudeMV = 
	highGainNumPES * fDigCntsPerPEHiGain * gHighGainMVPerDC;

      //double highGainAreaPE      = pWaveForm->GetWaveFormSum();

      // **********************************************************
      // Build the low gain wave form using the appropriate template and 
      // linearity 
      // *********************************************************
      //pWaveForm->BuildLowGainWaveForm(highGainChargeDC); 
      //pWaveForm->BuildLowGainWaveForm(highGainAmplitudeDC); 
     
      //pWaveForm->BuildLowGainWaveForm(highGainAmplitudeMV); 

      double interpolatedLinearity;
      pWaveForm->BuildLowGainWaveForm(highGainIsochronicAmplitudeMV,
				      interpolatedLinearity); 
 
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
      //    waveform by the hi/lo gain factor fLowGainToHighGainPeakRatio 
      //    (Than 2016:Hamamatsu 1/10.25=.0976, photonis = 1/7.52=.13298) 
      //  4 Furthor reduce the waveform height by  by the interpolated 
      //    template liniarity factor, which is the 
      //    relataive template pulse height for equal area single pes between 
      //    this template and template0)
      //  5. Scale the nomalized LowGain pulse to have this resultant height. 
      // *******************************************************************
      // ******************************************************************
      //  CARE's scaling(which I think is the correct one to use)
      double scaleFactor = fLowGainToHighGainPeakRatio * interpolatedLinearity;
      

      // **************************
      // we will mutilpy by fDigCntsPerPEHiGain (DC/PE) in the call to
      // generateTraceSamples() 
      // *****************************************
      pWaveForm->ScaleWaveForm(scaleFactor);
 
      // *******************************************************************
      // Now make the trace, and get resulting "size:charge"
      // *******************************************************************
      double lowGainChargeDC;
      int    lowGainStartIndex = waveFormStartIndex + gFADCLowGainLookBackBins;
      
      //  ****************************************
      // Generate the FADC trace, applying the DC/PE conversion
      // *****************************************
      generateTraceSamples(lowGainChargeDC, pWaveForm,  lowGainStartIndex, 
			   numSamplesTrace, fLowGainPedestal);
      int startTracePulseLG = getFADCTracePulseStart(fLowGainPedestal);

      double lowGainCharge7BinsDC = 0;
      if(fFADCTrace.size() - startTracePulseLG < 7 ) {
	goodAreas = false;
      }
      else {
	lowGainCharge7BinsDC = getWindowArea(startTracePulseLG,7);
      }

      if(fLowGainDiagnosticPrint&& goodAreas) {
	double lowGainSizeDC  = lowGainChargeDC  - 
	  (numSamplesTrace * fLowGainPedestal);
	double lowGainSize7BinsDC  = lowGainCharge7BinsDC  - 
	  (7 * fLowGainPedestal);
	double highGainSizeDC = highGainChargeDC - 
	  (numSamplesTrace * fFADCTracePed);
	double highGainSize7BinsDC = highGainCharge7BinsDC - 
	  (7 * fFADCTracePed);
	//double lowGainTraceAmplitudeDC = getFADCTraceMax();
	std::cout << "**LowGainTrace##" << " " 
		  << pWaveForm->GetPECount() << " " 
		  << highGainSize7BinsDC << " "          //No pedestal
		  << pWaveForm->GetLowGainIndex() << " " //template iindex
		  << pWaveForm->GetLinearity() << " "  //templatelinearity
		  << interpolatedLinearity << " "      //linearity used.
		  << pWaveForm->GetSize() << " "       //LG template amplitude
		  << lowGainSize7BinsDC << " ";  //no pedestal
	if (fDebugPrint) {
	  std::cout << lowGainSize7BinsDC/highGainSize7BinsDC << " "
		    << highGainSizeDC << " "
		    << lowGainSizeDC << " "
		    << lowGainSizeDC/highGainSizeDC << " "; 
	}
	// *******************************************************************
	// Add on the fadc trace. Needs to be last. includes pedestal
	// *******************************************************************
	std::cout << (int)fFADCTrace.size() << " ";
	for (int m=0; m< (int) fFADCTrace.size(); m++) {
	  std::cout<<fFADCTrace.at(m)<<" ";
	}
	std::cout<<std::endl;
	std::cout<<std::flush;

      }
	  
    }
  // ***********************************************************************
  return;
}
// ***************************************************************************

double KSFADC::getWindowArea(int StartTraceIndex, int NumBinsToSum)
// ***************************************************************************
// Sum bins in trace statring at fStartTraceIndex. Result is in Digital Counts
// Since the makeFADCTrace creates a fFADCTrace in DC.
// ***************************************************************************
{
  int traceLength=fFADCTrace.size();
  if(traceLength-1 < StartTraceIndex+NumBinsToSum-1)
    {
      std::cout<<"KSFADC: Request for non-existant fFADCTrace bins in "
	"KSFADC::getWindowArea(first,last)actualsize: "
	       << StartTraceIndex << " " << StartTraceIndex+NumBinsToSum 
	       << " " << traceLength << std::endl;
      NumBinsToSum = traceLength- StartTraceIndex;
    }
  double sum=0;
  for(int i=0;i<NumBinsToSum;i++)
    {
      int index=StartTraceIndex+i;
      sum+=fFADCTrace.at(index);
    }
  return sum;
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
				  int waveFormStartIndex,int numSamplesTrace, 
				  double pedestalFADC) 
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
  for( int i=0; i<numSamplesTrace; i++) {
    // ********************************************************************
    //  Fadc electronics averages over 1 nsec.
    // ********************************************************************
    int fStartBin=waveFormStartIndex+i*fWaveFormBinsPerFADCSample;
    double fSum = pWaveForm->GetWaveFormSum(fStartBin,
					    fStartBin+fNumWaveFormBins1NS-1);
    // **********************************************************
    // This is where we add the constant ped.
    // Add uncorelated electronic noise. The fDigCntsPerPEHiGain is also 
    // correct here for the low gain. The  fLowGainToHighGainPeakRatio factor
    // in makeFADCTrace accounts for it.
    //
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
// ************************************************************************

int KSFADC::getFADCTraceMax()
{
  std::vector< int >::iterator it; 
  // ******From max of Trace
  it = max_element(fFADCTrace.begin(), fFADCTrace.end());
  return  *it;
}
// *********************************************************************

int KSFADC::getFADCTracePulseStart(double samplePedestal)
// *********************************************************************
// Using quick ansd dirty pulse location in trace , first sample that is 
// above .5 of max of sample and back up 2 samples.
// ********************************************************************
{
  // ***************************************************
  // As a quick and dirty way to get this code to work convert to a 
  // vector<float> subtracting the pedestal
  int traceLength=fFADCTrace.size();
  std::vector< float > trace(traceLength);
  for ( int i=0; i < traceLength; i++) {
    trace.at(i) = (float)fFADCTrace.at(i) - samplePedestal; 
  }

  // ******************************
  // extract the peak sample value
  // ******************************
  float traceMaxAmplitude = *std::max_element(trace.begin(),trace.end());

  // **********************************************************************
  // Find the index of first sample with a value exceeding half the maximum.
  // Note, std::find_if is guaranteed to return a valid iterator,
  // since the peak value itself fulfils the selection criterion.
  // **********************************************************************
  std::vector< float >::difference_type thresholdIndex = 
    std::distance(trace.begin(),std::find_if(trace.begin(),trace.end(), 
              std::bind2nd(std::greater< float >(),.5*traceMaxAmplitude) ) );
  // ***********************************
  //And back up 4 ns = 2 samples if we can
  // ***********************************
  if( thresholdIndex != 0 ) {
    thresholdIndex--;
  }
  if( thresholdIndex != 0 ) {
    thresholdIndex--;
  }
  // Return the index
  return (int) (thresholdIndex);
}
// *************************************************************************


