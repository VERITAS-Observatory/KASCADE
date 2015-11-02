/**
 * \class KSWaveForm
 * \ingroup common
 * \brief Class to hold and minupulate single pixels waveform.
 *  
 * Original Author: Glenn H. Sembroski
 * $Author$
 * $Date$
 * $Revision$
 * $Tag$
 *
 **/
// This is where the code starts.

#include "KSWaveForm.h"

extern "C" double Rexp(double rate);


KSWaveForm::KSWaveForm(KSCameraTypes CameraType, KSSinglePe* PSinglePe)
{
  fCameraType=CameraType;
  pfRandom=new TRandom3(0);

  pfSinglePe=PSinglePe;
  fSinglePeSizeNS      = pfSinglePe->getLengthNS();
  fSinglePeSizeNumBins = pfSinglePe->fNumBinsInPulse;
  fSinglePeArea        = pfSinglePe->getArea();

  fWaveFormInitalized=false;
  fDebug=false;
  //fDebug=true;  //Mostly to provide gdb breakpoint places.

  // ***************************************************************
  // LoGain: This is where we would readin/determine the logain template
  // pulses
  // **************************************************************
  // ********************************************************************
  // Allocate the wave form vectors.
  // ********************************************************************

  return;
}
// ********************************************************************

KSWaveForm::~KSWaveForm()
{
  //nothing to do
}
 // *********************************************************************

int KSWaveForm::InitWaveForm(double WaveFormStartNS,
			      double WaveFormLengthNS)
// ********************************************************************
// Allocate the wave form. Return size (number of elements)
// ********************************************************************
{
  //if(fDebug) {
  //  std::cout<<"Inializing waveForm"<<std::endl;
  // }
  fWaveFormInitalized=true;
  fWaveFormStartNS=WaveFormStartNS;
  fWaveFormLengthNS=WaveFormLengthNS;
  fNumWaveFormBins=(int)(fWaveFormLengthNS/gWaveFormBinSizeNS + 1.0);
  fWaveForm.clear();
  fWaveForm.resize(fNumWaveFormBins,0.0);

  fLowGainMode=false;
  pfSelectedWaveForm = &(pfSinglePe->fSinglePulse);  //Pointer to single pe.
  fWaveFormPETimesNS.clear();      // Holds all the times pe's and their pulse
  fWaveFormPEPulseHeights.clear(); // heights that were added to the high Gain 
                                   // Waveform. Used them to generate the Low 
                                   // Gain waveform.
  return fNumWaveFormBins;
}
// ********************************************************************

int KSWaveForm::BuildPeWaveForm(vector<double>& TimePe, double Efficiency)
// **********************************************************************
// In fWaveForm, build the wave form we expect from the real pe's 
// Thus no after pulsing.
// **********************************************************************
{
  if ( ! fWaveFormInitalized ) {
    cout<<"KSWaveForm::BuildPeWaveForm--Fatal--WaveForm not initalized"
	<<endl;
    exit(1);
  }

  fPECount=0;
  int numPes=TimePe.size();
  bool afterPulse=false;
  if(numPes>0){
    for(int i=0;i<numPes;i++){
      if(pfRandom->Rndm()<Efficiency){
	fPECount++;
	double peTime=TimePe.at(i)-fWaveFormStartNS;
	addPe(peTime,afterPulse);
      }
    }
  }
  if (fDebug && fPECount > 180 ) {
    cout<<"fPECount: "<<fPECount<<endl; //Provides a gdb break point place
  }
  
  return fPECount;
}
// **********************************************************************

int KSWaveForm::AddNoiseToWaveForm(bool AfterPulse, 
				   double NoiseRatePerNS)
// **********************************************************************
// In fWaveForm, Add the noise pulses to it.
// Note that this noise rate is not affected by the base efficiency!!!
// **********************************************************************
// Add pe at random times using Rexp for time intervels
{
  // ****************************** ************************************
  // The Base noise rate was  modified by the light cone collection area 
  // efficiency to account for different PMT areas/etc and the pixel 
  // hexagon area.
  // ******************************************************************
  if ( ! fWaveFormInitalized ) {
    cout<<"KSWaveForm::AddNoiseToWaveForm--Fatal--WaveForm not "
          "initalized"<<endl;
    exit(1);
  }
  double meanTimeGapNS= 1./NoiseRatePerNS;
  double noiseTimeNS  = -fSinglePeSizeNS + Rexp(meanTimeGapNS);
  int iCount = 0;

  while(noiseTimeNS<gWaveFormBinSizeNS*fNumWaveFormBins) {
    addPe(noiseTimeNS,AfterPulse);
    noiseTimeNS+=Rexp(meanTimeGapNS);
    iCount++;
  } 
 
  //cout<<"Num noise pe's:"<<icount<<endl;
  return iCount;
}
// *********************************************************************

void KSWaveForm::addPe(double PETimeNS,bool AfterPulse, double lowGainPePulseHeight)
// **********************************************************************
// Add a single pe to the waveForm array.
// lowGainPePulseHeight onl;y used for generating LowGain WaveForms. For high 
// Gain wave forms it is ignored.
// **********************************************************************
{
  // ***********************************************************
  // Set for high gain single pe or low gain template
  // ***********************************************************
  double singlePeSizeNS;
  double singlePeSizeNumBins;
  if( ! fLowGainMode ) {
    singlePeSizeNumBins = fSinglePeSizeNumBins;
    singlePeSizeNS      = fSinglePeSizeNS;
  }
  else{
    singlePeSizeNumBins = pfSelectedWaveForm->size();
    singlePeSizeNS      = (singlePeSizeNumBins-1) * gWaveFormBinSizeNS;
  }

  // ******************************************************************
  // See if any of the pe will be in the waveform window. 
  // PeTimeNS could be negative
  // *******************************************************************
  int endInWaveFormThisPeNS=PETimeNS+singlePeSizeNS+gWaveFormBinSizeNS;
  if(endInWaveFormThisPeNS<=0){ //End of pe before waveform start.
    return;
  }

  // ********************************************************************
  // The single pe we are adding ends after the start of the wave form.
  // See if it starts after the end of the waveform.
  // ********************************************************************
  int startBin=(int)(PETimeNS/gWaveFormBinSizeNS);// Noise can start at
                                                  // or before 0.
  if(startBin>fNumWaveFormBins-1){
    return; 
  }

  
  // ********************************************************************
  // Find where it starts. If before the wave form find place in the 
  // single pe that goes into first bin of wave form
  // ********************************************************************
  int peStartIndex=0;        // Nominal start and end index of single pe
  int peEndIndex=singlePeSizeNumBins-1;
  if(startBin<0) {           // We only have the tail end of the pe pulse
    peStartIndex=-startBin;  // So we skip the first startBin's of the 
    startBin=0;              // single pe and we put that in first wave 
                             // form bin
  }
  
  // ********************************************************************
  // If pulse ends after the end of the wave form reduce to that.
  // ********************************************************************
  if((startBin+singlePeSizeNumBins-1)>=fNumWaveFormBins) {
    peEndIndex=fNumWaveFormBins-startBin-1;
  }
  
  // Now load in the single pe
  int waveFormIndex=startBin;     
  double pulseHeight;
  // ********************************************************************
  // At this point we have a valid PETime pulse to add into out wave form.
  // Save the time for the time when we will have to regenerate a lo gain
  // pulse. Also save the pulse height
  // ********************************************************************
  if( ! fLowGainMode) {
  }
  if( ! fLowGainMode ) {
    pulseHeight=pfSinglePe->getPulseHeight(AfterPulse);

    fWaveFormPETimesNS.push_back(PETimeNS);
    fWaveFormPEPulseHeights.push_back(pulseHeight);
  }
  else{
    pulseHeight=1.0;
  }
  for(int i=peStartIndex;i<=peEndIndex;i++){
    fWaveForm.at(waveFormIndex) += pfSelectedWaveForm->at(i)*pulseHeight;
    waveFormIndex++;
  }
  return;
} 
// **********************************************************************

void KSWaveForm::RemovePedestalFromWaveForm(double WaveFormPedestal)
 // ********************************************************************
 // Remove Specified Pedestal from fWaveForm
 // ********************************************************************
{
  if ( ! fWaveFormInitalized ) {
    cout<<"KSWaveForm::RemovePedestalFromWaveForm--Fatal--WaveForm not "
      "initalized"<<endl;
    exit(1);
  }
  for(int i=0;i<(int)fWaveForm.size();i++)
    {
      fWaveForm.at(i)=fWaveForm.at(i)-WaveFormPedestal;
    }
  return;
}
// *********************************************************************

void KSWaveForm::AddPedestalToWaveForm(double WaveFormPedestal)
 // ********************************************************************
 // Add specified Pedestal to fWaveForm
 // ********************************************************************
{
  if ( ! fWaveFormInitalized ) {
    cout<<"KSWaveForm::AddPedestalToWaveForm--Fatal--WaveForm not "
      "initalized"<<endl;
    exit(1);
  }
  for(int i=0;i<(int)fWaveForm.size();i++)
    {
      fWaveForm.at(i)+=WaveFormPedestal;
    }
  return;
}
// *********************************************************************


void KSWaveForm::PrintWaveForm()
// **********************************************************************
// Dump wave form to ouput in form easy to plot with root. Used for 
// debugging.
// **********************************************************************
{
  if ( ! fWaveFormInitalized ) {
    cout << "KSWaveForm::PrintWaveForm--Fatal--WaveForm not "
      "initalized" << endl;
    exit( 1 );
  }
  int numBins = fWaveForm.size();
  for( int i=0; i<numBins; i++) {
      double binTime = fWaveFormStartNS + i * gWaveFormBinSizeNS;
      cout << binTime << " " << fWaveForm.at( i ) << " " << endl;
    }
  return;
}
// *********************************************************************

/*
double KSWaveForm::GetWaveFormSum()
{
  int    endIndex     = fWaveForm.size()-1;
  double waveFormsum  = GetWaveFormSum(0, endIndex);
  return waveFormsum;
}

double KSWaveForm::GetWaveFormSum(int StartBin, int EndBin)
{
  double waveFormSum = 0.;
  for( int i = StartBin; i<EndBin+1; i++) {
    waveFormSum += fWaveForm.at( i );
  }
  //if(fDebug) {
  //  PrintWaveForm();
  //  cout<<"GetWaveFormMax: "<<endl;
  // }
  return  waveFormSum;
}


// *********************************************************************

void KSWaveForm::ScaleWaveForm(double scaleFactor)
{
  for( int i=0; i< (int) fWaveForm.size(); i++) {
    fWaveForm.at(i) *= scaleFactor;
  }
  return;
}
// ***************************************************************


double KSWaveForm::GetWaveFormMax()
{
  std::vector< double >::iterator it;   //for pulse array
  // ******From max of waveFrom
  it = max_element(fWaveForm.begin(), fWaveForm.end());
  return  *it;
}
// *********************************************************************


double KSWaveForm::GetWaveFormFWHM()
{
  // ********************************************************************
  // This very crude FWHM. Only use for debugging
  // ********************************************************************
  double halfMax = GetWaveFormMax() / 2.0;
  int lowEdgeBin  = 0;
  int highEdgeBin = 0;
  for( int i=0; i< (int) fWaveForm.size(); i++) {
    if (fWaveForm.at(i) > halfMax ) {
      lowEdgeBin=i;
      break;
    }
  }
  //+8 just to make sure we are not fooled by local noise
  for( int i=lowEdgeBin+8 ; i< (int) fWaveForm.size(); i++) {
    if (fWaveForm.at(i) < halfMax ) {
      highEdgeBin=i;
      break;
    }
  }
  double FWHMns = ( highEdgeBin -lowEdgeBin ) * gWaveFormBinSizeNS;
  return FWHMns;
}
*/

// ******************************************************************

void KSWaveForm::BuildLowGainWaveForm(double templateDesignator)
// ********************************************************************
// This is where the HighGain waveform is converted into a lowgain wave form.
// Methods:
//   (old):  Take the High Gain Wave form in fWaveForm and "attenuate" it by 
//           multiplying by DigCntsPerPEHiGain/gFADCHiLoGainRatio.
//   (new):  1:Using the templateDesignator (which may be FADCTrace area in DC
//                                           or highGainWaveForm peak in DC
//                                           or highGainWaveForm peak in MV
//             depending on which file we have linked to the 
//             lowGainTemplates.txt file, find low Gain Template shape index.
//           2:Build a new fWaveForm using the fWaveFormPETimes out of 
//             additions of the selected template shape like we did for the 
//             High gain . Use 1.0 for pulseHeight. (no differce than if we 
//             used the recorded fWaveFormPEOPulseHeight)
//           3:Adjust new fWaveForm for area differences between high and low 
//             gain normalized pulse shapes and the Low Gain linearity for 
//             the selected low gain pulse shape.
// **********************************************************************
{
  fLowGainMode=true;  // Used by add Pe to ignore efficiency (already done 
                      // when adding pe's to fWaveFormPETimesNS). Also uses 
                      // fWaveFormPEPulseHeights  for
                      // all single pe pulse heights 

  // **********************************************************
  // Find Low gain template we want.  Set up pointer to wave form and get
  // linearity value
  // ***********************************************************
  fLowGainIndex= pfSinglePe->getLowGainIndexAndLinearity(
							templateDesignator, 
  							fLinearity);
 //							fLinearity) +14;
    
  pfSelectedWaveForm = 
                    &pfSinglePe->fLowGainWaveForm.at(fLowGainIndex).fWaveForm;
  
  fSize = pfSinglePe->fLowGainWaveForm.at(fLowGainIndex).GetSize();
  fLinearity = pfSinglePe->fLowGainWaveForm.at(fLowGainIndex).GetLinearity();

  
  
  // *************************************************************
  // Actually build the waveform. (sky noise pe have already been added)
  // Just cleanout the waveform and resize it, all other stuff (startbin time 
  // ect. remains the same as the original
  // *************************************************************
  fWaveForm.clear();
  fWaveForm.resize(fNumWaveFormBins, 0.0);
  int numPeTimes = fWaveFormPETimesNS.size();
  for ( int i = 0; i < numPeTimes; i++ ) {
    double peTime        = fWaveFormPETimesNS.at(i);
    double pePulseHeight =  fWaveFormPEPulseHeights.at(i);
    addPe(peTime, false, pePulseHeight);
  }
  return;
}
// **********************************************************************  
