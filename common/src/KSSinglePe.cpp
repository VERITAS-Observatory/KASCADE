/**
 * \class KSCFD
 * \ingroup common
 * \brief File of methods for KSSinglePe.
 * Original Author: Glenn H. Sembroski
 * $Author$
 * $Date$
 * $Revision$
 * $Tag$
 *
 **/

#include <iostream>
#include "KSSinglePe.h"

#ifndef _NOROOT
  ClassImp(KSSinglePe)
#endif

KSSinglePe::KSSinglePe()
{
  initSinglePe(kBaseRiseTimeNS,kBaseFallTimeNS);
}
// *********************************************************************

KSSinglePe::KSSinglePe(double singlePulseRiseNS,double singlePulseFallNS)
{
  initSinglePe(singlePulseRiseNS,singlePulseFallNS);
}
// *********************************************************************

KSSinglePe::~KSSinglePe()
{
  //Nothing here
}
// **************************************************************************

void KSSinglePe::initSinglePe(double singlePulseRiseNS,
			                            double singlePulseFallNS)
{
  pRandom=new TRandom3(0);
  setRiseFallTimes(singlePulseRiseNS,singlePulseFallNS);
// ****************************************************************
  // Load in the Low Gain tempale waveforms. We assume a standard Default file 
  // name here. Assume links to prober file (Upgrade or NewArray) version as 
  // applicable
  // ****************************************************************
  readLowGainWaveForms("LowGainWaveForms.txt");
}
// ************************************************************************

void KSSinglePe::setRiseFallTimes(double SinglePulseRiseTimeNS, 
				        double SinglePulseFallTimeNS)
// ************************************************************************
//    This method builds the single pe pulse shape. Max value is normalized 
//    to 1. Scale the size of the pulse to the risetime using interpolation.
//    Inital pulse has kBaseRiseTimeNS risetime.  Also determine area.
// ************************************************************************
{
  fSinglePulseRiseTimeNS=SinglePulseRiseTimeNS;  //Comes from options usually
  fSinglePulseFallTimeNS=SinglePulseFallTimeNS;  // or KSCommon defaults

                                                    //Ratio of pulse widths.
  double fRiseTimeRatio=(fSinglePulseRiseTimeNS/kBaseRiseTimeNS);
  double fFallTimeRatio=(fSinglePulseFallTimeNS/kBaseFallTimeNS);
  int fNumBinsInRisingPulse=int(fRiseTimeRatio*kBaseRiseSize);
  int fNumBinsInFallingPulse=int(fFallTimeRatio*kBaseFallSize);
  if(fNumBinsInRisingPulse<1 || fNumBinsInFallingPulse<1){
    std::cout<<"KSSinglePe: Rise or Fall Size is too small. risesize:"
	     <<fNumBinsInRisingPulse<<" Fall size:"<<fNumBinsInFallingPulse 
	     <<std::endl;
    exit(1);
  }

  // **************************************************************************
  // In the hope of keeping the timing the same in the FADC window for all 
  // pulses we will try to keep the position of the peak the same as the Base 
  // pulse. If this is not possible (rise time is faster than base pulse we 
  // will fill in with zero's
  // Otherwise if its expanded we drop off the starting bins
  // **************************************************************************
  //Keep peak at same place
  fNumBinsInPulse = kBaseRiseSize + fNumBinsInFallingPulse;

  // **************************************************************************
  // We also want to limit the total length of the pulse (determines run time 
  // of ksAomega to kMaxSinglePePulseSize. Cut tail (reduce 
  // fNumBinsInFallingPulse) to  make this so.  Note that we keep and use the 
  // fFallTimeRatio when making the falling pulse
  // **************************************************************************

  if(fNumBinsInPulse> kMaxSinglePePulseSize){
    fNumBinsInPulse = kMaxSinglePePulseSize;
  }

  fNumBinsInFallingPulse=fNumBinsInPulse-kBaseRiseSize;
  
  fSinglePulse.clear();
  fSinglePulse.resize(fNumBinsInPulse,0); //non filled bins will be 0
  
  // std::cout<<"fRiseTimeRatio, fFallTimeRatio,fNumBinsInRisingPulse, "
  //           "fNumBinsInFallingPulse, fNumBinsInPulse, kBaseRiseSize: "
  //	   <<fRiseTimeRatio<<' '
  //	   <<fFallTimeRatio<<' '<<fNumBinsInRisingPulse<<' '
  //       << fNumBinsInFallingPulse
  //	   <<' '<< fNumBinsInPulse<<' '<<kBaseRiseSize<<std::endl;

  // ********************************************************************
  // Interpolate rising part of pulse
  // Note here that we always want the pulse to be centered at kBaseRiseSize "
  // with the First bin at value 0.
  // If requested rise is less than kBaseRise we fill in start with 0.
  // If reqesrted rise is greater than kBaseRise we chop off bins that would 
  // come before bin 0.
  // Note Upgrade pulse  is very narrow and has leading ~0 bins that can be 
  // dropped for slower rise pulses.
  // ********************************************************************
  if(fNumBinsInRisingPulse==kBaseRiseSize){        //No change from base.
    for(int i=0;i<kBaseRiseSize;i++){
      fSinglePulse.at(i)=kBasePulse[i];
    }
    //std::cout<<"KSSinglePe: Rising Slope:Base: "<<kBaseRiseTimeNS<<"ns"
    //         <<std::endl;
  }            
  else{
    int pBin=0;
    int startBin=fNumBinsInRisingPulse-kBaseRiseSize-1;
    if(startBin<0){
      startBin=0;
    }
 
    for (int j=startBin;j<fNumBinsInRisingPulse-1; j++){
	                                 //different from base:interpolate.
      double aindex=(j/fRiseTimeRatio);//fractional index within kBasePulse.
      int k=int(aindex);

      double fFraction=(aindex-(double)k);
 
      if(k+1>kBaseRiseSize){
	std::cout << "KSSinglePe: Index out of range for base.k:" << k
		  << std::endl;
	exit(1);
      }
      pBin=j-startBin;
      fSinglePulse.at( pBin ) = kBasePulse[k] + fFraction * ( kBasePulse[k+1] -
							      kBasePulse[k] );
    }
    //ends at same place. (1.0)
    fSinglePulse.at(kBaseRiseSize-1)=kBasePulse[kBaseRiseSize-1];  
  }
  
  // **********************************************************************
  //Now the falling second half of the pulse.
  // The tail expanda or contrack in the new pulse as requested.
  // **********************************************************************
  if(fSinglePulseFallTimeNS==kBaseFallTimeNS){      //No change from base.
    for(int i=0;i<kBaseFallSize;i++){
      int j=kBaseRiseSize+i;   // This reflects attempt to keep peak in the 
                               // same place
      int k=kBaseRiseSize+i;
      fSinglePulse.at(j)=kBasePulse[k];
    }
    //std::cout<<"KSSinglePe: Falling Slope:Base: "<<kBaseFallTimeNS<<"ns"
    //         <<std::endl;
  }
  else{
  //different from base:interpolate.
    for (int j=0;j<fNumBinsInFallingPulse-1; j++){ 

      double aindex=(j/fFallTimeRatio);//fractional index within kBasePulse.
      int k=int(aindex);
      double fFraction=aindex-(double)k;
      k=k+kBaseRiseSize;
      double fBaseDifference;
      if((k+1)==kBaseSize){
	fBaseDifference=0;
      }
      else if(k+1>kBaseSize){
	std::cout<<"KSSinglePe: Index out of range for base.k:"<<k
		 <<std::endl;
	exit(1);
      }
      else{
	fBaseDifference=(kBasePulse[k+1]-kBasePulse[k]);
      }

      int m=kBaseRiseSize+j;
      fSinglePulse.at(m)=kBasePulse[k]+fFraction*fBaseDifference;
    }
                                         //ends at same place.
    fSinglePulse.at(fNumBinsInPulse-1)=kBasePulse[kBaseSize-1];  
  }
  fLengthNS=fNumBinsInPulse*gWaveFormBinSizeNS;
  fArea=0;
  for(int i=0;i<fNumBinsInPulse;i++){
    fArea+=fSinglePulse.at(i);
  }
  return;
}
// ************************************************************************


double KSSinglePe::getPulseHeight(bool fAfterPulse)
// **************************************************************************
//  Generate pulse height for a sing pes
// **************************************************************************{
// 	Models Chucks measurment of single pe pulse height distribution	for
// 	a r1398 pmt. Mean of distribution is set to 1.0 (chucks channel 139)
// 	The distribution is modeled in two parts. 
// 	Area 1:Below height=.68 (channel 95) down to .158 its constant.0 below
// 	       .158 (channel 22) This area is .2865 of the total area under 
//             the curve. Throw over that to pick our areas.
// 	Area 2:Above .68 its a Gaussian, with a mean of 1.09(channel 152) and
// 		a sigma of .2865 (40 channels)
// 		Use a Gaussian for after pulsing.
// 		Choose some percent of the time (AFTER_PULSE_FRAC) to geneate
// 		our pulse height value from the afterpulse spectrum. Assume
// 		this spectrum is Gaussian with mean AFTER_PULSE_MEAN and width
// 		FAFTERPULSE_SIGMA. Afterpulses only show up in noise! 
{
  //const double fSinglePeGaussMean  = 1.0;      // Gaussian mean
  const double fSinglePeGaussMean  = 0.977;      // Gaussian mean(not 1 due to
                                                 // low side cut off at 0
  //const double fSinglePeGaussSigma = .275;     // Default Gaussian width
                                               // (if fSinglePeSigma=0)
  const double fAfterPulseFrac     = 3.e-4;    // Fraction of afterpulses
  const double fAfterPulseMean     = 2.0;      // Mean of afterpulses
  const double fAfterPulseSigma    = 6.0;      // Width of afterpulse spectrum
	
  // *******************************************************************
  // See if this is an after pulse.
  // *******************************************************************
  double fHeight;
  if(fAfterPulse)
    {
      double y=pRandom->Rndm();
      if(y<fAfterPulseFrac)
	{
	  while(1)
	    {
	      double fG=pRandom->Gaus();
	      //std::cout<<fG<<std::endl;
	      fHeight=fG*fAfterPulseSigma +fAfterPulseMean;
	      if(fHeight>=0)
		{
		  return fHeight;
		}
	    }
	}
    }
  // *******************************************************************
  // If we are not to make afterpulses or if this pulse wasn't chosen as
  // and afterpulse, pick from standard distribution.
  // *******************************************************************

  // *******************************************************************
  // Pick from a Gaussian with mean=0 and Sigma=1, modify result to 
  // mean=fSinglePeGaussMean and sigma=kSinglePePulseHeightSigma
  // *******************************************************************
  while(1)
    {
      double fG=pRandom->Gaus();
      //std::cout<<fG<<std::endl;
      fHeight=fG*kSinglePePulseHeightSigma + fSinglePeGaussMean;
      if(fHeight>0)
	{
	  //std::cout<<fHeight<<std::endl;
	  return fHeight;
	}
    }
}
// ****************************************************************************


void  KSSinglePe::PrintSinglePe()
// **************************************************************************
// Dump single pe to terminal
// **************************************************************************
{
  for(int i=0;i<fNumBinsInPulse;i++)
    {
      double binTimeNS=i*gWaveFormBinSizeNS;
      std::cout<<binTimeNS<<" "<<fSinglePulse.at(i)<<std::endl;
    }
  return;
}
// ***********************************************************************


// ***************************************************************************
// Low Gain template methods
// ***************************************************************************

void KSSinglePe::readLowGainWaveForms(std::string fileName)
{
  // ************************************************************************
  // Read LowGain wave forms from a text file (typically named 
  // "LowGainWaveForms.txt" use a link to the actual one if need be. Will be 
  // different for Upgrade or NewArray)
  // Text file format:
  // Line 1: "-9999 -9999" : flag for the start of a waveform
  // Line 2: "size (in DC ) (or Amplirude in Dc or amplitude in MV
  // , linearity" 
  // Line 3-end: time (in ns; starts at 0),  pulseheight
  // Next line (start of next pulse will begine as above (-9999 -9999)
  // waveform  height min == 0.0
  // waveform  height max == 1.0
  // Wave form times steps =  0.25 ns
  // All wave from start times =0.0
  // All Waveforms  start to rise at ~ 2 ns
  // All waveforms end at 64.00 ns
  // *************************************************************************
  bool debugPrint=false;

  std::cout <<  "Reading in Low Gain WaveForm file "<< fileName <<  std::endl;

  // *****************************************************************
  // Open the input file. (may be and probably is , a link)
  ifstream ifs(fileName.c_str());
  if (! ifs) {
    std::cout << " KSSingPe--Fatal-problems opening input LowGain waveform "
                 "file "<< fileName<< std::endl;
    exit(1);
  }
  int flag1;
  int flag2;

  ifs >> flag1 >> flag2;
  if ( ifs.eof() || ( flag1!= -9999 || flag2 != -9999 ) ) {
    std::cout << "KSSingPe--Fatal -Problems reading first line of file" 
	      << fileName
	<< std::endl;
    exit(1);
  }
 
  if(debugPrint) {
    std::cout << "flag1,flag2: "<<flag1<<" "<<flag2<< std::endl;
  }
  
  bool   finishedFile  = false;
  double startBinNS    = 0;
  int    templateIndex = -1;
  // *******************************************************************
  // Start pulse loop
  // *******************************************************************
  while (1) {
    double size;
    double linearity;
    ifs >> size >> linearity;
    if ( ifs.eof() ) {
      std::cout << "KSSingPe--Fatal -Unexpected eof Problems reading first "
	           "line of file " << fileName<< std::endl;
      exit(1);
    }
    templateIndex++;

    // *********************************************************************
    // Initalize the basic waveform object(good for templates)
    // *********************************************************************
    KSWaveFormBasic waveForm;
    
    bool pulseInited=false;
    double oldBinTime=0;
    double time;
    double pulseHeight;
    // **********************************************************************
    // Loop over reading in each time bin
    // **********************************************************************
    while(1) {     //readin and save the pulse

      ifs >> time >> pulseHeight;
      if (ifs.eof() ) {
	finishedFile=true;
	break;
      }
      if ( (int) time == -9999) {
	finishedFile=false;
	break;
      }
      
      // ********************************************************
      // Do some sanity checking: Make sure no missing bins
      // ********************************************************
      if( ! pulseInited) {
	startBinNS=time;
	oldBinTime=time - gWaveFormBinSizeNS;
        pulseInited=true;
      }
      
      if( abs(time-(oldBinTime+gWaveFormBinSizeNS) ) > 
	                                        gWaveFormBinSizeNS/1000 ) {
	std::cout << "Missing bin time: "<<oldBinTime+gWaveFormBinSizeNS
		  << std::endl;
      }
      
      if(debugPrint) {
	std::cout << "time,pulseHeight: "<< time << " "<< pulseHeight
		  << std::endl;
      }
      
      // ****************************************************************
      // Good time bin, save it away
      // ****************************************************************
      waveForm.fWaveForm.push_back(pulseHeight);
      oldBinTime=time;
    }
    
    waveForm.fSize = size;
    waveForm.fLinearity    = linearity ;
    waveForm.fStartBinNS   = startBinNS;
    fLowGainWaveForm.push_back(waveForm);
    
    //if(debugPrint) {
    //  std::cout << " LowGainWaveForm " << templateIndex <<": "
    //	      << "size,linearity,FWHMns,relArea:"<< size << " " 
    //	      << linearity << " "<<  waveForm.GetWaveFormFWHMns() <<" "
    //	      << waveForm.GetWaveFormSum(0,160) /
    //                         fLowGainWaveForm.at(0).GetWaveFormSum(0,160)
    //	      << std::endl;
    //}

    if (finishedFile) {
      break;
    }
  }
  // ******************************************************************
  // For picking templates from high values determine the ranges that each
  // template covers. Also used for interpolating linearity
  // Low limit extends to halfway down to previous template mean and halfway 
  // up to next template mean. We obvioulsy only need to keep high limit.
  // low limit will be previous templtes high limit.
  // ******************************************************************
  // template 0 hase no lower limit.  last template has no upper limit.
  // ******************************************************************
  
  // *******************************************************************
  // template 0 (linear regime), use its fgiven upper value as is (don't go 
  // haflway up to next template)
  // *******************************************************************
  fLowGainWaveForm.at(0).fUpperHighGainArea = 
                                       fLowGainWaveForm.at(0).fSize;
  fLowGainWaveForm.at(0).fUpperLinearity =fLowGainWaveForm.at(0).fLinearity;


  int numWaveForms=fLowGainWaveForm.size();
  for(int i = 1; i < numWaveForms-1; i++ ) {
    fLowGainWaveForm.at(i).fUpperHighGainArea = 
                               (fLowGainWaveForm.at(i).fSize +
                                 fLowGainWaveForm.at(i+1).fSize)/ 2.;
    // **************************************************
    // And do the same for the linearity
    // **************************************************
    if ( i == numWaveForms-1){
      fLowGainWaveForm.at(i).fUpperLinearity = 
	fLowGainWaveForm.at(i).fLinearity;
    }
    else{
      fLowGainWaveForm.at(i).fUpperLinearity = 
	(fLowGainWaveForm.at(i).fLinearity +
	 fLowGainWaveForm.at(i+1).fLinearity)/ 2.;
    }
  }
  std::cout << "Number of LowGain WaveForms:"<<  numWaveForms  << std::endl;
  return;
}
// ***************************************************************************

int KSSinglePe::getLowGainIndexAndLinearity(double selectionValue, 
					    double& linearity)
// ***************************************************************************
// Find Index of the template whose fSize matches the 
// selectionValue value.  Matching means that the templateSelection value is 
// less than halfway to the next templates value. (except for the linear 
// first one where we use the givern value. Thes valuse set when template 
// file is read in
// ***************************************************************************
{
  // ***********************
  // Start the search. Most likely will be lowest so start there
  // **********************


  int templateIndex=fLowGainWaveForm.size()-1; //Highest it could be

  // ******************************************************************
  for(int i=0; i< (int) fLowGainWaveForm.size()-1; i++) {
    if ( selectionValue < fLowGainWaveForm.at(i).fUpperHighGainArea) {
       templateIndex= i;
       break;
    }
  }
  // *******************************************************************
  // Now interpolate linearity
  // *******************************************************************

  // ****************
  // At extremes linearity is constant over range of applacability ot that 
  // template
  // ****************
  if(templateIndex == 0 || templateIndex == (int)fLowGainWaveForm.size()-1) {
    linearity=fLowGainWaveForm.at(templateIndex).fUpperLinearity;
  }
  else{
    // **********************
    // Interpolate
    // **********************
    double upperArea=fLowGainWaveForm.at(templateIndex).fUpperHighGainArea;
    double lowerArea=fLowGainWaveForm.at(templateIndex-1).fUpperHighGainArea;
    double ratio = (selectionValue - lowerArea) / (upperArea-lowerArea);
    
    double upperLinearity=fLowGainWaveForm.at(templateIndex).fUpperLinearity;
    double lowerLinearity=fLowGainWaveForm.at(templateIndex-1).fUpperLinearity;
    linearity=lowerLinearity + (ratio * (upperLinearity-lowerLinearity) );
  }
  return templateIndex;
}
// *************************************************************************
