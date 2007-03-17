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

extern "C" float pran(float* fXDummy);
extern "C" double Gauss();

KSSinglePe::KSSinglePe()
{
  setRiseFallTimes(kBaseRiseTimeNS,kBaseFallTimeNS);
}
// ************************************************************************

KSSinglePe::KSSinglePe(double singlePulseRiseNS,double singlePulseFallNS)
{
  setRiseFallTimes(singlePulseRiseNS,singlePulseFallNS);
}
// ************************************************************************

void  KSSinglePe::setRiseFallTimes(double SinglePulseRiseTimeNS, 
				        double SinglePulseFallTimeNS)
// ************************************************************************
//    This method builds the single pe pulse shape. Max value is normalized 
//    to 1. Scale the size of the pulse to the risetime using interpolation.
//    Inital pulse has 2.0 ns risetime.  Also determine area.
// ************************************************************************
{
  fSinglePulseRiseTimeNS=SinglePulseRiseTimeNS;
  fSinglePulseFallTimeNS=SinglePulseFallTimeNS;

                                                    //Ratio of pulse widths.
  double fRiseTimeRatio=(fSinglePulseRiseTimeNS/kBaseRiseTimeNS);
  double fFallTimeRatio=(fSinglePulseFallTimeNS/kBaseFallTimeNS);
  int fNumBinsInRisingPulse=int(fRiseTimeRatio*kBaseRiseSize);
  int fNumBinsInFallingPulse=int(fFallTimeRatio*kBaseFallSize);
  if(fNumBinsInRisingPulse<1 || fNumBinsInFallingPulse<1)
    {
      std::cout<<"KSSinglePe: Rise or Fall Size is too small. risesize:"
	       <<fNumBinsInRisingPulse<<" Fall size:"<<fNumBinsInFallingPulse 
	       <<std::endl;
      exit(1);
    }
  fNumBinsInPulse=fNumBinsInRisingPulse+fNumBinsInFallingPulse;
  pfSinglePulse.clear();
  pfSinglePulse.resize(fNumBinsInPulse);

  // ********************************************************************
  // Interpolate rising part of pulse
  // ********************************************************************
  if(fNumBinsInRisingPulse==kBaseRiseSize)
    {                                   //No change from base.
      for(int i=0;i<fNumBinsInRisingPulse;i++)pfSinglePulse.at(i)=kBasePulse[i];
    }            
  else
    {
      pfSinglePulse.at(0)=kBasePulse[0];    //Starts at same place
      for (int j=1;j<fNumBinsInRisingPulse-1; j++)
	                                 //different from base:interpolate.
	{
	  double aindex=(j/fRiseTimeRatio);//fractional index within 
	                                   //kBasePulse.
	  int k=int(aindex);
	  double fFraction=(aindex-(double)k);
 
	  if(k+1>kBaseRiseSize)
	    {
	      std::cout<<"KSSinglePe: Index out of range for base.k:"<<k
		       <<std::endl;
	      exit(1);
	    }
	  pfSinglePulse.at(j)=kBasePulse[k]+
	                           fFraction*(kBasePulse[k+1]-kBasePulse[k]);
	}
                                         //ends at same place. (1.0)
      pfSinglePulse.at(fNumBinsInRisingPulse-1)=kBasePulse[kBaseRiseSize-1];  
    }
  // **********************************************************************
  //Now the falling second half of the pulse.
  // **********************************************************************
  if(fNumBinsInFallingPulse==kBaseFallSize)
    {                                   //No change from base.
      for(int i=0;i<kBaseFallSize;i++)
	{
	  int j=fNumBinsInRisingPulse+i;
	  int k=kBaseRiseSize+i;
	  pfSinglePulse.at(j)=kBasePulse[k];
	}
    }
  else
    {
      for (int j=0;j<fNumBinsInFallingPulse-1; j++)
	                                 //different from base:interpolate.
	{
	  double aindex=(j/fFallTimeRatio);//fractional index within 
	                                   //kBasePulse.
	  int k=int(aindex);
	  double fFraction=aindex-(double)k;
	  k=k+kBaseRiseSize;
	  double fBaseDifference;
	  if((k+1)==kBaseSize)
	    {
	      fBaseDifference=0;
	    }
	  else if(k+1>kBaseSize)
	    {
	      std::cout<<"KSSinglePe: Index out of range for base.k:"<<k
		       <<std::endl;
	      exit(1);
	    }
	  else
	    {
	      fBaseDifference=(kBasePulse[k+1]-kBasePulse[k]);
	    }

	  int m=fNumBinsInRisingPulse+j;
	  pfSinglePulse.at(m)=kBasePulse[k]+fFraction*fBaseDifference;
	}
                                         //ends at same place.
      pfSinglePulse.at(fNumBinsInPulse-1)=kBasePulse[kBaseSize-1];  
    }
  fLengthNS=fNumBinsInPulse*gWaveFormBinSizeNS;
  fArea=0;
  for(int i=0;i<fNumBinsInPulse;i++)fArea+=pfSinglePulse.at(i);
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
      float y=pran(&fXDummy);
      if(y<fAfterPulseFrac)
	{
	  while(1)
	    {
	      double fG=Gauss();
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
      double fG=Gauss();
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

double KSSinglePe::getMeanFADCArea(KSCameraTypes fCameraType, KSFADC& fFADC)
// *************************************************************************
// Get mean area for a single pe after its WaveFormn is converted to a FADC
// trace. Do this by taking 1000 single pes. Do this starting with pes at each 
// .25 ns position. This is to find best estimate taking account of the FADC
// digitization
// *************************************************************************

{
  //Number of time steps
  int fNumWaveFormBinsPerFADCBin=(int)(gFADCBinSizeNS/gWaveFormBinSizeNS);

  double fSinglePeFADCAreaSum=0;

  //Number of Bins in FADC wave form.
  int fNumSamplesTrace=
    (int)(fNumBinsInPulse*gWaveFormBinSizeNS/gFADCBinSizeNS)+ 1+1;  
                                              //Extra 1 is just for insruance.

  //Number of bins in Wave form to span FADC Trace
  int fNumWaveFormBins=fNumSamplesTrace*fNumWaveFormBinsPerFADCBin;

  //Loop over progressive starting place of pe pulse.
  for(int j=0;j<fNumWaveFormBinsPerFADCBin;j++)
    {
      //Make a WaveForm of 1000 single pes offset in time by j bins;
      std::vector<double> fPulse;
      //Make wave form big enough for FADC Trace
      
      fPulse.resize(fNumWaveFormBins);
      for(int i=0;i<fNumBinsInPulse;i++)
	{
	  int k=j+i;
	  fPulse.at(k)=1000.*pfSinglePulse.at(i);
	}
      // **********************************************************************
      // Because we use so many pe's here we don't really need to worry about 
      // the pedestal.  But do so anyways
      // *********************************************************************
      fFADC.makeFADCTrace(fPulse,0,fNumSamplesTrace,false,gPedestal[VERITAS499]);
      fSinglePeFADCAreaSum+=(fFADC.getWindowArea(0,fNumSamplesTrace)-
			     fNumSamplesTrace*gPedestal[VERITAS499])/1000.;
    }
  double fSinglePeMeanFADCArea=fSinglePeFADCAreaSum/fNumWaveFormBinsPerFADCBin;
  return fSinglePeMeanFADCArea;
}
// *************************************************************************


void  KSSinglePe::PrintSinglePe()
// **************************************************************************
// Dump single pe to terminal
// **************************************************************************
{
  for(int i=0;i<fNumBinsInPulse;i++)
    {
      double fBinTime=i*gWaveFormBinSizeNS;
      std::cout<<fBinTime<<" "<<pfSinglePulse.at(i)<<std::endl;
    }
  return;
}

