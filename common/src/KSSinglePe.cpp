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

void  KSSinglePe::setRiseFallTimes(double fSinglePulseRiseTimeNS, 
				        double fSinglePulseFallTimeNS)
// ************************************************************************
//    This method builds the single pe pulse shape. Max value is normalized 
//    to 1. Scale the size of the pulse to the risetime using interpolation.
//    Inital pulse has 2.0 ns risetime.  Also determine area.
// ************************************************************************
{                                                    //Ratio of pulse widths.
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
  pfSinglePulse=new double[fNumBinsInPulse];

  // ********************************************************************
  // Interpolate rising part of pulse
  // ********************************************************************
  if(fNumBinsInRisingPulse==kBaseRiseSize)
    {                                   //No change from base.
      for(int i=0;i<fNumBinsInRisingPulse;i++)pfSinglePulse[i]=kBasePulse[i];
    }            
  else
    {
      pfSinglePulse[0]=kBasePulse[0];    //Starts at same place
      for (int j=1;j<fNumBinsInRisingPulse-1; j++)
	                                 //different from base:interpolate.
	{
	  double aindex=(j/fRiseTimeRatio);//fractional index within 
	                                   //kBasePulse.
	  int k=int(aindex);
	  if(k>kBaseRiseSize-2)
	    {
	      std::cout<<"KSSinglePe: Index out of range for base.k:"<<k
		       <<std::endl;
	      exit(1);
	    }
	  pfSinglePulse[j]=kBasePulse[k]+
	    (aindex-(double)k)*(kBasePulse[k+1]-kBasePulse[k]);
	}
                                         //ends at same place. (1.0)
      pfSinglePulse[fNumBinsInRisingPulse-1]=kBasePulse[kBaseRiseSize-1];  
    }
  // **********************************************************************
  //Now the falling second half of the pulse.
  // **********************************************************************
  if(fNumBinsInFallingPulse==kBaseFallSize)
    {                                   //No change from base.
      for(int i=1;i<kBaseFallSize-1;i++)
	{
	  int j=fNumBinsInRisingPulse+i;
	  int k=kBaseRiseSize+i;
	  pfSinglePulse[j]=kBasePulse[k];
	}
    }
  else
    {
      for (int j=1;j<fNumBinsInFallingPulse-1; j++)
	                                 //different from base:interpolate.
	{
	  double aindex=(j/fFallTimeRatio);//fractional index within 
	                                   //kBasePulse.
	  int k=int(aindex);
	  double fFraction=aindex-(double)k;
	  k=k+kBaseRiseSize;
	  if(k>kBaseSize-2)
	    {
	      std::cout<<"KSSinglePe: Index out of range for base.k:"<<k
		       <<std::endl;
	      exit(1);
	    }
	  int m=fNumBinsInRisingPulse+j;
	  pfSinglePulse[m]=kBasePulse[k]+
	    (fFraction)*(kBasePulse[k+1]-kBasePulse[k]);
	}
                                         //ends at same place.
      pfSinglePulse[fNumBinsInPulse-1]=kBasePulse[kBaseSize-1];  
    }
  fLengthNS=fNumBinsInPulse*gWaveFormBinSizeNS;
  fArea=0;
  for(int i=0;i<fNumBinsInPulse;i++)fArea+=pfSinglePulse[i];
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
  const double fSinglePeGaussMean  = 1.0;      // Gaussian mean
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
	      fHeight=Gauss()*fAfterPulseSigma +fAfterPulseMean;
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
      fHeight=Gauss()*kSinglePePulseHeightSigma + fSinglePeGaussMean;
      if(fHeight>0)
	{
	  return fHeight;
	}
    }
}
// ****************************************************************************

double KSSinglePe::getMeanFADCArea(KSCameraTypes fCameraType, KSFADC& fFADC)
// *************************************************************************
//Get mean area for a single pe after its WaveFormn is converted to a FADC
// trace. Do this by taking 1000 single pes.
// *************************************************************************

{
  //Make a WaveForm of 1000 single pes;
  int fNumTraceBins=fNumBinsInPulse*(int)(gWaveFormBinSizeNS/gFADCBinSizeNS)+1;
  std::vector<double> fPulse;
  fPulse.resize(fNumBinsInPulse);
  for(int i=0;i<fNumBinsInPulse;i++)
    {
      fPulse[i]=1000.*pfSinglePulse[i];
    }
  fFADC.makeFADCTrace(fPulse,0,fNumTraceBins,false);
  double fSinglePeMeanFADCArea=fFADC.getWindowArea(0,fNumTraceBins)/1000.;
  return fSinglePeMeanFADCArea;
}
