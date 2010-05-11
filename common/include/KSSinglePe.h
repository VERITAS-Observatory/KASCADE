/**
 * Original Author: Glenn H. Sembroski
 * $Author$
 * $Date$
 * $Revision$
 * $Tag$
 *
 **/

#ifndef KSSINGLEPE_H
#define KSSINGLEPE_H

#include <cstdlib>
#include "KSCommon.h"
#include "KSFADC.h"
#include <vector>

#include <TRandom3.h>


//const double kSinglePePulseHeightSigma=.275;
const double kSinglePePulseHeightSigma=.45;




// ********************************************************************
// Original Single PE. Measured in lab at Purdue by John Finley
// ********************************************************************
//const double kBaseRiseTimeNS=2.0;  //ns
//const double kBaseFallTimeNS=8.5;  //ns
//const int    kBaseSize=67;
//const int    kBaseRiseSize=17;
//const int    kBaseFallSize=kBaseSize-kBaseRiseSize;
//
//const static double kBasePulse[kBaseSize]=
//    {
//      0.0,  .011,  .022,  .033,  .044,  .067,  .089,   .15,  .22,  
//    .34,   .46,   .58,   .69,   .80,   .91,   .96,   1.0,  .99,  
//    .98,   .97,   .96,   .94,   .91,   .89,   .86,   .81,  .78,  
//    .76,   .73,   .69,   .66,   .63,   .60,   .56,   .51,  .47,  
//    .43,   .39,   .37,   .36,   .33,   .31,   .30,   .28,  .27,  
//    .26,   .24,   .22,   .21,   .20,   .18,   .17,   .15,  .13,  
//    .12,   .12,   .11,  .089,  .078,  .060,  .044,  .038, .033, 
//    .022, .011,  .005,   0.0
//    };
// ***********************************************************************

 
// ***********************************************************************
// VERITAS single Pe measured by Pat Moriarty/Mary Kertzman April, 2010
// Measured 16 channels from T1 T2 and T4. This is average pulse at FADC
// Code to generate the curve by Nepomuk Otte (This is mostly his work!).
// Source saved in KASCADE repository under KASCADE/calibrartions/SinglePe.
// ***********************************************************************
const double kBaseRiseTimeNS=3.2;  //ns
const double kBaseFallTimeNS=8.5;  //ns
const int    kBaseSize=120;
const int    kBaseRiseSize=27;
const int    kBaseFallSize=kBaseSize-kBaseRiseSize;

const static double kBasePulse[kBaseSize]=
  {
    0.008, 0.011, 0.012, 0.015, 0.021, 0.031, 0.035, 0.041, 0.056,
    0.084, 0.105, 0.138, 0.183, 0.225, 0.287, 0.355, 0.434, 0.507,
    0.595, 0.682, 0.758, 0.828, 0.889, 0.939, 0.965, 0.983, 1.000,
    0.994, 0.976, 0.940, 0.920, 0.889, 0.857, 0.826, 0.796, 0.758,
    0.722, 0.686, 0.661, 0.622, 0.599, 0.567, 0.547, 0.528, 0.496,
    0.481, 0.459, 0.430, 0.418, 0.391, 0.376, 0.358, 0.339, 0.327,
    0.315, 0.298, 0.283, 0.264, 0.261, 0.256, 0.246, 0.227, 0.215,
    0.214, 0.195, 0.188, 0.179, 0.175, 0.170, 0.152, 0.152, 0.150,
    0.138, 0.135, 0.130, 0.133, 0.120, 0.119, 0.114, 0.112, 0.105,
    0.107, 0.097, 0.101, 0.096, 0.089, 0.082, 0.088, 0.078, 0.076,
    0.078, 0.078, 0.074, 0.073, 0.068, 0.067, 0.062, 0.071, 0.064,
    0.057, 0.062, 0.054, 0.062, 0.054, 0.055, 0.050, 0.051, 0.055,
    0.045, 0.043, 0.045, 0.041, 0.046, 0.042, 0.039, 0.040, 0.050,
    0.034, 0.020, 0.010
  };

// *************************************************************************
 

class KSSinglePe
{
 private:
  double fSinglePulseRiseTimeNS;
  double fSinglePulseFallTimeNS;
  double fLengthNS;       //ns
  double fArea;   
  //float fXDummy;
  TRandom3* pRandom;
 
 public:
  KSSinglePe();
  KSSinglePe(double riseTimeNS,double fallTimeNS);
  ~KSSinglePe(){};

  void setRiseFallTimes(double riseTimeNS,double fallTimeNS);
  double getLengthNS(){return fLengthNS;};
  double getArea(){return fArea;};
  double getMeanFADCArea(KSCameraTypes fCameraType, KSFADC& fFADC);
  double getPulseHeight(bool fAfterPulse);
  double getRiseTimeNS(){return fSinglePulseRiseTimeNS;};
  double getFallTimeNS(){return fSinglePulseFallTimeNS;};

  void  PrintSinglePe();

  std::vector< double > pfSinglePulse;
  int fNumBinsInPulse;
};  

#endif
