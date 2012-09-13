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
//  const double kBaseRiseTimeNS=2.0;  //ns
//  const double kBaseFallTimeNS=8.5;  //ns
//  const int    kBaseSize=67;
//  const int    kBaseRiseSize=17;
//  const int    kBaseFallSize=kBaseSize-kBaseRiseSize;
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
//Original VERITAS single pe pulse
//const double kBaseRiseTimeNS=3.2;  //ns
//const double kBaseFallTimeNS=8.5;  //ns
//const int    kBaseSize=120;
//const int    kBaseRiseSize=27;
//const int    kBaseFallSize=kBaseSize-kBaseRiseSize;
//const static double kBasePulse[kBaseSize]=
//  {
//    0.008, 0.011, 0.012, 0.015, 0.021, 0.031, 0.035, 0.041, 0.056,
//    0.084, 0.105, 0.138, 0.183, 0.225, 0.287, 0.355, 0.434, 0.507,
//    0.595, 0.682, 0.758, 0.828, 0.889, 0.939, 0.965, 0.983, 1.000,
//    0.994, 0.976, 0.940, 0.920, 0.889, 0.857, 0.826, 0.796, 0.758,
//    0.722, 0.686, 0.661, 0.622, 0.599, 0.567, 0.547, 0.528, 0.496,
//    0.481, 0.459, 0.430, 0.418, 0.391, 0.376, 0.358, 0.339, 0.327,
//    0.315, 0.298, 0.283, 0.264, 0.261, 0.256, 0.246, 0.227, 0.215,
//    0.214, 0.195, 0.188, 0.179, 0.175, 0.170, 0.152, 0.152, 0.150,
//    0.138, 0.135, 0.130, 0.133, 0.120, 0.119, 0.114, 0.112, 0.105,
//    0.107, 0.097, 0.101, 0.096, 0.089, 0.082, 0.088, 0.078, 0.076,
//    0.078, 0.078, 0.074, 0.073, 0.068, 0.067, 0.062, 0.071, 0.064,
//    0.057, 0.062, 0.054, 0.062, 0.054, 0.055, 0.050, 0.051, 0.055,
//    0.045, 0.043, 0.045, 0.041, 0.046, 0.042, 0.039, 0.040, 0.050,
//    0.034, 0.020, 0.010
//  };
// ************************************************************************

// ************************************************************************
// VERITAS Upgrade single Pe measured by Nepomuk Spring 2012
// ************************************************************************
const double kBaseRiseTimeNS=1.7;  //ns
const double kBaseFallTimeNS=4.75;  //ns
const int    kBaseRiseSize=27;
const int    kBaseSize=175;
const int    kMaxSinglePePulseSize=175;    //Cut off tail to not exceed this length
const int    kBaseFallSize=kBaseSize-kBaseRiseSize;

const static double kBasePulse[kBaseSize]=
  {
    0.00861,0.000293, 0.00243,-0.00156,-0.00591, -0.0146, -0.0106,-0.00435,
    0.00823,  0.0101,-0.000694,-0.00029,0.00229,  0.0111,-0.000993, 0.0262,
     0.0559,  0.0981,   0.149,   0.256,   0.396,   0.556,   0.726,   0.871,
      0.965,   0.996,       1,   0.971,   0.932,   0.881,    0.84,   0.817,
      0.773,   0.706,   0.647,   0.598,    0.56,   0.507,   0.446,   0.388,
      0.331,   0.284,    0.25,   0.239,   0.239,   0.235,   0.202,   0.197,
      0.161,   0.135,   0.127,   0.128,   0.135,   0.129,    0.13,   0.119,
      0.105,  0.0975,   0.098,   0.086,  0.0795,  0.0764,  0.0771,  0.0825,
     0.0934,  0.0879,  0.0783,  0.0764,  0.0601,  0.0453,  0.0563,  0.0709,
     0.0696,  0.0618,  0.0486,  0.0513,  0.0506,  0.0403,  0.0572,  0.0562,
     0.0538,  0.0344,  0.0498,  0.0413,   0.047,  0.0457,   0.033,  0.0281,
     0.0332,  0.0286,  0.0267,  0.0393,  0.0523,  0.0417,  0.0421,  0.0449,
     0.0373,  0.0434,  0.0344,  0.0276,  0.0308,  0.0398,  0.0372,   0.035,
     0.0324,   0.041,  0.0429,  0.0237,  0.0372,  0.0366,  0.0364,  0.0257,
     0.0204,  0.0249,  0.0246,  0.0241,  0.0215,  0.0169,  0.0375,  0.0259,
     0.0263,  0.0147,  0.0112,   0.024,  0.0176,  0.0286,  0.0349,  0.0286,
     0.0372,  0.0315,  0.0444,  0.0275,  0.0216,  0.0139,  0.0138,  0.0299,
     0.0305,  0.0252,  0.0286,  0.0318,  0.0224, 0.00783,  0.0194,  0.0245,
     0.0333,  0.0223,  0.0144,   0.012,  0.0145,  0.0181,  0.0143,   0.025,
     0.0334,  0.0233,  0.0256,  0.0241,  0.0173,  0.0307,  0.0248,  0.0176,
     0.00956, 0.0206,   0.021,  0.0234,   0.021,  0.0136,  0.0226,  0.0214,
     0.0234,  0.0271,  0.0181,   0.024,  0.0119,  0.0248,  0.0306
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
  virtual ~KSSinglePe();

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
#ifndef _NOROOT
  ClassDef(KSSinglePe,0);
#endif
};  

#endif
