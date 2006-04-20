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

#include "KSCommon.h"
#include "KSFADC.h"


//const double kSinglePePulseHeightSigma=.275;
const double kSinglePePulseHeightSigma=.45;
const double kBaseRiseTimeNS=2.0;  //ns
const double kBaseFallTimeNS=8.5;  //ns
const int    kBaseSize=67;
const int    kBaseRiseSize=17;
const int    kBaseFallSize=kBaseSize-kBaseRiseSize;

const static double kBasePulse[kBaseSize]=
    {
      0.0,  .011,  .022,  .033,  .044,  .067,  .089,   .15,  .22,  
    .34,   .46,   .58,   .69,   .80,   .91,   .96,   1.0,  .99,  
    .98,   .97,   .96,   .94,   .91,   .89,   .86,   .81,  .78,  
    .76,   .73,   .69,   .66,   .63,   .60,   .56,   .51,  .47,  
    .43,   .39,   .37,   .36,   .33,   .31,   .30,   .28,  .27,  
    .26,   .24,   .22,   .21,   .20,   .18,   .17,   .15,  .13,  
    .12,   .12,   .11,  .089,  .078,  .060,  .044,  .038, .033, 
    .022, .011,  .005,   0.0
    };
 
class KSSinglePe
{
 private:
  double fLengthNS;       //ns
  double fArea;   
  float fXDummy;
 
 public:
  KSSinglePe();
  KSSinglePe(double riseTimeNS,double fallTimeNS);
  ~KSSinglePe(){};

  void setRiseFallTimes(double riseTimeNS,double fallTimeNS);
  double getLengthNS(){return fLengthNS;};
  double getArea(){return fArea;};
  double getMeanFADCArea(KSCameraTypes fCameraType, KSFADC& fFADC);
  double getPulseHeight(bool fAfterPulse);

  double* pfSinglePulse;
  int fNumBinsInPulse;
};  

#endif
