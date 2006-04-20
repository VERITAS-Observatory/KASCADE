/**
 * Original Author: Glenn H. Sembroski
 * $Author$
 * $Date$
 * $Revision$
 * $Tag$
 *
 **/

#ifndef KSCFD_H
#define KSCFD_H
 
#include "KSPixel.h"
#include "KSCommon.h"
//                                            (WHIPPLE490,VERITAS499)
const double kCFDGain[2]                    = {       3.0,      1.51};
const double kCFDOffsetPE[2]                = {       0.0,       0.0}; 
const double kCFDMinTimeAboveThresholdNS[2] = {       0.0,       2.5};

class KSCFD
{
 private:
  KSCameraTypes fCameraType;
  int fNumCFDDelayBins;
  double fCFDGain;
  double fCFDOffsetPE;
  int fCFDMinTimeAboveThresholdBins;

  std::vector<double> fMainPulse;
  std::vector<double> fNegativePulse;

 public:
  KSCFD(KSCameraTypes CameraType);
  ~KSCFD();

  bool isFired(KSPixel& fPixel, double fStartTimeOffsetNS);

};  

#endif
