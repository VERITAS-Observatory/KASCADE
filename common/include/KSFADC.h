/**
 * Original Author: Glenn H. Sembroski
 * $Author$
 * $Date$
 * $Revision$
 * $Tag$
 *
 **/

#ifndef KSFADC_H
#define KSFADC_H
#include "KSCommon.h"

#include <iostream>
#include <vector>

#include <TRandom3.h>
 
class KSFADC
{
 private:
  KSCameraTypes fCameraType;
  TRandom3* pRandom;
 public:
  KSFADC();
  ~KSFADC();
  void SetCameraType(KSCameraTypes CameraType);
  void SetDigCntsPerPEGains(double fDigCntsPerPEHiGain);

  void makeFADCTrace(std::vector<double>& fWaveForm, int fWaveFormStartIndex,
		     int& fTraceLengthBins, bool EnableHiLoGainProcessing, 
		     double fFADCTracePed);
  double getWindowArea(int fStartTraceIndex, int fNumBinsToSum);
  void  Print(int fStartTraceIndex, int fNumBinsToPrint);

  std::vector<int> fFADCTrace;
  int fFADCLoTraceStart;
  bool fFADCLowGain;
  double fDigCntsPerPEHiGain;
  double fDigCntsPerPELoGain;
};  

#endif
