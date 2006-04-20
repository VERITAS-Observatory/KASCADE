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
 
class KSFADC
{
 private:
  KSCameraTypes fCameraType;

 public:
  KSFADC();
  ~KSFADC();
  void SetCameraType(KSCameraTypes CameraType){fCameraType=CameraType;return;};
  void makeFADCTrace(std::vector<double>& fWaveForm, int fWaveFormStartIndex,
		     int& fTraceLengthBins, bool EnableHiLoGainProcessing);
  double getWindowArea(int fStartTraceIndex, int fNumBinsToSum);

  std::vector<int> fFADCTrace;
  bool fFADCLowGain;
};  

#endif
