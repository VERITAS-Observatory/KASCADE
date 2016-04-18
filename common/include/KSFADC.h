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
#include "KSWaveForm.h"
#include "KSCommon.h"

#include <iostream>
#include <vector>

#include <TRandom3.h>
 
class KSFADC
{
 private:
  KSCameraTypes fCameraType;
  TRandom3* pRandom;
  double fLowGainToHighGainPeakRatio;
  int    fTraceID;

 public:
  KSFADC();
  ~KSFADC();
  void SetCameraType(KSCameraTypes CameraType);
  void SetDigCntsPerPEGains(double fDigCntsPerPEHiGain);
  void SetLowGainToHighGainPeakRatio(double LowGainToHighGainPeakRatio) 
         {fLowGainToHighGainPeakRatio = LowGainToHighGainPeakRatio; return;};
  void makeFADCTrace(KSWaveForm* pfWaveForm, int waveFormStartIndex,
		     int traceLengthBins, bool EnableHiLoGainProcessing, 
		     double FADCTracePed, double lowGainPedestalFADC);
  double getWindowArea(int fStartTraceIndex, int fNumBinsToSum);
  void   Print(int fStartTraceIndex, int fNumBinsToPrint);
  bool   generateTraceSamples(double& traceArea, KSWaveForm* pWaveForm, 
			      int waveFormStartIndex, 
			      int numSamplesTrace, double pedestalFADC);
  bool   clipTrace();
  int    getFADCTraceMax();
  std::vector<int> fFADCTrace;
  int    fFADCLoTraceStart;
  double fDigCntsPerPEHiGain;
   double fDigCntsPerPELoGain;   //not used anymore
  double fFADCTracePed;
  double fLowGainPedestal;
  bool   fIsLowGainTrace;

  //Lo Gain Clipping study variables
  double fPixelHiSizeBeforeLoGainConversion; //inlcudes PedSum
  int    fPixelLoSizeBeforeLoGainClip;
  int    fPixelLoSizeAfterLoGainClip;
};  

#endif
