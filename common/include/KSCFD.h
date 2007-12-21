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
#include <vector>
#include <fstream>
#include <iostream>

class KSCFD
{
 private:
  KSCameraTypes fCameraType;
  int fNumCFDDelayBins;
  double fCFDFraction;
  double fCFDOffsetPE;
  int fCFDTriggerDelayBins;
  int fLastThresholdCheckBin;
  int fLastCFDCrossingCheckBin;

  std::vector<double> fCFDPulse;
  std::vector<double> fNegativePulse;

  bool aboveThresholdAboveOffset(KSPixel& fPixel,
					 int& offsetCrossingBin);
  bool findAboveThreshold(KSPixel& fPixel, int& thresholdCrossingBin);

  void makeInternalCFDWaveForm(KSPixel& fPixel, bool printWaveForms);
  bool findNextOffsetCrossing(int& thresholdCrossingBin);
  std::ofstream fMyFile;
 public:
  KSCFD(KSCameraTypes CameraType);
  ~KSCFD();

  bool isFired(KSPixel& fPixel, double fStartTimeOffsetNS, 
	       double fLastTimeOffsetNS,double fLastCFDCrossingOffsetNS,
	       int nx, int ny);
  void PrintWaveForm(int pixelID, int nx, int ny, int seqNum, 
		     double time, std::vector<double>& waveForm,
		     double waveFormStartNS);

};  

#endif
