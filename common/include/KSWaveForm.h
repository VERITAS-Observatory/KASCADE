/**
 * Original Author: Glenn H. Sembroski
 * $Author$
 * $Date$
 * $Revision$
 * $Tag$
 *
 **/
// This is a class that holds and manipulates a single pixels waveform from
// the pmt-preamp into the cfd and FADC

#ifndef KSWAVEFORM_H
#define KSWAVEFORM_H

#include <string>
#include <vector>
#include <iostream>
#include <iomanip>
using namespace std;

#include "KSSinglePe.h"
#include "KSCommon.h"
#include "KSWaveFormBasic.h"
#include <TRandom3.h>

class KSWaveForm : public KSWaveFormBasic
// *******************************************************
// ** WaveForm class for KASACADE (also good for templates)
// *******************************************************
{
 public:
  KSWaveForm(KSCameraTypes CameraType, KSSinglePe* PSinglePe);
  virtual ~KSWaveForm();
  int  InitWaveForm(double WaveFormStartNS,double WaveFormLengthNS);
  int  BuildPeWaveForm(vector<double>& TimePe, double Efficiency);
  int  AddNoiseToWaveForm(bool AfterPulse, double NoiseRatePerNS);
			 
  void RemovePedestalFromWaveForm(double WaveFormPedestal);
  void AddPedestalToWaveForm(double WaveFormPedestal);
  void PrintWaveForm();
  int    GetNumWaveFormBins(){return fNumWaveFormBins;};
  void   BuildLowGainWaveForm(double highGainChargeDC);
  int    GetLowGainIndex(){return fLowGainIndex;};
  int    GetPECount(){return fPECount;};
  int    GetNumPES(){return (int)fWaveFormPETimesNS.size();};
  bool   fDebug;

 private:
  void addPe(double PETimeNS,bool AfterPulse, double lowGainPePulseHeight=1.0);

  KSCameraTypes fCameraType;
  TRandom3*     pfRandom;
  KSSinglePe*   pfSinglePe;

  vector < double >* pfSelectedWaveForm;

  bool fWaveFormInitalized;
  //vector < double > fWaveForm;

  //Hi Gain WaveForm stuff
  double fWaveFormStartNS;
  double fWaveFormLengthNS;
  int    fNumWaveFormBins;
  int    fPECount;

  //Low Gain waveform stuff
  vector < double > fWaveFormPETimesNS;
  vector < double > fWaveFormPEPulseHeights;
  bool   fLowGainMode;
  int    fLowGainIndex;

  //Single pe stuf
  double fSinglePeArea;
  double fSinglePeSizeNS;
  int    fSinglePeSizeNumBins;
};
// ***************************************************************************
#endif
