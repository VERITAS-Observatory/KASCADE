/**
 * Original Author: Glenn H. Sembroski
 * $Author$
 * $Date$
 * $Revision$
 * $Tag$
 *
 **/
// This is a class that holds and manipulates a single pixels waveform.
// Can be used for holding low gain templates.

#ifndef KSWAVEFORMBASIC_H
#define KSWAVEFORMBASIC_H

#include <vector>
#include <iostream>
#include <iomanip>
using namespace std;

#include "KSCommon.h"

class KSWaveFormBasic 
// *******************************************************
// ** WaveForm class for KASACADE (also good for templates)
// *******************************************************
{
 public:
  KSWaveFormBasic();
  virtual ~KSWaveFormBasic();
  double GetWaveFormSum();
  double GetWaveFormSum(int i, int j);
  double GetWaveFormMax();
  double GetWaveFormFWHMns();
  double GetWaveFormElement(int Index){return fWaveForm.at(Index);};
  void   SetWaveFormElement(int Index, double Value)
                                       {fWaveForm.at(Index) = Value; return;};
  void   ScaleWaveForm(double scaleFactor);

  double GetLinearity(){return  fLinearity;};

  vector < double > fWaveForm;

  // Special for templates
  double fSize;             //Mean value this template is good for
  double fLinearity;
  double fUpperHighGainArea;//Upper value of high gain area that this
                           // template is good for, lower is just upper
                           // of previous template
  double fUpperLinearity;  //Interpolated upper linearity at fUpperHighGainArea
  double fStartBinNS;

};
// ***************************************************************************
#endif
