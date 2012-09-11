/**
 * Original Author: Glenn H. Sembroski
 * $Author$
 * $Date$
 * $Revision$
 * $Tag$
 *
 **/
// This is a class that holds all the stuff needed by ksTrigger and ksAomega
// for a pixel in a camera


#ifndef KSPIXEL_H
#define KSPIXEL_H


#include <string>
#include <vector>

#include "KSSinglePe.h"
#include "KSFADC.h"
#include "KSCommon.h"

#include <TRandom3.h>


class KSPixel
// *******************************************************
// ** Pixel class for KASACADE
// *******************************************************
{
 public:
  KSPixel();
  KSPixel(KSCameraTypes CameraType, double DigCntPerPEHiGain);
  virtual ~KSPixel();
  void InitPixel();
  void InitWaveForm(double WaveFormStartNS,double WaveFormLengthNS);
  void BuildPeWaveForm();
  int  AddNoiseToWaveForm(bool afterPulse);
  void DetermineNoisePedestals();
  void RemovePedestalFromWaveForm(double waveFormPedestal);
  void AddPedestalToWaveForm(double waveFormPedestal);
  double  GetCharge(double triggerTimeNS, bool pedestalEvent);
  double  GetCharge(double triggerTimeNS)
                                     {return GetCharge(triggerTimeNS,false);};
  void PrintWaveForm(int nx, int ny, int seqNum,double time);
  void PrintPulseHeightsOfLightPulse();
 private:
  void addPe(double fPeTime,bool afterPulse);


  float fXDummy;
  KSCameraTypes fCameraType;
  TRandom3* pfRandom;

 
 public:
  KSSinglePe* pfSinglePe;
  KSFADC fFADC;

  int    fID;
  double fHalfSpacingDeg;
  double fRadiusDeg;
  double fXDeg;
  double fYDeg;
  std::vector<int> fAdjacentPixels;

  double fBaseEfficiency;     //Efficency from config file
  double fEfficiency;         //Overall efficency to see pes (0.0 <--> 1.0 )
  double fThreshold;          //Disc trigger threshold (pes)      
  double fDiscNoise;          //Ave # pes in effective disc gate from sky
  double fNoiseRatePerNS;

  int    fDisc;               //Counts hits.
  double fDiscPulseHeight;
  bool   fDiscTrigger;        //This pixels fires

  // Timing stuff
  std::vector<double> fTimePe;

  //WaveForm stuff
  std::vector<double> fWaveForm;
  double fWaveFormStartNS;
  double fWaveFormLengthNS;
  int    fNumWaveFormBins;
  double fWaveFormNightSkyPedestal;
  double fPedPE;
  double fChargeVarPE;  //Whipple:units of Pes.
  double fPedDC;
  double fChargeVarDC;  //Veritas:units of FADC DC

  std::vector< double> fCFDTriggerTimeNS;


  double fRelativeGain;
  double fPedVarRel;
  bool   fBadPixel; 

  //Single pe stuf
  double fSinglePeArea;
  double fSinglePeMeanFADCArea;
  double fSinglePeSizeNS;
  int    fSinglePeSizeNumBins;
};
// ***************************************************************************
#endif
