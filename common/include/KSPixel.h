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




class KSPixel
// *******************************************************
// ** Pixel class for KASACADE
// *******************************************************
{
 public:
  KSPixel();
  KSPixel(KSCameraTypes CameraType);
  virtual ~KSPixel();
  void InitPixel();
  void InitWaveForm(double fWaveFormStart,double fWaveFormLength);
  void BuildPeWaveForm();
  void AddNoiseToWaveForm(bool fAfterPulse);
  void DetermineNoisePedestals();
  void RemoveNightSkyPedestalFromWaveForm();
  void AddPedestalToWaveForm(double fWaveFormPedestal);
  double  GetCharge(double fTriggerTimeNS);
  void PrintWaveForm(int nx, int ny, int seqNum,double time);

 private:
  void addPe(double fPeTime,bool fAfterPulse);
  float fXDummy;
  KSCameraTypes fCameraType;
  
 public:
  KSSinglePe fSinglePe;
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
  int    fNumWaveFormBins;
  double fWaveFormNightSkyPedestal;
  double fPedPE;
  double fChargeVarPE;  //Whipple:units of Pes.
  double fPedDC;
  double fChargeVarDC;  //Veritas:units of FADC DC

  double fCFDTriggerTimeNS;


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
