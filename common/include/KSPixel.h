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
#include <iostream>
#include <iomanip>
using namespace std;


#include "KSSinglePe.h"
#include "KSWaveForm.h"
#include "KSFADC.h"
#include "KSCommon.h"

#include <TRandom3.h>


class KSPixel
// *******************************************************
// ** Pixel class for KASACADE
// *******************************************************
{
 public:
  KSPixel(KSCameraTypes CameraType, double DigCntPerPEHiGain, 
		  double SinglePeRiseTimeNS, double SinglePeFallTimeNS,
		  double lowGainToHighGainPeakRatio);
  virtual ~KSPixel();
  void InitPixelWaveForm();

  // Wave form wrappers. All handled within KSWaveForm class.
  void InitWaveForm(double WaveFormStartNS,double WaveFormLengthNS);
  void BuildPeWaveForm();
  int  AddNoiseToWaveForm(bool afterPulse);
  void RemovePedestalFromWaveForm(double waveFormPedestal);
  void AddPedestalToWaveForm(double waveFormPedestal);
  double GetWaveFormElement(int Index);
  int  GetNumberWaveFormElement(){return fNumWaveFormBins;};
  void DetermineNoisePedestals();

  double  GetCharge(double triggerTimeNS, bool pedestalEvent);
  double  GetCharge(double triggerTimeNS)
                                     {return GetCharge(triggerTimeNS,false);};


  void PrintWaveForm(int nx, int ny, int seqNum,double time);
  void PrintPulseHeightsOfLightPulse();




 private:
  //void addPe(double fPeTime,bool afterPulse);
  double getMeanFADCArea(KSCameraTypes fCameraType, double HVGain,
						 int numPesInPulse, double timeSpread, 
						 int startFADCBin, int endFADCBin);
  double getLowGainMeanFADCArea(KSCameraTypes fCameraType,  int templateIndex,
								int numPesInPulse, 
								double timeSpread, int startFADCBin, 
								int endFADCBin);

  float fXDummy;
  KSCameraTypes fCameraType;
  TRandom3* pfRandom;
  double fLowGainToHighGainPeakRatio; 
 public:
  KSSinglePe* pfSinglePe;
  KSFADC fFADC;

  int    fID;
  double fHalfSpacingDeg;
  double fRadiusDeg;
  double fXDeg;
  double fYDeg;
  vector<int> fAdjacentPixels;

  double fBaseEfficiency;     //Efficency from config file
  double fEfficiency;         //Overall efficency to see pes (0.0 <--> 1.0 )
  double fThreshold;          //Disc trigger threshold (pes)      
  double fDiscNoise;          //Ave # pes in effective disc gate from sky
  double fNoiseRatePerNS;

  int    fDisc;               //Counts hits.
  double fDiscPulseHeight;
  bool   fDiscTrigger;        //This pixels fires

  // Timing stuff
  vector<double> fTimePe;

  //WaveForm stuff
  KSWaveForm* pfWaveForm;
  double fWaveFormStartNS;
  double fWaveFormLengthNS;
  int    fNumWaveFormBins;
  double fWaveFormNightSkyPedestal;
  double fPedPE;
  double fChargeVarPE;  //Whipple:units of Pes.
  double fPedDC;
  double fChargeVarDC;  //Veritas:units of FADC DC

  vector< double> fCFDTriggerTimeNS;


  double fRelativeGain;
  double fPedVarRel;
  bool   fBadPixel; 

  //Single pe stuf
  double fSinglePeArea; //Area of our single pe waveform pulse. NOT in DC!

  double fSinglePeMeanFADCAreaNoRounding;
  double fSinglePeMeanFADCAreaNoRounding7Bins;
  double fSinglePeMeanFADCAreaNoRounding16Bins;
  double fSinglePeMeanFADCArea;
  double fSinglePeMeanFADCArea7Bins;
  double fSinglePeMeanFADCArea16Bins;
  double fLowGainTmplt0FADCAreaNoRounding;
  double fLowGainTmplt0FADCAreaNoRounding7Bins;
  double fLowGainTmplt0FADCAreaNoRounding16Bins;
  double fSinglePeSizeNS;
  int    fSinglePeSizeNumBins;
};
// ***************************************************************************
#endif
