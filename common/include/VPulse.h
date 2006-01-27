#ifndef VPULSE_H
#define VPULSE_H

//: A class for building pmt waveforms and modeling FADC and CFD response

/*
  This class defines and fills an array that models the output of a PMT.
  You can fill the pulse with noise and/or add single Monte-Carlo generated
  pes pulses. Pulse height is chosen from a measured pulse heigt distribution.
  Single pulse shape is measured pulse shape. Both Pulse shape and pulse 
  height distribution may need improvment. Also model FADC and CFD response to
  waveform and 
*/

//#include <blitz/array.h>
//using namespace blitz;
#include "Pulse_defs.h"         // Various internal constants defined here.
#include "words.h"
#include <cstring>
#include <fstream>
#include <iostream>

using namespace std;

class VSinglePe
{
  float fSize;       //ns
  float fArea;   
 public:
  float* fSingPulse;
  VSinglePe();
  VSinglePe(float riseTime);
  ~VSinglePe(){};
  void setRiseTime(float riseTime);
  float getSize(){return fSize;};
  float getArea(){return fArea;};
};  

class VPulse
{
  int fNbins;                  // Number of bins in pulse array
  float fSinglePeSizeNs;
  int fSinglePeSizeBins;
  float fSinglePeArea;
  float fPedestal;            //pedestal /channel
  bool fFadcHiLo; // fadc hilow flag.
  bool fFadcHit;   // fadc hit flag
  VSinglePe fSinglePe;
  //  Array<float,1> fPulse;
  //  Array<float,1> fDpulse;
  //  Array<float,1> fMainPulse;
  //  Array<unsigned char,1> fFadcPulse;   // Allocate the fadc array.
  float* fPulse;               //pe pulse array. Length declared in constructer
  float* fDpulse;
  float* fMainPulse;
  float* fNPulse;
  float* fNFadc;
  unsigned char* fFadcPulse;   // The fadc array.
  word32 fFadcSamples[kFadcSamples];

 public:

  VPulse();                    //Creates waveform of unit length(to be exteded
                               //later with method setLength
  ~VPulse(){};
  void setLength(float lenNs); //Resize array to length lenNs
  void setSinglePeRiseTime(const float riseTime);
  void InitPulse();                  // Init to zero.
  void InitFadc();                  // Init to zero.
  void Noise(float peRateNs, bool aftrPls);
                                // Fills up waveform array with noise pes.
  int NoisePedestal(float peRateNs, int Bins);
                                  //Create a noise pulse waveform.
                                  //Convert to fadc samples.(Bins of them) 
                                  //Return average summed Fadc value for 
                                  //kFadcSamples.
  void AddPe(float timeNs, bool aftrPls); 
                                // Adds a pepulse to the waveform array at time
  float getArea(float singleArea);    // Returns area in units of pes.

  void  Fadc(float startTime);  // Converts pulse to fadc bin sampling.
  unsigned short  getFadcArea();  //gets area under fadc curve.
  float getFadcArea(float singleArea);//sums FADC values to get charge. Any 
                                      //existing pedestal is included. 
                                      //Hi/lo not converted

  bool getFadcHiLo(){return fFadcHiLo;};
  bool getFadcHit(){return fFadcHit;};

  float Cfd(float threshold,float start); 
                                //Find CFD trigger time in pulse.
  float Vcfd(float threshold,float start, float offset); 
                               //Find VERITAS CFD trigger time in fPulse.
  float FindPedestal();        //Find present asvearge value of pulse/bin
  void RemovePedestal(float ped);  //Subtracts ped value from waveform bin by 
                                //bin.Mimics AC coupling.Keep track of total 
                                //removed pedestal in fPedestal
  void AddPedestal(float ped);  //Add in a pedestal offset 

  float GetPedestal(){return fPedestal;}; // Returns back amount taked away by
                                          // RemovePedestal calls.
  float getSinglePeSizeNs();
  float getSinglePeArea(){return fSinglePeArea;};
  word32* getFadcSamples(){return fFadcSamples;};
  word32 getFadcSample(int index){return fFadcSamples[index];};
  word32 getNumFadcSamples(){return kFadcSamples;};
  void PrintfPulse(float startTime);
  void PrintfDpulse();
  void PrintfMainPulse();
  void PrintFadc();
};
#endif //VPULSE_H

