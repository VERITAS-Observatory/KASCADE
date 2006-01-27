#ifndef PULSE_DEFS_H
#define PULSE_DEFS_H
//! Constant definitions used internally by the pes pulse waveform routines.
/*
  Constant definitions used internally by the pes pulse waveform routines.
*/

const float kPulseBinSize=.25;        //ns
const int kFadcSampleBins=int(1./kPulseBinSize);  //This model; Average over
                                                    // 1 ns
//const float kPedOffset=20.;    //Added Pedestal.(To keep neg pedvar swings.)
const float kPedOffset=15.;       //As of 30/06/03 JB Rio-Rico.
//const float kPedOffset=0.;    //Added Pedestal.(To keep neg pedvar swings.)

//Fadc hit threshold:
//const float kFadcPedc=1.5;       //Gain to produce FADC DC.
const float kFadcPedc=2.0;       //As of 30/06/03 JB Rio-Rico.
//const float kHitThreshold=10.; //threshold to keep a channel(pes,without ped)
const float kHitThreshold=0.0;  
//const float kFadcHitThreshold=kHitThreshold+kPedOffset*kFadcPedc;
                                            //Threshold in DC.
const float kFadcHitThreshold=0.0;//0 means no zero supression:all channels hit

//  float kSinglePePulseRise=2.0; //ns
const float kSinglePePulseRise=3.5; //ns

//const float kNoiseRate=.293; //pes/ns
const float kNoiseRate=.117; //pes/ns
//const float kNoiseRate=.234; //pes/ns
//const float kNoiseRate=.468; //pes/ns

const float kAdcDelay=-8.0;            //ns
const float kFadcBinSize=2.0;         //ns
const long  kFadcSamples=16;
const float kFadcHiGain=1.0/6.0;  //This may be wrong.JB implies differnetly in
                                  //differrent places. May be .78/4.85=1/6.38

const float kCfdWidth=10.; //output pulse width/deadtime.
const float kVcfdWidth=10.; //output pulse width/deadtime.

const float kCfdDelay=4.0;  //ns
//const float kCfdGain=1.51; // (1/.66)
//const float kCfdDelay=2.0;  //ns
//const float kCfdGain=1.1; 
const float kCfdGain=1./.33; 

const float kVcfdOffset=0.0;
//const float kVcfdDelay=3.0;  //ns

//2/5 frac setup:
/*
const float kVcfdGain=2.5; // (5/2)=(1+(2/3))*(3/2):fraction=2/5=.4
const float kVcfdDelay=3.0;  //ns
const float kVcfdTrigDelay=2.5; //ns
*/

//.66 frac
const float kVcfdGain=1.51; // frac=.66
const float kVcfdTrigDelay=2.5; //ns
const float kVcfdDelay=1.5;  //ns

//VVV study frac=1-t1/7
/*const float kVcfdDelay=1.0;  //ns
const float kVcfdGain=1./(1.-kVcfdDelay/7.);
const float kVcfdTrigDelay=.5; //ns
*/
const int   kBinRatio=int(kFadcBinSize/kPulseBinSize);
const int   kCfdDelayBins=int(kCfdDelay/kPulseBinSize);
const int   kVcfdDelayBins=int(kVcfdDelay/kPulseBinSize);
const int   kVcfdTrigDelayBins=int(kVcfdTrigDelay/kPulseBinSize);

#endif //PULSE_DEFS_H

