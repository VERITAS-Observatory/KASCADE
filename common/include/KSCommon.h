/**
 * Original Author: Glenn H. Sembroski
 * $Author$
 * $Date$
 * $Revision$
 * $Tag$
 *
 **/
// This is the common specifications header for ksTrigger and ksAomega

#ifndef KSCOMMON_H
#define KSCOMMON_H

#include <TMath.h>    //For PI

enum KSCameraTypes                         {VERITAS499=0,WHIPPLE490=1};
const double gFocalLengthM[2]            = {        12.0,         7.3};
const double gFacetDiameterM[2]          = {        0.61,       0.602};
const double gMirrorRadiusSquared[2]     = {        36.0,    24.98733};
const double gPSFNorthSouthDeg[2]        = {      0.0190,        0.06};
const double gPSFEastWestDeg[2]          = {      0.0190,        0.06};
//Veritas PSF is .09Deg@80% for gPSFNorthSouthDeg andgPSFEastWestDeg =.0190 
//As Veritas is .09 as of April 2009
//Veritas PSF is .13(?)Deg@80% for gPSFNorthSouthDeg andgPSFEastWestDeg =.0330 

const double gLatitude[2]                = {    0.552828,    0.552978};
const double gEastLongitude[2]           = {    -1.93649,   -1.935190};
const int    gNumChannelsCamera[2]       = {         500,         492};
const int    gNumPixelsCamera[2]         = {         499,         490};
const int    gNumTriggerPixels[2]        = {         463,         331};
const int    gNumImagePixels[2]          = {         499,         379};
const int    gNumLines[2]                = {          13,          11};

//const double gPixelHalfSpacingMM[2]      = {       15.74,         7.5};
//Full spacing is 31.4mm Changed 10/10/12
const double gPixelHalfSpacingMM[2]      = {       15.7,         7.5};
const double gPixelActiveCathRadiusMM[2] = {       12.00,         5.0};

const double gPedestal[2]                = {        14.5,         2.0};
const int    gNightSkyWaveFormNS[2]      = {        1000,        3000};
const double gElectronicNoiseSigmaPe[2]  = {         .28,         0.0};
//const double gElectFADCNoiseSigmaDC[2]   = {         .49,         0.0};
const double gElectFADCNoiseSigmaDC[2]   = {         .35,         0.0};

//pre OCT 2009
//const int    gFADCNumSamples[2]          = {          24,          10};

//Oct 2009:
const int    gFADCNumSamples[2]          = {          20,          10};

const int    gFADCWinSize[2]             = {           7,          10};
const double gFADCDigCntsPerPEHiGain[2]  = {        1.61,         0.0};
const double gRealDataSinglePeDC[2]      = {        5.00,        10.0};
//Amount to back up from the image trigger time to get the whole pulse for 
//charge integeration. 
const double gFADCTOffsetNS[2]           = {        10.0,          9.0};


//Oct 2012 Upgrade: Start pulse at sample 3(skip 2 more bins)2ns/bin
const double gFADCWindowOffsetNS[2]      = {         4.0,         8.0};

//Oct 2009 Start pulse at sample 4(skip 3 more bins)2ns/bin
//const double gFADCWindowOffsetNS[2]      = {         6.0,         8.0};
//Pre Oct 2009 Start pulse at sample 5(skip  4 more bins)2ns/bin
//const double gFADCWindowOffsetNS[2]      = {         8.0,         8.0};
//Start pulse at sample 6(skip 5 more bins)2ns/bin
//const double gFADCWindowOffsetNS[2]      = {        10.0,         8.0};


const double gFADCChargeOffsetNS[2]      = {        14.0,         0.0};

//const double gFADCTOffsetNS[2]           = {        9.0,         9.0};
//const double gFADCWindowOffsetNS[2]      = {        9.0,         8.0};

//                                        
const double gCFDFraction[2]             = {         0.4,        0.33};
const double gCFDDelayNS[2]              = {         2.5,         4.0};
const double gCFDTriggerDelayNS[2]       = {         1.5,         0.0};
const double gCFDOffsetPE[2]             = {         1.0,         0.0};
const double gPSTPulseWidthNS[2]         = {        10.0,        10.0}; 
//Strobe delay after mult trigger in PST to latch pattern address
const double gPSTStrobeDelayNS           = 6.0;

// ***********************************************************************
//Whipple ECL Pulses to PST's from CFD's measured by GHS in 09/01
//Whipple measured
//const double gSinglePeRiseTimeNS[2]      = {         2.0,         2.0};
//const double gSinglePeFallTimeNS[2]      = {         6.0,         8.5};

// ***********************************************************************
//VERITAS Single Pe measured by Nepomuk Otte, Pat Moriarty, Mary Kertzman
// April 2010
// ***********************************************************************
// Veritas measured:
//const double gSinglePeRiseTimeNS[2]      = {         3.2,         2.0};
//const double gSinglePeFallTimeNS[2]      = {         8.5,         8.5};
const double gSinglePeRiseTimeNS[2]      = {          1.7,         2.0};
const double gSinglePeFallTimeNS[2]      = {         4.75,         8.5};

//const double gSinglePeRiseTimeNS[2]      = {         2.8,         2.0};
//const double gSinglePeFallTimeNS[2]      = {         8.0,         8.5};

//const double gSinglePeRiseTimeNS[2]      = {         3.6,         2.0};
//const double gSinglePeFallTimeNS[2]      = {         9.0,         8.5};

//const double gSinglePeFallTimeNS[2]      = {        10.0,         8.5};



//WHIPPLE490 Outer rings: 3 rings, 37 pmts/ring in a circle no lightcones
const double g490OuterRingDiameterM[3]= {0.3503422, 0.401752, 0.450367};
const double g490OuterPixelRadiusMM  = 12.5;
const double g490OuterRingsAngularStepDeg=360.0/37.0;//Angular step size deg 
const double gPulseHeightWidthFactor = 0.135;   //Used in pulseheight modeling

const double gRad2Deg=180./TMath::Pi();
const double gDeg2Rad=TMath::Pi()/180.;
//const double gWhip490RotRad=-7.306*gDeg2Rad; //Whipple 490 camera is rotated 
//                            //by this much: Handled in WhippleCams.h

const double gOverflowTime=1000001.;

const double gWaveFormBinSizeNS=.25;     // Bin size in ns

// double gFADCDelayNS           =-8.0;
const double gFADCBinSizeNS         = 2.0;
const int    gFADCHiLoGainThreshold = 250;
const int    gFADCLowGainDelayNS    =  20;
const double gFADCHiLoGainRatio     = 6.0;
const double gTrigMultiplicityWidthNS = 10.0;  // WHIPPLE490 CFD Pulse width
const int    gAbsNXAbsNYMax         =16000;    //Ranges of nx,ny

const std::string gDefaultStartOfRunTime="2007-09-11 00:02:00 UTC";
#endif
