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

#include <cmath>    //For M_PI

enum KSCameraTypes                         {VERITAS499=0,WHIPPLE490=1};
const double gFocalLengthM[2]            = {        12.0,         7.3};
const double gFacetDiameterM[2]          = {        0.61,       0.602};
const double gMirrorRadiusSquared[2]     = {        36.0,    24.98733};
const double gSpotSizeNorthSouthDeg[2]   = {       0.025,        0.06};
const double gSpotSizeEastWestDeg[2]     = {       0.025,        0.06};

const double gLatitude[2]                = {    0.552828,    0.552978};
const double gEastLongitude[2]           = {    -1.93649,   -1.935190};

const int    gNumPixelsCamera[2]         = {         499,         490};
const int    gNumTriggerPixels[2]        = {         463,         331};
#const int    gNumLines[2]                = {          13,          11};
const double gPixelHalfSpacingMM[2]      = {       15.74,         7.5};
#const double gPixelRadiusMM[2]           = {       12.48,         5.0};

const int    gFADCNumSamples[2]          = {          24,          10};
const double gFADCDigCntsPerPEHiGain[2]  = {        5.47,         4.2};
const double gFADCDigCntsPerPELowGain[2] = {        .911,         0.0};
const int    gFADCPedestalDCPerBin[2]    = {          20,           2};
const double gFADCTOffsetNS[2]           = {         8.0,         8.0};

const double gCFDDelayNS[2]              = {         1.5,         4.0};
const double gCFDTriggerDelayNS[2]       = {         2.5,         2.5};

//WHIPPLE490 Outer rings: 3 rings, 37 pmts/ring in a circle no lightcones
const double g490OuterRingDiameterM[3]= {0.3503422, 0.401752, 0.450367};
const double g490OuterPixelRadiusMM  = 12.5;
const double g490OuterRingsAngularStepDeg=360.0/37.0;//Angular step size deg 
const double gPulseHeightWidthFactor = 0.135;   //Used in pulseheight modeling

const double gRad2Deg=180./M_PI;
const double gDeg2Rad=M_PI/180.;
const double gWhip490RotRad=-7.306*gDeg2Rad; //Whipple 490 camera is rotated 
                                             //by this much

const double gOverflowTime=1000001.;

const int    gNightSkyWaveFormNS=1000;   //1 microsec
const double gWaveFormBinSizeNS=.25;     // Bin size in ns


const double gFADCDelayNS           =-8.0;
const double gFADCBinSizeNS         = 2.0;
const int    gFADCHiLoGainThreshold = 250;
const int    gFADCLowGainDelayBins  =   4;

const double gTrigMultiplicityWidthNS = 10.0;  // WHIPPLE490 CFD Pulse width

#endif
