/**
 * Original Author: Glenn H. Sembroski
 * $Author$
 * $Date$
 * $Revision$
 * $Tag$
 *
 **/
// This is derived from structures.f90 and kascade.h  


#ifndef KSSEGMENTDATACLASSES_H
#define KSSEGMENTDATACLASSES_H

#include <string>
#include <iostream>

//#include <cstdlib> // I tried to include this (for the exit90 function) but
                     // it screws up the transform call (specfically the 
                     // toupper resolution during compile
//using namespace std; // This is the actual cause of the problem>Beats me why

const int kNumCharAtmSpec=120;
const int kNumCharMagFldSpec=15;


class KSSegmentHeadData
// *******************************************************
// ** SEGMENT HEADER STRUCTURE **
// *******************************************************
{
 public:
  KSSegmentHeadData();
  virtual ~KSSegmentHeadData();

  int fType;                     // Primary particle type.
  double fGeVEnergyPrimary;	 // Primary particle energy(TeV.)
  double fDlInitial;		 // Primary particle direction cosigns
  double fDmInitial;		 
  double fDnInitial;             // (can recreate dn)
  double fEnergyThresholdMeV;       // Threshold level for gammas,e and mu.
  double fMaxCoulombScatSegmentLength; // maximum length of segment for 
                                       // multiple Coulomb scattering(gm/cm**2)
  double fInjectionDepth;        // Injection depth in gm/cm**2
  double fObservationAltitudeM;   // Observation altitude in meters
  int    fShowerID;	                 // File id # for this shower.
                                  // Earths magnetic field spec:
  char   fEarthsMagneticFieldSpec[kNumCharMagFldSpec];  
                                                         // F=none.W=W10m
  int    fFunctionEnableFlags[3];  //Enable(=1) flags for kascade functions:
                                   // [0]= Earths Magnetic field
                                   // [1]= Ionization
                                   // [2]= Multiple Scattering
  char   fAtmosphereSpecification[kNumCharAtmSpec];


  //char   fVersion[80];	          //Version of kascade that made this file
  void PrintSegmentHead();
  bool setAtmosphereSpecification(std::string atmSpec);
  std::string getAtmosphereSpecification();
  
};
// ***************************************************************************


class KSSegmentData
// *******************************************************
// class to hold single track segment
// *******************************************************
{
 public:
  KSSegmentData();
  virtual ~KSSegmentData();
  void PrintSegment();

  double fXStart;  //Initial xy(meters),z of segment.
  double fYStart;
  double fHStart;
  double fDlStart; //Initial x,y direction cosigns of segment
  double fDmStart;
  double fEndTime;//relative time(ns) at end of segment.
  double fHEnd;   //final altitude of segment
  double fDlEnd;  //final direction cosigns of segment.
  double fDmEnd;
  double fGamma;  //gamma at middle of segment.
  int nspec;      //particle type
};
#endif
