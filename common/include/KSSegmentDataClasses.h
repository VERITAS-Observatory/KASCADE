/**
 * Original Author: Glenn H. Sembroski
 * $Author$
 * $Date$
 * $Revision$
 * $Tag$
 *
 **/
// This is derived from structures.f90 and kascade.h  
// This is preliminary class which I plan to upgrade all of KASCADE to 
// eventually. Not used by general KASCADE as of yet.


#ifndef KSSEGMENTDATACLASSES_H
#define KSSEGMENTDATACLASSES_H

#include <string>

class KSSegmentHeadData
// *******************************************************
// ** SEGMENT HEADER STRUCTURE **
// *******************************************************
{
 public:
  KSSegmentHeadData();
  virtual ~KSSegmentHeadData();
  void PrintSegmentHead();

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
  char   fEarthsMagneticFieldSpec[10];   // Earths magnetic field spec:
                                         // F=none.W=W10m
  int    fFunctionEnableFlags[3];  //Enable(=1) flags for kascade functions:
                                   // [0]= Earths Magnetic field
                                   // [1]= Ionization
                                   // [2]= Multiple Scattering



  //char   fVersion[80];	          //Version of kascade that made this file
  
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
