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


#ifndef KSROOTSEGMENTDATACLASSES_H
#define KSROOTSEGMENTDATACLASSES_H




#include <string>
#include "KSSegmentDataClasses.h"


#ifndef _NOROOT
  #include "TObject.h"
#endif


#ifdef _NOROOT
  class KSRootSegmentHeadData
#else
  class KSRootSegmentHeadData : public TObject
#endif
// *******************************************************
// ** SEGMENT HEADER STRUCTURE **
// *******************************************************
{
 public:
  KSRootSegmentHeadData();
  KSRootSegmentHeadData(KSSegmentHeadData* pfSegmentHead);

  virtual ~KSRootSegmentHeadData();
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
  char   fEarthsMagneticFieldSpec[10];// Earths magnetic field spec:
                                       //F=none.W=W10m
  char   fVersion[80];	          //Version of kascade that made this file
#ifndef _NOROOT
  ClassDef(KSRootSegmentHeadData,1);
#endif

};
// ***************************************************************************

#ifdef _NOROOT
   class KSRootSegmentData
#else
   class KSRootSegmentData : public TObject
#endif

// *******************************************************
// class to hold single track segment
// *******************************************************
{
 public:
  KSRootSegmentData();
  virtual ~KSRootSegmentData();
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
#ifndef _NOROOT
  ClassDef(KSRootSegmentData,1);
#endif
};
#endif
