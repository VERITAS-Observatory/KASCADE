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

#include <TObject.h>
#include <string>

class KSSegmentHeadData : public TObject 
// *******************************************************
// ** SEGMENT HEADER STRUCTURE **
// *******************************************************
{
 public:
  KSSegmentHeadData();
  virtual ~KSSegmentHeadData();
  int fType;                     // Primary particle type.
  double fTEP;		         // Primary particle energy(TeV.)
  double fDli;		         // Primary particle direction(can recreate dn)
  double fDmi;		
  double fEnergyThreshold;       // Threshold level for gammas,e and mu.
  double fInjectionDepth;        // Injection depth in gm/cm**2
  double fObservationAltitude;   // Observation altitude in meters.
  double fXInitial;	         // Origin of shower.
  double fYInitial;	
  int  fIDFile;	                 // File id # for this shower.
  std::string fMagneticFieldSpec;// Earths magnetic field spec:F=none.W=W10m
  std::string version;	          //Version of kascade that made this file
  
  //ClassDef("KSSegmentHeadData",1);
};
// ***************************************************************************



class KSSegmentData : public TObject
// *******************************************************
// class to hold single track segment
// *******************************************************
{
 public:
  KSSegmentData();
  virtual ~KSSegmentData();

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

  //ClassDef("KSSegmentData",1);
};
#endif
