/**
 * Original Author: Glenn H. Sembroski
 * $Author$
 * $Date$
 * $Revision$
 * $Tag$
 *
 **/
// This is derived from structures.f90  
// This is preliminary class which I plan to upgrade all of KASCADE to 
// eventually. Not used by general KASCADE as of yet.


#ifndef KSPEDATACLASSES_H
#define KSPEDATACLASSES_H

#include <TObject.h>
#include <string>
#include "KSSegmentDataClasses.h"

class KSPeHeadData : public TObject 
// *******************************************************
// ** PE HEADER STRUCTURE **
// *******************************************************
{
 public:
  KSPeHeadData();
  virtual ~KSPeHeadData();

  KSSegmentHeadData fSegmentHead;
  double dl;		// Mount direction(can recreate dnm)
  double dm;		// Mount direction(can recreate dnm)
  double fXSeg;	        // Grid width in x direction.
  double fYSeg;	        // Grid width in y direction.
  double fHeightOfObs;	// Observation altitude in meters.
  double fXOffset;      //X,Y coord of core in area 0,0
  double fYOffset;
  std::string fPEType;  //'VI'=visable photons,'VT' Veritas triangular array.
  std::string fVersion;	//Version of kaslite that made this file.
  //ClassDef("KSPeHeadData",1);
};
// ***************************************************************************



class KSPeData : public TObject
// *******************************************************
// class to hold single pe's
// *******************************************************
{
 public:
  KSPeData();
  virtual ~KSPeData();

  int nx;       //Grid coords (x,y)
  int ny;
  double time;  // Time of hit
  double dlr;   // dl of photon relative to mount
  double dmr;   // dm of photon(dn can be recreated)
  int nnext;    // Segment identifier.
  double xm;    //X position of hit in grid rectangle.
  double ym;    //Y position of hit in grid rectangle.
  int spec;     //Type of emitting particle.
  int lambda;   //Wavelength of emmited photon(nm)
  double em_alt;//emmison altitude
  //ClassDef("KSPeData",1);
};
#endif
