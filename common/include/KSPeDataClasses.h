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


#include <string>
#include "KSSegmentDataClasses.h"



class KSPeHeadData
// *******************************************************
// ** PE HEADER STRUCTURE **
// *******************************************************
{
 public:
  KSPeHeadData();
  virtual ~KSPeHeadData();
  void PrintPeHead();

  double fXAreaWidthM;	// Grid width in x direction in meters.
  double fYAreaWidthM;	// Grid width in y direction in merters.
  double fXCoreOffsetM; //X,Y coord of core in area 0,0
  double fYCoreOffsetM;
  double fEfficiency;
  bool   fWhippleMount;
  bool   fVeritasMount;
  bool   fTriangularGrid;
  bool   fSquareGrid;
  bool   fNorthSouthGrid;
  bool   fRandomCoreOffset; 
  bool   fWhipplePMTs;
  bool   fVeritasPMTs;
  bool   fADPPMTs;
  //  char   fVersion[10];	      //Version of kaslite that made this file.
};
// ***************************************************************************




class KSPeData
// *******************************************************
// class to hold single pe's
// *******************************************************
{
 public:
  KSPeData();
  virtual ~KSPeData();
  void PrintPe();

  int    fNx;              // Grid coords (x,y)
  int    fNy;
  double fTime;            // Time of hit
  double fPhotonDl;      // dl of photon 
  double fPhotonDm;      // dm of photon(dn can be recreated)
  int    fSegmentID;       // Segment identifier.
  double fX;               // X position of hit in grid rectangle.
  double fY;               // Y position of hit in grid rectangle.
  int    fTrackType;       // Type of emitting particle.
  int    fLambda;          // Wavelength of emmited photon(nm)
  double fEmissionAltitude;// emmison altitude
};
#endif
