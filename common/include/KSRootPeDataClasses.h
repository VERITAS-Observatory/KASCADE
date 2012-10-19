/**
 * Original Author: Glenn H. Sembroski
 * $Author$
 * $Date$
 * $Revision$
 * $Tag$
 *
 **/
// This is derived from structures.f90. This is the ROOT usable version
// It duplicates (except fdor the root stuff) KSRootPeDataClasses  
// This is preliminary class which I plan to upgrade all of KASCADE to 
// eventually. Not used by general KASCADE as of yet.


#ifndef KSROOTPEDATACLASSES_H
#define KSROOTPEDATACLASSES_H


#include <string>
#include "KSRootSegmentDataClasses.h"
#include "KSSegmentDataClasses.h"
#include "KSPeDataClasses.h"

#ifndef _NOROOT
#include "TObject.h"
#endif


#ifdef _NOROOT
    class KSRootPeHeadData
#else
    class KSRootPeHeadData : public TObject
#endif

// *******************************************************
// ** PE HEADER STRUCTURE **
// *******************************************************
{
 public:
  KSRootPeHeadData();
  KSRootPeHeadData(KSPeHeadData* pfPeHead);
  virtual ~KSRootPeHeadData();
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
  bool   fUpgradePMTs;
#ifndef _NOROOT
  //ClassDef(KSRootPeHeadData,1);
  ClassDef(KSRootPeHeadData,2);   //Change ADP to Upgrade. ADP no longer used
#endif
};
// ***************************************************************************




#ifdef _NOROOT
    class KSRootPeData
#else
    class KSRootPeData : public TObject
#endif
// *******************************************************
// class to hold single pe's
// *******************************************************
{
 public:
  KSRootPeData();
  virtual ~KSRootPeData();
  void PrintPe();
  void CopyInPe(KSPeData* pfPe);
  void CopyOutPe(KSPeData* pfPe);

  int    fNx;              // Grid coords (x,y)
  int    fNy;
  double fTime;            // Time of hit
  double fPhotonDl;        // dl of photon 
  double fPhotonDm;        // dm of photon(dn can be recreated)
  int    fSegmentID;       // Segment identifier.
  double fX;               // X position of hit in grid rectangle.
  double fY;               // Y position of hit in grid rectangle.
  int    fTrackType;       // Type of emitting particle.
  int    fLambda;          // Wavelength of emmited photon(nm)
  double fEmissionAltitude;// emmison altitude
#ifndef _NOROOT
  ClassDef(KSRootPeData,1);
#endif
};
#endif
