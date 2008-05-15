/**
 * Original Author: Glenn H. Sembroski
 * $Author$
 * $Date$
 * $Revision$
 * $Tag$
 *
 **/
// ************************************************************************
// This is a class that holds  the conversion from kastriggermain.f90 to 
// C++stuff 
// ************************************************************************

#ifndef KSTILTANDTRACE_H
#define KSTILTANDTRACE_H


#include <iostream>
#include <fstream>
#include <vector>

const double kCLightMpS=2.99792e+8;
const double kCLightMpNS= kCLightMpS/1.e9;
 
// *******************************************************
class KSTiltAndTrace
{
 private:
  double fDnMinTight;               //Direciton cosign to z axis tight limit
  double fDnMinLoose;               //Direciton cosign to z axis loose limit
  double fMirrorRadiusSquaredM2;
  double fFacetDiameterM;
  double fFocalLengthM;
  double fJitterWidthEWRad;
  double fJitterWidthNSRad;
  double fMetersPerDeg;

  double fPe[3];  //Position of photon in mirror plane system (Meters)
  double fDir[3]; //Direction of photon in mirror plane system
  
  double fFacet[3]; //Position of facet origen in mirror plane system (m)
  double fPeFacit[3];  //position on facet of where photon hits it
  double fToFocalLength[3];//optical axis to focal plane in mirror plane system
  double fFacetNormal[3];  //Normal vector from center of facet.
  double fFacetNormalDir[3];  //Direction cosigns of facet normal vector.
  double fPh[3]; //Little vector from focal plane to facet focal point
  double fPeFacetNormalDir[3]; //Normal dir of curved facet mirror at point 
                              //photon hits
  double fParallel[3];
  double fPerpendicular[3];
  double fReflectedDir[3];

  float dummy;

public:
  KSTiltAndTrace(double DnMinTight,double DnMinLoose,
		 double MirrorRadiusSquaredM2,double FacetDiameterM,
		 double FocalLengthM, double JitterWidthEWDeg,
		 double JitterWidthNSDeg,double MetersPerDeg);
  virtual ~KSTiltAndTrace();
  int Tilt();
  void FullAberationTrace();

  double fDlr;   //Inital Photon direction cosigns.
  double fDmr; 
  double fDnr; 

  double fTDlm;    //Telescope pointing direction cosigns (Mirror plane normal)
  double fTDmm; 
  double fTDnm;

  double fXg;     //Telescope position on the ground(meters) 
  double fYg; 

  double fXDl;    //X vector of camera(focal) plane for this mount direction
  double fXDm; 
  double fXDn; 

  double fYDl;    //Y vector of camera (focal) plane for this mount direction 
  double fYDm; 
  double fYDn; 

  double fTime;        //Photon impact time on ground (NS)
  double fPeTime;      //Photon time when it impacts camera (focal) plane.
  double fPeTimeTilt;  //Photon time when it goes through mirror plane

  double fW[3];   //Position of photon in camera plane (Deg)

};
// ***************************************************************************


#endif
