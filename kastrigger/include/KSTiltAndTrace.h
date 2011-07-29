/**
 * Original Author: Glenn H. Sembroski
 * $Author$
 * $Date$
 * $Revision: 1.3
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
#include <cmath>
#include <string>
#include <vector>

#include "KSFacets.h"

using namespace std;

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
  double fFocalPlaneLocationM;
  double fAlignmentPlaneLocationM;
  string fFacetAlignmentMethod;
  string fFacetLocationFileName;


  vector< double > fPeFacet;        //position on facet of where photon hits it
  vector< double > fFacetNormalDir; //Dir-cosigns of facet normal vector.
  vector< double > fPh;          //Vector from focal plane to facet focal point
  vector< double > fPeFacetNormalDir;//Normal dir of curved facet mirror at 
                                     //point photon hits
  vector< double > fParallel;
  vector< double > fPerpendicular;
  vector< double > fReflectedDir;
  vector< double > fDir;          //Direction of photon in mirror plane system

  float dummy;

public:
  KSTiltAndTrace(double DnMinTight,double DnMinLoose,
		 double MirrorRadiusSquaredM2,double FacetDiameterM,
		 double FocalLengthM, double JitterWidthEWDeg,
		 double JitterWidthNSDeg,double MetersPerDeg);

  KSTiltAndTrace(double DnMinTight,double DnMinLoose,
		 double MirrorRadiusSquaredM2,double FacetDiameterM,
		 double FocalLengthM, double JitterWidthEWDeg,
		 double JitterWidthNSDeg,double MetersPerDeg,
		 double FocalPlaneLocationM,  double AlignmentPlaneLocationM,
		 string MirrorAlignmentMethod,string FacetLocationFileName);
 
  virtual ~KSTiltAndTrace();
  int Tilt();
  int  FullAberationTrace();

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

  vector <double> fPe;  //x,y only
  vector <double> fW;   //Position of photon in camera plane (Deg)
  
  KSFacets* pMirrorFacets;
};
// ***************************************************************************


#endif
