//-*-mode:c++; mode:font-lock;-*-
/** * \class VAAzElRaDecXY
 * \ingroup common
 * \brief General conversion class between camera coords (X,Y) to Az,El to RA Dec 
 *
 * Original Author: Glenn H. Sembroski
 * $Author$
 * $Date$
 * $Revision$
 * $Tag$
 *
 **/
// *********************************************************************
// This is copied from the vegas/common/src/VAAzElRADecXY.cpp with the
// vegas dependencies removed. This is so KASCADE/common has no vegas 
// dependencies.
// *********************************************************************
// These deinitions tell the makefile which library the cpp file
// should be included in
// VA_LIBRARY_TAG: libSP24common.a
// VA_LIBRARY_TAG: libSP24commonLite.a

//Modified:

#include "KSAzElRADecXY.h"

using namespace std;

#ifdef __MAKECINT__
#pragma link C++ class KSAzElRADecXY;
#endif

#ifndef NOROOT
  #ifdef D_USEVEGAS
    ClassImp(KSAzElRADecXY);
  #endif
#endif

KSAzElRADecXY::KSAzElRADecXY(double EastLongitude, double Latitude)
{
  fEastLongitude=EastLongitude;
  fLatitude=Latitude;
  fCosLatitude=cos(fLatitude);
  fSinLatitude=sin(fLatitude);
}
KSAzElRADecXY::KSAzElRADecXY(double EastLongitude, double Latitude, 
			     double RASource2000, double DecSource2000)
{
  fEastLongitude=EastLongitude;
  fLatitude=Latitude;
  fCosLatitude=cos(fLatitude);
  fSinLatitude=sin(fLatitude);
  fRASource2000=RASource2000;
  fDecSource2000=DecSource2000;
}
KSAzElRADecXY::~KSAzElRADecXY()
{
  //Nothing here
}
// **************************************************************************


#ifdef D_UseVegas
void KSAzElRADecXY::XY2RADec2000(double fX, double fY, VATime& fTime, 
				 double RASource2000, double DecSource2000,
				 double& fRA2000, double& fDec2000)
{
  fRASource2000=RASource2000;
  fDecSource2000=DecSource2000;
  XY2RADec2000(fX, fY, fTime, fRA2000, fDec2000);
}


void KSAzElRADecXY::XY2RADec2000(double fX, double fY, VATime& fTime, 
				 double& fRA2000, double& fDec2000)
// **************************************************************************
// This routone converts non-derotated camera plane coordinates(in deg)
// to RA,Dec 2000
// **************************************************************************
// Notes: VATime is by refereence so we don't construct and destruct a copy
// each time this is called. We don't change it. This is for speed reasons.
{
  //convert RA and Dec of the tangent point (always in epoch 2000) to Az Elev
  //Since the focal plane coordinates x,y are parallel and perpendicular to
  //the altitude direction (verticle from azimuth position on horizen to
  //zenith) respectivly, we first need to convert ra_src,dec_src coords to 
  //azimuth,altitude coords: calculate our position and convert that back to 
  //ra,dec.
  double fElSrc;
  double fAzSrc;
  RADec2000ToAzEl(fRASource2000,fDecSource2000,fTime,fAzSrc,fElSrc);

  //Now determine projection of x,y from focal plane tangent at az_trk and
  //alt_trk. Assume y axis is verticle with +y corresponding to increasing 
  //altitude. Assume x axis is horizontal with +x coresponding to increasing 
  //azimuth.These assumptions must be checked!~!!!!!!!!!!
  // *********************************************************************
  // 23/06/06 Michael Danial and others say both axis need to be reversed
  // +y corresponds to decreasing altitude
  // +x corresponds to decreasing azimuth
  //So done.
  // **********************************************************************
  //  double xi=(fX*M_PI/180.);
  //  double eta=(fY*M_PI/180.);

  double xi=-(fX*M_PI/180.);
  double eta=-(fY*M_PI/180.);
 


  // First the most probable source location

  double fAzXY,fElXY;

  slaDtp2s(xi,eta,fAzSrc,fElSrc,&fAzXY,&fElXY);

  //now convert this az and el to RA2000 Dec2000 coords.

  AzEl2RADec2000(fAzXY,fElXY,fTime,fRA2000,fDec2000);
  return;
}
// ************************************************************************


void KSAzElRADecXY::AzEl2RADec2000(double fAz, double fEl, VATime& fTime, 
				   double& fRA2000,double& fDec2000)
// ************************************************************************
// Convert Az and El at fTime to RaDec epoch 2000
// ************************************************************************
{
  double fHourangle;
  double fDecApparent;
  slaDh2e(fAz, fEl, fLatitude, &fHourangle, &fDecApparent); 

  //convert hour angle back to ra
  //double fLocalSidereal = slaGmst(fTime.getMJDDbl()) + fEastLongitude;
  //radians
  double fLocalSidereal = slaGmst(fTime.getMJDDbl()) + fEastLongitude + 
                                                  slaEqeqx(fTime.getMJDDbl());
  double fRAApparent=(fLocalSidereal-fHourangle); 

  // Convert the apparent RA and Dec to the RA Dec for epoch 2000
  slaAmp(fRAApparent,fDecApparent,fTime.getMJDDbl(),2000,&fRA2000,&fDec2000);
  return;
}
// ************************************************************************

void KSAzElRADecXY::RADec2000ToAzEl(double fRA2000,double fDec2000,
				    VATime& fTime,double& fSourceAz, 
				    double& fSourceElev)
// ************************************************************************
// Convert RA2000,Dec2000 at fTime to Az/Elev
// ************************************************************************
{
  //convert RA2000 and Dec2000 (always in epoch 2000) to apparent
  //Ra Dec
  double fRAApparent;
  double fDecApparent;
  slaMap(fRA2000,fDec2000,0,0,0,0,2000,fTime.getMJDDbl(),
	 &fRAApparent,&fDecApparent);
  //Get Hourangle.First we need to get the time into greenich mean solar time.
                                                    //local siderial time: 
  //double fLocalSidereal = slaGmst(fTime.getMJDDbl()) + fEastLongitude;
  //radians
  double fLocalSidereal = slaGmst(fTime.getMJDDbl()) + fEastLongitude + 
                                                  slaEqeqx(fTime.getMJDDbl());
  double fHourangle = slaDranrm(fLocalSidereal) - fRAApparent; 

  //convert fHourangle,fDecApparent coords to 
  //azimuth,altitude coords
  slaDe2h(fHourangle,fDecApparent,fLatitude,&fSourceAz,&fSourceElev);
  return;
}
// **************************************************************************

//
void  KSAzElRADecXY::RADec2000ToXY( double RA2000, double Dec2000,
				    VATime& Time,double RASource2000, 
				    double DecSource2000,double& X,
				    double& Y)
{
  fRASource2000=RASource2000;
  fDecSource2000=DecSource2000;
  RADec2000ToXY(RA2000,Dec2000,Time, X, Y);
  return;
}

void KSAzElRADecXY::RADec2000ToXY( double fRA2000, double fDec2000,
				   VATime& fTime, double& fX,double& fY)
// **************************************************************************
// Find camera plane coordinates(in deg) at Time for equitorial location
// RA2000,Dec2000.  It is assumed for this call that the 
// RASource2000,DecSource2000 of the center of the camera has already been 
// loaded.
// **************************************************************************
// Method is to convert to fRA2000,fDec2000 to  Az,El and then convert 
// AZ,El to X/Y
// **************************************************************************
{
  // *******************************************************************
  //convert RA and Dec of the star point (always in epoch 2000) to Az Elev
  //Focal plane coordinates x,y are parallel and perpendicular to
  //the altitude direction (vertical from azimuth position on horizen to
  //zenith) respectivly
  // *********************************************************************
  double fAz; 
  double fElev;
  RADec2000ToAzEl(fRA2000,fDec2000,fTime,fAz,fElev);
  AzElToXY(fAz,fElev,fTime,fX,fY);
  return;
}
// **************************************************************************

void KSAzElRADecXY::AzElToXY( double fAz, double fElev, VATime& fTime, 
		      double RASource2000, double DecSource2000, double& fX,
		      double& fY)
{
  fRASource2000=RASource2000;
  fDecSource2000=DecSource2000;
  AzElToXY(fAz, fElev, fTime, fX, fY);
  return;
}
// **************************************************************************

void KSAzElRADecXY::AzElToXY( double fAz, double fElev, VATime& fTime, 
			      double& fX,double& fY)
/// **************************************************************************
// Find camera plane coordinates(in deg) at Time for location
// Az,El.  It is assumed for this call that the 
// RASource2000,DecSource2000 of the center of the camera has already been 
// loaded.
// **************************************************************************
{
  // *******************************************************************
  //convert RA and Dec of the tangent point (always in epoch 2000) to Az Elev
  //Since the focal plane coordinates x,y are parallel and perpendicular to
  //the altitude direction (vertical from azimuth position on horizen to
  //zenith) respectivly, we first need to convert ra_src,dec_src coords to 
  //azimuth,altitude coord
  // *********************************************************************
  double fElSrc;
  double fAzSrc;
  RADec2000ToAzEl(fRASource2000,fDecSource2000,fTime,fAzSrc,fElSrc);

  // **********************************************************************
  // Get xi,eta of 'star' in tangent plane.
  // **********************************************************************
  int j;
  double xi;
  double eta;
  slaDs2tp(fAz, fElev, fAzSrc, fElSrc, &xi, &eta, &j);

  // ************************************************************************
  // Convert back to our X,Y which is in degrees.
  // ************************************************************************

  fX=-(xi*180./M_PI);
  fY=-(eta*180./M_PI);
  return;
}
// ********************************************************************
#endif


void KSAzElRADecXY::XYToDlDmDn( double fX, double fY, double fAzSrc, 
				double fElSrc, double& fDl, double& fDm, 
				double& fDn)
// *********************************************************************
//  The fX,fY are in camera coordinates(degrees). Get the unit vector in ground
//  plane coords of the direction. 
// Kascade definition X + east, y + south. 
// Vegas definition X + east, y+ north and z + up
// Using VEGAS  Definition here
// ********************************************************************
{
  //Get the az,elev of this X,Y posistion.
  double xi=-(fX*M_PI/180.);
  double eta=-(fY*M_PI/180.);
 
  // First the az/elev of the  source location
  double fAzXY,fElXY;
  slaDtp2s(xi,eta,fAzSrc,fElSrc,&fAzXY,&fElXY);

  fDn=cos(M_PI/2-fElXY);                    //dn 
  fDm=sin(M_PI/2-fElXY)*cos(fAzXY);           //dm
  fDl=sin(M_PI/2-fElXY)*sin(fAzXY);           //dl
  return;
}
// ********************************************************************


void DlDmDnToAzEl( double Dl, double Dm, double Dn, double& fAz, 
		     double& fEl)
// **************************************************************************
//   Get the Az and Elevation(radians) of a vector X 
// **************************************************************************
// Kascade definition X + east, y + south. 
// Vegas definition X + east, y+ north and z + up
// USING VEGAS Definition here
//***************************************************************************
{
  double fLatitude;
  double fLongitude;
  double pfX[3];
  pfX[0]=Dl;
  pfX[1]=Dm;
  pfX[2]=Dn;
  slaDcc2s(pfX,&fLongitude,&fLatitude);

  fEl=fLatitude;       
  fAz=(M_PI/2)-fLongitude; //From 0 on X CCW axis to 0 On y axis CW
  fAz = slaDranrm(fAz);
  return;
}
// *************************************************************************

void KSAzElRADecXY::DlDmDnToXY( double Dl, double Dm, double Dn, 
				double fAzSrc, double fElSrc, double& fX, 
				double& fY)
// *********************************************************************
//  The fX,fY are in camera coordinates(degrees). 
// ********************************************************************
{
  double fAz;
  double fElev;
  DlDmDnToAzEl(Dl,Dm,Dn,fAz,fElev);

  // **********************************************************************
  // Get xi,eta of 'star' in tangent plane.
  // **********************************************************************
  int j;
  double xi;
  double eta;
  slaDs2tp(fAz, fElev, fAzSrc, fElSrc, &xi, &eta, &j);

  // ************************************************************************
  // Convert back to our X,Y which is in degrees.
  // ************************************************************************

  fX=-(xi*180./M_PI);
  fY=-(eta*180./M_PI);
  return;
}
// **************************************************************************


#ifdef D_UseVegas

void  KSAzElRADecXY::Derotate( VATime& fTime, double fX, double fY,  
			       double RASource2000, double DecSource2000, 
			       double& fXDerotated, double& fYDerotated)
// ********************************************************************
//  The fX,fY are in camera coordinates(degrees). In order to combine data 
//  from different runs (i.e. different sidereal times) they must be 
//  de-rotated to the same hour angle.
// ********************************************************************
{
  fRASource2000=RASource2000;
  fDecSource2000=DecSource2000;
  Derotate(fTime, fX, fY, fXDerotated, fYDerotated);
  return;
}
// ********************************************************************

void  KSAzElRADecXY::Derotate( VATime& fTime, double fX, double fY, 
			       double& fXDerotated, double& fYDerotated)
// *********************************************************************
//  The fX,fY are in camera coordinates(degrees). In order to combine data 
//  from different runs (i.e. different sidereal times) they must be 
//  de-rotated to the same hour angle.
// ********************************************************************
{
  //convert RA and Dec of the tangent point (always in epoch 2000) to Az Elev
  //Since the focal plane coordinates x,y are parallel and perpendicular to
  //the altitude direction (verticle from azimuth position on horizen to
  //zenith) respectivly, we first need to convert ra_src,dec_src coords to 
  //azimuth,altitude coords:
  double fElSrc;
  double fAzSrc;
  RADec2000ToAzEl(fRASource2000,fDecSource2000,fTime,fAzSrc,fElSrc);


  //Now get a derotation angle.

  double fDerotangle = atan2(-1.0*fCosLatitude*sin(fAzSrc),
			     (cos(fElSrc)*fSinLatitude - 
			      sin(fElSrc)*cos(fAzSrc)));
  //Now derotate X and Y
  fXDerotated = fX*cos(fDerotangle) - fY*sin(fDerotangle);
  fYDerotated = fX*sin(fDerotangle) + fY*cos(fDerotangle);
  fXDerotated=-fXDerotated; // flip coordinate
  fYDerotated=-fYDerotated; // flip coordinate
  return;
}
#endif
