//-*-mode:c++; mode:font-lock;-*-
/**
 * Original Author: Glenn H. Sembroski
 * $Author$
 * $Date$
 * $Revision$
 * $Tag$
 *
 **/

// *********************************************************************
// This is copied from the vegas/common/include/VAAzElRADecXY.h with the
// vegas dependencies removed. This is so KASCADE/common has no vegas 
// dependencies.
// *********************************************************************

#ifndef KSAXElRADECXY_H
#define KSAXElRADECXY_H

#include "VASlalib.h"
#include "VASlamac.h"

#ifdef D_UseVegas
   #include "VATime.h"
#endif

#include <cmath>
//
class KSAzElRADecXY
{
private:
  double fEastLongitude;
  double fLatitude;
  double fRASource2000;
  double fDecSource2000;
  double fCosLatitude;
  double fSinLatitude;

public:
  KSAzElRADecXY(double EastLongitude, double Latitude);                     
  KSAzElRADecXY(double EastLongitude, double Latitude, double RASource2000,
		double DecSource2000);                     
  virtual ~KSAzElRADecXY();
  
#ifdef D_UseVegas
  void XY2RADec2000(double X, double Y, VATime& Time, double& RA2000, 
		    double& Dec2000);
  void XY2RADec2000(double X, double Y, VATime& Time, double RASource2000, 
		    double DecSource2000, double& RA2000, double& Dec2000);
  void AzEl2RADec2000(double Az, double El, VATime& Time, double& RA2000,
		      double& Dec2000);
  void RADec2000ToAzEl(double fRA2000,double fDec2000,VATime& fTime,
		       double& fSourceAz, double& fSourceElev);
  void RADec2000ToXY( double RA2000, double Dec2000, VATime& Time, double& X,
		      double& Y);
  void RADec2000ToXY( double RA2000, double Dec2000, VATime& Time, 
		      double RASource2000, double DecSource2000, double& X,
		      double& Y);

  void AzElToXY( double Az, double El, VATime& Time, double& X,
		 double& Y);
  void AzElToXY( double Az, double El, VATime& Time, 
		 double RASource2000, double DecSource2000, double& X,
		 double& Y);
#endif

  void XYToDlDmDn( double X, double Y, double fAzSrc, double fElSrc, 
		   double& Dl, double& Dm, double& Dn);
  void DlDmDnToXY( double Dl, double Dm, double Dn, double fAzSrc, 
		   double fElSrc, double& fX, double& fY);
  void DlDmDnToAzEl( double Dl, double Dm, double Dn, double& fAz, 
		     double& fEl);

#ifdef D_UseVegas
  void Derotate(VATime& Time, double X, double Y,  double RASource2000, 
		double DecSource2000, double& XDeRotated, double& YDerotated);
  void Derotate(VATime& Time, double X, double Y, double& XDeRotated, 
		double& YDerotated);

  void setRASource2000(double RA){fRASource2000=RA;return;};
  void setDecSource2000(double Dec){fDecSource2000=Dec;return;};
  ClassDef(KSAzElRADecXY,0); //Not to be streamed.
#endif
};
#endif
