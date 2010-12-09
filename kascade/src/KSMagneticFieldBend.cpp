//-*-mode:c++; mode:font-lock;-*-
/**
 * \class KSMagneticFieldBend
 * \ingroup
 * \brief This code is used to bend a particles trajectory with a magnetic 
 * field
 *
 * Original Author: Glenn H. Sembroski * $Author$
 * $Date$
 * $Revision$
 * $Tag$
 *
 **/

//Written by:
// G.H.Sembroski
//Physics Dept.
//Purdue Univ.
//West Lafayette, In. 479096
//sembrosk@physics.purdue.edu
//765-494-5172

#include "KSMagneticFieldBend.h"

KSMagneticFieldBend::KSMagneticFieldBend(string location)
// *********************************************************************
// Bend a particle in a magnetic field
// *********************************************************************
{
  // ****************************************************************
  // Initalize field. 
  // ****************************************************************
  fB.clear();
  fV.clear();
  fVPerp.clear();
  fVxPerp.clear();
  fVyPerp.clear();
  fB.resize(3);
  fV.resize(3);
  fVPerp.resize(3);
  fVxPerp.resize(3);
  fVyPerp.resize(3);

  //Only Veritas for now.
  // ****************************************************************
  if(location=="V"){
    // ***********************************************************
    // Veritas magnetic field is calculated from the website:
    //  http://www.ngdc.noaa.gov/geomagmodels/struts/calcIGRFWMM
    // ***********************************************************
    //fBFieldGauss=0.4714;
    //fDipAngleDeg=58.23; 
    //fAngleEastDeg=10.4; 
    //Debug test
    fBFieldGauss=0.4714;
    fDipAngleDeg=0.0; 
    fAngleEastDeg=0.0; 
    cout<<"WHIPPLE magnetic field values being used. B Field(gauss): "
	<<fBFieldGauss<<" DIP_ANGLE(deg): "<<fDipAngleDeg<<" ANGLE_EAST(deg): "
	<< fAngleEastDeg<<endl;

    fB.at(0) = cos(fDipAngleDeg*kDeg2Rad)*sin(fAngleEastDeg*kDeg2Rad);
    fB.at(1) = -cos(fDipAngleDeg*kDeg2Rad)*cos(fAngleEastDeg*kDeg2Rad);
    fB.at(2) = sin(fDipAngleDeg*kDeg2Rad);
    normalize(fB);
  }
}
// ******************************************************************

void KSMagneticFieldBend::bendTrajectory(double KenergyTeV, double pathM, 
					 int ispec,
					 double& dl1, double& dm1, double& dn1,
					 int qz, double xmass)
// ********************************************************************
// find new direction after bending in magnetic field
// ********************************************************************
{
  // ***************************
  //Neutrals don't bend
  // ***************************
  if(qz==0){
    return;
  }
  // *******************************************************************
  // Get radius of curvature in this field, this particle
  // *******************************************************************
  //      CALC MOMENTUM OF particle
  // *******************************************************************

  double energyTeV=KenergyTeV+xmass;       //Energy of particle in Tev.
  double momentumParticleMeV= sqrt(energyTeV*energyTeV-xmass*xmass)*1.e6;
                                           //Momentum in MeV

  // ********************************************************
  // Calcultae radius of curvature.
  // For heavies with charge != 1 radius goes like inverse of charge.
  // ********************************************************
       
  double radius=momentumParticleMeV/(0.03*fBFieldGauss*abs((float)qz));

  // ************************************************************
  // radius is not the true radius. we need to correct by multiplying by
  // sin(theta)  (theta=  angle between v and b). 
  // However we only use radius below in an eq where that sin(theta) is 
  // cancelled by another sin(theta). 
  // ****************************************************************
  // angle of revolution:
  // Derivation:
  // trueRadius=radius*sin(theta)
  //
  // component of velocity around radius:
  // VyPerp=V*sin(theta)
  //
  // Time of one revolution:
  // T=2*PI*trueRadius/VyPerp
  //  =2*PI*radius*sin(theta)/V*sin(theta)
  //
  // Time we revovle is time to go distance pathM
  // t=pathM/V
  //
  // Thus radians we revolve is:
  // angleRevRad= (t/T)*2*PI
  //            = (pathM/V)*2*PI/(2*PI*radius/V)
  //            = pathM/radius
  // ******************************************************************

  double angleRevRad=pathM/radius;             // angle in radians.
  
  if(qz<0) angleRevRad=-angleRevRad; // Use sign of charge to see which way
                                         // we bend.
  
  // ********************************************************************
  // Now the calculation. This is the long form which I understand. I've seen
  // shorter versions. (Ex. kskascademain.for)
  // *********************************************************************
  // WE use vectors as possible. Velocity unit vector first
  // *********************************************************************

  fV.at(0)=dl1;
  fV.at(1)=dm1;
  fV.at(2)=dn1;
  normalize(fV); //Just being careful

  // ***************************************************************
  // We need the component of the velocity vector that is perpendicular to B
  // and V. It will be in the plane of 'rotation' and will point in the 
  // direction of the v cross B force.
  // ***************************************************************
  // Implimentation note:Always pass vector by reference. its much faster.
  // ***************************************************************
  // fVyPerp will have length of fSinTheta
  
  getCrossProduct(fV,fB,fVyPerp);

  //
  // ***************************************************************
  // Find the unit vector that is perpendicular to B and fVyPerp
  // This is in the V,B plane and in the plane of rotation
  // ***************************************************************
  // fVxPerp will have length of fSinTheta

  getCrossProduct(fVyPerp,fB,fVxPerp);

  // ***************************************************************
  // Now find the components of V that are perpendicular to the VxPerp 
  // direction.
  // ***************************************************************
  // V Parrallel is difference (its also not a unit vector!!)
  // ****************************************************************
  fVParallel.at(0)=fV.at(0)-fVxPerp.at(0);
  fVParallel.at(1)=fV.at(1)-fVxPerp.at(1);
  fVParallel.at(2)=fV.at(2)-fVxPerp.at(2);

  // *****************************************************************
  // Now we actually get to find the new direction after rotation through 
  // angle of the VPerp vector. 
  // *****************************************************************
  fVPerp.at(0)=fVxPerp.at(0)*cos(angleRevRad)+fVyPerp.at(0)*sin(angleRevRad);
  fVPerp.at(1)=fVxPerp.at(1)*cos(angleRevRad)+fVyPerp.at(1)*sin(angleRevRad);
  fVPerp.at(2)=fVxPerp.at(2)*cos(angleRevRad)+fVyPerp.at(2)*sin(angleRevRad);
  
  // **********************************************************************
  // Form final vector
  // **********************************************************************
  fV.at(0)=fVPerp.at(0)+fVParallel.at(0);
  fV.at(1)=fVPerp.at(1)+fVParallel.at(1);
  fV.at(2)=fVPerp.at(2)+fVParallel.at(2);

  normalize(fV);
  dl1=fV.at(0);
  dm1=fV.at(1);
  dn1=fV.at(2);

  return;
}
// ************************************************************************

void KSMagneticFieldBend::normalize(vector<double>& A)
{
  double magnitude=sqrt(A.at(0)*A.at(0) + A.at(1)*A.at(1) + A.at(2)*A.at(2) );
  A.at(0)=A.at(0)/magnitude;
  A.at(1)=A.at(1)/magnitude;
  A.at(2)=A.at(2)/magnitude;
  return;
}
// *************************************************************************
  
void KSMagneticFieldBend::getCrossProduct(vector<double>& A, 
					  vector<double>& B, 
					  vector<double>& C)
{
  C.at(0)=A.at(1)*B.at(2)-A.at(2)*B.at(1);
  C.at(1)=A.at(2)*B.at(0)-A.at(0)*B.at(2);
  C.at(2)=A.at(0)*B.at(1)-A.at(1)*B.at(0);
  return;
}
// **************************************************************************

