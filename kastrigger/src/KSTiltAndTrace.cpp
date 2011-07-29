//-*-mode:c++; mode:font-lock;-*-
/**
 * \class KSTiltAndTrace 
 * \brief Class to do ray tracing for Davis-Cotton tels.
 * Original Author: Glenn H. Sembroski 
 * $Author$
 * $Date$
 * $Revision$
 * $Tag$
 *
 **/
#include "KSTiltAndTrace.h"
// **********************************************************************
// This is a conversion from kastriggermain.f90 to C++
// 13/05/08 GHS
// **********************************************************************

extern "C" float pran(float* dummy);
extern "C" double gaussfast();
extern "C" void geom8(double* dl,double* dm,double* dn,double* tix,
		      double* tiy);

KSTiltAndTrace::KSTiltAndTrace( double DnMinTight,double DnMinLoose,
				double MirrorRadiusSquaredM2,
				double FacetDiameterM,double FocalLengthM,
				double JitterWidthEWRad,
				double JitterWidthNSRad, double MetersPerDeg)
{
  fDnMinTight=DnMinTight;
  fDnMinLoose=DnMinLoose;
  fMirrorRadiusSquaredM2=MirrorRadiusSquaredM2;
  fFacetDiameterM=FacetDiameterM;
  fFocalLengthM=FocalLengthM;
  fJitterWidthEWRad=JitterWidthEWRad;
  fJitterWidthNSRad=JitterWidthNSRad;
  fMetersPerDeg=MetersPerDeg;

  //defaults
  fFocalPlaneLocationM     = fFocalLengthM;
  fAlignmentPlaneLocationM = fFocalPlaneLocationM;
  fFacetAlignmentMethod    = "WHIPPLE";
  fFacetLocationFileName   = " ";

  // ***********************************************************
  //Init vectors
  // ***********************************************************
  fW.clear();
  fPeFacet.clear();        //position on facet of where photon hits it
  fFacetNormalDir.clear(); //Dir-cosigns of facet normal vector.
  fPh.clear();          //Vector from focal plane to facet focal point
  fPeFacetNormalDir.clear();//Normal dir of curved facet mirror at 
  fParallel.clear();
  fPerpendicular.clear();
  fReflectedDir.clear();
  fDir.clear(); //Direction of photon in mirror plane system

  fW.resize(2);
  fPeFacet.resize(3); 
  fFacetNormalDir.resize(3);
  fPh.resize(3);
  fPeFacetNormalDir.resize(3);
  fParallel.resize(3);
  fPerpendicular.resize(3);
  fReflectedDir.resize(3);
  fDir.resize(3);
  // **************************************************************



  pMirrorFacets= new KSFacets(fMirrorRadiusSquaredM2, fFacetDiameterM,
			      fFocalLengthM, fFocalPlaneLocationM,  
			      fAlignmentPlaneLocationM, fFacetAlignmentMethod,
			      fFacetLocationFileName);
}

// **************************************************************************
// This version of constructor adds focal plane location
// *************************************************************************
KSTiltAndTrace::KSTiltAndTrace( double DnMinTight,double DnMinLoose,
				double MirrorRadiusSquaredM2,
				double FacetDiameterM,double FocalLengthM,
				double JitterWidthEWRad,
				double JitterWidthNSRad, double MetersPerDeg,
				double FocalPlaneLocationM, 
				double AlignmentPlaneLocationM, 
				std::string FacetAlignmentMethod,
				std::string FacetLocationFileName)
{
  fDnMinTight              = DnMinTight;
  fDnMinLoose              = DnMinLoose;
  fMirrorRadiusSquaredM2   = MirrorRadiusSquaredM2;
  fFacetDiameterM          = FacetDiameterM;
  fFocalLengthM            = FocalLengthM;
  fJitterWidthEWRad        = JitterWidthEWRad;
  fJitterWidthNSRad        = JitterWidthNSRad;
  fMetersPerDeg            = MetersPerDeg;
  fFocalPlaneLocationM     = FocalPlaneLocationM;
  fAlignmentPlaneLocationM = AlignmentPlaneLocationM;
  fFacetAlignmentMethod    = FacetAlignmentMethod;
  fFacetLocationFileName   = FacetLocationFileName;

   // ***********************************************************
  //Init vectors
  // ***********************************************************
  fW.clear();
  fPeFacet.clear();        //position on facet of where photon hits it
  fFacetNormalDir.clear(); //Dir-cosigns of facet normal vector.
  fPh.clear();          //Vector from focal plane to facet focal point
  fPeFacetNormalDir.clear();//Normal dir of curved facet mirror at 
  fParallel.clear();
  fPerpendicular.clear();
  fReflectedDir.clear();
  fDir.clear(); //Direction of photon in mirror plane system

  fW.resize(2);
  fPeFacet.resize(3); 
  fFacetNormalDir.resize(3);
  fPh.resize(3);
  fPeFacetNormalDir.resize(3);
  fParallel.resize(3);
  fPerpendicular.resize(3);
  fReflectedDir.resize(3);
  fDir.resize(3);
  // **************************************************************

 pMirrorFacets = new KSFacets(fMirrorRadiusSquaredM2, fFacetDiameterM,
			      fFocalLengthM, fFocalPlaneLocationM,  
			      fAlignmentPlaneLocationM, fFacetAlignmentMethod,
			      fFacetLocationFileName);
}
// **************************************************************************

KSTiltAndTrace::~KSTiltAndTrace()
{
  // nothing to see here
}

// ***************************************************************************

int  KSTiltAndTrace::Tilt()
// ***************************************************************************
//  This is a gets x,y in focal plan of tilted telescope
// ***************************************************************************
{
  // ************************************************************************
  // The length of the track from the ground to the tilted mount mirror plane 
  // is: dist=-((DLm,DMm,DNm) . (xg,yg,zg))/((DLm,DMm,DNm) . (DL1,DM1,DN1))
  // (DLm,DMm,DNm) is normal to mount mirror plane pointed at sky.
  // The term in the denominater is the dot product of the normal to the
  // mirror plane and the direction of the photon. Calculate it first. Its
  // the relative dn of the photon.
  // ************************************************************************

  double vn=(fTDlm*fDlr + fTDmm*fDmr + fTDnm*fDnr);// fDnr always positive
						   // fTdnm always negative
  int dump=0;
  if(fabs(vn)<fDnMinLoose)      //vn and fDnMinLoose are direciton cosigns to
                                   // Z axis
    {
      dump=1; // flag to drop this pe
      fW.at(0)=1;
      //pran(&dummy);  //These are for synching rnadom numbers during debugging
      //pran(&dummy);
      //gaussfast();
      //gaussfast();

      return dump;
    }

  // ************************************************************************
  // We want to preserve the angle to N(that is fDnr=-vn)  and we also
  // want fDlr,fDmr,-vn to be a unit vector.
  // ***********************************************************************
    if(fabs(vn)>=1.0)
    {
      vn=-1.0;                 // Always assume negative mount.
    }

  // ********************************************************
  // Ok. See if this photon hits the telescope mirror.
  // Now the numerater. This is perendicular distance from xg,yg,zg=0
  // to mirror plane of mount.
  // *********************************************************
  double xn=-(fTDlm*fXg+fTDmm*fYg);        // zg*dn=0. xn in meters

  double dist=xn/vn; // Length of vector from hobs to intersection of mount 
                     //plane (Meters).

  // *********************************************************************
  // 	Now correct timing for tilted mirror plane. Pe must go DIST distance
  // 	further.
  // *********************************************************************
  fPeTimeTilt=fTime+(dist/kCLightMpNS);

  // ************************************************************************
  // Now the x,y intercepts of the track in the mirror plane:
  // Use the x,y unit vectors as defined in MOUNT_VECINI
  // First the vector of this intercept point.
  // X'=X(xseg,yseg)+PhotonUnitVector*dist.
  // PhotonUnitVector*dist is vector from Xseg,Yseg to intersection point
  // in mirror plane
  // ************************************************************************
  double xmount=fXg+fDlr*dist;// Z direction increases down from the ground 
  double ymount=fYg+fDmr*dist;// plane. Define hobs as Z=0
  double zmount=fDnr*dist;
			
  // ***********************************************************************
  // Now convert this to the supplied X,Y unit vectors in the mirror plane
  // Dot this with the new x' and y' unit vectors to get x,y in
  // mirror plane
  // This is a transformation to within the mirror plane.
  // ***********************************************************************
  fPe.clear();
  fPe.resize(2);
  fPe.at(0)=xmount*fXDl+ymount*fXDm+zmount*fXDn;
  fPe.at(1)=xmount*fYDl+ymount*fYDm+zmount*fYDn;

  // ***********************************************************************
  // See if this photon is within pmt mirror radius.
  //Note. We could put code here to find which FACET we hit. This might
  // improve the Aberration calculation slightly also.
  // Do it the simple way:
  // ***********************************************************************
  double fPhotonRad2=(fPe.at(0)*fPe.at(0)+fPe.at(1)*fPe.at(1));
  if(fPhotonRad2>fMirrorRadiusSquaredM2)
    {
      dump=1;		//  Gets here if mirror is missed.
      fW.at(0)=2;
      //pran(&dummy);
      //pran(&dummy);
      //gaussfast();
      //gaussfast();
      return dump;	// Drop this pe.
    }
  // ***********************************************************
  // 	Now get relative direction of photon to mirror plane.
  // ***********************************************************
  if(vn==-1.0)
    {	// Photon direction along opticle axis?
      fDir.at(0)=0.;  // DL
      fDir.at(1)=0.;  // DM
      fDir.at(2)=1.0;  // DN	(positive)
    }
  else
    {
      // *********************************************************
      // Form the relative vector components of photon to mount.
      // 	Again dot products with x',y' unit vectors.
      // **********************************************************
      fDir.at(0) = fDlr*fXDl + fDmr*fXDm + fDnr*fXDn;
      fDir.at(1) = fDlr*fYDl + fDmr*fYDm + fDnr*fYDn;
      fDir.at(2)=sqrt(1-fDir.at(0)*fDir.at(0)-fDir.at(1)*fDir.at(1));
                                           //Positive sign means down going.
    }

  // ******************************************************************
  // 	THIS IS WHERE WE DO OPTICAL ABERRATIONS.
  // 	W10m_full_aberration finds where in the focal plane this photon lands
  // 	after applying  both  global and facet aberations for the whipple
  // 	10m mirror. Save mirror plane time.
  // ********************************************************************
  fW.at(0)=0;

  dump=FullAberationTrace();    //Trace the photon to the camera focal plane
  
  if(dump==0){
    double vnActual=sqrt(fW.at(0)*fW.at(0)+fW.at(1)*fW.at(1));  //Deg
    // Why cos? Because vnNow and fDnMinTight is a direction cosign to z axis 
    double vnNow = cos(vnActual*3.14159265/180.0);
    if(vnNow<fDnMinTight)
      {
	dump=1;
	fW.at(0)=3;
      }
  }
  return dump;
}
// *************************************************************************
int KSTiltAndTrace::FullAberationTrace()
// ***************************************************************************
// Aberration of a Cotten-davis mirror 
// determined by  exact ray tracing plus a Gaussian jitter added for
// pointing/spotsize errors.
// ************************************************************************
// We add capability to move focal plane to focus things (fFocalPlaneLocationM)
// and we capability to align mirror facets to fAlignmentPlaneLocationM.
// ***********************************************************************
// The Whipple 10m and Veritas mirrors are a spherical surface of 
// radius=fFocalLengthM. On this surface are attached small hexagon shaped
//  mirror 
// facets of diameter=fFacetDiameterM. These mirror facets are spherical with 
// focal length equal to  fFocalLengthM (their radii of curvature is thus 
// 2*fFocalLengthM).
// However!!! the Cotten-Davis trick is to not have the mirrors oriented
// tangent to the global surface (ie have them pointing back to the 
// fFocalLengthM
// origin of the radius of curvature of the global mirror which would give
// really bad sperical aberrations!) but rather the indidual facets are 
// pointed back along the optical axis to a point 2 times the global surface 
// radius, ie. 2*fFocalLengthM. This reduces the sperical abertions by a whole 
// bunch.  There are, however, some aberrations left. This is especially
// true for off axis rays. This code attemps to calculate the actual 
// direction of the reflected ray. This is tough since we don't want to
// find where on a particular facet a photon lands (well maybe later using
// algorithums we set up for finding pixels). So when we have a photon we
// generate a facet under it positioned randomly.
// We then do an exact ray trace to the focal plane. 
// We jitter the normal of the facet with GEOM.
// ***************************************************************************
// Modifications due to focal plane location not at MCC (Mirror Center of 
// Curvatre ie at a height of radius=fFocalLengthMabove the mirror plane)
// Further modifcations are to have the Facet alignment plane location, 
// fAlignamentPlaneLocationM, be different from MCC and from fFocalLengthM. 
// This allows for focusing at shower max.
//   When we change the location of the alignment plane:
//   fFocalPlaneLocationM != fFocalLengthM, then we have several changes:
//   1. MCGILL alignament method depends on alignment plane position 
//      (fAlignamentPlaneLocationM). WHIPPLE alignment does not.
//   When we change the location of the focal plane plane:
//   1. Intercept plane at fFocalPlaneLocationM
// ***************************************************************************
// Note: Z is negative since vector goes from the forcal plane to the mirror.
// ***************************************************************************
// This code is an adoption of that orignally written in C by Dave Lewis
// in 89 Sept. Re written as F90 by GHS.
// Plus a facet modeling derived from a pascal program from M. Hillas I think.
// ****************************************************************************
// Note that everything is in metric units.
// ****************************************************************************
// fPe:	Position in meters where photon hits the mirror in the
// 	mirror	plane.(optical axis goes through origin).
// fDir: Direction cosign vector of incident ray. In plane of 
// 	 mirror. 
// fW:	Vector in the focal plane where this photon hits(in deg).
// fPeTimeTilt:Is time pe hits mirror plane if no mirror was
// 	there.
// fPeTime: Time it hits the focal plane after reflection.
// ****************************************************************************
// Written by:
// G.H.Sembroski
// Physics Dept
// Purdue University
// W.Lafayette, In 47907
// E-Mail: "sembroski@physics.purdue.edu"
// 09/2/95
// *************************************************************************
// Modified:
// 01/12/97 GHS  V:1:1:4:1:1.2
// 	Include time adjustment calculations  for the mirror aberations
// 	Upon return from the call to full_aberation, fPeTime 
// 	will have the pe arrival time at the focal plane adjusted for 
// 	differences in path lengths across the cotton_davis
// 	mirror.
// 	1:First, to improve substatialy the accuracy of this
// 	calculation we improve on our approximation that the x,y on the 
// 	facet is the same as the xy in the mirror plane. Obviously for
// 	tilted tracks this is not true but its not too bad an
// 	approximation(dn<3 deg). To improve on this(a lot!) back track 
// 	the track of the photon to the altitude of the facet origen and
// 	use the	xy there as the basis for the xy of the reflection 
// 	point. This is easy(except for chosing the correct sign of 
// 	things)	and quick to calculate and will remove 80% of our 
// 	timinig and position error.
// 	Add code to make sure all facets fit within radius of mirror.
// **************************************************************************
// The shape for a SPHERE is rm**2 = (xm**2 + ym**2 + zm**2) where we set
// rm = FOCAL_LENGTHFL = 7.3 m.(or 12m for Veritas)
// Use the center of the sphere as the origin and the z axis chosen to be
// vertical.
// **************************************************************************
// Note on origin: A focal plane at MCC (z = fFocalLengthM) is the origin for 
// some of these vectors. Our actual focal plane will be at 
// fFocalPlaneLocationM
// Positive is along the axis away from the mirror(into the sky, ie UP!).
// This is a change from the external coord system which has z positive
// going the opposite direction (into the ground) and had the origen in the 
// mirror plane.
// ***************************************************************************
// We generate the x,y of a typical facet mirror that this photon would
// hit. This is an approximation. In the future I will include code to
// actually figure which facet is hit and where it is. Fake it for now.
// ***************************************************************************


// ************************************************************************
// And so we begin!!!
// ************************************************************************
//  Convert relative direction of photon to mirror plane to our new z axis 
//  direction.(change sign of Z)!
// I may have a left hand coord system here but it doesn't matter(I think!)
// ***************************************************************************
{
  fDir.at(2)=-fDir.at(2);  //reverse Z direciton

  // *************************************************************************
  // First: Get X,Y,Z position vector of the facet (fFacet) where this photon 
  // hits. Z is relative to an origin which is fFocalLengthM along the optical
  // axis from the mirror plane and will be negative.
  // *************************************************************************
  vector <double> fFacet(3);
  int facetIndex=-1;

  int dump= pMirrorFacets->FindFacetLocation(fPe, fFacet,facetIndex);
  if(dump!=0){
    fW.at(0)=4;
    return dump;
  }

   
  // **************************************************************************
  // Backtrack the pe track (direction of Pe is fDir where z is positive 
  // going up) to this fFacet.at(2) altitude. This will gives us the
  // x,y,z point relative to the MCC where the photon crossed the plane 
  // parrallel to the mirror plane but at the mirror facet height positon. Use
  // this new x,y in our approximation as to where the photon refelcts from 
  // the facet. Note there is a small chance this new x,y loc isn't really
  // still on this facet but assume it is. ASSUMPTION!
  // **************************************************************************
  double zDist=fFocalLengthM+fFacet.at(2);  // height of facet above mirror 
                                            // plane.
  double path=zDist/fDir.at(2);            // Path length between mirror plane
                                        // and facet plane. Should be negative
 
  fPeFacet.at(0)=fPe.at(0)+path*fDir.at(0); //X,Y Positon vector (from 
                                            //center of mirror)
                                   //to where the photon
                                   //intecepts the facet plane
  fPeFacet.at(1)=fPe.at(1)+path*fDir.at(1); //Determine fPeFacet(2) below

  // *********************************************************************
  // Get a vector that is normal to center of facet
  // This depends on the alignment method specified
  // *********************************************************************
  vector < double > fFacetNormal(3);
  
  dump= pMirrorFacets->FindFacetNormal(fFacet, fFacetNormal,facetIndex);
  if(dump!=0){
    fW.at(0)=4;
    return dump;
  }

  // *****************************************************************
  // Find  Facet normal direction
  // At this point for both WHIPPLE and MCGILL fFacetNormal goes from the
  // center of the facet to the optical axis. Z is positive from the mirror
  // towards the focal plane. Get the length of this vector.
  // *****************************************************************
  double mag=sqrt(fFacetNormal.at(0)*fFacetNormal.at(0) +
		  fFacetNormal.at(1)*fFacetNormal.at(1) +
		  fFacetNormal.at(2)*fFacetNormal.at(2) );
  
  fFacetNormalDir.at(0)=fFacetNormal.at(0)/mag;  //Unit vector. facet normal.
  fFacetNormalDir.at(1)=fFacetNormal.at(1)/mag;
  fFacetNormalDir.at(2)=fFacetNormal.at(2)/mag;

  
  // ***********************************************************************
  // Now we need the facet unit vector normal at the point of the photon
  // intercept.
  // We know the center of curvature of the facet mirror will be beyond
  // the optical axis along the fFacetNormalDir vector a distance of 
  // 2*fFocalLengthM. So make that little vector and call it fPh
  // ***********************************************************************
  double magPh=2*fFocalLengthM-mag;
  fPh.at(0)=fFacetNormalDir.at(0)*magPh;
  fPh.at(1)=fFacetNormalDir.at(1)*magPh;
  fPh.at(2)=fFacetNormalDir.at(2)*magPh;
  
  // *********************************************************************
  // Find z value of where photon hits the facet mirror. This must
  // satisy the sphere equation from the fPh point with radius of curvature
  // of 2*fFocalLengthM and use the facet altitude plane fPeFacet.at(0),
  // fPeFacet.at(1) values (which we know shold be adjusted some more, but 
  // screw it!)
  // Remember sign convention.
  // fPeFacet goes positive up (from fFocalPlaneLocationM not MCC (Mirror 
  // Center of Curvature). We put origin at our focal plane
  // Find difference in z from facet center to point of photon reflection
  // ******************************************************************
  
  fPeFacet.at(2)=mag*fFacetNormalDir.at(2)+fPh.at(2)-
                sqrt((2*fFocalLengthM)*(2*fFocalLengthM)-
		    (fPeFacet.at(0)-fPh.at(0))*(fPeFacet.at(0)-fPh.at(0))- 
		    (fPeFacet.at(1)-fPh.at(1))*(fPeFacet.at(1)-fPh.at(1)) );

  // *******************
  // use this to adjust fFacet.at(2) (which is based at fFocalPlaneLocationM)
  // for fPeFacet(2]
  fPeFacet.at(2)=fFacet.at(2)+fPeFacet.at(2); 
  
  // ******************************************************************
  // 	Adjust the petime for the fact that it hits at PeFacet not at
  // 	the mirror plane. Note this is always a reduction.
  // ********************************************************************
  zDist=fFocalPlaneLocationM+fPeFacet.at(2);// height of reflection point 
                                            // above mirror plane.
  path=zDist/fDir.at(2);		// Path length between mirror plane
					// and facet plane. Should be negative.
  fPeTime=fPeTimeTilt+(path/kCLightMpNS);
  

  // ********************************************************************
  // So we now have fPeFacet the vector from the focal plane origen at 
  // fFocalPlaneLocation to the point where 
  // the photon hits the facet.
  // Vector from reflection point to center of facet   
  // ****************************************************************
  std::vector <double> fReflect(3);
  fReflect.at(0)=fFacet.at(0)-fPeFacet.at(0);
  fReflect.at(1)=fFacet.at(1)-fPeFacet.at(1);
  fReflect.at(2)=fFacet.at(2)-fPeFacet.at(2);

  // *****************************************************************
  // Get vector from reflection point to center of curvature
  // center of curvature of the facet(the PH point).
  // ********************************************************************
  
  fPeFacetNormalDir.at(0)=fReflect.at(0) + fFacetNormal.at(0) + fPh.at(0);
  fPeFacetNormalDir.at(1)=fReflect.at(1) + fFacetNormal.at(1) + fPh.at(1);
  fPeFacetNormalDir.at(2)=fReflect.at(2) + fFacetNormal.at(2) + fPh.at(2);
  
  // ********************************************************************
  // Covert this to a unit vector. This is the normal to the surface we
  // are reflecting from.
  // ********************************************************************
  
  double magPeFacetNormalDir=
    sqrt(fPeFacetNormalDir.at(0)*fPeFacetNormalDir.at(0) +
	 fPeFacetNormalDir.at(1)*fPeFacetNormalDir.at(1) +
	 fPeFacetNormalDir.at(2)*fPeFacetNormalDir.at(2));
  
  fPeFacetNormalDir.at(0)=fPeFacetNormalDir.at(0)/ magPeFacetNormalDir;
  fPeFacetNormalDir.at(1)=fPeFacetNormalDir.at(1)/ magPeFacetNormalDir;
  fPeFacetNormalDir.at(2)=fPeFacetNormalDir.at(2)/ magPeFacetNormalDir;
  

 // *****************************************************************
  // At this point we include the effect of mirror imperfections and
  // pointing errors by throwing over a Gaussian of widths fJitterWidthEWRad
  // and fJitterWidthNSDeg and using GEOM to re-orient the normal vector 
  // randomly. Since this is a reflection we need only 1/2 of the spotsize
  // jitter. This is waht the fJitterWidthEWRad and fJitterWidthNSRad have.
  // Instead of raidal guassian dist with random in phi, use Gaussin in x
  // and Gaussian in y. I don't know why this is a better model but it 
  // matches the measured surface brightness distribution much better.
  // Also, use totaly r*8 version. This gets around roundoff problem for 
  // small angles.
  //  Use seperate jitter widths for e-w and ns(top-down) in case some day we
  //  want oval PSF.
  // *****************************************************************
  double tix=(gaussfast()*fJitterWidthEWRad);
  double tiy=(gaussfast()*fJitterWidthNSRad);
  double dl=fPeFacetNormalDir.at(0);  //Do this so fortran call to geom8 works.
  double dm=fPeFacetNormalDir.at(1);
  double dn=fPeFacetNormalDir.at(2);

  geom8( &dl, &dm, &dn, &tix, &tiy);

  fPeFacetNormalDir.at(0)=dl;   //restore
  fPeFacetNormalDir.at(1)=dm;
  fPeFacetNormalDir.at(2)=dn;

  // *****************************************************************
  // Now we can do the reflection of the incident ray. The reflected ray
  // will be the sum of the part of the incident ray that is perendicular
  // to the facet mirror with the negative of the component of the incident
  // ray that is parallel.
  // The dot product gives us the projection along fPeFacetNormalDir: the 
  // parallel part
  // ******************************************************************
  double projection=(fDir.at(0)*fPeFacetNormalDir.at(0) +
		     fDir.at(1)*fPeFacetNormalDir.at(1) +
		     fDir.at(2)*fPeFacetNormalDir.at(2));
  fParallel.at(0)=fPeFacetNormalDir.at(0)*projection;
  fParallel.at(1)=fPeFacetNormalDir.at(1)*projection;
  fParallel.at(2)=fPeFacetNormalDir.at(2)*projection;
  
  // *******************************************************************  
  // Perpendicular component is just difference.
  // *******************************************************************
  fPerpendicular.at(0)=fDir.at(0)-fParallel.at(0);
  fPerpendicular.at(1)=fDir.at(1)-fParallel.at(1);
  fPerpendicular.at(2)=fDir.at(2)-fParallel.at(2);

  // *****************************************************************
  // Find dl,dm,dn the unit reflected direction vector
  // It is unitized already(except for any roundoff. Ignore that)
  // *****************************************************************
  
  fReflectedDir.at(0)=fPerpendicular.at(0)-fParallel.at(0);
  fReflectedDir.at(1)=fPerpendicular.at(1)-fParallel.at(1);
  fReflectedDir.at(2)=fPerpendicular.at(2)-fParallel.at(2);
  
  // ******************************************************************
  // Now find position vector in focal plane of where this photon hits.
  // First get length of vector from hit point on mirror to hit point in 
  // focal plane. z componet is equal to z component of the hit position.
  // Cosin of angle is fReflectedDir.at(2). so:
  
  double distToMirror=-fPeFacet.at(2)/fReflectedDir.at(2); // Always positive
  
  // **************************************************************
  // This is also the path length of the reflected photon from the facet to
  // the focal plane. Adjust PE_TIME of arrrival of the photon.
  // The correction should always be postitive.
  // **************************************************************
  fPeTime=fPeTime+(distToMirror/kCLightMpNS);

  // ****************************************************************
  // Use this to scale unit vector.(we really only need x,y components)
  // Sum of mirror hit position vector and focal plane hit vector is
  // focal plane vector.
  // also convert to deg
  // ****************************************************************
  
  fW.at(0)=(fPeFacet.at(0)+(distToMirror*fReflectedDir.at(0)))/fMetersPerDeg;
  fW.at(1)=(fPeFacet.at(1)+(distToMirror*fReflectedDir.at(1)))/fMetersPerDeg;
  return dump;
}
// ******************************************************************


