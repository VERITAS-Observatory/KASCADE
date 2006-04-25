/**
 * \class KSMountDirection 
 * \ingroup common
 * \brief File of methods for KSMountDirection.
 *  
 
 * Original Author: Glenn H. Sembroski
 * $Author$
 * $Date$
 * $Revision$
 * $Tag$
 *
 **/
// This is where the code starts.

#include "KSMountDirection.h"

extern "C" float pran(float* fXDummy);

void W10mGetRaDecFromVec(double* X, double& ra, double& dec,double fLatitude);
void W10mGetVecFromRaDec(double ra, double dec, double* X,double fLatitude);

KSMountDirection::KSMountDirection(KSTeHeadData* pTeHead, 
				   double DriftedGammaStepSizeRad)
{
  pfTeHead                 = pTeHead;
  fDriftingGammas          = pfTeHead->fDriftingGammas;
  fMultipleMountDirections = pfTeHead->fMultipleMountDirections;
  fNumDirections           = pfTeHead->fNumDirections;
  fStepSizeRad             = DriftedGammaStepSizeRad;
  fCameraType              = pfTeHead->fCameraType; 
}

KSMountDirection::~KSMountDirection()
{
  //nothing to do
}

void KSMountDirection::createMountDirections()
// ************************************************************************
// Set up for the multiple mount directions stuff
// ************************************************************************
//The following comes from the config files ie. from pfDataIn:
//fMultipleMountDirections:Flag that indicates we will be looking for 
//                        triggers for each event multiple times with 
//                        the mount pointed in different directions, 
//                        usually this is false for gammas and true for
//                        everything else (but see below for drifting 
//                        gammas)
//fMaximumThetaRad:   Nominally maximum distance from the inital Mount
//                   direction that we will go
//fStepSizeRad:Used only by drifting gammas as the size of theta 
//                   steps we will take.
//fNumDirections:Number of directions we will use for each event.
//                   Not used by drifting gammas
{
  if(fDriftingGammas)
    {
      // --------------------------------------------------------------
      //For the whipple strip we need to get GAMMA efficiencies all over 
      //the camera. Do this by treating gammas as hadrons (tilting the 
      //mount)We want to step in theta only up to MaximumThetaDeg (2. degs 
      //nominally) and since we set the step size to StepSize (nominally 
      // =.1 deg=.4 min)),then we don't really care about phi steps use 
      //the smallest we can (2 I think)
      // *****************************************************************
      fPhiSteps=2;                //2 is the minimum number I think
      //Integer round down here
      fThetaSteps = 
	(int)( (pfTeHead->fMaximumThetaRad/fStepSizeRad) +
	       fStepSizeRad/2. + 1  );
      fNumDirections = fPhiSteps*((fThetaSteps-1)*fThetaSteps/2) + 1;
      fMultipleMountDirections=true;
    }
  else if(!fMultipleMountDirections)
    {
      fNumDirections=1;               //Treat as gamma. No redirection
    }

// ***********************************************************************
//  Allocate Some direction arrays and load them up.
// ***********************************************************************
  pfDlm    = new double[fNumDirections];
  pfDmm    = new double[fNumDirections];
  pfDnm    = new double[fNumDirections];
  pfXDlm   = new double[fNumDirections];
  pfXDmm   = new double[fNumDirections];
  pfXDnm   = new double[fNumDirections];
  pfYDlm   = new double[fNumDirections];
  pfYDmm   = new double[fNumDirections];
  pfYDnm   = new double[fNumDirections];
  pfSTheta = new double[fNumDirections];
  pfSPhi   = new double[fNumDirections];


// ****************************************************************************
// Build all the direction arrays for use by W10m_FullAperation and W10m_tilt
// We produce a set of mount directions and we produce the X and Y focal plane
// vectors for each direction.
// ****************************************************************************
// Major error:20/03/06 GHS I have been using the wrong convention for X and
// Y directions. The X,Y vectors that are used in whippletilt and 
// fullaberation, are assumed to be along the X,Y directions (as defined by the
// pixel coordinates(azimultih and elevation) of the focal plane. Not along 
// Ra and Dec in the focal plane as we have been using.
// *************************************************************************
// Conventions(see above note):
// All directions of the mount (M) are given in the upward(-Z) direction 
// convention (ie M dn is neg.). X unit vector is along azimuth and in the 
// focal plane. That is  X is perendicular to M and perpendicular to Zenith.  
// That is X direction is Zenith x M.  Y unit vector is along  elevation. That
// is Y direction is X x M
// ****************************************************************************
// If we want XRA along RA(but we don't, see above note) and in the focal 
// plane then it is perpendicular to M and perpendicular to celestial North 
// pole vector P (for polaris) That is,the XRA direction is M x P.  YDEC unit 
// vector would then be along  +DEC. That is: YDEC directin is XRA x M. Not 
// used anymore.
// ****************************************************************************
// For hadrons (multipleDirections) we random;ly pick the new direrctions 
// (fNumDirections of them) from a circle of radius fMaximumThetaRad.
// ****************************************************************************
//	Modified:
// Previous notes:
//  Use only iphi=1 and iphi=itheta for gamma_drift. Hadrons same as before. 
//  Gamma_drift step_size is .4 min along +/- ra (iphi=1,itheta=iphi). Added a
//  number of routines to do all this. Some c++ ones in Veritas.cpp. 
//  Problems with our old scheme of itheta/iphi. Seen in 2d x,y plots.  We can
//  see effects of phi steps (5 fold symmetries). Replace (for hadrons only, 
//  leave gammas and drift scan as is) with choosing random directions. Chose 
//  directios randomly within direction circle. 
//  For a driftscan make up an array of unit vectors pointing in the RA, 
//  (constant) DEC directions the scan will go through. Steps are in
//  driftscan_step_size(nominally .4 min) per itheta.For compatability with 
//  hadron processing we are using gphi_side=2 but will only fill iphi=1 and 
//  iphi=itheta. (+ and - steps in RA but constant DEC). To do this easily we 
//  are going find the RA and DEC that the original M vector points to, say on 
//  Jan 0 2000. This uses the elev,az of the mount. To get new offset mount 
//  vectors we get new a new ra value which steps of drift_step (nominally 
//  .4 min) in both + and - ra directions for gtheta_max steps in each 
//  direction. Get Ra and Dec for MJD for jan 0 2000 (arbitrary date. and this 
//  mount direction)
// **************************************************************************
// INit original directions
// *************************************************************************

                      //define polaris
  //  double fPolaris[3]={0.0,cos(gLatitude[fCameraType]),
  //		         -sin(gLatitude[fCameraType])};
  double fZenith[3]={0.0,0.0,-1.0};
  double fMount[3]= {pfTeHead->fMountDl,//Direction unit vector 
		     pfTeHead->fMountDm,//From input config file.
		     pfTeHead->fMountDn};
  double fX0[3];   // X direction:Zenith X Mount
  double fY0[3];                 // Y direction: fX0 cross Mount
  unitCrossProduct(fZenith,fMount,fX0);//Give X0 in focal plane and Horitzontal
  unitCrossProduct(fX0,fMount,fY0);    //Gives Y0 in focal plane


// *************************************************************************
//Gamma drift scan init only
// *************************************************************************
  double fRA;
  double fDec;
  if(fDriftingGammas)
    {   
      W10mGetRaDecFromVec(fMount,fRA,fDec,gLatitude[fCameraType]);
    }                                          // See notes above

// *************************************************************************
//Iterate over all directions(only 1 step for pure gammas)
// *************************************************************************
  std::cout<<"fNumDirections: "<<fNumDirections<<std::endl;

  for(int ithphi=0;ithphi<fNumDirections;ithphi++)
    {
      double fM[3];        // Hadrons(and not gamma drift): random directions
      double fRANew;
      double fPhi=0;
      double fTheta=0;
      //double fTempRA=0;
      //double fTempDec=0;
      if(!fDriftingGammas && fMultipleMountDirections)
	{
	  if(ithphi==0)  //Do theta=0 special. phi==0
	    {
	      for(int k=0;k<3;k++)fM[k]=fMount[k];   
	      pfSTheta[ithphi]=0;     //Save theta and phi in radians
	      pfSPhi[ithphi]=0;
	    }
	  else
	    {
	      fPhi=2*M_PI*pran(&fXDummy);    //Must be in Radians
	      fTheta=pfTeHead->fMaximumThetaRad *
		sqrt(pran(&fXDummy));//Random r**2 distribution
	      
	      //repoint mount unit vector by theta,phi
	      vectorRedirect(fTheta, fPhi, fX0,fY0,fMount,fM);
	      pfSTheta[ithphi]=fTheta;     //Save theta and phi in radians
	      pfSPhi[ithphi]=fPhi;
	    }
	}
      else if(fDriftingGammas)
	{  
	  if(ithphi==0)  //Do theta=0 special. only phi=0.(itheta=1,iphi=1)
	    {
	      for(int k=0;k<3;k++)fM[k]=fMount[k];   
	      pfSTheta[ithphi]=0;     //Save theta and phi in radians
	      pfSPhi[ithphi]=0;
	    }
 	  else
	    {
	      int fITheta;
	      int fIPhi;
	      getIThetaIPhi(ithphi,fPhiSteps,fITheta,fIPhi);
	      if((fIPhi!=1) && (fIPhi!=fITheta))    
		//Drifting gammas ignore directions we 
		{                     //don't want
		  continue;
		}
	      if(fIPhi==1)
		{
		  //Along positive X (-ra)
		  fRANew=fRA-(fITheta-1)*fStepSizeRad;
		}
	      else
		{                                    //Along negative X (+ra)
		  fRANew=fRA+(fITheta-1)*fStepSizeRad;
		}
	      W10mGetVecFromRaDec(fRANew,fDec,fM,gLatitude[fCameraType]);
	    }
	}     
      else
	{  //Single direciton (gammas)
	  for(int k=0;k<3;k++)fM[k]=fMount[k];
	  pfSTheta[ithphi]=0;     //Save theta and phi in radians
	  pfSPhi[ithphi]=0;
	} 
      getVector(fM,pfDlm[ithphi],pfDmm[ithphi],pfDnm[ithphi]);

      // ********************************************************************
      //Now X,Y unit vectors in mirror plane (X inf focal plane Perpendicular 
      //to zenith and mount direction.
      // ********************************************************************
      double fX[3];
      double fY[3];
      if(fM[0]==fZenith[0] && fM[1]==fZenith[1] && fM[2]==fZenith[2])
     	{
	  double fXtemp[3]={1.0,0.0,0.0}; //Force to east if we are strictly 
	                                  //verticle
	  getVector(fXtemp,pfXDlm[ithphi],pfXDmm[ithphi],pfXDnm[ithphi]);
	}
      else
	{
	  unitCrossProduct(fZenith,fM,fX);//Give fX in focal plane
	  // and Horitzontal
	  unitCrossProduct(fX,fM,fY);    //Gives fY in focal plane
      	  getVector(fX,pfXDlm[ithphi],pfXDmm[ithphi],pfXDnm[ithphi]);
      	  getVector(fY,pfYDlm[ithphi],pfYDmm[ithphi],pfYDnm[ithphi]);
      	}
    }
  return;
}
// **************************************************************************


void KSMountDirection::unitCrossProduct(double* X,double* Y,double* Z)
// ***************************************************************************
// Produce cross product of X x Y and put into Z. Give Z unit length.
// ***************************************************************************
{
  Z[0]=X[1]*Y[2]-X[2]*Y[1];      // Cross product
  Z[1]=X[2]*Y[0]-X[0]*Y[2];
  Z[2]=X[0]*Y[1]-X[1]*Y[0];
      
  double fZSumSquared=0;
  for(int k=0;k<3;k++)      // Normalize
    {
      fZSumSquared += (Z[k]*Z[k]);
    }
  double fLength=sqrt(fZSumSquared);
  for(int k=0;k<3;k++)      // Normalize
    {
      Z[k]=Z[k]/fLength;
    }
  return;
}

void KSMountDirection::vectorRedirect(double theta, double phi, double* X, double* Y,
			      double* Z,double* R)
// *************************************************************************
// Returns a unit vector defined by theta and phi(in radians) in coord system 
// with unit vectors defineing  x axis: X,  y axis: Y and z axis: Z.  This 
// replaces the old W10M_VECTOR_REDIRECT which was relative to an arbitray x 
// axis. Assume that x,y,z unit vectors are perpendicular. Works for left or 
// right handed system I think. Resutlant unit vector in R
// ****************************************************************************
// New vector will be: r=sin(theta)*cos(phi)*x+sin(theta)*sin(phi)*y+
// cos(theta)z
{
  for(int k=0;k<3;k++)
    { 
      R[k]=sin(theta)*cos(phi)*X[k]+sin(theta)*sin(phi)*Y[k]+cos(theta)*Z[k]; 
    }
  // Normalize
  
  double fRSumSquared=0;
  for(int k=0;k<3;k++)    
    {
      fRSumSquared += (R[k]*R[k]);
    }
  double fLength=sqrt(fRSumSquared);
  for(int k=0;k<3;k++)      // Normalize
    {
      R[k]=R[k]/fLength;
    }
  return;
}
//  ***********************************************************************

void KSMountDirection::getIThetaIPhi(int ithphi,int nsides,int& itheta,int& iphi)
// ***************************************************************************
// 	For a specific itheta and nsides get corresponding itheta/iphi.
// ***************************************************************************
// ***************************************************************************
//        defn: ithphi= ((itheta-2)*(itheta-1)/2)*nsides + iphi
// ***************************************************************************
// Easiest way to do this is to search.
// First  a special case
{
  if(ithphi==0)
    {
      itheta=1;
      iphi=1;
      return;
    }
// useing triangular series formula (1+2+3+...+n)=(n+1)*n/2
  itheta=1;
  while(1) 
    {
      itheta=itheta+1;
             // nthphi will be base value (or last ithphi for previous itheta)
      int nthphi=(((itheta-1)*(itheta-2))/2)*nsides;
      if(nthphi>=ithphi)
	{
	  itheta=itheta-1;
	  break;
	}
    }
  iphi=ithphi-(((itheta-1)*(itheta-2))/2)*nsides;
  return;
}
//  ***********************************************************************

void KSMountDirection::getVector(double* X,double& x1,double& x2,double& x3)
// *************************************************************************
//         Copy a vector into components
// *************************************************************************
{
  x1=X[0];
  x2=X[1];
  x3=X[2];
  return;
}
// ***************************************************************************
void KSMountDirection::writeMountDirections(std::ofstream* pfOutFile)
// ***************************************************************************
// Write the mount directions to a binary file.
// ***************************************************************************
{
  int fLength=sizeof(double)*fNumDirections;
  pfOutFile->write((char*)&fLength, sizeof(int));
  pfOutFile->write((char*)pfDlm, fLength);
  pfOutFile->write((char*)pfDmm, fLength);
  pfOutFile->write((char*)pfDnm, fLength);
  pfOutFile->write((char*)pfXDlm, fLength);
  pfOutFile->write((char*)pfXDlm, fLength);
  pfOutFile->write((char*)pfXDlm, fLength);
  pfOutFile->write((char*)pfYDmm, fLength);
  pfOutFile->write((char*)pfYDnm, fLength);
  pfOutFile->write((char*)pfSTheta, fLength);
  pfOutFile->write((char*)pfSPhi, fLength);
  return;
}
// ***************************************************************************
// ***************************************************************************
void KSMountDirection::readMountDirections(std::ifstream* pfInFile)
// ***************************************************************************
// Read the mount directions from a binary file.
// ***************************************************************************
{
  int fLength=0;
  pfInFile->read((char*)fLength, sizeof(int));
  if(!pfInFile->good())
    {
      std::cout<<"KSMOuntDirection--Failed to read Te Pixel Data."
	       <<std::endl;
      exit(1);
    }
  fNumDirections = fLength/sizeof(double);
 // ***********************************************************************
//  Allocate Some direction arrays and load them up.
// ***********************************************************************
  pfDlm    = new double[fNumDirections];
  pfDmm    = new double[fNumDirections];
  pfDnm    = new double[fNumDirections];
  pfXDlm   = new double[fNumDirections];
  pfXDmm   = new double[fNumDirections];
  pfXDnm   = new double[fNumDirections];
  pfYDlm   = new double[fNumDirections];
  pfYDmm   = new double[fNumDirections];
  pfYDnm   = new double[fNumDirections];
  pfSTheta = new double[fNumDirections];
  pfSPhi   = new double[fNumDirections];

  pfInFile->read((char*)pfDlm, fLength);
  pfInFile->read((char*)pfDmm, fLength);
  pfInFile->read((char*)pfDnm, fLength);  
  pfInFile->read((char*)pfXDlm, fLength);
  pfInFile->read((char*)pfXDmm, fLength);
  pfInFile->read((char*)pfXDnm, fLength);  
  pfInFile->read((char*)pfYDlm, fLength);
  pfInFile->read((char*)pfYDmm, fLength);
  pfInFile->read((char*)pfYDnm, fLength);
  pfInFile->read((char*)pfSTheta, fLength);
  pfInFile->read((char*)pfSPhi, fLength);
  return;
}

void W10mGetRaDecFromVec(double* X, double& fRA, double& fDec, 
			                                      double fLatitude)
  // **************************************************************************
  //   Get the Ra and Dec of a vector X at sideraltime=12:00(chosen arbitray 
  //   time)
  // **************************************************************************
{
  double elevation=M_PI/2-(acos(fabs(X[2])));
  double az=0.0;
  if(X[1]==0 && X[0]==0)
    {      //At zenith
      az=0.0;
    }
  else if(X[1]==0 && X[0]>0)    //along + x axis  (270 deg)
    {
      az=3*M_PI/2;
    }
  else if(X[1]==0 && X[0]<0)    //along - x axis (90 deg)
    {
      az=M_PI/2;
    }
  else if(X[1]>0 && X[0]<=0.0)     //Quadrant 1 (0 to 90 deg)
    {
      az=-atan(X[0]/X[1]);
    }
  else if(X[1]<0 && X[0]<=0.0)     //Quadrant 2 (90 to 180 deg)
    {
      az=M_PI/2+atan(X[0]/X[1]);
    }
  else if(X[1]<0 && X[0]>=.0)      //Quadrant 3 (180 to 270 deg)
    {
      az=M_PI-atan(X[0]/X[1]);
    }
  else if(X[1]>0 && X[0]>0.0)       //Quadrant 4 (270 to 360 deg)
    {
      az=2*M_PI-atan(X[0]/X[1]);
    }

  //Determine ra and dec from az,elev  
  double hourangle;

  //Convert az and elevation to hourangle and dec    
  slaDh2e(az, elevation, fLatitude, &hourangle, &fDec); 
 
  //Convert hour angle back to ra
  fRA=((M_PI/2)-hourangle); //Assumes sideraltime is 6:00 
  fRA= slaDranrm(fRA);  
  return;
}

void W10mGetVecFromRaDec(double fRA, double fDec, double* X, double fLatitude)
// **************************************************************************
//   Using an  Ra and Dec and a sideral time of 12:00 get position vector 
// **************************************************************************
{
  double elevation, azimuth;
  double hourangle = (M_PI/2) - fRA; //sidereal time is 6:00

  slaDe2h(hourangle,fDec,fLatitude,&azimuth,&elevation);

  X[2] = -fabs(sin(elevation));
  double length=sqrt(1-X[2]*X[2]);
  if(length==0)
    {
      X[0]=0;
      X[1]=0;
    }
  else
    {
      X[0]= -sin(azimuth)*length;
      X[1]= cos(azimuth)*length;
    }
  return;
}

