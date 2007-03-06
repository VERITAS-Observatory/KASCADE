//-*-mode:c++; mode:font-lock;-*-
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
extern "C" void  GetAzElevFromVec(double* pfX, double& fAzimuth, 
				  double& fElevation);
extern "C" void  GetVecFromXY( double fX, double fY, double fAzSrc, 
			       double fElSrc, double* fM);
extern "C" void  GetXYFromVec(double fAzSrc, double fElSrc,double* fM, 
			      double& fX, double& fY);

void W10mGetRaDecFromVec(double* X, double& ra, double& dec,double fLatitude);
void W10mGetVecFromRaDec(double ra, double dec, double* X,double fLatitude);



KSMountDirection::KSMountDirection(KSTeHeadData* pTeHead, 
				   double GammaStepSizeRad)
{
  pfTeHead                 = pTeHead;
  fGammas2D                = pfTeHead->fGammas2D;
  fMultipleMountDirections = pfTeHead->fMultipleMountDirections;
  fNumDirections           = pfTeHead->fNumDirections;
  fStepSizeRad             = GammaStepSizeRad;
  fCameraType              = pfTeHead->fCameraType; 
}

KSMountDirection::~KSMountDirection()
{
  //nothing to do
}

void KSMountDirection::createMountDirections(double fXAreaWidthM, 
					     double fYAreaWidthM)
// ************************************************************************
// Set up for the multiple mount directions stuff
// ************************************************************************
//The following comes from the config files ie. from pfDataIn:
//fMultipleMountDirections:Flag that indicates we will be looking for 
//                        triggers for each event multiple times with 
//                        the mount pointed in different directions, 
//                        usually this is false for gammas and true for
//                        everything else (but see below for drifting 
//                        gammas and Gammas2D)
//fMaximumThetaRad:   Nominally maximum distance from the inital Mount
//                   direction that we will go
//fStepSizeRad:Used only by drifting gammas and Gammas2D as the size of 
//                   steps we will take.
//fNumDirections:Number of directions we will use for each event.
// ****************************************************************************
// Build all the direction arrays for use by W10m_FullAperation and W10m_tilt
// We produce a set of mount directions and we produce the X and Y focal plane
// vectors for each direction.
// ****************************************************************************
// The X,Y vectors that are used in whippletilt and 
// fullaberation, are assumed to be along the X,Y directions (as defined by the
// pixel coordinates(azimultih and elevation) of the focal plane.
// *************************************************************************
// Conventions(see above note):
// All directions of the mount (M) are given in the upward(-Z) direction 
// convention (ie M dn is neg.). X unit vector is along azimuth and in the 
// focal plane. That is  X is perendicular to M and perpendicular to Zenith.  
// That is X direction is Zenith x M.  Y unit vector is along  elevation. That
// is Y direction is X x M
// ****************************************************************************
// For hadrons (multipleDirections) we randomly pick the new direrctions 
// (fNumDirections of them) from a circle of radius fMaximumThetaRad.
// ****************************************************************************
//  For Gammas2D  we go  in steps in X and Y out to radius MaxThetaDeg
//  This is a special mode for the ML Tracking anaylsis
// ****************************************************************************
//	Modified:
// Previous notes:
//  Use only iphi=1 and iphi=itheta for drifting_gammas. Hadrons same as 
//  before. 
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
{
  fMount[0] = pfTeHead->fMountDl;//Direction unit vector 
  fMount[1] = pfTeHead->fMountDm;//From input config file.
  fMount[2] = pfTeHead->fMountDn;
  
  // ******************************************************** *******
  // Set up the conversion routine.
  // Find az/elev in vegas coords for the mount
  // ***************************************************************
                          // fMount in KASCADE, fMntV in VEGAS
  double fMntV[3];       //Convert to Vegas system.
  fMntV[0]=fMount[0];    //X same
  fMntV[1]=-fMount[1];   //Y changes sign
  fMntV[2]=-fMount[2];   //Z changes sign
  GetAzElevFromVec(fMntV,fAzMount,fElevMount);


  // *******************************************************************
  // Gammas2D. Grid in X and Y in steps of  fStepSizeDeg out to max
  // sqrt(X**2+Y**2)<= fMaxThetaDeg.
  // Used vectors fot temp sotorage unitl we know how many we have, then 
  // transfer over.
  // *******************************************************************
  if(fGammas2D)
    {
      // For the Maximum Likelihood Gamma source Point Spread Function(PSF) 
      // calculation we set up a grid of directions. Starting at 0 in field 
      // of view and stepping 
      // fStepSizeRad(converted back to deg) in positive steps in both X and 
      // Y out to fMaximumThetaRad (again back in deg). That is we only do
      // the positive quadrant in X and Y. The other quadrants can be filled 
      // in by reflections. We do ignore directions past sqrt(Y**2+X**2)>
      // fMaximumThetaRad/gDeg2Rad
      // ********************************************************************
      // Values for fMaximumThetaRad and fStepSizeRad derived from original
      // deg from config file: MaximumThetaDeg, GammaStepSizeDeg
      // *******************************************************************
      
      fMaxThetaDeg=pfTeHead->fMaximumThetaRad/gDeg2Rad;
      fStepSizeDeg=fStepSizeRad/gDeg2Rad;
      fNumXSteps=(int)(fMaxThetaDeg/fStepSizeDeg + 
		    fStepSizeDeg/2.) + 1;
      fNumYSteps=fNumXSteps;
      fMultipleMountDirections=true;
      
      fAomega=fXAreaWidthM*fYAreaWidthM;
      std::cout<<"KSMountDirection:fAomega: "<<fAomega<<" m**2"<<std::endl;

      std::vector< double > pfX;
      std::vector< double > pfY;
      pfX.clear();
      pfY.clear();

      double fX;
      double fY;
      for(int i=0;i<fNumXSteps;i++)
	{
	  fX=i*fStepSizeDeg;
	  for(int j=0;j<fNumYSteps;j++)
	    {
	      fY=j*fStepSizeDeg;
	      if(sqrt(fX*fX+fY*fY)<=fMaxThetaDeg)
		{
		  pfX.push_back(fX);
		  pfY.push_back(fY);
		}
	    }
	}

      fNumDirections=(int) pfX.size();
      allocateDirectionArrays(fNumDirections);


      double fM[3];
      for(int i=0;i<fNumDirections;i++)
	{
	  fX=pfX.at(i);
	  fY=pfY.at(i);

	  GetVecFromXY( fX, fY, fAzMount, fElevMount, fM);

	  // ************************************************************
	  // fM came back in VEGAS coord system. Convert to KASCADE system
	  // ************************************************************
	  fM[1]=-fM[1];
	  fM[2]=-fM[2];

	  double fTheta=sqrt(fX*fX+fY*fY)*gDeg2Rad;
	  double fPhi=0;
	  if(fTheta>0)
	    {
	      fPhi=acos(fX/sqrt(fX*fX+fY*fY));
	      if(fY<0)
		{
		  fPhi=2*M_PI-fPhi;
		}
	    }
	  loadDirectionArrays(i,fTheta,fPhi,fM);
	}
    }

  // **********************************************************
  // Hadrons(and not gamma drift): random directions
  // **********************************************************
  if(!fGammas2D && fMultipleMountDirections)
    {
      double fOmega=2*M_PI*(1.-cos(pfTeHead->fMaximumThetaRad))/fNumDirections;
      fAomega=fXAreaWidthM*fYAreaWidthM*fOmega;
      std::cout<<"KSMountDirection:fAomega: "<<fAomega<<" m**2-sr"<<std::endl;

      double fZenith[3]={0.0,0.0,-1.0};
      double fX0[3];   // X direction:Zenith X Mount
      double fY0[3];                 // Y direction: fX0 cross Mount
      unitCrossProduct(fZenith,fMount,fX0);//Give X0 in focal plane Horitzontal
      unitCrossProduct(fX0,fMount,fY0);    //Gives Y0 in focal plane

      allocateDirectionArrays(fNumDirections);
      // *********************************************************************
      //Iterate over all directions
      // *********************************************************************
      for(int ithphi=0;ithphi<fNumDirections;ithphi++)
	{
	  double fM[3];  
	  double fPhi=0;
	  double fTheta=0;
	  fPhi=2*M_PI*pran(&fXDummy);    //Must be in Radians
	  fTheta=pfTeHead->fMaximumThetaRad *sqrt(pran(&fXDummy));
	                                 //Random r**2 distribution
	  
	  //repoint mount unit vector by theta,phi
	  vectorRedirect(fTheta, fPhi, fX0,fY0,fMount,fM);

	  loadDirectionArrays(ithphi,fTheta,fPhi,fM);
	}
    }
 
  // ***********************************************************************
  // Single gamma direction
  // ***********************************************************************
  if( !fGammas2D && !fMultipleMountDirections)
    {
      fNumDirections=1;               //Treat as gamma. No redirection
      fAomega=fXAreaWidthM*fYAreaWidthM;
      std::cout<<"KSMountDirection:fAomega: "<<fAomega<<" m**2"<<std::endl;

      allocateDirectionArrays(fNumDirections);       
      loadDirectionArrays(0,0,0,fMount);    
    }

  std::cout<<"KSMountDirection:fNumDirections: "<<fNumDirections
	   <<std::endl;
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
// ***************************************************************************

double KSMountDirection::dotProduct(double* pfA, double* pfB)
{
  double fDotProd=0;
  for(int i=0;i<3;i++)
    {
      fDotProd+=pfA[i]*pfB[i];
    }
  return fDotProd;
}
// *************************************************************************

void KSMountDirection::vectorRedirect(double theta, double phi, double* X, 
				      double* Y,double* Z,double* R)
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

void KSMountDirection::getIThetaIPhi(int ithphi,int nsides,int& itheta,
				     int& iphi)
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
  pfOutFile->write((char*)&fAomega,sizeof(float));
  int fLength=sizeof(double)*fNumDirections;
  pfOutFile->write((char*)&fLength, sizeof(int));
  pfOutFile->write((char*)pfDlm, fLength);
  pfOutFile->write((char*)pfDmm, fLength);
  pfOutFile->write((char*)pfDnm, fLength);
  pfOutFile->write((char*)pfXDlm, fLength);
  pfOutFile->write((char*)pfXDmm, fLength);
  pfOutFile->write((char*)pfXDnm, fLength);
  pfOutFile->write((char*)pfYDlm, fLength);
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
  pfInFile->read((char*)&fAomega, sizeof(float));
  std::cout<<"KSMountDirection:fAomega: "<<fAomega<<" m**2-sr"<<std::endl;
  int fLength=0;
  pfInFile->read((char*)&fLength, sizeof(int));
  if(!pfInFile->good())
    {
      std::cout<<"KSMountDirection--Failed to read fLength."
	       <<std::endl;
      exit(1);
    }
  fNumDirections = fLength/sizeof(double);
  std::cout<<"KSMountDirection:fNumDirections: "<<fNumDirections<<std::endl;
  // ***********************************************************************
  //  Allocate Some direction arrays and load them up.
  // ***********************************************************************
  allocateDirectionArrays(fNumDirections);
  
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
  if(!pfInFile->good())
    {
      std::cout<<"KSMountDirection--Failed when reading Mount Direction Data."
	       <<std::endl;
      exit(1);
    }
  return;
}
// *************************************************************************

void KSMountDirection::allocateDirectionArrays(int fNumDir)
// ********************************************************************
//  Allocate Some direction arrays and load them up.
// ********************************************************************
{
  pfDlm    = new double[fNumDir];
  pfDmm    = new double[fNumDir];
  pfDnm    = new double[fNumDir];
  pfXDlm   = new double[fNumDir];
  pfXDmm   = new double[fNumDir];
  pfXDnm   = new double[fNumDir];
  pfYDlm   = new double[fNumDir];
  pfYDmm   = new double[fNumDir];
  pfYDnm   = new double[fNumDir];
  pfSTheta = new double[fNumDir];
  pfSPhi   = new double[fNumDir];
  return;
}
// *************************************************************************

void KSMountDirection::loadDirectionArrays(int fIthPhi, double fSTheta,
					   double fSPhi, double* fDir)
// ************************************************************************
// Load up the direction arrays.
// ************************************************************************
{
  pfSTheta[fIthPhi]=fSTheta;     //Save theta and phi in radians
  pfSPhi[fIthPhi]=fSPhi;
                                 //Save direction cosigns
  getVector(fDir,pfDlm[fIthPhi],pfDmm[fIthPhi],pfDnm[fIthPhi] );
  
  // *****************************************************************
  // Now X,Y unit vectors in mirror plane (X in focal plane 
  // Perpendicular to zenith and mount direction.
  // *****************************************************************
  double fZenith[3]={0.0,0.0,-1.0};
  double fX[3];
  double fY[3];
  if(fDir[0]==fZenith[0] 
     && fDir[1]==fZenith[1] 
     && fDir[2]==fZenith[2] )
    {
      fX[0]=1.0;//Force to east if we are verticle
      fX[1]=0.0;
      fX[2]=0.0;
    }
  else
    {
      unitCrossProduct(fZenith,fDir,fX);//Give fX in focal plane
    }

  unitCrossProduct(fX,fDir,fY);    //Gives fY in focal plane
  getVector(fX,pfXDlm[fIthPhi],pfXDmm[fIthPhi],pfXDnm[fIthPhi]);
  getVector(fY,pfYDlm[fIthPhi],pfYDmm[fIthPhi],pfYDnm[fIthPhi]);

  //Debug
  double fXProj;
  double fYProj;
  double fDirV[3];
  fDirV[0]=fDir[0];
  fDirV[1]=-fDir[1];  //convert from KASCADE to VEGAS coord system.
  fDirV[2]=-fDir[2];
  GetXYFromVec(fAzMount, fElevMount, fDirV, fXProj, fYProj);

  std::cout<<fIthPhi<<" "<<pfDlm[fIthPhi]<<" "<<pfDmm[fIthPhi]<<" "
	   <<pfDnm[fIthPhi]<<" "<<fXProj<<" "<<fYProj<<std::endl;
  //Enddebug

  return;
}
// ************************************************************************


void W10mGetRaDecFromVec(double* X, double& fRA, double& fDec, 
			                                   double fLatitude)
  // ************************************************************************
  //   Get the Ra and Dec of a vector X at sideraltime=12:00(chosen arbitray 
  //   time)
  // ************************************************************************
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

// ************************************************************************
