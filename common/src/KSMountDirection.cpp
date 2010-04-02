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

#include <cstdlib>
#include "KSMountDirection.h"

extern "C" float pran(float* fXDummy);

KSMountDirection::KSMountDirection(KSTeHeadData* pTeHead, 
				   double GammaStepSizeRad)
{
  pfTeHead                 = pTeHead;
  fGammas2D                = pfTeHead->fGammas2D;
  fMultipleMountDirections = pfTeHead->fMultipleMountDirections;
  fNumDirections           = pfTeHead->fNumDirections;
  fStepSizeRad             = GammaStepSizeRad;
  fCameraType              = pfTeHead->fCameraType;
  // Set up conversion utility class. We won't usde for RA/Dec calculations 
  // so the the EastLongitude and Latitude values we give are arbitrary.
  // So just use Base camp
  pfConvert = new KSAzElRADecXY(-1.93649,0.552828);
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
//
//fMultipleMountDirections:Flag that indicates we will be looking for 
//                        triggers for each event multiple times with 
//                        the mount pointed in different directions, 
//                        usually this is false for gammas and true for
//                        everything else (but see below for Gammas2D)
//fMaximumThetaRad:   Nominally maximum distance from the inital Mount
//                   direction that we will go
//fStepSizeRad:Used only by Gammas2D as the size of steps we will take.
//fNumDirections:Number of directions we will use for each event.
//
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
//  For Gammas2D  we go  in steps in Y out to radius MaxThetaDeg
//  This is a special mode for the ML Tracking anaylsis and inital Wobble 
//  source generation
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
  pfConvert->DlDmDnToAzEl(fMntV[0],fMntV[1],fMntV[2],fAzMount,fElevMount);


  // *******************************************************************
  // Gammas2D. Array in  Y in steps of  fStepSizeDeg out to fMaxThetaDeg.
  // *******************************************************************
  if(fGammas2D)
    {
      // For the Maximum Likelihood Gamma source Point Spread Function(PSF) 
      // calculation we set up directions along the Y axis. This is also used 
      // for the simulation of South Wobble offsets for vegas.
      // Starting at 0 in field of view and stepping 
      // fStepSizeRad (converted back to deg) in positive steps in Y out to 
      // fMaximumThetaRad (again back in deg). That is we only do
      // the positive Axis in Y. 
      // For ML signal histo generation we can then make a grid over all 
      // field of view by rotations.
      // ********************************************************************
      // Values for fMaximumThetaRad and fStepSizeRad derived from original
      // deg from config file: MaximumThetaDeg, GammaStepSizeDeg
      // *******************************************************************
      
      fMaxThetaDeg=pfTeHead->fMaximumThetaRad/gDeg2Rad;
      fStepSizeDeg=fStepSizeRad/gDeg2Rad;
      fNumYSteps=(int)(fMaxThetaDeg/fStepSizeDeg + fStepSizeDeg/2.) + 1;
      fMultipleMountDirections=true;
      fAomega=fXAreaWidthM*fYAreaWidthM;
      std::cout<<"KSMountDirection:fAomega: "<<fAomega<<" m**2"<<std::endl;

      std::vector< double > pfY;
      pfY.clear();
      double fX=0.0;
      double fY=0.0;
      for(int i=0;i<fNumYSteps;i++)
	{
	  fY=i*fStepSizeDeg;
	  pfY.push_back(fY);
	}

      fNumDirections=(int) pfY.size();
      allocateDirectionArrays(fNumDirections);

      double fM[3];
      for(int i=0;i<fNumDirections;i++)
	{
	  fX=0.0;
	  fY=pfY.at(i);
	  pfConvert->XYToDlDmDn(fX,fY,fAzMount,fElevMount,fM[0],fM[1],fM[2]);

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
  pfConvert->DlDmDnToXY(fDirV[0],fDirV[1],fDirV[2],fAzMount, fElevMount,
			fXProj, fYProj);
  
  std::cout<<fIthPhi<<" "<<pfDlm[fIthPhi]<<" "<<pfDmm[fIthPhi]<<" "
  	   <<pfDnm[fIthPhi]<<" "<<fXProj<<" "<<fYProj<<std::endl;
  //Enddebug

  return;
}
// ************************************************************************
