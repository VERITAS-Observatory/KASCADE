//-*-mode:c++; mode:font-lock;-*-
/** 
 * \class ksSumFiles
 * \brief This code simulates photons from a point source and writes the 
 *  their final positon in the camera plane to an ouput file.
 *
 * Original Author: Glenn H. Sembroski 
 * $Author$
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

// 15-May-2005
//Modified:



#include <iostream>
#include <iomanip>
#include <fstream>
#include <string>
#include <cmath>
#include <time.h>
#include <exception>

#include "VAConfigurationHelper.h"
#include "VAConfigurationData.h"
#include "VAConfigInfo.h"
#include "VAException.h"
#include "VSOptions.hpp"

#include "KSCommon.h"
#include "KSTiltAndTrace.h"

extern "C" void ranstart(int* printseedflag, char* random_seed_file_name, 
			 int length);
extern "C" void ranend(int* printseedflag, char* random_seed_file_name, 
		       int length);
extern "C" float pran(float* dummy);

void unitCrossProduct(double* X,double* Y,double* Z);
void getVector(double* X,double& x1,double& x2,double& x3);
void findXSegYSeg(double az,double el, double radius, double& xSeg, 
		  double& ySeg);
void usage(const std::string& progname, 
	   const VAOptions& command_line)
{
  std::cout << std::endl;
  std::cout << "makePSF: Usage: " << progname 
	    << " [options]  <Ouput File Name>" 
	    << std::endl;
  std::cout<<"makePSF: Options: "<<std::endl;
  command_line.printUsage(std::cout);
}

int main(int argc, char** argv)
{ 
  try
    {
      time_t thetime=time(NULL);
      std::cout<<"makePSF:  START------"<<ctime(&thetime)<<std::endl;
      
      // **********************************************************************
      // Pick up command line arguments
      // **********************************************************************
      std::string progname = *argv;
      VAOptions command_line(argc,argv);

      int fNumPhotons=10000;
      if(!command_line.findWithValue("NumberOfPhotons",fNumPhotons,
			   "Number of Photons to generate. Defaults to "
			   "10000.")
	 == VAOptions::FS_FOUND)
	{
	fNumPhotons=10000;
	}
      std::cout<<"makePSF: Number of Photons to Simulate: "<<fNumPhotons
	       <<std::endl;

      double fSourceAzDeg=0;
      double fSourceZenithDeg=0;

      if(!command_line.findWithValue("SourceAzDeg",fSourceAzDeg,
			   "Source Azimuth in Deg, Defaults to 0 Deg.")
	 == VAOptions::FS_FOUND)
	{
	  fSourceAzDeg=0;
	}
      std::cout<<"makePSF: Source Azimuth(Deg): "<<fSourceAzDeg
	       <<std::endl;
 

     if(!command_line.findWithValue("SourceZenithDeg",fSourceZenithDeg,
			   "Source Zenith angle in Deg, Defaults to 0 Deg.")
	 == VAOptions::FS_FOUND)
	{
	  fSourceZenithDeg=0;
	}
      std::cout<<"makePSF: Source Zenith(Deg): "<<fSourceZenithDeg
	       <<std::endl;

      double fTelescopeAzDeg=0;
      double fTelescopeZenithDeg=0;
      if(!command_line.findWithValue("TelescopeAzDeg",fTelescopeAzDeg,
			   "Telescope Azimuth in Deg, Defaults to 0 Deg.")
	 == VAOptions::FS_FOUND)
	{
	  fTelescopeAzDeg=0;
	}
      std::cout<<"makePSF: Telescope Azimuth(Deg): "<<fTelescopeAzDeg
	       <<std::endl;
 
     if(!command_line.findWithValue("TelescopeZenithDeg",fTelescopeZenithDeg,
			   "Telescope Zenith angle in Deg, Defaults to 0 Deg.")
	 == VAOptions::FS_FOUND)
	{
	  fTelescopeZenithDeg=0;
	}
      std::cout<<"makePSF: Telescope Zenith(Deg): "<<fTelescopeZenithDeg
	       <<std::endl;


      double fPSFNSDeg=0;
      double fPSFEWDeg=0;
      if(!command_line.findWithValue("PSFNorthSouthDeg",fPSFNSDeg,
				     "PSF NorthSouth Jitter of PSF. Defaults "
				     "to 0 deg.")
	 == VAOptions::FS_FOUND)
	{
	  fPSFNSDeg=0;
	}

      std::cout<<"makePSF: PSF NorthSouth Jitter Deg): "<<fPSFNSDeg<<std::endl;


      if(!command_line.findWithValue("PSFEastWestDeg",fPSFEWDeg,
				     "PSF East-West Jitter of PSF. Defaults "
				     "to 0 deg.")
	 == VAOptions::FS_FOUND)
	{
	  fPSFEWDeg=0;
	}

      std::cout<<"makePSF: PSF East-West Jitter Deg): "<<fPSFNSDeg<<std::endl;

      std::string fRandomSeedFileName;
      if(!command_line.findWithValue("RandomSeedFileName",fRandomSeedFileName,
				     "Ranlux seed file")
	 == VAOptions::FS_FOUND)
	{
	  fRandomSeedFileName="makePSF.ran";
	}


      // --------------------------------------------------------------------
      // Test for "help" options
      // --------------------------------------------------------------------
      bool help=false;
      if(command_line.find("help","Print this message") != 
	 VAOptions::FS_NOT_FOUND) 
	{
	  help = true;
	}
      if(command_line.find("h","Print this message") != 
	 VAOptions::FS_NOT_FOUND) 
	{
	  help = true;
	}
      // ---------------------------------------------------------------------
      // All the command line options that the program is able to  handle 
      // have been processed, so make sure there are no more command lines
      // options available.
      // ---------------------------------------------------------------------

      if(!command_line.assertNoOptions())
	{
	  std::cerr << progname << ": unknown options: ";
	  for(int i=1;i<argc;i++)std::cerr << argv[i];
	  std::cerr << std::endl;
	  std::cerr << std::endl;
	  usage(progname, command_line);
	  exit(EXIT_FAILURE);
	}
 
      //Ouput file name: Left on the command line should now be only the 
      // program name and Ouput file name. Get it.
      argv++;
      argc--;
      if(argc!=1)
	{
	  std::cout<<"ksTrigger: More than 1 argument given. Assume -help "
	    "reqested"<<std::endl;
	  usage(progname, command_line);
	  exit(EXIT_FAILURE);
	}
      std::string outputFileName=*argv;

      // --------------------------------------------------------------------
      // Print usage if help is requested
      //---------------------------------------------------------------------
      if(help)
	{
	  usage(progname, command_line);
	  exit(EXIT_SUCCESS);
	}

      // ******************************************************************
      // Set up random number generator
      // ******************************************************************
      int printseeds=1;
      ranstart(&printseeds,(char*)fRandomSeedFileName.c_str(),
	       (int)fRandomSeedFileName.length());
       
      std::cout<<"makePSF:Output file name: "<<outputFileName
	       <<std::endl;


      // ******************************************************************
      // set up the output file. Add a header for root TTree process ease
      // ********************************************************************
      std::ofstream out(outputFileName.c_str());
      out<<"WX/F:WY/F:Time/F"<<std::endl;
   
      // ********************************************************************
      // Set up constant parameters for KSTiltAndTrac 
      // (and whippletilt is need be)
      // Cheat a little: Get values from ksTrigger VERITAS run
      double fMirrorRadius=6.0; 

      double fMinimumDnTight           = 0.998991154907304; 
      double fMinimumDnLoose           = 0.998055261938139; 
      double fMirrorRadiusSquared      = fMirrorRadius*fMirrorRadius;
      double fFacetDiameterM           = 0.61;
      double fFocalLengthM             = 12.0;
      double fJitterWidthEastWestRad   = (fPSFEWDeg/2.)*gDeg2Rad;
      double fJitterWidthNorthSouthRad = (fPSFNSDeg/2.)*gDeg2Rad;
      double fMetersPerDeg             = 0.2099234293685;
 

      KSTiltAndTrace fTiltAndTrace ( fMinimumDnTight, fMinimumDnLoose, 
				      fMirrorRadiusSquared,fFacetDiameterM,
				      fFocalLengthM,fJitterWidthEastWestRad,
				      fJitterWidthNorthSouthRad,fMetersPerDeg);

      // ******************************************************************
      // Now set up all the other things we will need
      // ******************************************************************
      // Mount direction
      double fTDlm=0;
      double fTDmm=0;
      double fTDnm=1.0;

      if( fTelescopeAzDeg >= 0.0 && fTelescopeZenithDeg >= 0.0 )
	{
	  fTDlm = sin(fTelescopeZenithDeg*gDeg2Rad) * 
                                                sin(fTelescopeAzDeg*gDeg2Rad);
	  fTDmm = -sin(fTelescopeZenithDeg*gDeg2Rad) * 
                                                cos(fTelescopeAzDeg*gDeg2Rad);
	  fTDnm = -cos(fTelescopeZenithDeg*gDeg2Rad);

	  if( fabs(fTDlm) < 1.e-15)
	    {
	      fTDlm = 1.e-15;
	    }
	  if( fabs(fTDmm) < 1.e-15)
	    {
	      fTDmm = 1.e-15;
	    }
	  if( fabs(fTDnm) < 1.e-15)
	    {
	      fTDnm = 1.e-15;
	    }
	}
      else
	{
	  std::cout<<"makePSF: Illegal values for Telescope Azimuth or Zenith"
		   <<std::endl;
	  exit(0);
	}

      // *****************************************************************
      // Now X,Y unit vectors in mirror plane (X in focal plane 
      // Perpendicular to zenith and mount direction.
      // *****************************************************************
      double fZenith[3]={0.0,0.0,-1.0};
      double fX[3];
      double fY[3];
      double fDir[3];
      fDir[0]=fTDlm;
      fDir[1]=fTDmm;
      fDir[2]=fTDnm;
      
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
      double fXDl;
      double fXDm;
      double fXDn;
      getVector(fX,fXDl,fXDm,fXDn);
      
      double fYDl;
      double fYDm;
      double fYDn;
      getVector(fY,fYDl,fYDm,fYDn);

      // *************************************************************
      // Source location direction(photon direction actually)
      double fDlp=0;
      double fDmp=0;
      double fDnp=0;

      if( fSourceAzDeg >= 0.0 && fSourceZenithDeg >= 0.0 )
	{
	  fDlp =-sin(fSourceZenithDeg*gDeg2Rad) * sin(fSourceAzDeg*gDeg2Rad);
	  fDmp = sin(fSourceZenithDeg*gDeg2Rad) * cos(fSourceAzDeg*gDeg2Rad);
	  if(fabs(fDlp)<1.e-15)
	    {
	      fDlp=1.e-15;
	    }
	  if(fabs(fDmp)<1.e-15)
	    {
	      fDmp=1.e-15;
	    }
	  fDnp = sqrt(1.-fDlp*fDlp-fDmp*fDmp);
	}
      else
	{
	  std::cout<<"makePSF: Illegal values for Photon source Azimuth or "
	    "Zenith" <<std::endl;
	  exit(0);
	}

      double fTime=0.0;

      // *******************************************************************
      // Assign stuff thats constant photon to photon
      // *******************************************************************
      fTiltAndTrace.fDlr=fDlp;
      fTiltAndTrace.fDmr=fDmp;
      fTiltAndTrace.fDnr=fDnp;
      
      fTiltAndTrace.fTDlm=fTDlm;
      fTiltAndTrace.fTDmm=fTDmm;
      fTiltAndTrace.fTDnm=fTDnm;
      
      fTiltAndTrace.fXDl=fXDl;
      fTiltAndTrace.fXDm=fXDm;
      fTiltAndTrace.fXDn=fXDn;
      
      fTiltAndTrace.fYDl=fYDl;
      fTiltAndTrace.fYDm=fYDm;
      fTiltAndTrace.fYDn=fYDn;
      
      fTiltAndTrace.fTime=fTime;
      
      // *****************************************************************
      // In  the main loop. All we really vary is the position on the 
      // ground. Lets let the program decide if we hit or not.
      // So first get the gound xseg,yseg
      // *****************************************************************
      double fTelescopeElDeg=90.0-fTelescopeZenithDeg;
      double fXSeg;
      double fYSeg;
      findXSegYSeg(fTelescopeAzDeg,fTelescopeElDeg,fMirrorRadius,fXSeg,fYSeg);

      // ********************************************************************
      // Here we go!
      // ********************************************************************
      int countGood=0;
      float fDummy;
      for(int i=0;i<fNumPhotons;i++)
	{
	  fTiltAndTrace.fXg=fXSeg*(pran(&fDummy)-.5);
	  fTiltAndTrace.fYg=fYSeg*(pran(&fDummy)-.5);
	  int dump=fTiltAndTrace.Tilt();	  
	  if(dump==0)
	    {
	      out<< fTiltAndTrace.fW[0]<<" "<<fTiltAndTrace.fW[1]
		       <<" "<< fTiltAndTrace.fPeTime<<std::endl;
	      countGood++;
	    }
	}
     
      std::cout<<"makePSF: End of Run at: "<<ctime(&thetime)<<std::endl;
      std::cout<<"makePSF: "<<countGood<<" good events out of: "<<fNumPhotons
	       <<std::endl;

      // ----------------------------------------------------------------------
      // Save the random number generator seeds.
      // ----------------------------------------------------------------------
      ranend(&printseeds,(char*)fRandomSeedFileName.c_str(),
	     (int)fRandomSeedFileName.length());
      return 0;
    }
  
  catch (const std::exception &e) 
    {
      std::cerr<<"Error: "<<e.what()<<std::endl;
      return 1;
    }
  catch(...)
    {
      std::cout<<"ksSumFiles - Fatal--Unknown exception found."
	       <<std::endl;
      return 1;
    }
}
// **************************************************************************

void unitCrossProduct(double* X,double* Y,double* Z)
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
// ***************************************************************************
void findXSegYSeg(double az,double el, double radius, double& xSeg, 
		  double& ySeg)
// *************************************************************************
// For a mirror with radius(meters) pointing at az,el(deg) 
// (az=0: Y+, az=90: X+)
// Find grid square (meters) that will fit the elliptical shadow of the mirror
// *************************************************************************
// Written by:
//  Ben Zitzer
//  Physics Dept.
//  Purdue Univ
// West Lafayette, In 47907
// zitzer@purdue.edu
// Jan 29, 2008
// ***************************************************************************
{
  // **************************************************************
  // Check parametrers
  // **************************************************************
  while (az>=360)
    {
      az=az-360.0;
    }
  if(el>90)          //set limits on elevation
    {
      el=90.0;
    }
  if(el<=0.0)
    {
      el=1.0;
    }

  double zn=90.0-el;
  double azRad=az*gDeg2Rad;
  double znRad=zn*gDeg2Rad;
  
  double a=cos(azRad)*cos(azRad) + sin(azRad)*sin(azRad)*cos(znRad)*cos(znRad);
  double b=2*cos(azRad)*sin(azRad)*sin(znRad)*sin(znRad);
  double c=sin(azRad)*sin(azRad)+cos(azRad)*cos(azRad)*(cos(znRad)*cos(znRad));
  
  ySeg=2*radius*sqrt(1.0/(c-b*b/(4.0*a)));
  xSeg=2*radius*sqrt(1.0/(a-b*b/(4.0*c)));

  //std::cout<<"radiusSqr; "<<radiusSqr<<std::endl;
  //std::cout<<"a,b,c: "<<a<<" "<<b<<" "<<c<<std::endl;
  //std::cout<<"Xseg: "<<xseg<<" Yseg: "<<yseg<<std::endl;
  return;
}
// **************************************************************************

void getVector(double* X,double& x1,double& x2,double& x3)
// *************************************************************************
//         Copy a vector into components
// *************************************************************************
{
  x1=X[0];
  x2=X[1];
  x3=X[2];
  return;
}
