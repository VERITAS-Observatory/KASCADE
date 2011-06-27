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
using namespace std;

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
void usage(const string& progname, 
	   const VAOptions& command_line)
{
  cout << endl;
  cout << "makePSF: Usage: " << progname 
	    << " [options] " 
	    << endl;
  cout<<"makePSF: Options: "<<endl;
  command_line.printUsage(cout);
}

int main(int argc, char** argv)
{ 
  try
    { 
      //Input is usually a configuration file
      //We start with defining and setting up the configuration for this
      // processing.
      time_t thetime=time(NULL);
      cout<<"makePSF:  START------"<<ctime(&thetime)<<endl;
      
      // **********************************************************************
      // Pick up command line arguments
      // **********************************************************************
      string progname = *argv;
      VAOptions command_line(argc,argv,true);
      VAConfigInfo config_file;
    // ------------------------------------------------------------------------
    // Test for "load configuration file" options
    // ------------------------------------------------------------------------

      bool load_config = false;
      string load_filename;
      if(command_line.findWithValue("config",load_filename,
			     "Load Configuration File")==VAOptions::FS_FOUND)
	{
	  load_config = true;	
	}

    // ------------------------------------------------------------------------
    // Test for "save configuration file" options
    // ------------------------------------------------------------------------

      bool save_config = false;
      bool only_save_config = false;
      string save_filename;
      if(command_line.findWithValue("save_config",save_filename,
				  "Save a configuration file with all "
				  "configuration values before processing.")
	 == VAOptions::FS_FOUND){
	save_config=true;
      }
      if(command_line.findWithValue("save_config_and_exit",save_filename,
			      "Save a configuration file with all "
			      "configuration values and immediately exit.")
	 == VAOptions::FS_FOUND){
	only_save_config=true;
      }
      // ---------------------------------------------------------------------

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



      int fNumPhotons=10000;
      double fSourceAzDeg=0;
      double fSourceZenithDeg=0;
      double fTelescopeAzDeg=0;
      double fTelescopeZenithDeg=0;
      double fPSFNSDeg=0;
      double fPSFEWDeg=0;
      string fRandomSeedFileName="makePSF.ran";
      string fOutputFileName=" ";
      double fDistanceToSourceKM=-1;
      double fFocalPlaneLocationM=12.0;
      double fAlignmentPlaneLocationM=-1;//Flag that we need to use default
                                         //(fFocalPlaneLocationM)
      string fFacetAlignmentMethod="MCGILL";
      string fFacetLocationFileName=" ";

      doVAConfiguration(config_file, command_line,"NumberOfPhotons",
			fNumPhotons,"makePSF", 
			"Number of Photons to generate. Defaults "
			"to 10000.");

      doVAConfiguration(config_file, command_line,"SourceAzDeg",fSourceAzDeg,
			"makePSF","Source Azimuth in Deg, Defaults to 0 Deg.");

      doVAConfiguration(config_file, command_line,"SourceZenithDeg",
			fSourceZenithDeg,"makePSF",
			"Source Zenith angle in Deg, Defaults to 0 Deg.");

      doVAConfiguration(config_file, command_line,"TelescopeAzDeg",
			fTelescopeAzDeg,"makePSF",
			"Telescope Azimuth in Deg, Defaults to 0 Deg.");

      doVAConfiguration(config_file, command_line,"TelescopeZenithDeg",
			fTelescopeZenithDeg,"makePSF",
			"Telescope Zenith angle in Deg, Defaults to 0 Deg.");

      doVAConfiguration(config_file, command_line,"PSFNorthSouthDeg",fPSFNSDeg,
			"makePSF",
			"PSF NorthSouth Jitter of PSF. Defaults to 0 deg.");

      doVAConfiguration(config_file, command_line,"PSFEastWestDeg",fPSFEWDeg,
			"makePSF",
			"PSF East-West Jitter of PSF. Defaults to 0 deg.");

      doVAConfiguration(config_file, command_line,"DistanceToSourceKM",
			fDistanceToSourceKM,"makePSF",
			"Distance to point source from telescope in km. -1 "
			"indicates infinity (a star). This is for modeling "
			"optics of shower images (which are typically at ~ "
			"10 km). Default is -1 (infinity).");

      doVAConfiguration(config_file, command_line,"FocalPlaneLocationM",
			fFocalPlaneLocationM,"makePSF",
			"Distance from center of Veritas mirror to focal "
			"plane. This allows us to move the focal plane when "
			"we model focusing when looking at shower images. "
			"Default is Veritas mirror focus length of 12.0000 "
			"meters");
 
      doVAConfiguration(config_file, command_line,"AlignmentPlaneLocationM",
			fAlignmentPlaneLocationM,"makePSF",
			"Distance from center of Veritas mirror to the "
			"plane to be used for alignment for MCGILL Method. "
			"Not used with WHIPPLE alignment method. Typically "
			"this distance is the same as FocalPlaneLocationM "
			"(and defualts to it) but it can be set to something "
			"different. This is used when modeling "
			"\"defocusing\" the telescope to bring shower max "
			"into better focus.");

      doVAConfiguration(config_file, command_line,"MirrorAlignmentMethod",
			fFacetAlignmentMethod,"makePSF",
			"Mirror alignment method to simulate. Options are the "
			"original WHIPPLE method or the new (as of spring "
			"2009) MCGILL method. The default is MCGILL");

      doVAConfiguration(config_file, command_line,"FacetLocationFileName",
			fFacetLocationFileName,"makePSF",
			"A \".txt\" file with the locations (in meters) of "
			"all facets (including those locations that are not "
			"used, like the ones at the center of the Mirror). "
			"This file is TTree::ReadFile() compatable with a "
			"header line that includes the variables XM,YM and "
			"EXIST. For example, the line might look like: "
			"\"FACETID/I:XM/F:YM:EXIST\". The variable EXIST has "
			"a value of 1.0 if the facet exists or 0.0 if not. "
			"If file name is not specified then the default of "
			"using the Hillas method of randomly generating "
			"facets locations for each photon is used.");

      doVAConfiguration(config_file, command_line,"RandomSeedFileName",
			fRandomSeedFileName,"makePSF","File name where we "
			"store the seed for the Ranlux random number "
			"generator.");

      doVAConfiguration(config_file, command_line,"OutputFileName",
			fOutputFileName,"makePSF",
			"Output .txt filename, File used as "
			"input to fill a root TTree using the "
			"TTree::ReadFile() method.");



      // ---------------------------------------------------------------------
      // All the command line options that the program is able to  handle 
      // have been processed, so make sure there are no more command lines
      // options available.
      // ---------------------------------------------------------------------


      // ----------------------------------------------------------------------
      // All the command line options that the program is able to  handle have
      // been processed, so make sure there are no more command lines options
      // available.
      // ----------------------------------------------------------------------
      if(!command_line.assertNoOptions())
	{
	  cerr << progname << ": unknown options: ";
	  for(int i=1;i<argc;i++)cerr << argv[i];
	  cerr << endl;
	  cerr << endl;
	  usage(progname, command_line);
	  exit(EXIT_FAILURE);
	}
 
      if(argc!=1)
	{
	  cout<<"ksTrigger: More than 0 argument given. Assume -help "
	    "reqested"<<endl;
	  usage(progname, command_line);
	  exit(EXIT_FAILURE);
	}


      // --------------------------------------------------------------------
      // Print usage if help is requested
      //---------------------------------------------------------------------
      if(help)
	{
	  usage(progname, command_line);
	  exit(EXIT_SUCCESS);
	}
      // ---------------------------------------------------------------------
      // Load the configuration file if we have been asked to
      // ---------------------------------------------------------------------

      if(load_config){
	config_file.loadConfigFile(load_filename);
      }
      // ---------------------------------------------------------------------
      // Save the configuration file if we have been asked to
      // ---------------------------------------------------------------------
      if((save_config)||(only_save_config)){
	config_file.saveConfigFile(save_filename);
      }
      if(only_save_config){
	exit(EXIT_SUCCESS);
      }

      // *******************************************************************
      // Check validity of input parameters
      // *******************************************************************
      if(fNumPhotons<=0){
	cout<<"makePSF:NumberOfPhotons option must be >0"<<endl;
	usage(progname, command_line);
	exit(EXIT_FAILURE);
      }
      else{
	cout<<"makePSF: Number of Photons to Simulate: "<<fNumPhotons
	       <<endl;
      }
      
      if(fSourceAzDeg<0 ||fSourceAzDeg>360.0){
	cout<<"makePSF:SourceAzDeg out of range (0,360)"<<endl;
	usage(progname, command_line);
	exit(EXIT_FAILURE);
      }
      else{
	cout<<"makePSF: Source Azimuth(Deg): "<<fSourceAzDeg
		 <<endl;
      }

      if(fSourceZenithDeg<0 ||fSourceAzDeg>90.0){
	cout<<"makePSF:SourceZenithDeg out of range (0,90)"<<endl;
	usage(progname, command_line);
	exit(EXIT_FAILURE);
      }
      else{
	cout<<"makePSF: Source Zenith(Deg): "<<fSourceZenithDeg
		 <<endl;
      }

      if(fTelescopeAzDeg<0 ||fTelescopeAzDeg>360.0){
	cout<<"makePSF:TelescopeAzDeg out of range (0,360)"<<endl;
	usage(progname, command_line);
	exit(EXIT_FAILURE);
      }
      else{
	cout<<"makePSF: Telescope Azimuth(Deg): "<<fTelescopeAzDeg
		 <<endl;
      }

      if(fTelescopeZenithDeg<0 ||fTelescopeAzDeg>90.0){
	cout<<"makePSF:TelescopeZenithDeg out of range (0,90)"<<endl;
	usage(progname, command_line);
	exit(EXIT_FAILURE);
      }
      else{
	cout<<"makePSF: Telescope Zenith(Deg): "<<fTelescopeZenithDeg
		 <<endl;
      }
      
      if(fPSFNSDeg<0){
	cout<<"makePSF:PSFNorthSouthDeg must be >= 0"<<endl;
	usage(progname, command_line);
	exit(EXIT_FAILURE);
      }
      else{
	cout<<"makePSF: PSF NorthSouth Jitter Deg): "<<fPSFNSDeg
		 <<endl;
      }
      
      if(fPSFEWDeg<0){
	cout<<"makePSF:PSFEastWestDeg must be >= 0"<<endl;
	usage(progname, command_line);
	exit(EXIT_FAILURE);
      }
      else{
	cout<<"makePSF: PSF East-West Jitter Deg): "<<fPSFNSDeg
		 <<endl;
      }


      if(fDistanceToSourceKM>0){
	cout<<"makePSF: Distance to point source from mirror center(KM):"
		 <<fDistanceToSourceKM<<endl;
      }
      else{
	cout<<"makePSF: Point source is at infinity (a star!)"
		 <<endl;
      }

      if(fFacetLocationFileName == " "){
	cout<<"makePSF: Using default Hillas Facet location method"<<endl;
      }
      else{
	cout<<"MakePSF: Obtaining mirror facet locations from file: "
	    <<fFacetLocationFileName<<endl;
      }

      int alignment=-1;
      if(fFacetAlignmentMethod=="WHIPPLE"){
	alignment=0;
      }
      else if(fFacetAlignmentMethod=="MCGILL"){
	alignment=1;
      }

      if(alignment!=-1){
	cout<<"makePSF: Using "<<fFacetAlignmentMethod
		 <<" mirror alignment method"<<endl;
      } 
      else{
	cout<<"makePSF: Unknown mirror alignment method specified:"
		 << fFacetAlignmentMethod<<":"<<endl;
	cout<<"makePSF: Allowable mirror alignment methods are: WHIPPLE "
	           "or MCGILL"<<endl;
	exit(EXIT_FAILURE);
      }



      cout<<"makePSF: Distance from focal plane to center of "
	         "mirror (meters):"
	       <<fFocalPlaneLocationM<<endl;
      
      if(fFacetAlignmentMethod=="MCGILL"){
	
	if(fAlignmentPlaneLocationM<0){
	  fAlignmentPlaneLocationM=fFocalPlaneLocationM;
	}
	cout<<"makePSF: Distance from Facet alingment plane to center "
	           "of mirror (meters):"<<fAlignmentPlaneLocationM
		 <<endl;
     	cout<<"makePSF: Facet alingment plane used with MCGILL alignment "
                   "method only:"<<endl;
      }
      
      cout<<"makePSF: Random Seed file name: "<<fRandomSeedFileName
	       <<endl;

      if(fOutputFileName==" "){
	cout<<"makePSF:OutputFileName must be specified"<<endl;
	usage(progname, command_line);
	exit(EXIT_FAILURE);
      }
      else{
	cout<<"makePSF: Output .txt file name: "<<fOutputFileName
		 <<endl;
      }
      // ******************************************************************



      // ******************************************************************
      // Set up random number generator
      // ******************************************************************
      int printseeds=1;
      ranstart(&printseeds,(char*)fRandomSeedFileName.c_str(),
	       (int)fRandomSeedFileName.length());
       


      // ******************************************************************
      // set up the output file. Add a header for root TTree process ease
      // ********************************************************************
      ofstream out(fOutputFileName.c_str());
      out<<"ZnOffsetDeg/F:focalPlaneM:Alignment/I:SourceZKm/F:JitterXDeg:"
	   "JitterYDeg:WX/F:WY/F:TimeNS/F:TimeMirNS/F:XGndM/F:YGndM/F:"
	   "XMirM/F:YMirM/F"<<endl;
   
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
      double fMetersPerDeg             = 1./(atan(1./fFocalLengthM)*gRad2Deg);
      //double fMetersPerDeg             =0.2099234293685;
 

      KSTiltAndTrace fTiltAndTrace ( fMinimumDnTight, fMinimumDnLoose, 
				     fMirrorRadiusSquared,fFacetDiameterM,
				     fFocalLengthM,fJitterWidthEastWestRad,
				     fJitterWidthNorthSouthRad,fMetersPerDeg,
				     fFocalPlaneLocationM,
				     fAlignmentPlaneLocationM,
				     fFacetAlignmentMethod,
				     fFacetLocationFileName);

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
	  cout<<"makePSF: Illegal values for Telescope Azimuth or Zenith"
		   <<endl;
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
      
      if(fDir[2]<-1.0+1.e-15)
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
	  cout<<"makePSF: Illegal values for Photon source Azimuth or "
	    "Zenith" <<endl;
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
      float ZnOffsetDeg=fSourceZenithDeg-fTelescopeZenithDeg;

      for(int i=0;i<fNumPhotons;i++)
	{
	  fTiltAndTrace.fXg=fXSeg*(pran(&fDummy)-.5);
	  fTiltAndTrace.fYg=fYSeg*(pran(&fDummy)-.5);
	  // ************************************************************
	  // If our point source is not at infinity (not a star) bur rather 
	  // closer (say at some shower max) the photon direction needs to be
	  // changed.
	  // A vector from the new source position to the origin on the gorund
	  // will be the photon direction unit vector 
	  // direction of the source times the distance from the source to the
	  // gorund.
	  // *************************************************************
	  if(fDistanceToSourceKM>0){

	    double fSourcedl=fDlp*fDistanceToSourceKM;
	    double fSourcedm=fDmp*fDistanceToSourceKM;
	    double fSourcedn=fDnp*fDistanceToSourceKM;
	    // ********************************
	    // A vector from the source to where the photon hits the ground
	    // will be this vector plus the xseg,yseg vector (in the ground 
	    // plane)Rmemeber fXg and fYg are in meters!!!
	    // ********************************
	    //Note we need use of telescope pointing here ok as is only for 
	    //zenith

	    fSourcedl=fSourcedl+fTiltAndTrace.fXg/1000.;
	    fSourcedm=fSourcedm+fTiltAndTrace.fYg/1000.;
	    
	    // ********************************
	    // Make this a unit vector to get the new photon direction vector
	    // ********************************

	    double fLength=sqrt( fSourcedl*fSourcedl+
				 fSourcedm*fSourcedm+
				 fSourcedn*fSourcedn);
	    fTiltAndTrace.fDlr=fSourcedl/fLength;
	    fTiltAndTrace.fDmr=fSourcedm/fLength;
	    fTiltAndTrace.fDnr=fSourcedn/fLength;
	  }

	  // **********************************************
	  // Find where this ophton hits the focal plane.
	  // This is where all the work gets done
	  // **********************************************
	  int dump=fTiltAndTrace.Tilt();	  
	  if(dump==0) //Did we hit focal plane?
	    {
	      out<<ZnOffsetDeg            <<" "<<fFocalPlaneLocationM<<" "
		 <<alignment              <<" "<<fDistanceToSourceKM<<" "
		 <<fPSFEWDeg              <<" "<<fPSFNSDeg<<" "
		 <<fTiltAndTrace.fW[0]    <<" "<<fTiltAndTrace.fW[1]<<" "
		 <<fTiltAndTrace.fPeTime  <<" "<<fTiltAndTrace.fPeTimeTilt<<" "
		 <<fTiltAndTrace.fXg      <<" "<<fTiltAndTrace.fYg<<" "
		 <<fTiltAndTrace.fPe.at(0)<<" "<<fTiltAndTrace.fPe.at(1)
		 <<endl;
	      countGood++;
	    }
	}
     
      cout<<"makePSF: End of Run at: "<<ctime(&thetime)<<endl;
      cout<<"makePSF: "<<countGood<<" good events out of: "<<fNumPhotons
	       <<endl;

      // ----------------------------------------------------------------------
      // Save the random number generator seeds.
      // ----------------------------------------------------------------------
      ranend(&printseeds,(char*)fRandomSeedFileName.c_str(),
	     (int)fRandomSeedFileName.length());
      return 0;
    }
  
  catch (const exception &e) 
    {
      cerr<<"Error: "<<e.what()<<endl;
      return 1;
    }
  catch(...)
    {
      cout<<"ksSumFiles - Fatal--Unknown exception found."
	       <<endl;
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

  //cout<<"radiusSqr; "<<radiusSqr<<endl;
  //cout<<"a,b,c: "<<a<<" "<<b<<" "<<c<<endl;
  //cout<<"Xseg: "<<xseg<<" Yseg: "<<yseg<<endl;
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
