//-*-mode:c++; mode:font-lock;-*-
/**
 * \class ksLight
 * \ingroup parametrization
 * \brief This code parses command line, loads config files as needed and runs
 * the shower generation routine kskaslitemain.for
 * 
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

// 03-March-2005
//Modified:
#include "VAConfigurationHelper.h"
#include "VAConfigurationData.h"
#include "VAConfigInfo.h"
#include "VAException.h"
#include "VSOptions.hpp"
#include "KSLightDataIn.h"
#include "KSSegmentFile.h"
#include "KSPeFile.h"
#include "KSSegmentDataClasses.h"
#include "KSPeDataClasses.h"
#include "KSAzElRADecXY.h"
#include "KSCommon.h"



#ifdef KSGRISU
     #include "KSGrISUPilotFiles.h"
#endif

#include "stdint.h"
#include <time.h>
#include  <string>
using namespace std;

extern "C" void ranstart(int* printseedflag, char* random_seed_file_name, 
			 int length);
extern "C" void ranend(int* printseedflag, char* random_seed_file_name, 
			 int length);
extern "C" float pran(float* dummy);

extern "C" void kskaslitemain(bool* WhippleMount,   bool* VeritasMount, 
			      bool* TriangularGrid, bool* SquareGrid, 
			      bool* NorthSouthGrid, bool* RandomCoreOffset, 
			      bool* WhipplePMTs,    bool* VeritasPMTs, 
			      bool* ADPPMTs,        bool* benchmark_flag, 
			      float* dli, float* dmi, float* dni, float* hobs,
			      int* itype, float* tep, float* xseg, 
			      float* yseg, float* xoff, float* yoff, int* id,
			      float* efficency);

extern "C" void kssegmentread(bool* goodread, float* XStart, float* YStart, 
			      float* HStart, float* DlStart, float* DmStart,
			      float* EndTime, float* HEnd, float* DlEnd, 
			      float* DmEnd, float* Gamma, int* Spec);

extern "C" void kspewrite(int* nx, int* ny, float* time, float* dl, 
			  float* dm, int* id, float* x, float* y, int* type,
			  float* lambda, float* emalt);
extern "C" void ksreadwavelengthtable(char* fTableName, float* fTable);

void findXSegYSeg(double az,double el, double radius, double& xSeg, 
		  double& ySeg);


KSLightDataIn*     pfDataIn;

KSSegmentFile*     pfSegFile;
KSSegmentHeadData* pfSegmentHead;
KSSegmentData*     pfSegment;

KSPeFile*      pfPeFile;
KSPeData*      pfPe;
KSPeHeadData*  pfPeHead;

void usage(const std::string& progname, const VAOptions& command_line)
{
  std::cout << std::endl;
  std::cout << "ksLight: Usage: " << progname 
       << " [options]  <Output File>" << std::endl;
  std::cout <<" Output file name required unless -GrISUOuputFile option "
              "sepecified in command line"<<std::endl;
  std::cout<<"ksLight:Options: "<<std::endl;
  command_line.printUsage(std::cout);
}

int main(int argc, char** argv)
{
  //Input is usually a configuration file (May be GrISU pilot file) 

  //We start with defining and setting up the configuration for this
  // processing.
  float dummy;
  
  try
    {
      time_t thetime=time(NULL);
      std::cout<<"ksLight: START------"<<ctime(&thetime)<<std::endl;

      std::string progname = *argv;

      VAOptions command_line(argc,argv);
      VAConfigInfo config_file;

  //At this point we want to start our process of defining the configuration
  //values that we may need to generate out shower. 

  //The following calls to configure() of each process or that we may use 
  //allows each of those process to register keywords and setup pointers to 
  //the default configuration values that may need to be set. These have the 
  //format of "sDefaultVariableName". The actual variable used by the classes
  //have the name format: fVariableName. The fVariableName variables are NOT 
  //loaded at this time. Only the sDefaultVariableName are.  At this time some
  // of these sDefault... values are set with the true default values. These 
  //calls  also search the command line to see if the original default values 
  //may have been overridden by keyword/values in the command line.
  // ***********************************************************************

    // ********************************************************************
    // Register kaslight.par/GrISU Pilot file parameters.
    // ********************************************************************
      
      KSLightDataIn::configure(config_file, command_line);   
      

    // ********************************************************************
    // Test all defined options for main
    // ********************************************************************

    // ------------------------------------------------------------------------
    // Test for "GrISU Output file" options
    // ------------------------------------------------------------------------
 #ifdef KSGRISU
     bool MakeGrISUOutputFile=false;
      bool MakeKascadeBinaryFile=true;
      std::string GrISUOutputFilename;
      if(command_line.findWithValue("GrISUOutputFile",GrISUOutputFilename,
		 "Create an GrISU Output file only. This is a text file of "
		 "track segments of the shower written in a format readable "
		 "by GrISU cherenkf7.c program. If this option chosen, no "
                 "binary segment file (suitable for input to the kaslite "
                 "program) is written.")
	 == VAOptions::FS_FOUND)
	{
	  MakeKascadeBinaryFile=false;
	  MakeGrISUOutputFile=true;
	}

    // ------------------------------------------------------------------------
    // Test for "GrISU Pilot file" option
    // ------------------------------------------------------------------------
      bool UseGrISUPilotFile=false;
      std::string GrISUPilotFilename;
      if(command_line.findWithValue("GrISUPilotFile",GrISUPilotFilename,
		 "Use a GrISU Pilot file to specifiy config options. This "
		 "option supeceeds the use of a standard config file")
	 == VAOptions::FS_FOUND)
	{
	
	  UseGrISUPilotFile=true;
	}

#endif

    // ------------------------------------------------------------------------
    // Test for "load configuration file" options
    // ------------------------------------------------------------------------

      bool load_config = false;
      std::string load_filename;
      if(command_line.findWithValue("config",load_filename,
			     "Load Configuration File")==VAOptions::FS_FOUND)
	{
	  load_config = true;	
#ifdef KSGRISU
	  if(UseGrISUPilotFile)
	    {
	      std::cout<<"A GrISU Pilot file has been specified."<<std::endl;
	      std::cout<<" The -config="<<load_filename
		       <<" options is ignored!!"<<std::endl;
	      load_config = false;
	    }
#endif
	}

    // ------------------------------------------------------------------------
    // Test for "save configuration file" options
    // ------------------------------------------------------------------------

      bool save_config = false;
      bool only_save_config = false;
      std::string save_filename;
      if(command_line.findWithValue("save_config",save_filename,
				  "Save a configuration file with all "
				  "configuration values before processing.")
	 == VAOptions::FS_FOUND)
	save_config=true;
      if(command_line.findWithValue("save_config_and_exit",save_filename,
			      "Save a configuration file with all "
			      "configuration values and immediately exit.")
	 == VAOptions::FS_FOUND)
	only_save_config=true;
    // ------------------------------------------------------------------------


    // -----------------------------------------------------------------------
    // Test for "help" options
    // -----------------------------------------------------------------------
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


    // ------------------------------------------------------------------------
    // All the command line options that the program is able to  handle 
    // have been processed, so make sure there are no more command lines
    // options available.
    // ------------------------------------------------------------------------

      if(!command_line.assertNoOptions())
	{
	  std::cerr << progname << ": unknown options: ";
	  for(int i=1;i<argc;i++)std::cerr << argv[i];
	  std::cerr << std::endl;
	  std::cerr << std::endl;
	  usage(progname, command_line);
	  exit(EXIT_FAILURE);
	}
 
    // ------------------------------------------------------------------------
    // Load the configuration file if we have been asked to
    // ------------------------------------------------------------------------
          
      if(load_config)
	{
	  config_file.loadConfigFile(load_filename);
	}
     
    // ------------------------------------------------------------------------
    // Load GrISU Pilot file options if specified
    // ------------------------------------------------------------------------
#ifdef KSGRISU
      if(UseGrISUPilotFile)
	{
	  KSGrISUPilotFile GrISU(GrISUPilotFilename);
	  GrISU.Convert2Config();
	  config_file.loadConfigFile(GrISU.getConfigFileName());
	}
#endif
	

    // ------------------------------------------------------------------------
    // Save the configuration file if we have been asked to
    // ------------------------------------------------------------------------
    
      if((save_config)||(only_save_config))
	config_file.saveConfigFile(save_filename);
      if(only_save_config)exit(EXIT_SUCCESS);

    //Run file name: Left on the command line should now be only the program
    //name and run file name. Get it.
      argv++;
      argc--;
      if(argc<2)
	{
	  usage(progname, command_line);
	  exit(EXIT_FAILURE);
	}
     
      std::string SegFileName=*argv;
      argv++;
      std::string PeFileName=*argv;
     

  //Now that all the possible keywords that may be found in the configuration
  //file have been registered with config we can read in the configuration 
  //file and set any sDefault... types values it has. This will overide any
  //defualt values but NOT any command line values.
  //If any keyword is found that has not been registered we die!

    // -----------------------------------------------------------------------
    // Print usage if help is requested
    //------------------------------------------------------------------------
      if(help)
	{
	  usage(progname, command_line);
	  exit(EXIT_SUCCESS);
	}


     // ------------------------------------------------------------------------
    //Open the input seg file and read in the seg header
    // ------------------------------------------------------------------------
      std::cout<<"ksLight: Input Segment File: "<<SegFileName<<std::endl;
      pfSegmentHead= new KSSegmentHeadData ;
      pfSegFile=new KSSegmentFile();
      pfSegFile->Open(SegFileName.c_str());
      pfSegFile->ReadSegmentHead(pfSegmentHead);
      pfSegmentHead->PrintSegmentHead();

      pfSegment=new KSSegmentData;

    // --------------------------------------------------------------------
    // Load up the pe header now that the config stuff is loaded
    // --------------------------------------------------------------------

      pfPeHead= new KSPeHeadData;
      pfDataIn=new KSLightDataIn(pfPeHead);
 
    // ------------------------------------------------------------------------
    // Initalize the random number generator.
    // ------------------------------------------------------------------------
      int printseeds=1;
      int rslength=pfDataIn->fRandomSeedFileName.length();
      ranstart(&printseeds,(char*)pfDataIn->fRandomSeedFileName.c_str(),
	       rslength);
      
    // ------------------------------------------------------------------------
    //  Set up grid size. 
    // ------------------------------------------------------------------------
    // ------------------------------------------------------------------------
    // Make Whipple or VERITAS triangular array the same size
    // For a EastWest grid: A TRiangular array is achieved by having: 
    // 1:ny as before. 2:NY even then nx as before. 3:NY odd then NX center 
    // shifted 1/2 fXAreaWidthM to right.To insure correct triangular spacing 
    // and a minimum dimension of 12m use 12m x 13.856m
    // ------------------------------------------------------------------------
      if(pfPeHead->fWhippleMount || pfPeHead->fVeritasMount)
	{
	  pfPeHead->fXAreaWidthM=12.0;       //SQUARE grid
	  pfPeHead->fYAreaWidthM=12.0;
	}

      // *****************************************************************
      // Modify for inclination angle. Mirror shadow gets bigger. 
      //Note: This is also not right because this is primary direction not 
      //      mount direction. The error is maximum at 70 deg where we come
      //      up short by ~.4 m if mount is actually at 72 deg. So add ./5 m to
      //      make YSeg and Xseg, but don't add this to Yseg if the inclination
      //      <1.5deg. For <1.5 deg inclination we want to keep T1-T4 
      //      seperation as close as possible to acutal seperation of 35.01 
      //      meters. 3*12m/cos(thetaX)~ 36 m  is the best we can do.
      // ******************************************************************
      KSAzElRADecXY convertCoords(-1.93649,0.552828);//Base camp but not needed
      double azCC;
      double elCC;
      double dnCC=-(-pfSegmentHead->fDnInitial);  //Kascade to vegas convention
      double dmCC=-(-pfSegmentHead->fDmInitial);  //AND particle dir to mount 
      double dlCC=(-pfSegmentHead->fDlInitial);   //dir conversion.
      convertCoords.DlDmDnToAzEl(dlCC, dmCC, dnCC, azCC, elCC);

      double az=azCC/gDeg2Rad;   //convert to deg.
      double el=elCC/gDeg2Rad;
      std::cout<<"ksLight: Primary coming from: azimuth: "<<az
	       <<"deg. Elevation: "<<el<<"deg."<<std::endl;
      // *****************************************************************
      // Make room for 2 deg wobble except when we want t1-t4 to stay
      // at 36 m
      // *****************************************************************
      double xSeg;
      double ySeg;
      if(az<1.5 ||fabs(az-360.0)<1.5 || fabs(az-180)<1.5) //NorthSouth
	{
	  if( (90.0-el)<1.5 )    //zenith.keep yseg=12.0 (t1-t4)
	    {
	      pfPeHead->fXAreaWidthM=pfPeHead->fXAreaWidthM+.5; 
	    }
	  else
	    {
	      findXSegYSeg(az,el,(pfPeHead->fXAreaWidthM+.5)/2.0,xSeg,ySeg);
	      pfPeHead->fXAreaWidthM=xSeg; 
	      pfPeHead->fYAreaWidthM=ySeg;
	    }
	}
      else if(fabs(az-90.0)< 1.5 || fabs(az-270.0)< 1.5) //EastWest
	{
	  // ***********************************************************
	  // I'm not adding the .5 m here to Yseg since I rally want for e-w 
	  // inclination to keep T1-T4 distance as close  as possible to the 
	  // actuall which is 35 m. This means we loose a bit if we wobble  
	  // north or south but I think this is more improtant.
	  // ************************************************************
	  findXSegYSeg(az,el,(pfPeHead->fXAreaWidthM+.5)/2.0,xSeg,ySeg);
	  pfPeHead->fXAreaWidthM=xSeg;
	}
      else      //Arbitrary direction. add .5 m for wobbble room both 
	        //direcitons
	{
	  double xSeg;
	  double ySeg;
	  findXSegYSeg(az,el,(pfPeHead->fXAreaWidthM+.5)/2.0,xSeg,ySeg);

	  pfPeHead->fXAreaWidthM=xSeg;
	  pfPeHead->fYAreaWidthM=ySeg;
	}
      // *******************************************************************
      // For the triangular grid we need to have the yseg/xseg ratio be
      // sqrt(4/3) for NorthSouth and  invers of that for eastWest
      // We ignore T1-T4 stuff here. Only used with Square grid.
      // *******************************************************************
 
      if(pfPeHead->fTriangularGrid) //Triangular grid
	{
	  if(pfPeHead->fNorthSouthGrid)
	    {
	      //NS grid 
	      double newYSeg=sqrt(4./3.)*pfPeHead->fXAreaWidthM; 
	      if(newYSeg>pfPeHead->fYAreaWidthM)
		{
		  pfPeHead->fYAreaWidthM=newYSeg;
		}
	      else
		{
		  double newXSeg=pfPeHead->fYAreaWidthM/sqrt(4./3.);
		  pfPeHead->fXAreaWidthM=newXSeg;
		}
	    }
	  else
	    {
	      //EW grid
	      double newXSeg=sqrt(4./3.)*pfPeHead->fYAreaWidthM; 
	      if(newXSeg>pfPeHead->fXAreaWidthM)
		{
		  pfPeHead->fXAreaWidthM=newXSeg;
		}
	      else
		{
		  double newYSeg=pfPeHead->fXAreaWidthM/sqrt(4./3.);
		  pfPeHead->fYAreaWidthM=newYSeg;
		}
	    }
	}
 
    // ------------------------------------------------------------------------
    // Generate a random X_OFFSET and a Y_OFFSET for this shower if requested.
    // Note: These offsets by arbitray convention will be 'ADDED' to things.
    // ------------------------------------------------------------------------
      if(pfPeHead->fRandomCoreOffset)
	{
	  pfPeHead->fXCoreOffsetM=(pran(&dummy)-.5)*pfPeHead->fXAreaWidthM;
	  pfPeHead->fYCoreOffsetM=(pran(&dummy)-.5)*pfPeHead->fYAreaWidthM;
	}
      else
	{
	  pfPeHead->fXCoreOffsetM=0;
	  pfPeHead->fYCoreOffsetM=0;
	}

    // ------------------------------------------------------------------------
    // Create the ouput Pe file and write the seg header and the pe header
    // to it and print the pe header(were no ready to do that).
    // ------------------------------------------------------------------------
      std::cout<<"ksLight: Output Pe File: "<<PeFileName<<std::endl;
      pfPeFile=new KSPeFile();
      pfPeFile->Create(PeFileName.c_str());
      pfPeFile->WriteSegmentHead(pfSegmentHead);
      pfPeFile->WritePeHead(pfPeHead);
      pfPeHead->PrintPeHead();

      pfPe=new KSPeData();

    // ------------------------------------------------------------------------
    // Make the light for the shower
    // ------------------------------------------------------------------------

    // At this point we have all the input parameters. Make the shower:
    // This call is a little funny so we can pass the output segment file 
    // floats instead of doubles.
      float dl   = (float) pfSegmentHead->fDlInitial;
      float dm   = (float) pfSegmentHead->fDmInitial;
      float dn   = (float) pfSegmentHead->fDnInitial; 
      float hobs = (float) pfSegmentHead->fObservationAltitudeM; 
      float tep  = (float) pfSegmentHead->fGeVEnergyPrimary/1000.;
      float xseg = (float) pfPeHead->fXAreaWidthM;
      float yseg = (float) pfPeHead->fYAreaWidthM;
      float xoff = (float) pfPeHead->fXCoreOffsetM;
      float yoff = (float) pfPeHead->fYCoreOffsetM;
      float eff  = (float)pfPeHead->fEfficiency;

      kskaslitemain(&pfPeHead->fWhippleMount,&pfPeHead->fVeritasMount, 
		    &pfPeHead->fTriangularGrid, &pfPeHead->fSquareGrid, 
		    &pfPeHead->fNorthSouthGrid, &pfPeHead->fRandomCoreOffset, 
		    &pfPeHead->fWhipplePMTs,    &pfPeHead->fVeritasPMTs, 
		    &pfPeHead->fADPPMTs,        &pfDataIn->fBenchmarkFlag,
		    &dl, &dm, &dn, &hobs, &pfSegmentHead->fType, &tep, 
		    &xseg,&yseg,&xoff, &yoff,&pfSegmentHead->fShowerID,&eff);
    // done
      pfSegFile->Close();
      std::cout<<"ksLight: KSSegmentFile reports Number of Segements "
	"read: "<<(long)pfSegFile->getNumSegments()<<endl;

      pfPeFile->Close();
      std::cout<<"ksLight: KSPeFile reports Number of Pe's written: "
	       <<(long)pfPeFile->getNumPes()<<endl;

    // ------------------------------------------------------------------------
    // Save the random number generator seeds.
    // ------------------------------------------------------------------------
      ranend(&printseeds,(char*)pfDataIn->fRandomSeedFileName.c_str(),
	       rslength);


      thetime=time(NULL);
      std::cout<<"ksLight: NORMAL END------"<<ctime(&thetime)
	       <<std::endl;
      //and we are done
      return 0;
    }
 
  catch(VAException &ex)
    {
      std::cerr<<ex;
      return 1;
    }
  catch(...)
    {
      std::cout<<"Fatal--Unknown exception found."<<std::endl;
      return 1;
    }
}
// ****************************************************************************

void  kssegmentread(bool* goodread, float* XStart, float* YStart, 
		    float* HStart, float* DlStart, float* DmStart, 
		    float* EndTime, float* HEnd, float* DlEnd, float* DmEnd,
		    float* Gamma, int* Spec)
// ***************************************************************************
// Loadup and write the segment to the output segment file.
// ***************************************************************************
{
  *goodread=pfSegFile->ReadSegment(pfSegment);

  if(*goodread)
    {
      *XStart=pfSegment->fXStart;
      *YStart=pfSegment->fYStart;
      *HStart=pfSegment->fHStart;
      *DlStart=pfSegment->fDlStart; //Initial x,y direction cosigns of segment
      *DmStart=pfSegment->fDmStart;
      *EndTime=pfSegment->fEndTime; //relative time(ns) at end of segment.
      *HEnd=pfSegment->fHEnd;   //final altitude of segment
      *DlEnd=pfSegment->fDlEnd;  //final direction cosigns of segment.
      *DmEnd=pfSegment->fDmEnd;
      *Gamma=pfSegment->fGamma;  //gamma at middle of segment.
      *Spec=pfSegment->nspec;
    }
  return;
}
// ***************************************************************************

void  kspewrite(int* nx, int* ny, float* time, float* dl, float* dm, int* id,
		float* x, float* y, int* type, float* lambda, float* emalt)
// ***************************************************************************
// Loadup and write the pes to the output pe file.
// ***************************************************************************
{
  pfPe->fNx=*nx;                  // Grid coords (x,y)
  pfPe->fNy=*ny;                  // Grid coords (x,y)
  pfPe->fTime=*time;              // Time of hit
  pfPe->fPhotonDl=*dl;            // dl of photon.
  pfPe->fPhotonDm=*dm;            // dm of photon(dn can be recreated)
  pfPe->fSegmentID=*id;           // Segment identifier.
  pfPe->fX=*x;                    // X position of hit in grid rectangle.
  pfPe->fY=*y;                    // Y position of hit in grid rectangle.
  pfPe->fTrackType=*type;         // Type of emitting particle.
  pfPe->fLambda=(int)*lambda;     // Wavelength of emmited photon(nm)
  pfPe->fEmissionAltitude=*emalt; // emmison altitude

  //  std::cout<<"X,Y: "<<pfPe->fX<<" "<<pfPe->fY<<std::endl;

  pfPeFile->WritePe(pfPe);
  return;
}
// ***************************************************************************

void ksreadwavelengthtable(char* fTableName, float* fTable)
// ***************************************************************************
// Read wavelength data from a file. File has format: wavelength, value
// wavelength starts at 180 and goes to 700 in steps of 5 nm.
// Assume fTable is float array 105 long 
// value may be for various things: Mirror reflectivity, quantum efficency , 
// filter transmission etc. Used manily for VERITAS, WHIPPLE tables presently
// hard coded into kskaslightmain.for.
// ***************************************************************************
{
  std::cout<<"Reading file: "<<fTableName<<std::endl;
  ifstream fIn;
  fIn.open(fTableName);
  float fWavelength;
    for(int i=0;i<105;i++)
    {
      fIn>>fWavelength>>fTable[i];
      std::cout<<fWavelength<<" "<<fTable[i]<<std::endl;
    }
  fIn.close();
  return;
}

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
