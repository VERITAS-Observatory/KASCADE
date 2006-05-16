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
    // All this: abs(sin(abs(acos(dl))-5.*pi/180 stuff is to elongate the 
    // area in the direction of tilt of the mount. A litttle extra
    // tilt(5 deg) is added to prevent possible multiple use of a photons
    // (one might hits a telescope  on its edge in one grid rectangle and 
    // could continue on to hit the opposite edge of the telescope in an 
    // adjacent rectangle. This prevents that(out to 5 deg photons).
    // ------------------------------------------------------------------------
    // Make Whipple or VERITAS triangular array the same size
    // For a EastWest grid: A TRiangular array is achieved by having: 
    // 1:ny as before. 2:NY even then nx as before. 3:NY odd then NX center 
    // shifted 1/2 fXAreaWidthM to right.To insure correct triangular spacing 
    // and a minimum dimension of 12m use 12m x 13.856m
    // ------------------------------------------------------------------------

      if(pfPeHead->fWhippleMount || pfPeHead->fVeritasMount)
	{
	  if(pfPeHead->fNorthSouthGrid)
	    {
	      pfPeHead->fXAreaWidthM=12.0;       //NS grid 
	      pfPeHead->fYAreaWidthM=sqrt(4./3.)*pfPeHead->fXAreaWidthM; 
	    }
	  else
	    {
	      pfPeHead->fYAreaWidthM=12.0;       //EW grid
	      pfPeHead->fXAreaWidthM=sqrt(4./3.)*pfPeHead->fYAreaWidthM;
	    }
	}
      else 
	{                           
	  pfPeHead->fXAreaWidthM=12.0;       //SQUARE grid
	  pfPeHead->fYAreaWidthM=12.0;
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

      pfPe=new KSPeData;

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



