//-*-mode:c++; mode:font-lock;-*-
/**
 * \class ksKascade
 * \ingroup parametrization
 * \brief This code parses command line, loads config files as needed and runs
 * the shower generation routine kskascademain
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


#include "KSKascadeDataIn.h"
#include "KSSegmentFile.h"


#ifdef KSGRISU
     #include "KSGrISUPilotFile.h"
#endif

#include "stdint.h"
#include <time.h>
#include  <string>
using namespace std;

extern "C" void ranstart(int* printseedflag, char* random_seed_file_name, 
			 int length);
extern "C" void ranend(int* printseedflag, char* random_seed_file_name, 
			 int length);

extern "C" void kskascademain(float* TEP, int* itype, float* dli,
			      float* dmi, float* dni, float* etr,
			      float* depth, float* tmax, float* hobs,
			      int* debena, int* fileID);

extern "C" void kskascadesetversion(char* version);

extern "C" void kskascadegetmagneticfield(char* magnet_field);

extern "C" void  kssegmentwrite(float* XStart, float* YStart, float* HStart, 
				float* DlStart, float* DmStart, 
				float* EndTime, float* HEnd, float* DlEnd, 
				float* DmEnd, float* Gamma, int* Spec);
KSKascadeDataIn* pfDataIn;
KSSegmentFile* pfSegFile;
KSSegmentHeadData* pfSegmentHead;
KSSegmentData* pfSegment;

void usage(const std::string& progname, const VAOptions& command_line)
{
  std::cout << std::endl;
  std::cout << "ksKascade: Usage: " << progname 
       << " [options]  <Output File>" << std::endl;
  std::cout <<" Output file name required unless -GrISUOuputFile option "
              "specified in command line"<<std::endl;
  std::cout<<"ksKascade: Options: "<<std::endl;
  command_line.printUsage(std::cout);
}

int main(int argc, char** argv)
{
  //Input is usually a configuration file (May be GrISU pilot file) 

  //We start with defining and setting up the configuration for this
  // processing.
  
  try
    {
      time_t thetime=time(NULL);
      std::cout<<"ksKascade:  START------"<<ctime(&thetime)<<std::endl;

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
    // Register kascade .par/GrISU Pilot file parameters.
    // ********************************************************************
      KSKascadeDataIn::configure(config_file, command_line);   

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
	      std::cout<<"ksKascade: A GrISU Pilot file has been specified."
		       <<std::endl;
	      std::cout<<"ksKascade: The -config="<<load_filename
		       <<" options is thus ignored!!"<<std::endl;
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
      if(argc==0)
	{
	  usage(progname, command_line);
	  exit(EXIT_FAILURE);
	}
 
      std::string SegFileName=*argv;


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

    // --------------------------------------------------------------------
    // Load up the segment header now that the config stuff is loaded
    // --------------------------------------------------------------------

      pfSegmentHead= new KSSegmentHeadData;
      pfDataIn=new KSKascadeDataIn(pfSegmentHead);
 
    // ------------------------------------------------------------------------
    // Create the ouput seg file and write the seg header and print it.
    // ------------------------------------------------------------------------
      std::cout<<"ksKascade: Output Segment File: "<<SegFileName<<std::endl;
      pfSegFile=new KSSegmentFile();
      pfSegFile->Create(SegFileName.c_str());
      pfSegFile->WriteSegmentHead(pfSegmentHead);
      pfSegmentHead->PrintSegmentHead();

      // --------------------------------------------------------------------
      // Print trace flags
      // --------------------------------------------------------------------
      std::cout<<"            Particle Trace enable      1-20   =";;
      for(int i=0;i<20;i++)
	{
	  std::cout<<pfDataIn->fParticleTraceEnableFlags[i]<<",";
	}
      std::cout<<std::endl;


      pfSegment=new KSSegmentData;

    // ------------------------------------------------------------------------
    // Initalize the random number generator.
    // ------------------------------------------------------------------------
      int printseeds=1;
      int rslength=pfDataIn->fRandomSeedFileName.length();
      ranstart(&printseeds,(char*)pfDataIn->fRandomSeedFileName.c_str(),
	       rslength);

    // ------------------------------------------------------------------------
    // Make the shower
    // ------------------------------------------------------------------------

    // At this point we have all the input parameters. Make the shower:
      // This call is a little funny so we can pass the output segment file 
      // name to a fortran subroutine. This is how you do it for Absoft 
      // Fortran.  Method might be different for other fortrans. I hope 
      // eventually to convert all input/ouput file access code to use C++ 
      // classes. Then we won't have to do this.

      float TEP    = (float) pfSegmentHead->fGeVEnergyPrimary/1000.;
      int   itype  =         pfSegmentHead->fType;
      float dli    = (float) pfSegmentHead->fDlInitial;
      float dmi    = (float) pfSegmentHead->fDmInitial;
      float dni    = (float) pfSegmentHead->fDnInitial;
      float etr    = (float) pfSegmentHead->fEnergyThresholdMeV;
      float depth  = (float) pfSegmentHead->fInjectionDepth;
      float tmax   = (float) pfSegmentHead->fMaxCoulombScatSegmentLength;
      float hobs   = (float) pfSegmentHead->fObservationAltitudeM;
      int   fileID = (int)   pfSegmentHead->fShowerID;

      kskascademain(&TEP, &itype, &dli, &dmi, &dni, &etr, &depth, &tmax, &hobs,
		    pfDataIn->fParticleTraceEnableFlags, &fileID);

    //and we are done
      pfSegFile->Close();

    // ------------------------------------------------------------------------
    // Save the random number generator seeds.
    // ------------------------------------------------------------------------
      ranend(&printseeds,(char*)pfDataIn->fRandomSeedFileName.c_str(),
	       rslength);

      thetime=time(NULL);
      std::cout<<"ksKascade:  NORMAL END------"<<ctime(&thetime)
	       <<std::endl;
  
      return 0;
    }
 
  catch(VAException &ex)
    {
      std::cerr<<ex;
      return 1;
    }
  catch(...)
    {
      std::cout<<"ksKascade: Fatal--Unknown exception found."<<std::endl;
      return 1;
    }
}
// ***************************************************************************

void kskascadegetmagneticfield(char* magnet_field)
{
  magnet_field[0] = pfSegmentHead->fEarthsMagneticFieldSpec[0];
  return;
}
// ***************************************************************************


void kskascadesetversion(char* version)
{
  int length=sizeof(version);
  for(int i=0;i<length;i++)
    {
      pfSegmentHead->fVersion[i]=version[i];
    }
  return;
}
// ***************************************************************************



void  kssegmentwrite(float* XStart, float* YStart, float* HStart, 
		     float* DlStart, float* DmStart, float* EndTime,
		     float* HEnd, float* DlEnd, float* DmEnd, float* Gamma, 
		     int* Spec)
// ***************************************************************************
// Loadup and write the segment to the output segment file.
// ***************************************************************************
{
  pfSegment->fXStart=*XStart;
  pfSegment->fYStart=*YStart;
  pfSegment->fHStart=*HStart;
  pfSegment->fDlStart=*DlStart; //Initial x,y direction cosigns of segment
  pfSegment->fDmStart=*DmStart;
  pfSegment->fEndTime=*EndTime; //relative time(ns) at end of segment.
  pfSegment->fHEnd=*HEnd;   //final altitude of segment
  pfSegment->fDlEnd=*DlEnd;  //final direction cosigns of segment.
  pfSegment->fDmEnd=*DmEnd;
  pfSegment->fGamma=*Gamma;  //gamma at middle of segment.
  pfSegment->nspec=*Spec;

  pfSegFile->WriteSegment(pfSegment);
  return;
}
