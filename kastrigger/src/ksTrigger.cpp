//-*-mode:c++; mode:font-lock;-*-
/**
 * \class ksTrigger
 * \ingroup parametrization
 * \brief This code parses command line, loads config files as needed and runs
 * the event trigger finding code. It saves triggering events (Te) to a 
 * ksTeFile(used to be call M files)
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

#include "KSPeFile.h"
#include "KSSegmentDataClasses.h"
#include "KSPeDataClasses.h"

#include "KSTeDataClasses.h"
#include "KSTeFile.h"
#include "KSTriggerDataIn.h"
#include "KSCommon.h"
#include "KSArea.h"

#ifdef KSGRISU
     #include "KSGrISUPilotFiles.h"
#endif

#include "stdint.h"
#include <time.h>
#include <string>
#include <cmath>
using namespace std;

extern "C" void ranstart(int* printseedflag, char* random_seed_file_name, 
			 int length);
extern "C" void ranend(int* printseedflag, char* random_seed_file_name, 
			 int length);
extern "C" float pran(float* dummy);

extern "C" void VeritasEventWeight(float* tep, float* type, float* weight,
				   bool* quiet);
extern "C" void WhippleEventWeight(float* tep, float* type, float* weight,
				   bool* quiet);


KSPeData*      pfPe;

void usage(const std::string& progname, const VAOptions& command_line)
{
  std::cout << std::endl;
  std::cout << "ksTrigger: Usage: " << progname 
       << " [options]  <Input sorted binary Pes File>  "
                      "<Output Binary Te Event File> " 
	    << std::endl;
  std::cout <<"ksTrigger: Input and Output file names required "
	    <<std::endl;
#ifdef KSGRISU
  std::cout <<"ksTrigger: unless -GrISUOuputFile option given in command line"
	    <<std::endl;
#endif
  std::cout<<"ksTrigger: Options: "<<std::endl;
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
      std::cout<<"ksTrigger: START------"<<ctime(&thetime)<<std::endl;

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
    // Register kastrigger.par/GrISU Pilot file parameters.
    // ********************************************************************
      KSTriggerDataIn::configure(config_file, command_line);   

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
	  std::cout<<"ksTrigger: No input and ouput arguments. Assume -help "
	    "reqested"<<std::endl;
	  usage(progname, command_line);
	  exit(EXIT_FAILURE);
	}
      std::string PesFileName=*argv;
      argv++;
      std::string TeFileName=*argv;

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
     // -----------------------------------------------------------------------
    //Open the input Pe file and read in the seg and pe headers
    // ------------------------------------------------------------------------
      std::cout<<"ksTrigger: Input Sorted Pe File: "<<PesFileName<<std::endl;
      KSSegmentHeadData* pfSegmentHead=new KSSegmentHeadData;
      KSPeHeadData* pfPeHead=new KSPeHeadData();
  
      KSPeFile* pfPesFile=new KSPeFile();
      pfPesFile->Open(PesFileName.c_str());
      if(pfPesFile==NULL)
	{
	  std::cout<<"Failed to open Input Pes File: "<<PesFileName<<std::endl;
	  return 1;
	}
      bool goodread=pfPesFile->ReadSegmentHead(pfSegmentHead);
      if(!goodread)
	{
	  std::cout<<"ksTrigger: Failed to read Segment Header from Pes File"
		   <<std::endl;
	  return 1;
	}
  
      pfSegmentHead->PrintSegmentHead();
  
      goodread=pfPesFile->ReadPeHead(pfPeHead);
      if(!goodread)
	{
	  std::cout<<"ksTrigger: Failed to read Pe Header from Pes File"
		   <<std::endl;
	  return 1;
	}
  
      pfPeHead->PrintPeHead();
 
    // --------------------------------------------------------------------
    // Load up the Te header now that the config stuff is loaded
    // --------------------------------------------------------------------
      KSTeHeadData* pfTeHead= new KSTeHeadData();
      KSTriggerDataIn* pfDataIn=new KSTriggerDataIn(pfTeHead);
 
    // --------------------------------------------------------------------
    //  Mount direction:dl,dm,dn
    // --------------------------------------------------------------------
      double fDl = pfDataIn->pfTeHead->fMountDl;
      double fDm = pfDataIn->pfTeHead->fMountDm;
      double fDn = pfDataIn->pfTeHead->fMountDn;
      // --------------------------------------------------------------------
      //Driftscan: Use elevation to determine mount dl,dm,dn 
      // --------------------------------------------------------------------

      if(pfDataIn->fUseElevationForDlDmDn)
	{   
	  fDl = -1.0e-15;                //Assume azimuth 0deg (north)
	  fDn = -cos((90.-pfDataIn->fMountElevationDeg)*gDeg2Rad);
	  fDm = sqrt(1.0-fDl*fDl-fDn*fDn);
	}
      
      // --------------------------------------------------------------------
      // Normalize Mount unit vector(Just being careful here.)
      // --------------------------------------------------------------------
                                              //Get length of starting vector
      double vlength=sqrt(fDl*fDl + fDm*fDm + fDn*fDn);
	                                      //normalize unit vector
      pfDataIn->pfTeHead->fMountDl=fDl/vlength;
      pfDataIn->pfTeHead->fMountDn=fDn/vlength;
      pfDataIn->pfTeHead->fMountDm=fDm/vlength;

      // --------------------------------------------------------------------
      // Initalize the random number generator.
      // --------------------------------------------------------------------
      int printseeds=1;
      int rslength=pfDataIn->fRandomSeedFileName.length();
      ranstart(&printseeds,(char*)pfDataIn->fRandomSeedFileName.c_str(),
	       rslength);

      
    // ----------------------------------------------------------------------
    // If we are weighting by spectrum, determine the weight;
    // ---------------------------------------------------------------------
      float fWeight=1.0;
      if(pfTeHead->fWeightBySpectrum)
	{
	  float fTep=pfSegmentHead->fGeVEnergyPrimary/1000.;
	  float	fType=pfSegmentHead->fType;
	  bool fQuiet=true;
	  if(pfTeHead->fCameraType==VERITAS499)
	    {
	      VeritasEventWeight(&fTep, &fType, &fWeight,&fQuiet);
	    }
	  else if(pfTeHead->fCameraType==WHIPPLE490)
	    {
	      WhippleEventWeight(&fTep, &fType, &fWeight,&fQuiet);
	    }
	  std::cout<<"ksTrigger: Cutting ouput events by spectrum weight of: "
		   <<fWeight<<std::endl;
	}

      // --------------------------------------------------------------------
      // Create the output Te file and write the Segment header, Pe header and 
      // Te header to it and print the Te header.
      // --------------------------------------------------------------------
      KSTeFile* pfTeFile=new KSTeFile();
      pfTeFile->Create(TeFileName.c_str());
      std::cout<<"ksTrigger: Output Trigger Event File: "<<TeFileName
	       <<std::endl;
      pfTeFile->WriteSegmentHead(pfSegmentHead);
      pfTeFile->WritePeHead(pfPeHead);

      KSArea fArea(pfPesFile,pfTeFile,pfSegmentHead,pfPeHead,pfDataIn,fWeight);

      //Need to wait until fArea constructed before saving TeHead
      pfTeFile->WriteTeHead(pfTeHead);
      pfTeHead->PrintTeHead();

      // ---------------------------------------------------------------------
      // Main event loop
      // ---------------------------------------------------------------------
      while(1)
	{
	  //std::cout<<"at4"<<std::endl;
	  bool fShowerEnded = fArea.ReadPes();
	  //std::cout<<"at5"<<std::endl;
	  fArea.ProcessImages();
	  //std::cout<<"at6"<<std::endl;
	  if(fShowerEnded)
	    {
	      break;
	    }
	}

      pfPesFile->Close();
      pfTeFile->Close();

      fArea.PrintStats();

    // ------------------------------------------------------------------------
    // Save the random number generator seeds.
    // ------------------------------------------------------------------------
      ranend(&printseeds,(char*)pfDataIn->fRandomSeedFileName.c_str(),
	       rslength);


      thetime=time(NULL);
      std::cout<<"ksTrigger: NORMAL END------"<<ctime(&thetime)
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


