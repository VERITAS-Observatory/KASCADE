//-*-mode:c++; mode:font-lock;-*-
/**
 * \class ksArrayTriggerRoot
 * \ingroup parametrization
 * \brief This code parses command line, loads config files as needed and runs
 * the array event trigger finding code. It
 * then creates events that look like real data and puts them into a
 * either a stage2 output root file or a VBF file.
 * FOR SINGLE SHOWER FILES ONLY
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

#include "KSArrayTriggerDataIn.h"
#include "KSArrayEvent.h"

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


void usage(const std::string& progname, const VAOptions& command_line)
{
  std::cout << std::endl;
  std::cout << "ksArrayTrigger: Usage: " << progname 
	    << " [options]  <Ouput Array Event File>  "
	    << std::endl;
  std::cout <<"ksArrayTrigger: Output file name required "
	    <<std::endl;
  std::cout<<"ksArrayTrigger: Options: "<<std::endl;
  command_line.printUsage(std::cout);
}

int main(int argc, char** argv)
{
  //Input is usually a configuration file
  //We start with defining and setting up the configuration for this
  // processing.
   
  try
    {
      time_t thetime=time(NULL);
      std::cout<<"ksArrayTrigger: START------"<<ctime(&thetime)<<std::endl;

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

      KSArrayTriggerDataIn::configure(config_file, command_line);   

    // ********************************************************************
    // Test all defined options for main
    // ********************************************************************

    // -----------------------------------------------------------------------
    // Test for "load configuration file" options
    // -----------------------------------------------------------------------
      bool load_config = false;
      std::string load_filename;
      if(command_line.findWithValue("config",load_filename,
			     "Load Configuration File")==VAOptions::FS_FOUND)
	{
	  load_config = true;	
	}

    // -----------------------------------------------------------------------
    // Test for "save configuration file" options
    // -----------------------------------------------------------------------
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
    // -----------------------------------------------------------------------
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
    // -----------------------------------------------------------------------
    // All the command line options that the program is able to  handle 
    // have been processed, so make sure there are no more command lines
    // options available.
    // -----------------------------------------------------------------------
      if(!command_line.assertNoOptions())
	{
	  std::cerr << progname << ": unknown options: ";
	  for(int i=1;i<argc;i++)std::cerr << argv[i];
	  std::cerr << std::endl;
	  std::cerr << std::endl;
	  usage(progname, command_line);
	  exit(EXIT_FAILURE);
	}
 
    // -----------------------------------------------------------------------
    // Load the configuration file if we have been asked to
    // -----------------------------------------------------------------------
      if(load_config)
	{
	  std::cout<<"ksArrayTrigger: Config File Name:"<<load_filename<<std::endl;
	  config_file.loadConfigFile(load_filename);
	}

    // -----------------------------------------------------------------------
    // Save the configuration file if we have been asked to
    // -----------------------------------------------------------------------
      if((save_config)||(only_save_config))
	config_file.saveConfigFile(save_filename);
      if(only_save_config)exit(EXIT_SUCCESS);

    //Input Array event Ouput file name: Left on the command line should now be
    // only the program name and Ouput file name. Get it.
      argv++;
      argc--;
      if(argc<1)
	{
	  std::cout<<"ksArrayTrigger: No Output root file name arguments. "
	    " Assume -help reqested"<<std::endl;
	  usage(progname, command_line);
	  exit(EXIT_FAILURE);
	}
      std::string fOutputFileName=*argv;

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

      std::cout<<"ksArrayTrigger: Output Array Event File: "
	       <<fOutputFileName<<std::endl;

    // --------------------------------------------------------------------
    // Load up the configuration values
    // --------------------------------------------------------------------
      KSAomegaDataIn* pfDataIn=new KSAomegaDataIn();
      pfDataIn->Print();
      
    // --------------------------------------------------------------------
    // Initalize the random number generator.
    // --------------------------------------------------------------------
      int printseeds=1;
      int rslength=pfDataIn->fRandomSeedFileName.length();
      ranstart(&printseeds,(char*)pfDataIn->fRandomSeedFileName.c_str(),
	       rslength);

      // ---------------------------------------------------------------------
      // The KSArrayEvent class does all the event processing and output file
      // creating and writing. It tests for triggers etc.
      // *********************************************************************
      // Note that this code works for either root input/output or VBF 
      // input/output modes. Mode used is determined by input option:
      // -DataType=VBFFILE or -DataType=ROOTFILE
      // ---------------------------------------------------------------------

      KSArrayEvent fArrayEvent(fOutputFileName,pfDataIn);

      // ---------------------------------------------------------------------
      // Main event loop
      // ---------------------------------------------------------------------
      while(1)
	{
	  bool fShowerEnded = fArrayEvent.FindTrigger();
	  if(fShowerEnded)
	    {
	      break;
	    }
	  fArrayEvent.SaveEvent();   //Write event to output file(s).
	}

      fArrayEvent.Close();

      //fArrayEvent.PrintStats();

    // ------------------------------------------------------------------------
    // Save the random number generator seeds.
    // ------------------------------------------------------------------------
      ranend(&printseeds,(char*)pfDataIn->fRandomSeedFileName.c_str(),
	       rslength);


      thetime=time(NULL);
      std::cout<<"ksArrayTrigger: NORMAL END------"<<ctime(&thetime)
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


