//-*-mode:c++; mode:font-lock;-*-
/**
 * \class ksAomega
 * \ingroup parametrization
 * \brief This code parses command line, loads config files as needed and runs
 * the electornic modeling version of the event trigger finding code. It then
 * creates events that llok like real data and puts them into either a
 * VBF file or into a stage2 output type root.
 *
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

#include "KSTeFile.h"
#include "KSSegmentDataClasses.h"
#include "KSPeDataClasses.h"
#include "KSTeDataClasses.h"

#include "KSAomegaDataIn.h"

#include "KSCommon.h"
#include "KSEvent.h"

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


void usage(const std::string& progname, const VAOptions& command_line)
{
  std::cout << std::endl;
  std::cout << "ksAomega: Usage: " << progname 
	    << " [options]  <Input Triggered Event (Te) binary File>  "
	    << std::endl;
  std::cout <<"ksAomega: Input file name required "
	    <<std::endl;
#ifdef KSGRISU
  std::cout <<"ksAomega: unless -GrISUOuputFile option given in command line"
	    <<std::endl;
#endif
  std::cout<<"ksAomega: Options: "<<std::endl;
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
      std::cout<<"ksAomega: START------"<<ctime(&thetime)<<std::endl;

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
      KSAomegaDataIn::configure(config_file, command_line);   

    // ********************************************************************
    // Test all defined options for main
    // ********************************************************************

 #ifdef KSGRISU
    // ----------------------------------------------------------------------
    // Test for "GrISU Output file" options
    // ----------------------------------------------------------------------
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

    // -----------------------------------------------------------------------
    // Test for "GrISU Pilot file" option
    // -----------------------------------------------------------------------
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

    // -----------------------------------------------------------------------
    // Test for "load configuration file" options
    // -----------------------------------------------------------------------
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
	  std::cout<<"ksAomega: Config File Name:"<<load_filename<<std::endl;
	  config_file.loadConfigFile(load_filename);
	}

#ifdef KSGRISU
    // -----------------------------------------------------------------------
    // Load GrISU Pilot file options if specified
    // -----------------------------------------------------------------------
      if(UseGrISUPilotFile)
	{
	  KSGrISUPilotFile GrISU(GrISUPilotFilename);
	  GrISU.Convert2Config();
	  config_file.loadConfigFile(GrISU.getConfigFileName());
	}
#endif
	

    // -----------------------------------------------------------------------
    // Save the configuration file if we have been asked to
    // -----------------------------------------------------------------------
      if((save_config)||(only_save_config))
	config_file.saveConfigFile(save_filename);
      if(only_save_config)exit(EXIT_SUCCESS);

    //Input Triggered event file name: Left on the command line should now be
    // only the program name and Input Te file name. Get it.
      argv++;
      argc--;
      if(argc<1)
	{
	  std::cout<<"ksAomega: No input Te file name arguments. Assume "
	    "-help reqested"<<std::endl;
	  usage(progname, command_line);
	  exit(EXIT_FAILURE);
	}
      std::string fTeFileName=*argv;

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

    // ----------------------------------------------------------------------
    //Open the input Te file and read in the seg, pe, and te headers
    // Note: The Te file may be a sum of events from many showers
    //       (See program ksTeSum).
    //       In that case the header records will all come from the 
    //       first shower in the file.
    // -----------------------------------------------------------------------
      std::cout<<"ksAomega: Input Trigger Event (Te) File: "<<fTeFileName
	       <<std::endl;
      KSSegmentHeadData* pfSegmentHead=new KSSegmentHeadData;
      KSPeHeadData* pfPeHead=new KSPeHeadData();
      KSTeHeadData* pfTeHead=new KSTeHeadData();
  
      KSTeFile* pfTeFile=new KSTeFile();
      pfTeFile->Open(fTeFileName.c_str());
      if(pfTeFile==NULL)
	{
	  std::cout<<"Failed to open Input Te File: "<<fTeFileName<<std::endl;
	  return 1;
	}
      bool goodread=pfTeFile->ReadSegmentHead(pfSegmentHead);
      if(!goodread)
	{
	  std::cout<<"ksAomega: Failed to read Segment Header from Te File"
		   <<std::endl;
	  return 1;
	}
  
      pfSegmentHead->PrintSegmentHead();
  
      goodread=pfTeFile->ReadPeHead(pfPeHead);
      if(!goodread)
	{
	  std::cout<<"ksAomega: Failed to read Pe Header from Te File"
		   <<std::endl;
	  return 1;
	}
  
      pfPeHead->PrintPeHead();
  
      goodread=pfTeFile->ReadTeHead(pfTeHead);
      if(!goodread)
	{
	  std::cout<<"ksAomega: Failed to read Te Header from Te File"
		   <<std::endl;
	  return 1;
	}
  
      pfTeHead->PrintTeHead();



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
      // The KSEvent class does all the event processing and output file
      // creating and writing. It builds pulse shapes, tests for triggers etc.
      // ---------------------------------------------------------------------

      KSEvent fEvent(pfTeFile,pfSegmentHead,pfPeHead,pfTeHead,pfDataIn);


      // ---------------------------------------------------------------------
      // Main event loop
      // ---------------------------------------------------------------------
      while(1)
	{
	  bool fShowerEnded = fEvent.BuildImage();
	  if(fShowerEnded)
	    {
	      break;
	    }
	  bool fTriggered=fEvent.ProcessImage();
	  if(fTriggered)
	    {
	      fEvent.SaveImage();   //Write event to output file(s).
	    }
	}

      fEvent.Close();

      fEvent.PrintStats();

      pfTeFile->Close();
      std::cout<<"ksAomega: KSTeFile reports Number of Te's read: "
	       <<(long)pfTeFile->getNumTes()<<endl;


    // ------------------------------------------------------------------------
    // Save the random number generator seeds.
    // ------------------------------------------------------------------------
      ranend(&printseeds,(char*)pfDataIn->fRandomSeedFileName.c_str(),
	       rslength);


      thetime=time(NULL);
      std::cout<<"ksAomega: NORMAL END------"<<ctime(&thetime)
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


