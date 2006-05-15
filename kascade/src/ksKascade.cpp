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
#include "KSGrISUPilotFile.h"

#include "stdint.h"
#include <time.h>
#include <string>
#include <iostream>
#include <fstream>

using namespace std;

extern "C" void ranstart(int* printseedflag, char* random_seed_file_name, 
			 int length);
extern "C" void ranend(int* printseedflag, char* random_seed_file_name, 
			 int length);

extern "C" void kskascademain(float* TEP, int* itype, float* dli,
			      float* dmi, float* dni, float* etr,
			      float* depth, float* tmax, float* hobs,
			      int* debena, int* fileID, int* functionenable);

extern "C" void kskascadegetmagneticfield(char* magnet_field);

extern "C" void kssegmentwrite(float* XStart, float* YStart, float* HStart, 
			       float* DlStart, float* DmStart, 
			       float* EndTime, float* HEnd, float* DlEnd, 
			       float* DmEnd, float* Gamma, int* Spec,
			       double* ZSTim, double* ZPTim, float* TeMid);
extern "C" void ksgrisuheadwrite(int* Spec,float* XInitial, float* YInitial,
				 float* HObs, float* Dli, float* Dmi, 
				 float* Tep);
extern "C" void ksgrisutailwrite();

extern "C" int KascadeType2CorsikaType(int fKType);

extern "C" void kscharstring2cout(char* coutString);

void printRandomSeedFile(std::string randomSeedFileName);

KSKascadeDataIn* pfDataIn;
KSSegmentFile* pfSegFile;
KSSegmentHeadData* pfSegmentHead;
KSSegmentData* pfSegment;

std::ofstream* logout=NULL;
std::streambuf* StdOutStreamBuf;
bool GrISUOutput=false;
bool MakeKascadeBinaryFile=true;
bool GrISUOutputToStdIO;
std::ostream* pfGrISUOut;       //Fancy stuff here see pf 643 of C++ Standard
std::filebuf* pfGrISUBuffer;         // Library book.

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
      std::string progname = *argv;
      VAOptions command_line(argc,argv);
      VAConfigInfo config_file;
     // -------------------------------------------------------------------
      // GrISU specifications
      // -------------------------------------------------------------------
    // ----------------------------------------------------------------------
      // The very first thing we do is test for "GrISU Pilot file" option. 
      // This is completely seperate from the use of the GrISU ouput options
      // (though the Pilot filer may set some of the options. Its use 
      // superceeds the use of the 
      // standard vegas type config file. Command line options will still
      // superceede and GrISU pilot file derived parameter values. 
      // ----------------------------------------------------------------------
      bool UseGrISUPilotFile=false;
      std::string GrISUPilotFileName;
      KSGrISUPilotFile* pfGrISUPilotFile;
      int fNumberOfShowers=1;      //Defualt do 1 shower only
      if(command_line.findWithValue("GrISUPilotFileName",GrISUPilotFileName,
		 "Use a GrISU Pilot file to specifiy config options. This "
		 "option supeceeds the use of a standard config file, "
		 "but not the command line options.")
	 == VAOptions::FS_FOUND)
	{
	  
	  UseGrISUPilotFile=true;
	  pfGrISUPilotFile= new KSGrISUPilotFile(GrISUPilotFileName);
	  if(pfGrISUPilotFile->fNUMBR>0)
	    {
	      fNumberOfShowers=pfGrISUPilotFile->fNUMBR;
	      //std::cout<<"fNumberOfShowers:"<<fNumberOfShowers<<std::endl;
	    }
	}
      //--------------------------------------------------------------------
      // Check the command line to see:
      // 1:Are we to write the segment data in ascii format
      //   (used by GrISU for  pipe-lineing data from ksKascade to the next
      //    program)
      // 2:See if the output is to go to standard ouput or a text file.
      //
      // In order for pipelining to work this program can not print info, 
      // warning etc to standard output. So:
      // 3:Get name to ksKascade logfile name and redirect log entries to it.
      // --------------------------------------------------------------------
        if(command_line.find("EnableGrISUOutput",
		 "Indicates that the segment data will be written in GrISU "
		 "ascii format readable by the GrISU cherenkf7.c "
		 "program. If this option is chosen, no binary segment file "
		 "(suitable for input to the ksLight program) is written. "
                 "Destination of this output data is determined by the value "
		 "of GrISUOutputFileName.")
	 == VAOptions::FS_FOUND)
	{
	  MakeKascadeBinaryFile=false;
	  GrISUOutput=true;
	  GrISUOutputToStdIO=true;  //Can be reset by specifying 
	                            //GrISUOutputFileName option
	}
       // -------------------------------------------------------------------
       // Test for "GrISU Output file" options. See if we are to redirect 
       // --------------------------------------------------------------------
      std::string GrISUOutputFileName("GrISU.txt"); //Dummy name
      if(command_line.findWithValue("GrISUOutputFileName",
				    GrISUOutputFileName,
		 "Create an GrISU Output file (only if EnableGrISUOutput is "
                 "true). This is a text file of "
		 "track segments of the shower written in a format readable "
		 "by the GrISU cherenkf7.c program.  If this option is not "
		 "chosen and the EnableGrISUOutput option is chosen then the "
		 "GrISU Output segment "
		 "track data will be redirected to Standard Output. If this "
                 "redirection to StdOut happens then the LogFileName option "
		 "indicates where the log/info messages of this program are "
		 "written. If no LogFileName is specified then the messages "
                 "go to /dev/null i.e lala land! This is for use with "
                 "pipe-lining to the GrISU cherenkf7 program.")
	 == VAOptions::FS_FOUND)
	{
	  if(!GrISUOutput)
	    {
	      std::cout<<"ksKascade::Fatal-EnableGrISUOutput option must be "
		"specifed if GrISUOutputFileName option specified"<<std::endl;
	      exit(1);
	    }
	  GrISUOutputToStdIO=false;
	}
      else if(UseGrISUPilotFile)
	{
	  if(pfGrISUPilotFile->fFILEO!=" ")
	    {
	      if(!GrISUOutput)
		{
		  std::cout<<"ksKascade::Fatal-EnableGrISUOutput option must "
		    "be specifed if GrISUOutputFileName option specified in "
		    "GrISU Pilot File"<<std::endl;
		  exit(1);
		}
	      GrISUOutputFileName=pfGrISUPilotFile->fFILEO;    
	      GrISUOutputToStdIO=false;
	    }
	}
      
      // *******************************************************************
      // Determine where the log output goes to and redirect cout to it if 
      // need be. This is independent of whether we are doing any GrISU ouput
      // *******************************************************************
      std::string LogFileName(" ");
      StdOutStreamBuf=cout.rdbuf();          //Save for restoration of stdout.
      std::streambuf* LogStreamBuf=NULL; 

      if(command_line.findWithValue("LogFileName",LogFileName,
		 "Redirect all run log messages to an ascii text file. This "
		 "option is useful if the output segment data is to be "
                 "written to StdOutput: EnableGrISUOutput option is true and "
		 "GrISUOutputFileName is not specified. If the output segment "
		 "data is to be written to StdOutput and LogFileName is not "
                 "specified, then the run log messages will be redirected to "
                 "/dev/null  i.e lala land!")
	 == VAOptions::FS_FOUND)
	{
	  // ********************************************************
	  // Redirect run log messages to this file from StdOut. See 
	  // pg 641 of C++ Standard Library book)
	  // ********************************************************
	  logout= new ofstream(LogFileName.c_str());
	  LogStreamBuf=logout->rdbuf();
	  cout.rdbuf(LogStreamBuf);        //redirect cout
	  //!!!!!!!!!!!!!Don't FORGET to restore cout at end of program.
	}
      else if(UseGrISUPilotFile)
	{
	  if(pfGrISUPilotFile->fFILEL!=" ")
	    {
	      LogFileName=pfGrISUPilotFile->fFILEL;
	      logout= new ofstream(LogFileName.c_str());
	      LogStreamBuf=logout->rdbuf();
	      cout.rdbuf(LogStreamBuf);        //redirect cout
	    }
	}
	      
      // ****************************************************************
      // Check to see if a LogFile has not been specified and we have GrISU
      // data to stdout
      // ****************************************************************
      if(logout==NULL&&GrISUOutputToStdIO)
	{
	  //If we are already sending GrISU data to stdOut and a 
	  // logfile has not been specified then redirect cout to
	  // /dev/null  i.e. messages go to la la land!
	  logout= new ofstream("/dev/null");
	  LogStreamBuf=logout->rdbuf();
	  cout.rdbuf(LogStreamBuf);        //redirect cout to lala land
	}



      time_t thetime=time(NULL);
      std::cout<<"ksKascade:  START------"<<ctime(&thetime)<<std::endl;

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
    // Test for "load configuration file" options
    // ------------------------------------------------------------------------

      bool load_config = false;
      std::string load_filename;

      if(command_line.findWithValue("config",load_filename,
		       "Load Configuration File. If GrISUPilotFile option "
		       "has been specified on the command line, then this "
                       "option is ignored!")==VAOptions::FS_FOUND)
	{
	  load_config = true;	
	  if(UseGrISUPilotFile)
	    {
	      std::cout<<"ksKascade: A GrISU Pilot file has been specified."
		       <<std::endl;
	      std::cout<<"ksKascade: The -config="<<load_filename
		       <<" options is thus ignored!!"<<std::endl;
	      load_config = false;
	    }
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
    // Load GrISU Pilot file options if specified
    // ------------------------------------------------------------------------
      if(UseGrISUPilotFile)
	{
	  pfGrISUPilotFile->Convert2Config();  //creates a config file
	  config_file.loadConfigFile(pfGrISUPilotFile->getConfigFileName());
	}
    // ------------------------------------------------------------------------
    // Load the configuration file if we have been asked to
    // ------------------------------------------------------------------------
      else if(load_config)
	{
	  config_file.loadConfigFile(load_filename);
	}
    // ------------------------------------------------------------------------
    // Save the configuration file if we have been asked to
    // ------------------------------------------------------------------------
      if((save_config)||(only_save_config))
	config_file.saveConfigFile(save_filename);
      if(only_save_config)exit(EXIT_SUCCESS);

      //Run file name: Left on the command line should now be only the program
      //name and run file name. Get it.(unless we are writing a GrISU output 
      //file).
      if(MakeKascadeBinaryFile)
	{
	  argv++;
	  argc--;
	}
      if(argc==0)
	{
	  usage(progname, command_line);
	  exit(EXIT_FAILURE);
	}
      std::string SegFileName;
      if(MakeKascadeBinaryFile)SegFileName=*argv;


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

	 // *************************************************************
	 // If GrISU format data is to written out, creater the ofstream
	 // to do that. Redirect to StdOut if it has been requested
	 // *************************************************************
      if(GrISUOutput)
	{
	  pfGrISUBuffer = new std::filebuf;
	  pfGrISUOut = new std::ostream(pfGrISUBuffer);
	  pfGrISUBuffer->open(GrISUOutputFileName.c_str(),ios::out);
	  if(GrISUOutputToStdIO)
	    {
	      //Redirect GrISU output data to StdOut
	      pfGrISUOut->rdbuf(StdOutStreamBuf); //This sets up 
	                                         //things so that writting to 
	                                         //pfGrISU go to Standard Ouput
	      //Check to see if a LogFile has been specified
	      if(logout==NULL)
		{
		  //If we are already sending GrISU data to stdOut and a 
		  // logfile has not been specified then redirect cout to
		  // /dev/null  i.e. messages go to la la land!
		  logout= new ofstream("/dev/null");
		  LogStreamBuf=logout->rdbuf();
		  cout.rdbuf(LogStreamBuf);        //redirect cout to lala land
		}
	    }
	}
 // --------------------------------------------------------------------
    // Load up the segment header now that the config stuff is loaded
    // --------------------------------------------------------------------

      pfSegmentHead= new KSSegmentHeadData;
      pfDataIn=new KSKascadeDataIn(pfSegmentHead);
 
    // ------------------------------------------------------------------------
    // Create the ouput seg file and write the seg header and print it.
    // ------------------------------------------------------------------------
       if(MakeKascadeBinaryFile)
	{
	  std::cout<<"ksKascade: Output Segment File: "<<SegFileName
		   <<std::endl;
	  pfSegFile=new KSSegmentFile();
	  pfSegFile->Create(SegFileName.c_str());
	  pfSegFile->WriteSegmentHead(pfSegmentHead);
	}
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
      printRandomSeedFile(pfDataIn->fRandomSeedFileName);

      int printseeds=0;  //Don't print seeds in ranstart.
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
      // ********************************************************************
      //GrISU requires we can make any number of showers with possibly a 
      //distribution of energies      
      // ********************************************************************
      for(int ns=0;ns<fNumberOfShowers;ns++)
	{
	  if(UseGrISUPilotFile)
	    {
	      double fEnergy=pfGrISUPilotFile->genrateGrISUShowerEnergy();
	      if(fEnergy>0)
		{
		  TEP=fEnergy/1000;
		}
	      fileID = ns;
	    }    
	  //std::cout<<TEP<<" "<<itype<<" "<<dli<<" "<<dmi<<" "<<dni<<" "
	  //	   <<etr<<" "<<depth<<" "<<tmax<<" "<<hobs<<" "
	  //	   <<pfDataIn->fParticleTraceEnableFlags<<" "<<fileID
	  //	   <<" "<<pfSegmentHead->fFunctionEnableFlags<<std::endl;

	    kskascademain(&TEP, &itype, &dli, &dmi, &dni, &etr, &depth, &tmax, 
			&hobs,pfDataIn->fParticleTraceEnableFlags, &fileID,
			pfSegmentHead->fFunctionEnableFlags);
	}

//and we are done
      if(MakeKascadeBinaryFile)
	{
	  pfSegFile->Close();
	  std::cout
	    <<"ksKascade: KSSegmentFile reports Number of Segements written: "
	    <<(long)pfSegFile->getNumSegments()<<std::endl;
	}
      // ----------------------------------------------------------------------
      // Save the random number generator seeds.
      // ----------------------------------------------------------------------
      ranend(&printseeds,(char*)pfDataIn->fRandomSeedFileName.c_str(),
	     rslength);
      printRandomSeedFile(pfDataIn->fRandomSeedFileName);
      
      thetime=time(NULL);
      std::cout<<"ksKascade:  NORMAL END------"<<ctime(&thetime)
	       <<std::endl;
      
      if(logout!=NULL)
	{
	  cout.rdbuf(StdOutStreamBuf);
	}
      return 0;
    }
 
  catch(VAException &ex)
    {
      std::cerr<<ex;
      if(logout!=NULL)
	{
	  cout.rdbuf(StdOutStreamBuf);
	}
      return 1;
    }
  catch(...)
    {
      std::cout<<"ksKascade: Fatal--Unknown exception found."<<std::endl;
      if(logout!=NULL)
	{
	  cout.rdbuf(StdOutStreamBuf);
	}
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


//void kskascadesetversion(char* version)
//{
//  int length=sizeof(version);
//  for(int i=0;i<length;i++)
//    {
//      pfSegmentHead->fVersion[i]=version[i];
//    }
//  return;
//}
// ***************************************************************************



void  kssegmentwrite(float* XStart, float* YStart, float* HStart, 
		     float* DlStart, float* DmStart, float* EndTime,
		     float* HEnd, float* DlEnd, float* DmEnd, float* Gamma, 
		     int* Spec, double* ZSTim, double* ZPTim, float* TeMid)
// ***************************************************************************
// Loadup and write the segment to the output segment file.
// ***************************************************************************
{
  if(MakeKascadeBinaryFile)
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
    }
  else if(GrISUOutput)
    {
      int fCorsikaSpec=KascadeType2CorsikaType(*Spec);
      double tsegss= *EndTime - (*ZSTim-*ZPTim);
      *pfGrISUOut<<std::setw(2)<<fCorsikaSpec<<' '
		<<std::scientific<<std::setprecision(6)<<std::setw(12)
		<<std::showpoint<<*XStart<<' '
		<<std::scientific<<std::setprecision(6)<<std::setw(12)
		<<std::showpoint<<*YStart<<' '
		<<std::scientific<<std::setprecision(6)<<std::setw(12)
		<<std::showpoint<<*HStart<<' '
		<<std::scientific<<std::setprecision(6)<<std::setw(12)
		<<std::showpoint<<*HEnd<<' '
		<<std::scientific<<std::setprecision(6)<<std::setw(12)
		<<std::showpoint<<*DlStart<<' '
		<<std::scientific<<std::setprecision(6)<<std::setw(12)
		<<std::showpoint<<*DmStart<<' '
		<<std::scientific<<std::setprecision(6)<<std::setw(12)
		<<std::showpoint<<*DlEnd<<' '
		<<std::scientific<<std::setprecision(6)<<std::setw(12)
		<<std::showpoint<<*DmEnd<<' '
		<<std::scientific<<std::setprecision(6)<<std::setw(12)
		<<std::showpoint<<tsegss<<' '
		<<std::scientific<<std::setprecision(6)<<std::setw(12)
		<<std::showpoint<<*TeMid<<' '
		<<std::scientific<<std::setprecision(6)<<std::setw(12)
		<<std::showpoint<<*ZPTim<<' '
		<<std::scientific<<std::setprecision(6)<<std::setw(12)
		<<std::showpoint<<*ZSTim<<' '
		<<std::endl;
    }
  return;
}
// **************************************************************************

void ksgrisuheadwrite(int* Spec,float* XInitial, float* YInitial,
			float* HObs, float* Dli, float* Dmi, float* Tep)
// **************************************************************************
// Writes the GrISU seg file header. May be going to stdout.
// **************************************************************************
{
  if(GrISUOutput)
    {
      int istore=-1000;           //dummy seed I think

      int fCorsikaSpec=KascadeType2CorsikaType(*Spec);
      *pfGrISUOut<<std::setw(5)<<-fCorsikaSpec<<' '
		<<std::scientific<<std::setprecision(6)<<std::setw(13)
		<<std::showpoint<<*XInitial<<' '
		<<std::scientific<<std::setprecision(6)<<std::setw(13)
		<<std::showpoint<<*YInitial<<' '
		<<std::scientific<<std::setprecision(6)<<std::setw(13)
		<<std::showpoint<<*HObs<<' '
		<<std::scientific<<std::setprecision(4)<<std::setw(11)
		<<std::showpoint<<*Dli<<' '
		<<std::scientific<<std::setprecision(4)<<std::setw(11)
		<<std::showpoint<<*Dmi<<' '
		<<std::scientific<<std::setprecision(4)<<std::setw(11)
		<<std::showpoint<<*Tep<<' '
		<<std::setw(5)<<istore<<std::endl;
    }
}
// **************************************************************************
void ksgrisutailwrite()
// ***************************************************************
// Write GrISU Tail record if needed
// ****************************************************************
{
  if(GrISUOutput)
    {
      *pfGrISUOut<<99;
      for(int i=0;i<12;i++)
	{
	  *pfGrISUOut<<" 0.0000E+00";
	}
      *pfGrISUOut<<std::endl;
    }
  return;
}
// ***********************************************************************

void kscharstring2cout(char* coutString)
// ***********************************************************************
// Routine to allow fortran routines to print to cout especiall if it has been
// redirected to a file.
// **********************************************************************
{
  std::string ksStr(coutString);
  if(!ksStr.empty())
    {
      std::cout<<ksStr<<std::endl;
    }
  return;
}
// **************************************************************************

void printRandomSeedFile(std::string randomSeedFileName)
// ***************************************************************************
// Print random seed file contents using cout. This allows for redirection of
// seeds printout to log files or ./dev/null if that is how cout has been 
// redirected
// ***************************************************************************
{
  std::ifstream fSeedsIn(randomSeedFileName.c_str());
  if(!fSeedsIn)
    {
      std::cout<<"ksKascade: Failed to open random seed file: "
	       <<randomSeedFileName<<std::endl;
      exit(1);
    }
  std::cout<<"Random seed vector in file "<<randomSeedFileName<<std::endl;
  std::string s;
  while(getline(fSeedsIn,s))
    {
      std::cout<<s<<std::endl;
    }
  return;
}
