//-*-mode:c++; mode:font-lock;-*-
/** 
 * \class ksSumFiles * \brief This code copies on vbdfiel to another making
 *  various changes on the way. Original change was the addition of the 
 * cos(zenith angle) to Sim.fAomega
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

// 06-April-2007
//Modified:
// ****************************************************************
// This program derived from ksMergeFiles.cpp. 
// *****************************************************************

#include <iostream>
#include <iomanip>
#include <fstream>
#include <string>
#include <cmath>
#include <time.h>
#include <exception>

#include <VBF/VBankFileReader.h>
#include <VBF/VBankFileWriter.h>
#include <VBF/VPacket.h>
#include <VBF/VArrayEvent.h>
#include <VBF/VDatum.h>
#include <VBF/VKascadeSimulationData.h>
#include <VBF/VKascadeSimulationHeader.h>
#include <VBF/VEventType.h>

// include the simulation data structure
#include <VBF/VSimulationData.h>

#include "VASlalib.h"
#include "VASlamac.h"
#include "VAException.h"
#include "VAOptions.h"
#include "VSOptions.hpp"
#include "VATime.h"
#include "VAAzElRADecXY.h"
#include "VAArrayInfo.h"
#include "VAArrayInfoFactoryLite.h"

#include "KSCommon.h"

// declare that we're using the VConfigMaskUtil namespace, which gives us
// easy access to parseConfigMask().
// include the configuration mask utilities, which give us parseConfigMask()
#include <VBF/VConfigMaskUtil.h>
using namespace VConfigMaskUtil;

void usage(const std::string& progname,
	   const VAOptions& command_line)
{
  std::cout << std::endl;
  std::cout << "ksCopyFile - Usage: " << progname 
	    << " <Input File name> <Output File Name>" 
	    << std::endl;
  std::cout<<"ksCopyFile - Options: "<<std::endl;
  command_line.printUsage(std::cout);
}

const uint8_t kGPSYear=6;
VAAzElRADecXY*  pfConvert;
bool fTrackingMode=false;
bool fObsAzSpecified;
bool fObsElSpecified;
bool fPriAzSpecified;
bool fPriElSpecified;

double fObsAz;
double fObsEl;
double fPriAz;
double fPriEl;
double fObsRA;
double fObsDec;
double fPriRA;
double fPriDec;
double fLatitude=0;
double fEastLongitude=0;
int main(int argc, char** argv)
{ 
  try
    {
      time_t thetime=time(NULL);
      std::cout<<"ksCopyFile -  START------"<<ctime(&thetime)<<std::endl;
      
      // **********************************************************************
      // Pick up command line arguments
      // **********************************************************************
      std::string progname = *argv;
      VAOptions command_line(argc,argv);

      if(!command_line.assertNoOptions())
	{
	  std::cerr << progname << ": unknown options: ";
	  for(int i=1;i<argc;i++)std::cerr << argv[i];
	  std::cerr << std::endl;
	  std::cerr << std::endl;
	  usage(progname, command_line);
	  exit(EXIT_FAILURE);
	}
      argv++;
      argc--;
      if(argc!=2)
	{
	  usage(progname, command_line);
	  exit(EXIT_FAILURE);
	}
 
      std::string fInputFileName=*argv;
      argv++;
      std::string fOutputFileName=*argv;

      std::cout<<"ksCopyFile - Input File: "<<fInputFileName
	       <<std::endl;
      std::cout<<"ksCopyFile - Output File: "<<fOutputFileName
	       <<std::endl;


      // ******************************************************************
      // Now we are ready to start. Begin by opening the input file;
      // ******************************************************************
      VBankFileReader* pfInputReader = NULL;
      VBankFileWriter* pfWriter=NULL;
      VPacket*       pfInputPacket   = NULL;      
      pfInputReader = new VBankFileReader(fInputFileName);

      int fNumInputPackets = pfInputReader->numPackets();
      // ******************************************************************
      // Initalize the ouput file: Copy over Source bank 0 (if available). We 
      // had the choice of the headers from the Source file or the Input file. 
      // For no good reason use the Source simulation headers.
      // Get the start-of-run event time, run number etc from first Input event.
      // ******************************************************************
      std::vector< bool> fConfigMask;
      uword32 fRunNumber = pfInputReader->getRunNumber();
      std::cout<<"ksCopyFile - RunNumber: "<<fRunNumber<<std::endl;
      std::cout<<"ksCopyFile - Number of Input Packets: "<<fNumInputPackets
	       <<std::endl;


      fConfigMask= pfInputReader->getConfigMask();
      pfWriter = new VBankFileWriter(fOutputFileName, fRunNumber, fConfigMask);
      if(pfWriter==NULL)
	{
	  std::cout<<"ksCopyFile--Output VBF file failed to "
	    "open"<<std::endl;
	  exit(EXIT_FAILURE);
	}	      
		      
      // ******************************************************
      // copy over the first packet, this is the header 
      // packet, no events in it
      // ******************************************************
      pfInputPacket=pfInputReader->readPacket(0);
      pfWriter->writePacket(0, pfInputPacket);
      delete pfInputPacket;
      int fNumOutputPackets=1;
      for(int i=1;i<fNumInputPackets;i++) //Packet 0 for header  packets
      	{
      	  if(i%10000==0)
	    {
	      std::cout<<"#";
	      std::cout.flush();
	    }
	  
	  
	  if(!pfInputReader->hasPacket(i))
	    {
	      std::cout<<"ksCopyFile - Missing packet. File: "<<fInputFileName
		       <<" at packet#: "<<i<<std::endl;
	      continue;
	    }
	  
	  pfInputPacket=pfInputReader->readPacket(i); 
	  // *************************************************
	  // Fix up simulation data bank in this packet
	  // *************************************************
	  if (!pfInputPacket->has(VGetSimulationDataBankName())  )
	    {
	      std::cout<<"ksCopyFile: Missing SimulationDataBank.File:"
		       <<fInputFileName<<" at packet#: "<<i<<std::endl;
	      continue;
	    } 
	  
	  VSimulationData *pfSimData =pfInputPacket->get< VSimulationData >
	    (VGetSimulationDataBankName());
	  double fZenith=pfSimData->fObservationZenithDeg*M_PI/180.;
	  
	  if (!pfInputPacket->has(VGetKascadeSimulationDataBankName())  )
	    {
	      std::cout<<"ksSumFiles: Missing SimulationDataBank.File:"
		       <<fInputFileName<<" at packet#: "<<i<<std::endl;
	      continue;
	    } 
	  
	  VKascadeSimulationData *pfKSimData =
	    pfInputPacket->get< VKascadeSimulationData >
	    (VGetKascadeSimulationDataBankName());
	  VKascadeSimulationData* pfWriteKSimData = 
	                                  pfKSimData->copyKascadeSimData();
	  
	  pfWriteKSimData->fAomega=pfKSimData->fAomega*cos(fZenith);
	  
	  
	  pfInputPacket->put(VGetKascadeSimulationDataBankName(),
			                                   pfWriteKSimData); 
	  pfWriter->writePacket(i,pfInputPacket);
	  fNumOutputPackets++;
	  
	  delete pfInputPacket;
	}
      

      // *****************************************************************
      // Were all done. close up the files and stuff;
      // *****************************************************************
	    
      //this creates the index and writes the checksum.
      pfWriter->finish();
      
      std::cout<<"ksCopyFile - Number of Output Packets written:"
	       <<fNumOutputPackets<<std::endl;
      std::cout<<"ksCopyFile - Normal end"<<std::endl;
      return 0;
    }
  
  catch (const std::exception &e) 
    {
      std::cerr<<"Error: "<<e.what()<<std::endl;
      return 1;
    }
  catch(...)
    {
      std::cout<<"ksCopyFile - Fatal--Unknown exception found."
	       <<std::endl;
      return 1;
    }
}
// **************************************************************************


