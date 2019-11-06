//-*-mode:c++; mode:font-lock;-*-
/** 
 * \class ksSumFiles * \brief This code reads in a .vbf file and changes the 
 * year. This is to fix a bug in ksSum which set year (andf thus season) to 
 * 2006 (V4). We may want the sims to be for V5 or V6.
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


#include <iostream>
#include <iomanip>
#include <fstream>
#include <string>
#include <cmath>
#include <time.h>
#include <exception>
#include <algorithm>

#include <VBF/VBankFileReader.h>
#include <VBF/VBankFileWriter.h>
#include <VBF/VPacket.h>
#include <VBF/VArrayEvent.h>
#include <VBF/VDatum.h>
#include <VBF/VKascadeSimulationData.h>
#include <VBF/VKascadeSimulationHeader.h>
#include <VBF/VEventType.h>

// includ e the simulation data structure
#include <VBF/VSimulationData.h>

#include "VAException.h"
#include "VAOptions.h"
#include "VSOptions.hpp"
//#include "VATime.h"
//#include "VAAzElRADecXY.h"
//#include "VAArrayInfo.h"
//#include "VAArrayInfoFactoryLite.h"

//#include "KSCommon.h"

// declare that we're using the VConfigMaskUtil namespace, which gives us
// easy access to parseConfigMask().
// include the configuration mask utilities, which give us parseConfigMask()
#include <VBF/VConfigMaskUtil.h>
using namespace VConfigMaskUtil;
// *************************************************************************

void usage(const std::string& progname,
           const VAOptions& command_line)
{
  std::cout << std::endl;
  std::cout << "ksFixYear - Usage: " << progname 
            << " [options]  <Output Merged/Randomized File Name>" 
            << std::endl;
  std::cout<<"ksFixYear - Options: "<<std::endl;
  command_line.printUsage(std::cout);
}
// ************************************************************************

void  CopyEventToOutputFile(VBankFileReader* pfReader, int PacketIndex, 
                            VBankFileWriter* pfWriter, uword32 RunNumber, 
                            uint8_t GPSYear)
// ***********************************************************************
// Copy a packet from the intput reader to the  output file. Be sure to 
// update all gps year values
// ************************************************************************
{
  VPacket* pfWritePacket = new VPacket();
  VPacket* pfPacket=pfReader->readPacket(PacketIndex); 
  // *************************************************
  // Copy over simulation data banks in this packet
  // *************************************************
  if (pfPacket->has(VGetKascadeSimulationDataBankName())  )
    {
      std::cout<<"ksFixYear - Missing KASCADE SimulationDataBank at "
        "packet#: "<<PacketIndex<<std::endl;
    } 
  else
    {
      // **********************************************
      // Copy over Simulation data bank in this 
      // packet
      // **********************************************
      VSimulationData *pfSimData =pfPacket->get< VSimulationData >
        (VGetSimulationDataBankName());
      //packet index = array event number
      pfSimData->fEventNumber = PacketIndex;
      pfSimData->fRunNumber  = RunNumber;

      VSimulationData* pfWriteSimData = pfSimData->copySimData();
      pfWritePacket->put(VGetSimulationDataBankName(), pfWriteSimData);  

      // **********************************************
      // copy over Kascade simulation data bank in this 
      // packet
      // **********************************************
      VKascadeSimulationData *pfKSimData = 
        pfPacket->get< VKascadeSimulationData >
        (VGetKascadeSimulationDataBankName());
      //packet index = array event number
      pfKSimData->fEventNumber = PacketIndex;
      pfKSimData->fRunNumber  = RunNumber;

      VKascadeSimulationData* pfWriteKSimData=pfKSimData->copyKascadeSimData();
      pfWritePacket->put(VGetKascadeSimulationDataBankName(), 
                         pfWriteKSimData);
    }
  
  // **********************************************
  // Now the ArrayEvents Fix year in array Trigger
  // *********************************************
  if (!pfPacket->hasArrayEvent())
    {
      std::cout<<"ksFixYear - Missing ArrayEventin at packet#: "
               <<PacketIndex<<std::endl;
      return;
    }

  VArrayEvent*   pfAEIn     = NULL;
  VArrayTrigger* pfAT       = NULL;
  VArrayEvent* pfAEOut  = new VArrayEvent();


  pfAEIn=pfPacket->getArrayEvent();
  pfAT = pfAEIn->getTrigger();

  // *****************************************************************
  // set the Run Number and event number
  // *******************************************************************
  pfAT->setRunNumber(RunNumber);
  pfAT->setEventNumber(PacketIndex);

  // *****************************************************************
  // set the Year
  // *******************************************************************
  pfAT->setGPSYear(GPSYear);
			  
  // ***********************************************************************
  // now put array trigger back into the array event
  // ************************************************************************
  VArrayTrigger* pfWriteAT=pfAT->copyAT();
  pfAEOut->setTrigger(pfWriteAT);
  
  // **************************************************
  // Now fix telescope events
  // *************************************************
  int fNumTriggeredTels =(int) pfAEIn->getNumEvents();
  for(int i=0;i<fNumTriggeredTels;i++) {
    VEvent* pfEvent = pfAEIn->getEvent(i);

    // set the event number and year
    pfEvent->setEventNumber(PacketIndex);
    pfEvent->setGPSYear(GPSYear);

    // add the event to the array event!
    VEvent* pfWriteEvent=pfEvent->copyEvent();
    pfAEOut->addEvent(pfWriteEvent);
  }

  pfAEOut->setRun(RunNumber);

  // put the array event into the packet
  pfWritePacket->putArrayEvent(pfAEOut);
			  
  // finally, write the packet into the file
  pfWriter->writePacket(PacketIndex,pfWritePacket);
  delete pfWritePacket;
  delete pfPacket;
  return;
}
// **************************************************************************
 
int main(int argc, char** argv)
{ 
  try {
    time_t thetime=time(NULL);
    std::cout<<"ksFixYear -  START------"<<ctime(&thetime)<<std::endl;
    
    // **********************************************************************
    // Pick up command line arguments
    // **********************************************************************
    std::string progname = *argv;
    VAOptions command_line(argc,argv);
    
    uint8_t GPSYear;
    if( (command_line.findWithValue("NewYear",GPSYear,
                                    "Year to change in all events")	
         == VAOptions::FS_FOUND)) {
      std::cout<<"ksFixYear - New Year"<< GPSYear<<std::endl;
    }	
    else{
      std::cout<<"ksFixYear - Fatal-- Year must be specified"<<std::endl;
      exit(1);
    }
    
    std::string fSourceFileName;
    if(! (command_line.findWithValue("SourceFile",fSourceFileName,
                                     "Input file which wiil be duplicated to an "
                                     "output file. ") 
          == VAOptions::FS_FOUND) ) {
      
      std::cout<<"ksFixYear - Fatal-- Source file must be specified"
               <<std::endl;
      exit(1);
    }
    
    std::string fOutputFileName;
    if(! (command_line.findWithValue("OutputFile",fOutputFileName,
                                     "Input file wiil be duplicated to this "
                                     "output file. If no ouput file name "
                                     "specified, output file name will be the "
                                     "input file prepended with 'V_'.")== 
          VAOptions::FS_FOUND) ) {
    }
    else{
      fOutputFileName = "V_" + fSourceFileName;
    }
        

    // -------------------------------------------------------------------
    // All the command line options that the program is able to  handle 
    // have been processed, so make sure there are no more command lines
    // options available.
    // -------------------------------------------------------------------

    if(!command_line.assertNoOptions()) {
      std::cerr << progname << ": unknown options: ";
      for(int i=1;i<argc;i++)std::cerr << argv[i];
      std::cerr << std::endl;
      std::cerr << std::endl;
      usage(progname, command_line);

      exit(EXIT_FAILURE);
    }
    argv++;
    argc--;
    if(argc!=1) {
      usage(progname, command_line);
      exit(EXIT_FAILURE);
    }
    
    std::cout<<"ksFixYear - Input File: "<<fSourceFileName
             <<std::endl;
    std::cout<<"ksFixYear - Output File:  "
             <<fOutputFileName<<std::endl;
  



    // ******************************************************************
    // Now we are ready to start. Begin by opening the input files;
    // We will then make 3 int vectors based on packet numbers: The first 
    // is of all the packets in the Base file that are not pedestal
    // events. The second is of the pedestal events packet numbers in the
    // Base file. The third is of the Source event packet numbers
    // that are not pedestal events and which match the direction index 
    // criteria.
    // ******************************************************************
    VBankFileReader* pfReader = new VBankFileReader(fSourceFileName);
    uword32 runNumber = pfReader->getRunNumber();
    VBankFileWriter* pfWriter = new VBankFileWriter(fOutputFileName, 
                                                    runNumber,
                                                    pfReader->getConfigMask());
    		  
    if(pfWriter==NULL) {
      std::cout<<"ksFixYear--Output VBF file failed to open"<<std::endl;
      exit(1);
    }	      
  
    int fNumVBFArrayEvents = pfReader->numPackets()-1;
	
    std::cout<<"ksFixYear: RunNumber: "<<pfWriter->getRunNumber()<<std::endl;

  		  
    // ******************************************************
    // copy over the first packet, this is the header 
    // packet, no events in it
    // ******************************************************
    VPacket* pfPacket=pfReader->readPacket(0);  //0=Location of header
    pfWriter->writePacket(0, pfPacket);
    delete pfPacket;

    //Packet 0 for header  packets	 
    for(int packetIndex=1;packetIndex<fNumVBFArrayEvents;packetIndex++) { 
      if(packetIndex%10000==0) {
        std::cout<<"#";
        std::cout.flush();
      }
    

      if(!pfReader->hasPacket(packetIndex)) {
      
        CopyEventToOutputFile(pfReader, packetIndex, pfWriter, runNumber, 
                              GPSYear);
      }
    } 
    // ***********************************
    // All done
    // Create the index file and writes the checksum.
    pfWriter->finish();
    return 0;
  }
  catch (const std::exception &e) {
    std::cerr<<"Error: "<<e.what()<<std::endl;
    return 1;
  }
  catch(...) {
    std::cout<<"ksFixYear - Fatal--Unknown exception found."
             <<std::endl;
    return 1;
  }
}
// ***********************************************************************
