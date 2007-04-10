//-*-mode:c++; mode:font-lock;-*-
/** 
 * \class ksSumFiles * \brief This code Merges 2 VBF files (usually a CR and a Gamma file).
 *  files into a single file. The first file is assumed to be the base (CR)
 *  file. Events are chosen Randomly from this file (To mix energies) at a
 *  specified Base rate. Events are chosen from the second file at a seperate 
 *  rate. Pedestals come from the first file at 1 per second.
 *  Possible to choose a specific fDirection from the second file.
 *  Events in the output file have approprite fArrayEventNum fEventNum and 
 *  fTime
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

#include "VAException.h"
#include "VAOptions.h"
#include "VSOptions.hpp"
#include "VATime.h"

// declare that we're using the VConfigMaskUtil namespace, which gives us
// easy access to parseConfigMask().
// include the configuration mask utilities, which give us parseConfigMask()
#include <VBF/VConfigMaskUtil.h>
using namespace VConfigMaskUtil;


extern "C" void ranstart(int* printseedflag, char* random_seed_file_name, 
			 int length);
extern "C" void ranend(int* printseedflag, char* random_seed_file_name, 
		       int length);
extern "C" float pran(float* dummy);

extern "C" double Rexp(double fScaleFactor);

void SetNextEventTime(VATime& fEventTime, double fEventRateHz);
void SetNextPedEventTime(VATime& fEventTime);
void CopyEventToMergedFile(VBankFileReader* pfReader, int fPacketIndex, 
			   VBankFileWriter* pfWriter, int fArrayEventNum, 
			   int fRunNumber, VATime& fEventTime);

void usage(const std::string& progname,
	   const VAOptions& command_line)
{
  std::cout << std::endl;
  std::cout << "ksMergeFiles - Usage: " << progname 
	    << " [options]  <Output Merged/Randomized File Name>" 
	    << std::endl;
  std::cout<<"ksMergeFiles - Options: "<<std::endl;
  command_line.printUsage(std::cout);
}

const uint8_t kGPSYear=6;


int main(int argc, char** argv)
{ 
  try
    {
      time_t thetime=time(NULL);
      std::cout<<"ksMergeFiles -  START------"<<ctime(&thetime)<<std::endl;
      
      // **********************************************************************
      // Pick up command line arguments
      // **********************************************************************
      std::string progname = *argv;
      VAOptions command_line(argc,argv);

      double fCosmicRayRateHz=100.0;
      command_line.findWithValue("CosmicRayRateHz",fCosmicRayRateHz,
			   "Rate in Hz at which events from the Cosmic Ray "
			   "file will be put into the ouput file. Default is "
			   "100.0 Hz");
      double fGammaRayRateHz=25.0/60.0;
      double fGammaRayRateMin=25.0;
      if(command_line.findWithValue("GammaRayRatePerMinute",fGammaRayRateMin,
			   "Rate per Minute at which events from the Gamma "
			   "Ray file will be put into the ouput file. "
			   "Default is 25/min")
	 == VAOptions::FS_FOUND)
	{
	  fGammaRayRateHz=fGammaRayRateMin/60.;
	}

      int fGammaDirection=-1;
      command_line.findWithValue("GammaRaySourceDirectionIndex",
				 fGammaDirection,
				 "Index of Sim.fDirection source direction to "
				 "use when including Gamma Ray Events. "
				 "Default is no selction on fDirection index");


 
      std::string fCosmicRayFileName;
      if(!command_line.findWithValue("CosmicRayFileName",fCosmicRayFileName,
				     "Input file name for Cosmic ray file. "
				     "Ouput file consists of events randomized"
				     "  from this file with specified rate and"
				     " with pedestal events from this file on "
				     "each second mark. Required!")
	 == VAOptions::FS_FOUND)
	{
	  std::cout<<"ksMergeFiles - CosmicRayFileName Options is required"
		   <<std::endl;
	  exit(1);
	}


      argv++;
      std::string fGammaRayFileName;
      bool fGammaRayFileSpecified=false;
      if(command_line.findWithValue("GammaRayFileName",fGammaRayFileName,
				    "If this options specified events are "
				    "randomly chosen from this file and "
				    "added to the output file at the "
				    "specified rate. If the "
				    "-GammaRaySourceDirectionIndex option is "
				    "given than only events with that "
				    "VKascadeSimulationData::fDirection are "
				    "used. Optional.")
	 == VAOptions::FS_FOUND)
	{
	  fGammaRayFileSpecified=true;
	}

      std::string fRandomSeedFileName;
      if(!command_line.findWithValue("RandomSeedFileName",fRandomSeedFileName,
				    "Ranlux seed file")
	 == VAOptions::FS_FOUND)
	{
	  fRandomSeedFileName="ksMergeFiles.ran";
	}

      // -------------------------------------------------------------------
      // All the command line options that the program is able to  handle 
      // have been processed, so make sure there are no more command lines
      // options available.
      // -------------------------------------------------------------------

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
      if(argc!=1)
	{
	  usage(progname, command_line);
	  exit(EXIT_FAILURE);
	}

      std::string fMergedFileName=*argv;

      std::cout<<"ksMergeFiles - Input Cosmic Ray File: "<<fCosmicRayFileName
	       <<std::endl;
      if(fGammaRayFileSpecified)
	{
	  std::cout<<"ksMergeFiles - Input Gamma Ray File:  "
		   <<fGammaRayFileName<<std::endl;
	}
      std::cout<<"ksMergeFiles - Output Merged File:     "<<fMergedFileName
	       <<std::endl;

      // ******************************************************************
      // Set up random number generator
      // ******************************************************************
      int printseeds=1;
      ranstart(&printseeds,(char*)fRandomSeedFileName.c_str(),
	       (int)fRandomSeedFileName.length());

      // ******************************************************************
      // Now we are ready to start. Begin by opening the input files;
      // We will then make 3 int vectors based on packet numbers: The first 
      // is of all the packets in the Cosmic Ray file that are not pedestal
      // events. The second is of the pedestal events packet numbers in the
      // cosmic ray file. The third is of the gamma ray event packet numbers
      // that are not pedestal; events and which match the direction index 
      // criteria.
      // ******************************************************************
      VBankFileReader* pfCRReader = NULL;
      VBankFileReader* pfGRReader = NULL;
      VBankFileWriter* pfWriter=NULL;

      VPacket*       pfCRPacket   = NULL;      
      VPacket*       pfGRPacket   = NULL;      
 
       pfCRReader = new VBankFileReader(fCosmicRayFileName);

      int fNumCRPackets = pfCRReader->numPackets();

      // ******************************************************************
      // Create and fill CR events packet numbers vector.
      // ******************************************************************
      std::vector< int > pfCREventPackets;
      pfCREventPackets.clear();
      std::vector< int > pfCRPedEventPackets;
      pfCRPedEventPackets.clear();

      VArrayEvent*   pfAEIn     = NULL;
      VArrayTrigger* pfAT       = NULL;

      for(int i=1;i<=fNumCRPackets;i++) //Packet 0 for header  packets
	{
	  if(!pfCRReader->hasPacket(i))
	    {
	      std::cout<<"ksMergeFiles - Missing packet. File: "
		       <<fCosmicRayFileName<<" at packet#: "<<i
		       <<std::endl;
	      continue;
	    }
	  pfCRPacket=pfCRReader->readPacket(i); 

	  if (!pfCRPacket->hasArrayEvent())
	    {
	      std::cout<<"ksMergeFiles - Missing ArrayEvent in File:"
		       <<fCosmicRayFileName<<" at packet#: "<<i<<std::endl;
	      delete pfCRPacket;
	      continue;
	    } 
	  pfAEIn=pfCRPacket->getArrayEvent();
	  pfAT = pfAEIn->getTrigger();
	  if(pfAT->getEventType().trigger==VEventType::PED_TRIGGER)
	    {
	      pfCRPedEventPackets.push_back(i);
	    }
	  else if(pfAT->getEventType().trigger==VEventType::L2_TRIGGER)
	    {	      pfCREventPackets.push_back(i);
	    }
	  else
	    {
	      std::cout<<"ksMergeFiles - Unacceptable event type "
		       <<pfAT->getEventType().trigger<<" from CosmiRay file "
		       <<fCosmicRayFileName<<" at packet#: "<<i<<std::endl;
	      delete pfCRPacket;
	      continue;
	    }
	 delete pfCRPacket; 	      
	}

      int fNumCREventPackets=pfCREventPackets.size();
      int fNumCRPedEventPackets=pfCRPedEventPackets.size();
      std::cout<<"ksMergeFiles - Number of Events Cosmic Ray File: "
	       <<fNumCREventPackets<<std::endl;
      std::cout<<"ksMergeFiles - Number of Pedestal Events Cosmic Ray File: "
	       <<fNumCRPedEventPackets<<std::endl;

      // ********************************************************************
      // Now do the same for the Gamma Rays
      // ********************************************************************
      std::vector< int > pfGREventPackets;
      if(fGammaRayFileSpecified)
	{
	  pfGRReader = new VBankFileReader(fGammaRayFileName);
	  int fNumGRPackets = pfGRReader->numPackets();

	  pfGREventPackets.clear();

	  for(int i=1;i<=fNumGRPackets;i++) //Packet 0 for header  packets
	    {
	      if(!pfGRReader->hasPacket(i))
		{
		  std::cout<<"ksMergeFiles - Missing packet. File: "
			   <<fGammaRayFileName<<" at packet#: "<<i
			   <<std::endl;
		  continue;
		}

	      pfGRPacket=pfGRReader->readPacket(i); 
	      
	      if (!pfGRPacket->hasArrayEvent())
		{
		  std::cout<<"ksMergeFiles - Missing ArrayEvent in File:"
			   <<fGammaRayFileName<<" at packet#: "<<i<<std::endl;
		  delete pfGRPacket;
		  continue;
		} 

	      // ***********************************************************
	      // NOte: NO DIRECTION SELECTION AS OF YET. Here is where it will
	      // go
	      // ************************************************************
	      pfAEIn=pfGRPacket->getArrayEvent();
	      pfAT = pfAEIn->getTrigger();
	      if(pfAT->getEventType().trigger==VEventType::PED_TRIGGER)
		{
		  delete pfGRPacket;
		  continue;  //ignore ped events
		}
	      else if(pfAT->getEventType().trigger==VEventType::L2_TRIGGER)
		{
		  pfGREventPackets.push_back(i);
		}
	      else
		{
		  std::cout<<"ksMergeFiles - Unacceptable event type "
			   <<pfAT->getEventType().trigger
			   <<" from CosmiRay file "<<fGammaRayFileName
			   <<" at packet#: "<<i<<std::endl;
		  delete pfGRPacket;
		  continue;
		}
	      delete pfGRPacket;
	      
	    }

	  int fNumGREventPackets=pfGREventPackets.size();
	  std::cout<<"ksMergeFiles - Number of Events Gamma Ray File: "
		   <<fNumGREventPackets<<std::endl;
	}
      

      // ******************************************************************
      // Initalize the ouput file: Copy over Gamma ray bank 0. We had the 
      // choice of the headers from the Gamma ray file or the CR file. For no 
      // good reason use the Gammar ray simulation headers.
      // Get the starteof run event time run number etc from first CR event.
      // ******************************************************************
      int fArrayEventNum=1;   //VBF events start at 1, 0 is for header
      std::vector< bool> fConfigMask;

      uword32 fRunNumber = pfCRReader->getRunNumber();
      std::cout<<"ksMergeFile: RunNumber: "<<fRunNumber<<std::endl;

      fConfigMask= pfCRReader->getConfigMask();
      pfWriter = new VBankFileWriter(fMergedFileName, fRunNumber, fConfigMask);
      if(pfWriter==NULL)
	{
	  std::cout<<"ksMergeFiles--Output VBF file failed to "
	    "open"<<std::endl;
	  exit(1);
	}	      
		      
      // ******************************************************
      // copy over the first packet, this is the header 
      // packet, no events in it
      // ******************************************************
      if(fGammaRayFileSpecified)
	{
	  pfGRPacket=pfGRReader->readPacket(0);
	  pfWriter->writePacket(0, pfGRPacket);
	  delete pfGRPacket;
	}		      
      else
	{
	  pfCRPacket=pfCRReader->readPacket(0);
	  pfWriter->writePacket(0, pfCRPacket);
	  delete pfCRPacket;
	}

      // ******************************************************
      // Now we need a first event time to use. Use first 
      // event time in the first event in the CR file
      // ******************************************************
      VATime fEventTime;
      uint8_t fGPSYear=kGPSYear;

      if(!pfCRReader->hasPacket(1))
	{
	  std::cout<<"ksMergeFiles - Missing packet #1. File: "
		   <<fCosmicRayFileName<<std::endl;
	  exit(1);
	}

      pfCRPacket=pfCRReader->readPacket(1);
      pfAEIn=pfCRPacket->getArrayEvent();
      if(pfAEIn->hasTrigger())
	{
	  pfAT = pfAEIn->getTrigger();
	  fEventTime.setFromVBF(fGPSYear,pfAT->getGPSTimeNumElements(),
				pfAT->getGPSTime());
	}
      else
	{
	  std::cout<<"ksMergeFiles - Problem reading ArrayTrigger first event"
		   <<std::endl;
	  exit(1);
	}
      std::cout<<"ksMergeFiles - Merged File RunStart Time: "<< fEventTime
	       <<std::endl;
      delete pfCRPacket;

      std::cout<<"ksMergeFiles:Merging Files.....Takes even longer"
	       <<std::endl;

      // ****************************************************************
      // Randomly select event times for the first event of each type:
      // CR, CR ped Gamma Ray. Use rexp delta T distribution
      // ****************************************************************
      VATime fCREventTime=fEventTime;
      VATime fCRPedEventTime=fEventTime;;
      VATime fGREventTime=fEventTime;;

      SetNextEventTime(fCREventTime,fCosmicRayRateHz);
      SetNextPedEventTime(fCRPedEventTime);
      if(fGammaRayFileSpecified)
	{
	  SetNextEventTime(fGREventTime,fGammaRayRateHz);
	}

      int fNumCREvents=0;
      int fNumCRPedEvents=0;
      int fNumGREvents=0;

      float fXDummy;

     // ********************************************************************
      // MAIN LOOP
      // ********************************************************************
      // Our loop will be over existance of CR events that haven't been 
      // written out. 
      // *********************************************************************
      
      std::vector< int >::iterator fPos;
      VATime fLastTime;
      while(pfCREventPackets.size()>0)
	{
	  // ***************************************************************
	  // Find which type of event to write. Is a GR event next?
	  // ***************************************************************
	  if(fGammaRayFileSpecified && fGREventTime<fCRPedEventTime && 
		  fGREventTime<fCREventTime)
	    {
	      if(pfGREventPackets.size()== 0 )
		{
		  std::cout<<"ksMergeFiles - Out of Gamma Ray events"
			   <<std::endl;
		  break;
		}
	      int fIndex=(int)(pfGREventPackets.size()*pran(&fXDummy));
	      if(fIndex==(int)pfGREventPackets.size())
		{
		  fIndex=pfGREventPackets.size()-1;
		}
	      int fPacketIndex=pfGREventPackets.at(fIndex);
	      fPos=find(pfGREventPackets.begin(),pfGREventPackets.end(),
			fPacketIndex);
	      pfGREventPackets.erase(fPos);
	      
	      CopyEventToMergedFile(pfGRReader,fPacketIndex,pfWriter,
				    fRunNumber,fArrayEventNum,fGREventTime);
	      
	      fLastTime=fGREventTime;
	      SetNextEventTime(fGREventTime,fGammaRayRateHz);	      
	      fNumGREvents++;
	    }
	  else if(fCREventTime<fCRPedEventTime)
	    {
	      // *************************************************
	      // Randomly pick from our vector a position in the vector and use
	      // that packet number.
	      // ******************************************************
	      int fIndex=(int)(pfCREventPackets.size()*pran(&fXDummy));
	      if(fIndex==(int)pfCREventPackets.size())
		{
		  fIndex=pfCREventPackets.size()-1;
		}
	      int fPacketIndex=pfCREventPackets.at(fIndex);
	      
	      // *********************************************************
	      // Now the trick. The reason we are using vectors. Remove the 
	      // fIndex element from the vector.
	      // pfCREventPackets.begin()[fIndex] is an iterator pointing to
	      // the fIndex entry.
	      // *********************************************************
	      fPos=find(pfCREventPackets.begin(),pfCREventPackets.end(),
		       fPacketIndex);
	      pfCREventPackets.erase(fPos);
	      
	      // *********************************************************

	      CopyEventToMergedFile(pfCRReader,fPacketIndex,pfWriter,
				    fRunNumber,fArrayEventNum,fCREventTime);
	      
	      fLastTime=fCREventTime;
	      SetNextEventTime(fCREventTime,fCosmicRayRateHz);	      
	      fNumCREvents++;
	    }
	  else if(pfCRPedEventPackets.size() > 0)
	    {
	      // ***********************************************************
	      // Just take the ped events as they come
	      // ***********************************************************
	      int fPacketIndex=pfCRPedEventPackets.at(0);
	      fPos=pfCRPedEventPackets.begin();
	      pfCRPedEventPackets.erase(fPos);

	      CopyEventToMergedFile(pfCRReader,fPacketIndex,pfWriter,
				    fRunNumber,fArrayEventNum,fCRPedEventTime);

	      fLastTime=fCRPedEventTime;
	      SetNextPedEventTime(fCRPedEventTime);	      
	      fNumCRPedEvents++;
	    }
	  else
	    {
	      std::cout<<"ksMergeFiles - Out of Pedestal events"
		       <<std::endl;
              break;
	    }
	}


      // *****************************************************************
      // Were all done. close up the files and stuff;
      // *****************************************************************
	    
      //this creates the index and writes the checksum.
      pfWriter->finish();
	  
      std::cout<<"ksMergeFiles - Output summary file closed with "
	       <<fArrayEventNum-1<<" events"<<std::endl;
      std::cout<<"ksMergeFiles - Number of Coismic Ray Events written:"
	       <<fNumCREvents<<std::endl;
      std::cout<<"ksMergeFiles - Number of Pedestal Events written:"
	       <<fNumCRPedEvents<<std::endl;
      std::cout<<"ksMergeFiles - Number of Gamma Ray Events written:"
	       <<fNumGREvents<<std::endl;

      std::cout<<"ksMergeFiles - End of Run at: "<<fLastTime<<std::endl;
      std::cout<<"ksMergeFiles - Normal end"<<std::endl;
      // ----------------------------------------------------------------------
      // Save the random number generator seeds.
      // ----------------------------------------------------------------------
      ranend(&printseeds,(char*)fRandomSeedFileName.c_str(),
	     (int)fRandomSeedFileName.length());
      return 0;
    }
  
  catch (const std::exception &e) 
    {
      std::cerr<<"Error: "<<e.what()<<std::endl;
      return 1;
    }
  catch(...)
    {
      std::cout<<"ksMergeFiles - Fatal--Unknown exception found."
	       <<std::endl;
      return 1;
    }
}
// **************************************************************************

void  CopyEventToMergedFile(VBankFileReader* pfReader,int fPacketIndex, 
			    VBankFileWriter* pfWriter, int fArrayEventNum, 
			    int fRunNumber, VATime& fEventTime)
// ***********************************************************************
// Copy a packet form th intput reader to the Merged ouput file. Be sure to 
// update all times run numbers and event numbers.
// ************************************************************************
{
  VPacket* pfWritePacket = new VPacket();
  VPacket* pfPacket=pfReader->readPacket(fPacketIndex); 
			  
  // *************************************************
  // Update event numbers here and maybe times
  // *************************************************
			  
  // *************************************************
  // Fix up simulation data bank in this packet
  // *************************************************
  if (!pfPacket->has(VGetKascadeSimulationDataBankName())  )
    {
      std::cout<<"ksMergeFiles - Missing KASCADE SimulationDataBank at "
	"packet#: "<<fPacketIndex<<std::endl;
    } 
  else
    {
      VSimulationData *pfSimData =pfPacket->get< VSimulationData >
				               (VGetSimulationDataBankName());
      // ***********************************************************
      // I think the following is ignored but do anyway. If it is ignored then
      //  we didn't have to dothis section
      pfSimData->fRunNumber=fRunNumber;
      pfSimData->fEventNumber=fArrayEventNum;
      VSimulationData* pfWriteSimData = pfSimData->copySimData();
      pfWritePacket->put(VGetSimulationDataBankName(), pfWriteSimData);  
      // **********************************************
      // Fix up Kascade simulation data bank in this 
      // packet
      // **********************************************
      VKascadeSimulationData *pfKSimData =
	                        pfPacket->get< VKascadeSimulationData >
				(VGetKascadeSimulationDataBankName());
      pfKSimData->fRunNumber=fRunNumber;
      pfKSimData->fEventNumber=fArrayEventNum;
      // *******************************************
      // At this point we ignore all the sim fIntegralRatePerEventHz etc 
      // variables


      VKascadeSimulationData* pfWriteKSimData=pfKSimData->copyKascadeSimData();
      pfWritePacket->put(VGetKascadeSimulationDataBankName(), pfWriteKSimData);  
    }
  // **********************************************
  // Now the ArrayEvents First fix times and event number in array Trigger
  // *********************************************
  VArrayEvent* pfAEOut  = new VArrayEvent();
  if (!pfPacket->hasArrayEvent())
    {
      std::cout<<"ksMergeFiles - Missing ArrayEventin at packet#: "
	       <<fPacketIndex<<std::endl;
      return;
    }

  VArrayEvent*   pfAEIn     = NULL;
  VArrayTrigger* pfAT       = NULL;

  pfAEIn=pfPacket->getArrayEvent();
  pfAT = pfAEIn->getTrigger();

  // *****************************************************************
  // set the Run Number and event number
  // *******************************************************************
  pfAT->setRunNumber(fRunNumber);
  pfAT->setEventNumber(fArrayEventNum);

  uint16_t fGPSWords[5];
  uint8_t  fGPSYear=kGPSYear;

  fEventTime.getForVBF(fGPSYear,5,fGPSWords);
  
  // ********************************************************************
  // SetGPS Time, I know it looks like a get but its not
  // ********************************************************************
  pfAT->getGPSTime()[0]=fGPSWords[0];
  pfAT->getGPSTime()[1]=fGPSWords[1];
  pfAT->getGPSTime()[2]=fGPSWords[2];
  pfAT->getGPSTime()[3]=fGPSWords[3];
  pfAT->getGPSTime()[4]=fGPSWords[4];
			  
  pfAT->setGPSYear(fGPSYear);
			  

  // ***********************************************************************
  // now put array trigger back into the array event
  // ************************************************************************
  VArrayTrigger* pfWriteAT=pfAT->copyAT();
  pfAEOut->setTrigger(pfWriteAT);
  
  // **************************************************
  // Now fix telescope events
  // *************************************************
  int fNumTriggeredTels =(int) pfAEIn->getNumEvents();

  VEvent* pfEvent = NULL;
  for(int i=0;i<fNumTriggeredTels;i++)
    {
      // set the event number
      pfEvent=pfAEIn->getEvent(i);
      pfEvent->setEventNumber(fArrayEventNum);
      pfEvent->getGPSTime()[0]=fGPSWords[0];
      pfEvent->getGPSTime()[1]=fGPSWords[1];
      pfEvent->getGPSTime()[2]=fGPSWords[2];
      pfEvent->getGPSTime()[3]=fGPSWords[3];
      pfEvent->getGPSTime()[4]=fGPSWords[4];
			      
      pfEvent->setGPSYear(fGPSYear);
      // **************************************
      // add the event to the array event!
      VEvent* pfWriteEvent=pfEvent->copyEvent();
      pfAEOut->addEvent(pfWriteEvent);
    }

  pfAEOut->setRun(fRunNumber);

  // put the array event back into the packet
  pfWritePacket->putArrayEvent(pfAEOut);
			  
  // finally, write the packet into the file
  pfWriter->writePacket(fArrayEventNum,pfWritePacket);
  delete pfWritePacket;
  delete pfPacket;

  fArrayEventNum++;
  return;
}
// **************************************************************************

void SetNextEventTime(VATime& fEventTime, double fEventRateHz)
// **********************************************************************
// Find time of next event following expanetial distribution of time gaps 
// between events.
// **********************************************************************
{
  double fMeanTimeBetweenEventsSec=1.0/fEventRateHz;
  double fEventTimeMJD=fEventTime.getMJDDbl();
  double fTimeGapDay=Rexp(fMeanTimeBetweenEventsSec)/(60.*60.*24.);
  fEventTimeMJD+=fTimeGapDay;
  fEventTime.setFromMJDDbl(fEventTimeMJD);
  return;
}
// *************************************************************************

void SetNextPedEventTime(VATime& fEventTime)
 {
   int fPedSeconds=fEventTime.getSec()+1;
   uint32_t fYear,fMonth,fDay,H,M,S,NS;
   fEventTime.getCalendarDate(fYear,fMonth,fDay);
   fEventTime.getTime(H,M,S,NS);
   NS=0;   //On the tick!
   fEventTime.setFromCalendarDateAndTime(fYear,fMonth,fDay,H,M,
						 fPedSeconds,NS);
   return;
 }
 // *********************************************************************

 
