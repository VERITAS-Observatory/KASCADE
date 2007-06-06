//-*-mode:c++; mode:font-lock;-*-
/** 
 * \class ksSumFiles * \brief This code Merges 2 VBF files and/or converts the
 * ouput file to a tracking frile froma drift scan. The input files are a 
 * BaseFile (usually a CR file) and an optional SourceFile (usually a  
 *  Gamma-ray  file).  Events are 
 *  chosen Randomly from the BaseFile file (To mix energies) at a
 *  specified BaseFileRate. Events are chosen from the Source file (if 
 *  specified) at a seperate rate. Pedestals come from the first file at 1 
 *  per second.
 *  Possible to choose a specific fDirection from the second file.
 *  Events in the output file have approprite fArrayEventNum fEventNum, fTime,
 *  fPrimaryAz, fObservationRA, fPrimaryDeg and fObservationDec
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
#include "VAAzElRADecXY.h"
#include "VAArrayInfo.h"
#include "VAArrayInfoFactoryLite.h"

#include "KSCommon.h"

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
			   VBankFileWriter* pfWriter, int& fArrayEventNum, 
			   int fRunNumber, VATime& fEventTime,double fObsRA,
			   double fObsDec, double fPriRA,double fPriDec);

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
VAAzElRADecXY*  pfConvert;
bool fTrackingMode=false;
double fObsAz=0;
double fObsEl=0;
double fPriAz=0;
double fPriEl=0;
double fLatitude=0;
double fEastLongitude=0;
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

      double fBaseFileRateHz=100.0;
      command_line.findWithValue("BaseFileRateHz",fBaseFileRateHz,
			   "Rate in Hz at which events from the Base "
			   "file will be put into the output file. Default is "
			   "100.0 Hz");
 
      double fSourceFileRateHz=25.0/60.0;
      double fSourceFileRateMin=25.0;
      if(command_line.findWithValue("SourceFileRatePerMinute",
				    fSourceFileRateMin,
				    "Rate per Minute at which events from the "
				    "Source file will be put into the output "
				    "file. Default is 25/min")
	 == VAOptions::FS_FOUND)
	{
	  fSourceFileRateHz=fSourceFileRateMin/60.;
	}
      

      if(command_line.find("TrackingMode",
			   "Enables simulation of a tracking run. Telescope "
			   "Ra/Dec is frozen, Primary and Observation Az/El "
			   "track telescope direction.Default is Drift scan "
			   "mode")
	 == VAOptions::FS_FOUND)
	{
	  std::cout<<"ksMergeFiles - Output file is a Tracking file"
		   <<std::endl;
	  fTrackingMode=true;
	}
      else
	{
	  std::cout<<"ksMergeFiles - Output file is a Drift Scan file"
		   <<std::endl;
	  fTrackingMode=false;
	}

	

      int  fSourceDirectionIndex=-1;
      command_line.findWithValue("SourceDirectionIndex",
				 fSourceDirectionIndex,
				 "Index of Sim.fDirection source direction to "
				 "use when including events from "
				 "the Source File. "
				 "Default is no selction on fDirection index");


 

      std::string fSourceFileName;
      bool fSourceFileSpecified=false;
      if(command_line.findWithValue("SourceFileName",fSourceFileName,
				    "If this options specified events are "
				    "randomly chosen from this file and "
				    "added to the output file at the "
				    "specified rate. If the "
				    "-SourceDirectionIndex option is "
				    "given than only events with that "
				    "VKascadeSimulationData::fDirection are "
				    "used. Optional.")
	 == VAOptions::FS_FOUND)
	{
	  fSourceFileSpecified=true;
	}

      std::string fRandomSeedFileName;
      if(!command_line.findWithValue("RandomSeedFileName",fRandomSeedFileName,
				    "Ranlux seed file")
	 == VAOptions::FS_FOUND)
	{
	  fRandomSeedFileName="ksMergeFiles.ran";
	}
      
      std::string fBaseFileName;
      if(!command_line.findWithValue("BaseFileName",fBaseFileName,
				     "Input file name for Base file. "
				     "Ouput file consists of events randomized"
				     "  from this file with specified rate and"
				     " with pedestal events from this file on "
				     "each second mark. Required!")
	 == VAOptions::FS_FOUND)
	{
	  std::cout<<"ksMergeFiles - BaseFileName Option is required"
		   <<std::endl;
	  usage(progname, command_line);
	  exit(EXIT_FAILURE);
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

      std::cout<<"ksMergeFiles - Input Base File: "<<fBaseFileName
	       <<std::endl;
      if(fSourceFileSpecified)
	{
	  std::cout<<"ksMergeFiles - Input Source File:  "
		   <<fSourceFileName<<std::endl;
	}
      std::cout<<"ksMergeFiles - Output Merged File:     "<<fMergedFileName
	       <<std::endl;

      // ******************************************************************
      // Set up random number generator
      // ******************************************************************
      int printseeds=1;
      ranstart(&printseeds,(char*)fRandomSeedFileName.c_str(),
	       (int)fRandomSeedFileName.length());

      // *****************************************************************
      // Set up convert object. Need Longitude and Latitude
      // Load the default VERITAS arrayInfo at fTime.
      // *****************************************************************
      VATime fTime("2006-08-23 22:00:00 UTC");
      VAArrayInfo* pfArrayInfo=
	VAArrayInfoFactoryLite::instance()->getArrayInfo(fTime); 
      fEastLongitude=pfArrayInfo->longitude();
      fLatitude=pfArrayInfo->latitude();

      pfConvert=new VAAzElRADecXY(fEastLongitude,fLatitude);


      // ******************************************************************
      // Now we are ready to start. Begin by opening the input files;
      // We will then make 3 int vectors based on packet numbers: The first 
      // is of all the packets in the Base file that are not pedestal
      // events. The second is of the pedestal events packet numbers in the
      // Base file. The third is of the Source event packet numbers
      // that are not pedestal events and which match the direction index 
      // criteria.
      // ******************************************************************
      VBankFileReader* pfBaseReader = NULL;
      VBankFileReader* pfSourceReader = NULL;
      VBankFileWriter* pfWriter=NULL;

      VPacket*       pfBasePacket   = NULL;      
      VPacket*       pfSourcePacket   = NULL;      
 
       pfBaseReader = new VBankFileReader(fBaseFileName);

      int fNumBasePackets = pfBaseReader->numPackets();

      // ******************************************************************
      // Create and fill Base events packet numbers vector.
      // ******************************************************************
      std::vector< int > pfBaseEventPackets;
      pfBaseEventPackets.clear();
      std::vector< int > pfBasePedEventPackets;
      pfBasePedEventPackets.clear();

      VArrayEvent*   pfAEIn     = NULL;
      VArrayTrigger* pfAT       = NULL;
      std::cout<<"ksMergeFiles - Creating Base File Event Packet Event "
	"number vector"<<std::endl;
      std::cout<<"ksMergeFiles - Takes a couple of minuets"<<std::endl;
      std::cout<<"ksMergeFiles - Events:(#=10000):";
      std::cout.flush();
      for(int i=1;i<fNumBasePackets;i++) //Packet 0 for header  packets
	{
	  if(i%10000==0)
	    {
	      std::cout<<"#";
	      std::cout.flush();
	    }


	  if(!pfBaseReader->hasPacket(i))
	    {
	      std::cout<<"ksMergeFiles - Missing packet. File: "
		       <<fBaseFileName<<" at packet#: "<<i
		       <<std::endl;
	      continue;
	    }
	  pfBasePacket=pfBaseReader->readPacket(i); 

	  if (!pfBasePacket->hasArrayEvent())
	    {
	      std::cout<<"ksMergeFiles - Missing ArrayEvent in File:"
		       <<fBaseFileName<<" at packet#: "<<i<<std::endl;
	      delete pfBasePacket;
	      continue;
	    } 
	  pfAEIn=pfBasePacket->getArrayEvent();
	  pfAT = pfAEIn->getTrigger();
	  if(pfAT->getEventType().trigger==VEventType::PED_TRIGGER)
	    {
	      pfBasePedEventPackets.push_back(i);
	    }
	  else if(pfAT->getEventType().trigger==VEventType::L2_TRIGGER)
	    {	      pfBaseEventPackets.push_back(i);
	    }
	  else
	    {
	      std::cout<<"ksMergeFiles - Unacceptable event type "
		       <<pfAT->getEventType().trigger<<" from Base file "
		       <<fBaseFileName<<" at packet#: "<<i<<std::endl;
	      delete pfBasePacket;
	      continue;
	    }
	 delete pfBasePacket; 	      
	}
      std::cout<<std::endl;

      int fNumBaseEventPackets=pfBaseEventPackets.size();
      int fNumBasePedEventPackets=pfBasePedEventPackets.size();
      std::cout<<"ksMergeFiles - Number of Events Base File: "
	       <<fNumBaseEventPackets<<std::endl;
      std::cout<<"ksMergeFiles - Number of Pedestal Events Base File: "
	       <<fNumBasePedEventPackets<<std::endl;

      double fRunLengthSec=fNumBaseEventPackets/fBaseFileRateHz;

      // ********************************************************************
      // Now do the same for the Source file
      // ********************************************************************
      std::vector< int > pfSourceEventPackets;
      double fSourceRunLengthSec;
      if(fSourceFileSpecified)
	{
	  pfSourceReader = new VBankFileReader(fSourceFileName);
	  int fNumSourcePackets = pfSourceReader->numPackets();

	  pfSourceEventPackets.clear();

	  std::cout<<"ksMergeFiles - Creating Source File Event Packet Event "
	    "number vector"<<std::endl;
	  std::cout<<"ksMergeFiles - Takes a few more minuets"<<std::endl;
	  for(int i=1;i<=fNumSourcePackets;i++) //Packet 0 for header  packets
	    {
	      if(!pfSourceReader->hasPacket(i))
		{
		  std::cout<<"ksMergeFiles - Missing packet. File: "
			   <<fSourceFileName<<" at packet#: "<<i
			   <<std::endl;
		  continue;
		}

	      pfSourcePacket=pfSourceReader->readPacket(i); 
	      
	      if (!pfSourcePacket->hasArrayEvent())
		{
		  std::cout<<"ksMergeFiles - Missing ArrayEvent in File:"
			   <<fSourceFileName<<" at packet#: "<<i<<std::endl;
		  delete pfSourcePacket;
		  continue;
		} 

	      // ***********************************************************
	      // NOte: NO DIRECTION SELECTION AS OF YET. Here is where it will
	      // go
	      // ************************************************************
	      pfAEIn=pfSourcePacket->getArrayEvent();
	      pfAT = pfAEIn->getTrigger();
	      if(pfAT->getEventType().trigger==VEventType::PED_TRIGGER)
		{
		  delete pfSourcePacket;
		  continue;  //ignore ped events
		}
	      else if(pfAT->getEventType().trigger==VEventType::L2_TRIGGER)
		{
		  pfSourceEventPackets.push_back(i);
		}
	      else
		{
		  std::cout<<"ksMergeFiles - Unacceptable event type "
			   <<pfAT->getEventType().trigger
			   <<" from CosmiRay file "<<fSourceFileName
			   <<" at packet#: "<<i<<std::endl;
		  delete pfSourcePacket;
		  continue;
		}
	      delete pfSourcePacket;
	      
	    }

	  int fNumSourceEventPackets=pfSourceEventPackets.size();
	  std::cout<<"ksMergeFiles - Number of Events Source File: "
		   <<fNumSourceEventPackets<<std::endl;
	  fSourceRunLengthSec=fNumSourceEventPackets/fSourceFileRateHz;
	  if(fSourceRunLengthSec<fRunLengthSec)
	    {
	      fRunLengthSec=fSourceRunLengthSec;
	    }
	}
      std::cout<<"ksMergeFiles - Expected Merged Run Length: "
	       <<fRunLengthSec/60.<<" min."<<std::endl;

      // ******************************************************************
      // Initalize the ouput file: Copy over Source bank 0 (if available). We 
      // had the choice of the headers from the Source file or the Base file. 
      // For no good reason use the Source simulation headers.
      // Get the start-of-run event time, run number etc from first Base event.
      // ******************************************************************
      int fArrayEventNum=1;   //VBF events start at 1, 0 is for header
      std::vector< bool> fConfigMask;

      uword32 fRunNumber = pfBaseReader->getRunNumber();
      std::cout<<"ksMergeFile: RunNumber: "<<fRunNumber<<std::endl;

      fConfigMask= pfBaseReader->getConfigMask();
      pfWriter = new VBankFileWriter(fMergedFileName, fRunNumber, fConfigMask);
      if(pfWriter==NULL)
	{
	  std::cout<<"ksMergeFiles--Output VBF file failed to "
	    "open"<<std::endl;
	  exit(EXIT_FAILURE);
	}	      
		      
      // ******************************************************
      // copy over the first packet, this is the header 
      // packet, no events in it
      // ******************************************************
      if(fSourceFileSpecified)
	{
	  pfSourcePacket=pfSourceReader->readPacket(0);
	  pfWriter->writePacket(0, pfSourcePacket);
	  delete pfSourcePacket;
	}		      
      else
	{
	  pfBasePacket=pfBaseReader->readPacket(0);
	  pfWriter->writePacket(0, pfBasePacket);
	  delete pfBasePacket;
	}

      // ******************************************************
      // Now we need a first event time to use. Use first 
      // event time in the first event in the Base file
      // ******************************************************
      VATime fEventTime;
      uint8_t fGPSYear=kGPSYear;

      if(!pfBaseReader->hasPacket(1))
	{
	  std::cout<<"ksMergeFiles - Missing packet #1. File: "
		   <<fBaseFileName<<std::endl;
	  exit(EXIT_FAILURE);
	}

      pfBasePacket=pfBaseReader->readPacket(1);
      pfAEIn=pfBasePacket->getArrayEvent();
      if(!pfAEIn->hasTrigger())
	{
	  std::cout<<"ksMergeFiles - Problem reading ArrayTrigger first "
		   <<std::endl;
	  exit(EXIT_FAILURE);
	}
      
      pfAT = pfAEIn->getTrigger();
      fEventTime.setFromVBF(fGPSYear,pfAT->getGPSTimeNumElements(),
				pfAT->getGPSTime());
      std::cout<<"ksMergeFiles - Merged File RunStart Time: "<< fEventTime
	       <<std::endl;

      // *********************************************************************
      // For tracking runs we need to know what the Ra/Dec will be
      // *********************************************************************
      double fObsRA=-1;
      double fObsDec=-1;
      double fPriRA=-1;
      double fPriDec=-1;
     
      if(fTrackingMode)
	{
	  VSimulationData *pfSimData =pfBasePacket->get< VSimulationData >
	                                       (VGetSimulationDataBankName());
	  double fObservationElev=
	    (double)( (90.0-pfSimData->fObservationZenithDeg)/gRad2Deg); 
	  double fObservationAz=
	    (double)(pfSimData->fObservationAzimuthDeg/gRad2Deg);
	  pfConvert->AzEl2RADec2000(fObservationAz,fObservationElev,fEventTime,
				  fObsRA,fObsDec);
	  //Move this Az/El to the ~ middle of the run.
	  double fRAShift=(fRunLengthSec/(60.*60.*24.))*M_PI;
	  fObsRA=fObsRA + fRAShift;

	  std::cout<<"ksMergeFiles - Tracking Direction RA: "
		   <<pfConvert->RAToString(fObsRA);
	  std::cout<<"ksMergeFiles - Tracking Direction Dec: "
		   <<pfConvert->DecToString(fObsDec);
	  double fPrimaryElev=
	    (double)( (90.0-pfSimData->fPrimaryZenithDeg)/gRad2Deg); 
	  double fPrimaryAz=
	    (double)(pfSimData->fPrimaryAzimuthDeg/gRad2Deg);
	  pfConvert->AzEl2RADec2000(fPrimaryAz,fPrimaryElev,fEventTime,
				  fPriRA,fPriDec);
	  // ************************************************************
	  //Move this Az/El to the ~ middle of the run(note implied div by 2).
	  // ************************************************************
	  fPriRA=fPriRA + fRAShift;
	  
	  std::cout<<"ksMergeFiles - Source Direction RA: "
		   <<pfConvert->RAToString(fPriRA);
	  std::cout<<"ksMergeFiles - Source Direction Dec: "
		   <<pfConvert->DecToString(fPriDec)<<std::endl;
	}
  
      delete pfBasePacket;





      std::cout<<"ksMergeFiles:Merging Files.....Takes even longer"
	       <<std::endl;


      // ****************************************************************
      // Randomly select event times for the first event of each type:
      // Base, Base Ped or Source. Use rexp delta T distribution
      // ****************************************************************
      VATime fBaseEventTime=fEventTime;
      VATime fBasePedEventTime=fEventTime;;
      VATime fSourceEventTime=fEventTime;;

      //SetNextEventTime(fBaseEventTime,fBaseFileRateHz);
      SetNextPedEventTime(fBasePedEventTime);
      if(fSourceFileSpecified)
	{
	  SetNextEventTime(fSourceEventTime,fSourceFileRateHz);
	}

      int fNumBaseEvents=0;
      int fNumBasePedEvents=0;
      int fNumSourceEvents=0;

    
      float fXDummy;

      // ********************************************************************
      // MAIN LOOP
      // ********************************************************************
      // Our loop will be over existance of Base  events that haven't been 
      // written out. 
      // *********************************************************************
      
      std::vector< int >::iterator fPos;
      VATime fLastTime;
      std::cout<<"ksMergeFiles - Creating Output file"<<std::endl;
      while(pfBaseEventPackets.size()>0)
	{
	  // ***************************************************************
	  // Find which type of event to write. Is a Source event next?
	  // ***************************************************************
	  if(fSourceFileSpecified && fSourceEventTime<fBasePedEventTime && 
		  fSourceEventTime<fBaseEventTime)
	    {
	      if(pfSourceEventPackets.size()== 0 )
		{
		  std::cout<<"ksMergeFiles - Ran out of Source events"
			   <<std::endl;
		  break;
		}
	      int fIndex=(int)(pfSourceEventPackets.size()*pran(&fXDummy));
	      if(fIndex==(int)pfSourceEventPackets.size())
		{
		  fIndex=pfSourceEventPackets.size()-1;
		}
	      int fPacketIndex=pfSourceEventPackets.at(fIndex);
	      fPos=std::find(pfSourceEventPackets.begin(),
			     pfSourceEventPackets.end(),
			     fPacketIndex);
	      if(fPos==pfSourceEventPackets.end())
		{
		  std::cout<<"ksMergeFiles - Failed to build interator for "
		    "position "<<fPacketIndex<<" in Source file"<<std::endl;
		  exit(EXIT_FAILURE);
		}

	      pfSourceEventPackets.erase(fPos);
	      
	      CopyEventToMergedFile(pfSourceReader,fPacketIndex,pfWriter,
				    fArrayEventNum,fRunNumber,fSourceEventTime,
				    fObsRA,fObsDec,fPriRA,fPriDec);
	      
	      fLastTime=fSourceEventTime;
	      SetNextEventTime(fSourceEventTime,fSourceFileRateHz);	      
	      fNumSourceEvents++;
	    }
	  else if(fBaseEventTime<fBasePedEventTime)
	    {
	      // *************************************************
	      // Randomly pick from our vector a position in the vector and use
	      // that packet number.
	      // ******************************************************
	      int fIndex=(int)(pfBaseEventPackets.size()*pran(&fXDummy));
	      if(fIndex==(int)pfBaseEventPackets.size())
		{
		  fIndex=pfBaseEventPackets.size()-1;
		}
	      int fPacketIndex=pfBaseEventPackets.at(fIndex);
	      
	      // *********************************************************
	      // Now the trick. The reason we are using vectors. Remove the 
	      // fIndex element from the vector.
	      // pfBaseEventPackets.begin()[fIndex] is an iterator pointing to
	      // the fIndex entry.
	      // *********************************************************
	      fPos=find(pfBaseEventPackets.begin(),pfBaseEventPackets.end(),
		       fPacketIndex);
	      if(fPos==pfBaseEventPackets.end())
		{
		  std::cout<<"ksMergeFiles - Failed to build interator for "
		    "position "<<fPacketIndex<<" in Base file"<<std::endl;
		  exit(EXIT_FAILURE);
		}
	      pfBaseEventPackets.erase(fPos);
	      
	      // *********************************************************

	      CopyEventToMergedFile(pfBaseReader,fPacketIndex,pfWriter,
				    fArrayEventNum,fRunNumber,fBaseEventTime,
				    fObsRA,fObsDec,fPriRA,fPriDec);
	      
	      fLastTime=fBaseEventTime;
	      SetNextEventTime(fBaseEventTime,fBaseFileRateHz);	      
	      if(fNumBaseEvents%10000==0)
		{
		  double fPercentDone=(float)(fNumBaseEvents)*100.0/
		                                      (float)fNumBasePackets;
		  std::cout<<"ksMergeFiles - Event#: "<<fNumBaseEvents<<"("
			   <<fPercentDone<<"%)"<<std::endl;

		}

	      fNumBaseEvents++;
	    }
	  else if(pfBasePedEventPackets.size() > 0)
	    {
	      // ***********************************************************
	      // Just take the ped events as they come
	      // ***********************************************************
	      int fPacketIndex=pfBasePedEventPackets.at(0);
	      fPos=pfBasePedEventPackets.begin();
	      if(fPos==pfBaseEventPackets.end())
		{
		  std::cout<<"ksMergeFiles - Failed to get pos for Pedestal "
		    "events"<<std::endl;
		  break;
		}

	      pfBasePedEventPackets.erase(fPos);

	      CopyEventToMergedFile(pfBaseReader,fPacketIndex,pfWriter,
				    fArrayEventNum,fRunNumber,
				    fBasePedEventTime,
				    fObsRA,fObsDec,fPriRA,fPriDec);

	      fLastTime=fBasePedEventTime;
	      SetNextPedEventTime(fBasePedEventTime);	      
	      fNumBasePedEvents++;
	    }
	  else
	    {
	      std::cout<<"ksMergeFiles - Out of Base Pedestal events"
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
      std::cout<<"ksMergeFiles - Number of Base Events written:"
	       <<fNumBaseEvents<<std::endl;
      std::cout<<"ksMergeFiles - Number of Base Pedestal Events written:"
	       <<fNumBasePedEvents<<std::endl;
      std::cout<<"ksMergeFiles - Number of Source Events written:"
	       <<fNumSourceEvents<<std::endl;

      std::cout<<"ksMergeFiles - End of Run at: "<<fLastTime<<std::endl;
      std::cout<<"ksMergeFiles - End Observation Az: "<<fObsAz*gRad2Deg
	       <<" Observation Zenith: "<<(90.-fObsEl*gRad2Deg)<<std::endl;
      std::cout<<"ksMergeFiles - End Primary Az: "<<fPriAz*gRad2Deg
	       <<" Prmary Zenith: "<<(90.-fPriEl*gRad2Deg)<<std::endl;
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
			    VBankFileWriter* pfWriter, int& fArrayEventNum, 
			    int fRunNumber, VATime& fEventTime,double fObsRA,
			    double fObsDec, double fPriRA, double fPriDec)
// ***********************************************************************
// Copy a packet form th intput reader to the Merged ouput file. Be sure to 
// update all times run numbers and event numbers.
// ************************************************************************
{
  VPacket* pfWritePacket = new VPacket();
  VPacket* pfPacket=pfReader->readPacket(fPacketIndex); 
			  
  // *************************************************
  // Update event numbers here and maybe times an maybe AzEl of obs and pri
  // *************************************************
  fObsAz=0;
  fObsEl=0;
  fPriAz=0;
  fPriEl=0;
  if(fTrackingMode)
    {
      pfConvert->RADec2000ToAzEl(fObsRA,fObsDec,fEventTime,fObsAz,fObsEl);
      pfConvert->RADec2000ToAzEl(fPriRA,fPriDec,fEventTime,fPriAz,fPriEl);
    }
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
      if(fTrackingMode)
	{
	  pfSimData->fObservationZenithDeg=90.0-(fObsEl*gRad2Deg);
	  pfSimData->fObservationAzimuthDeg=fObsAz*gRad2Deg;
	  pfSimData->fPrimaryZenithDeg=90.-(fPriEl*gRad2Deg);
	  pfSimData->fPrimaryAzimuthDeg=fPriAz*gRad2Deg;
	}
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
      pfWritePacket->put(VGetKascadeSimulationDataBankName(), 
			 pfWriteKSimData);  
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
  // Reset directions if we are tracking
  // ***********************************************************************
  if(fTrackingMode)
    {
      pfAT->setAltitude(0, (float)(90.0-(fObsEl*gRad2Deg)) );
      pfAT->setAzimuth(0,(float)(fObsAz*gRad2Deg));
    }


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
  if(fArrayEventNum==1)
    {
      std::cout<<"ksMergeFiles - Start Observation Az: "<<fObsAz*gRad2Deg
	       <<" Observation Zenith: "<<(90.-fObsEl*gRad2Deg)<<std::endl;
      std::cout<<"ksMergeFiles - Start Primary Az: "<<fPriAz*gRad2Deg
	       <<" Primary Zenith: "<<(90.-fPriEl*gRad2Deg)<<std::endl;
      double fDerotangle = atan2(-1.0*cos(fLatitude)*sin(fObsAz),
				 (cos(fObsEl)*sin(fLatitude) - 
			      sin(fObsEl)*cos(fObsAz)));
      std::cout<<"ksMergeFiles - Max Image rotation(deg): "
	       <<180.+fDerotangle*gRad2Deg<<std::endl;
    }

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

 
