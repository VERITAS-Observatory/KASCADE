//-*-mode:c++; mode:font-lock;-*-
/** 
 * \class ksSumFiles
 * \brief This code Appends all the VBF and/or root (kasAomega output)
 *  files into a single file, cutting on spectrum if so desired.
 *  Events in both output files have same fArrayEventNum and fEventNum
 *  Lots of this code from VBF/examples/PrintEvents.cpp
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

// 15-May-2005
//Modified:


#include <iostream>
#include <iomanip>
#include <fstream>
#include <string>
#include <cmath>
#include <time.h>

#include <VBF/VBankFileReader.h>
#include <VBF/VBankFileWriter.h>
#include <VBF/VPacket.h>
#include <VBF/VArrayEvent.h>
#include <VBF/VDatum.h>
#include <VBF/VKascadeSimulationData.h>
#include <VBF/VKascadeSimulationHeader.h>
#include <VBF/VEventType.h>

#include "VAVDF.h"
#include "VAKascadeSimulationData.h"
#include "VAException.h"
#include "VSOptions.hpp"
#include "VACommon.h"
#include "VATime.h"

#include "KSEventWeights.h"

#include "TROOT.h"
#include "TTree.h"

// declare that we're using the VConfigMaskUtil namespace, which gives us
// easy access to parseConfigMask().
// include the configuration mask utilities, which give us parseConfigMask()
#include <VBF/VConfigMaskUtil.h>
using namespace VConfigMaskUtil;

const double kEventRateHZ=250.0;  //Allows for ~20*e6 event in a day(limit of
                                  //Qstats time etc).

extern "C" void ranstart(int* printseedflag, char* random_seed_file_name, 
			 int length);
extern "C" void ranend(int* printseedflag, char* random_seed_file_name, 
		       int length);
extern "C" float pran(float* dummy);

extern "C" double Rexp(double fScaleFactor);


void usage(const std::string& progname, 
	   const VAOptions& command_line)
{
  std::cout << std::endl;
  std::cout << "ksSumFiles: Usage: " << progname 
	    << " [options]  <Input File List Name>" 
	    << std::endl;
  std::cout <<" Input File List Name is  required to be "
    "specified in command line"<<std::endl;
  std::cout<<"ksSumFiles: Options: "<<std::endl;
  command_line.printUsage(std::cout);
}

int main(int argc, char** argv)
{ 
  try
    {
      time_t thetime=time(NULL);
      std::cout<<"ksSumFiles:  START------"<<ctime(&thetime)<<std::endl;
      
      // **********************************************************************
      // Pick up command line arguments
      // **********************************************************************
      std::string progname = *argv;
      VAOptions command_line(argc,argv);

      bool fWeightBySpectrum=false;
      if(command_line.find("EnableSpectrumWeighting",
			   "Indicates that while appending events from files "
			   "from the input file list, cuts are to be made "
			   "accoring to a weighted spectrum. The weights are "
			   "determined from the spectrum for the particle "
			   "type of the list and the number of showers at "
			   "each energy. These values are derived from the "
			   "shower list.")
	 == VAOptions::FS_FOUND)
	{
	  fWeightBySpectrum=true;
	}
      std::string fRandomSeedFileName;
      if(!command_line.findWithValue("RandomSeedFileName",fRandomSeedFileName,
				    "Ranlux seed file")
	 == VAOptions::FS_FOUND)
	{
	  fRandomSeedFileName="ksSumFiles.ran";
	}
      std::string fOutputVBFFileName;
      bool fOutputVBF=false;
      if(command_line.findWithValue("OutputVBFFileName",fOutputVBFFileName,
				    "File name for summary VBF file")
	 == VAOptions::FS_FOUND)
	{
	  fOutputVBF=true;
	}
      else
	{
	  std::cout<<"No OutputVBFFileName Option:"<<std::endl;
	}
      std::string fOutputRootFileName;
      bool fOutputRoot=false;
      if(command_line.findWithValue("OutputRootFileName",fOutputRootFileName,
				    "File name for summary Root file")
	 == VAOptions::FS_FOUND)
	{
	  fOutputRoot=true;
	}
      else
	{
	  std::cout<<"No OutputRootFileName Option:"<<std::endl;
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
      if(argc<0)
	{
	  usage(progname, command_line);
	  exit(EXIT_FAILURE);
	}

      // ******************************************************************
      // Set up random number generator
      // ******************************************************************
      int printseeds=1;
      ranstart(&printseeds,(char*)fRandomSeedFileName.c_str(),
	       (int)fRandomSeedFileName.length());


      std::string ListFileName=*argv;
        
      std::cout<<"ksSumFiles:Input List File Name: "<<ListFileName
	       <<std::endl;

      if(!fOutputVBF && !fOutputRoot)
	{
	  std::cout<<"ksSumFiles:Either or both OutputVBFFileName or "
	    "OutputRootFileName options must be specified."<<std::endl;
	  exit(1);
	}

      if(fOutputVBF)
	{
	  std::cout<<"ksSumFiles:Output VBF File Name: "<<fOutputVBFFileName
		   <<std::endl;
	}
      if(fOutputRoot)
	{
	  std::cout<<"ksSumFiles:Output Root File Name: "<<fOutputRootFileName
		   <<std::endl;
	}


      // ********************************************************************
      // Do we need weights, that is do we make spectrum cuts?
      // ********************************************************************

      KSEventWeights* pfWeights=NULL;
      float fXDummy=0;
      if(fWeightBySpectrum)
	{
	  std::cout<<"ksSumFiles:Determining Spectrum weights"
	    ".....Takes a while"<<std::endl;
	  // -----------------------------------------------------------------
	  // Open input list file
	  // -----------------------------------------------------------------
	  
	  std::ifstream fListIn;
	  fListIn.open(ListFileName.c_str());
	  if(!fListIn)
	    {
	      std::cout<<"Failed to open Input List file: "<<ListFileName
		       <<std::endl;
	      return 0;
	    }

 	  // ****************************************************************
          // To determine spectrum weights we need to get 3 things:
	  // 1:Particle type: gamma, protons he4 etc. We allow only 1 particle
	  //   type in all the files in the List.
	  // 2:Shower energies. Assume these are ksAomega files with 
	  //   VASimulationHeader records. Also assume that we have a number 
	  //   of files at each energy.
	  // 3:Number of showers at each energy.
	  // *****************************************************************

	   std::map<int, fShwrMap_t > fTypes;   //Map of a map
	   std::map<int, fShwrMap_t >::iterator fTypesPos;
	   fShwrMap_t::iterator fShowerDataPos;

	  //Ready to read in showers. Note this list dones not have the .vbf or
	   //.root extentions in it.

	  std::string fInputFile;
	  while(getline(fListIn,fInputFile))
	    {
	      int fType;
	      int fEnergyGeV;
	      if(fOutputVBF)
		{
		  // *********************************************************
		  // Open a VBF file (if options indicate it exists)from list
		  // create a reader for the bank file.this will open the file 
		  // in a read-only fashion and will memory-map the index, 
		  // allowing you random access.
		  // **********************************************************
		  std::string fVBFFileName=fInputFile + ".vbf";
		  //std::cout<<"Get weights file:"<<fVBFFileName<<std::endl;
		  VBankFileReader reader(fVBFFileName);

		  // **************************************  *****************
		  // read header packet.  note that we have to dispose of this 
		  // packet when we're done with it.  note furthermore that 
		  // this function never returns NULL.  if there is an error,
		  // it just throws an exception.
		  // **********************************************************
		  VPacket* packet;
		  if(reader.hasPacket(0))
		    {
		      packet=reader.readPacket(0);  //0=Location of header
		    }
		  else
		    {
		      std::cout<<"ksSumFiles: No header packet with index 0 "
			"found in file: "<<fVBFFileName<<std::endl;
		      exit(1);
		    }
		  // ********************************************************* 
		  // Check this packet has a VKascadeSimulationHeader in it
		  // ********************************************************* 
		  if (!packet->has(VGetKascadeSimulationHeaderBankName())  )
		    {
		      std::cout<<"ksSumFiles: No KascadeSimulationHeader bank "
			"in file: "<<fVBFFileName<<std::endl;
		      exit(1);
		    }
		  // ********************************************************
		  // now get the KascadeSimulationHeader bank.  this will 
		  // never return NULL. If the packet doesn't contain an 
		  // KascadeSimulationHeader, this function throws an 
		  // exception.
		  // ********************************************************
		  VKascadeSimulationHeader *pfKVBFSimHead =
		    packet->get< VKascadeSimulationHeader >
		    (VGetKascadeSimulationHeaderBankName());
		  if(pfKVBFSimHead==NULL)
		    {
		      std::cout<<"ksSumFiles: File "<<fVBFFileName
			       <<"has no VASimulationHeader record"<<std::endl;
		      exit(1);
		    }
		  // ********************************************************
		  //Now get stuff from the header
		  // ********************************************************
		  //Primary type first
		  fType = pfKVBFSimHead->fCORSIKAParticleID;
		  // Now get energy
		  fEnergyGeV = (int)pfKVBFSimHead->fEnergyGeV;
		}
	      else
		{                     //Must be only a root file specified
		  // Open file from list
		  VAVDF fFileIn;
		  std::string fRootFileName=fInputFile + ".root";
		  fFileIn.OpenForStage3(fRootFileName);
		  VASimulationHeader* pfRootSimHead=
		                             fFileIn.getSimulationHeaderPtr();
		  VAKascadeSimulationHead *pfKRootSimHead;
		  if(pfRootSimHead->fSimulationPackage==
		                              VASimulationHeader::E_KASCADE)
		  {
		    pfKRootSimHead = 
		      dynamic_cast< VAKascadeSimulationHead* >(pfRootSimHead);
		    if(pfKRootSimHead==NULL)
		      {
			std::cout<<"ksSumFiles: File "<<fRootFileName
				 <<"has no VASimulationHeader record"
				 <<std::endl;
			exit(1);
		      }
		  }
		  else
		    {
		      std::cout<<"ksSumFiles: Wrong simulation package "
		  	"found.fSimulationPackage: "
		  	       << pfRootSimHead->fSimulationPackage<<std::endl;
		      exit(1);
		    }
		  //Now get stuff from the header
		  //Primary type first
		  fType = pfKRootSimHead->fCORSIKAParticleID;
		  // Now get energy
		  fEnergyGeV=(int)pfKRootSimHead->fEnergyGeV;
		}
		  
	      // **********************************************************
	      // Enter this stuff in the map. This is pretty tricky, we use
	      // iterators to avoid copying maps. Much faster that way
	      // **********************************************************
	      fTypesPos=fTypes.find(fType);
	      if(fTypesPos==fTypes.end())  //If fType doesn't exist as a key
		{                           //create it. See pg 206 C_++ std 
		                            //book for [] operator
		  fTypes[fType];
		  fTypesPos=fTypes.find(fType);
		}
	      fShowerDataPos=fTypesPos->second.find(fEnergyGeV);
	      if(fShowerDataPos==fTypesPos->second.end())
		{
		  fTypesPos->second[fEnergyGeV];
		  fShowerDataPos=fTypesPos->second.find(fEnergyGeV);
		}
	      int fNumShwr=fShowerDataPos->second;
	      fShowerDataPos->second=fNumShwr+1;
	    }  //End of while
	  //done going through the list. 
	  fListIn.close();
	  // ***************************************************************
	  // At this point our maps in fTypes have keys which are the different
	  // energies of all the showers in the List. The values for each of 
	  // the keys is the number of showers at each energy. These entries 
	  // are ordered in out type maps  in increasing energies (I hope)
	  // ***************************************************************
	  // Now get the weights. 
	  //From CORSIKA manual Table 4 (pg 80 in current manual)
	  //
	  //gamma        1
	  //e+           2
	  //e-           3
	  //muon+        5
	  //muon-        6
	  //neutron      13
	  //proton       14
	  //anti-proton  15
	  //ION(Z,A)     A x 100 + Z
	  //He(2,4)      402
	  //Fe(26,56)    5626
	  pfWeights = new KSEventWeights(fTypes);
	  pfWeights->calculateWeights();
	  pfWeights->Print();
	} //end of weight calculation


      // ******************************************************************
      // Now we are ready to start. Begin by creating the output files;
      
      VBankFileWriter* pfWriter=NULL;

      VAVDF* pfSumFile;
      VACalibratedArrayEvent* pfSumCalEvent=NULL;
      VAKascadeSimulationData* pfSumSimEvent=NULL;

      std::string fInputFile;
      bool fFirstFile=true;
      int fNumTels=1;
      VATime fFirstValidEventTime;
      VATime fLastValidEventTime;
      VATime fEventTime;
      double fMeanTimeBetweenEventsSec=1./kEventRateHZ;
      std::cout<<"ksSumFiles:Merging Files.....Takes even longer"
	       <<std::endl;
      // -----------------------------------------------------------------
      // Open input list file
      // -----------------------------------------------------------------
      
      std::ifstream fListIn;
      fListIn.open(ListFileName.c_str());
      if(!fListIn)
	{
	  std::cout<<"Failed to open Input List file: "<<ListFileName
		   <<std::endl;
	  return 0;
	}

      // ****************************************************************
      // Loop over files in the list
      // ****************************************************************
      int fArrayEventNum=1;   //VBF events start at 1, 0 is for header
      uword32 fRunNumber=0;
      int fNumPedEvents=0;
      int fNumNormalEvents=0;
      int fCountOut=0;
      while(getline(fListIn,fInputFile))
	{

	  int fType;
	  int fEnergyGeV;
	  VBankFileReader* pfReader;
	  VPacket*       pfPacket = NULL;
	  VArrayEvent*   pfAEIn   = NULL;
	  VEvent*        pfEvent  = NULL;
	  VArrayTrigger* pfAT     = NULL;
	  int fNumArrayEvents;
	  int fNumVBFArrayEvents;
	  int fNumRootArrayEvents;
	  std::string fVBFFileName;
	  

	  if(fOutputVBF)
	    {
	      // Use the KascadeSimulationHeader from the first file for the 
	      //summary file. May improve it a little later.
	      // Use first file in file list to get these things
	      // Set up to write things out. We do this using VBF commands 
	      // directly
	      // *************************************************************
	      // *************************************************************
	      // Open file from list
	      // ************************************************************
	      fVBFFileName= fInputFile + ".vbf";
	      pfReader = new VBankFileReader(fVBFFileName);
	      fNumVBFArrayEvents = pfReader->numPackets()-1;
	      fNumArrayEvents=fNumVBFArrayEvents;
	      //Now get stuff from the header
	      //Primary type first
	      pfPacket=pfReader->readPacket(0);  //0=Location of header

	      VKascadeSimulationHeader *pfKVBFSimHead =
		pfPacket->get< VKascadeSimulationHeader >
		(VGetKascadeSimulationHeaderBankName());
	      //Now get stuff from the header
	      //Primary type first
	      fType = pfKVBFSimHead->fCORSIKAParticleID;
	      // Now get energy
	      fEnergyGeV=(int)pfKVBFSimHead->fEnergyGeV;
	    }
	  VAVDF fFileIn;
	  VACalibratedArrayEvent* pfInCalEvent;
	  VASimulationHeader* pfRootSimHead;
	  VAKascadeSimulationHead *pfKRootSimHead;
	  VAKascadeSimulationData* pfKInSimEvent;
	  std::string fRootFileName;

	  if(fOutputRoot)
	    {
	      // Open file from list
	      fRootFileName= fInputFile + ".root";
	      fFileIn.OpenForStage3(fRootFileName);
	      
	      pfKInSimEvent = fFileIn.getKascadeSimulationDataPtr();
	      if(pfKInSimEvent==NULL)
	      	{
	      	  std::cout<<"ksSumFiles:Failed to get "
		    "VAKascadeSimulationData* in file:"<<fRootFileName
			   <<std::endl;
	      	  exit(1);
	      	}	      
	      fNumRootArrayEvents = fFileIn.getNumArrayEvents();
	      fNumArrayEvents = fNumRootArrayEvents;
	      pfRootSimHead = fFileIn.getSimulationHeaderPtr();
	      pfKRootSimHead = 
		dynamic_cast< VAKascadeSimulationHead* >(pfRootSimHead);
	      fType = pfKRootSimHead->fCORSIKAParticleID;
	      fEnergyGeV=(int)pfKRootSimHead->fEnergyGeV;
	      pfInCalEvent = fFileIn.getCalibratedArrayEventPtr();
	    }

	  if(fOutputVBF && fOutputRoot)
	    {
	      if( fNumVBFArrayEvents!=fNumRootArrayEvents)
		{
		  std::cout<<"ksSumFiles:: VBF and Root file have "
		    "differing number of events"<<std::endl;
		  std::cout<<fVBFFileName<<" has "
			   <<fNumVBFArrayEvents<<std::endl;
		  std::cout<<fRootFileName<<" has "
			   <<fNumRootArrayEvents<<std::endl;
		}
	    }
	  if(fNumArrayEvents>0)
	    {
	      // *********************************************************
	      // First input file with an event in it? Use its various 
	      // objects for the summary file
	      // Copy over the header packet and use the first event time 
	      // as our starting time
	      // **********************************************************
	      if(fFirstFile)
		{
		  if(fOutputVBF)
		    {
		      fRunNumber = pfReader->getRunNumber();
		      std::string fConfigMask("0");
		      pfWriter = new VBankFileWriter(fOutputVBFFileName,
						     fRunNumber,
				      parseConfigMask(fConfigMask.c_str()));
		      if(pfWriter==NULL)
			{
			  std::cout<<"ksSumFiles--Output VBF file failed to "
			    "open"<<std::endl;
			  exit(1);
			}	      
		      
		      // ******************************************************
		      // copy over the first packet, this is the header 
		      // packet, no events in it
		      // ******************************************************
		      pfWriter->writePacket(0, pfPacket);
		      
		      // ******************************************************
		      // Now we need a first event time to use. Use first 
		      // event time in the first event in the first file
		      // ******************************************************
		      pfPacket=pfReader->readPacket(1);
		      pfAEIn=pfPacket->getArrayEvent();
		      if(pfAEIn->hasTrigger())
			{
			  pfAT = pfAEIn->getTrigger();
			  fEventTime.setFromVBF(5,
						pfAT->getGPSTimeNumElements(),
						pfAT->getGPSTime());
			}
		      else
			{
			  std::cout<<"ksSumFiles: Problem reading "
			    "ArrayTrigger first event"<<std::endl;
			  exit(1);
			}
		      std::cout<<"RunStart Time: "<< fEventTime<<std::endl;
		    }         //End VBF file
		  if(fOutputRoot)
		    {
		      // *****************************************************
		      // First input file? Use its various objects for the 
		      // summaryfile

		      VARunHeader* pfInRunHeader=fFileIn.getRunHeaderPtr();
		      if(pfInRunHeader==NULL)
			{
			  std::cout<<"ksSumFiles: File "<<fRootFileName<<
			    " has no RunHeader"<<std::endl;
			  exit(1);
			}
		      fNumTels   = pfInRunHeader->pfRunDetails->fTels; 
		      fFirstValidEventTime = 
			pfInRunHeader->pfRunDetails->fFirstValidEventTime;
		      std::cout<<"RunStart Time: "<< fFirstValidEventTime
			       <<std::endl;
		      pfSumFile->createFile(fOutputRootFileName,fNumTels,
					    fFirstValidEventTime);
		      fEventTime=fFirstValidEventTime;
		      // ******************************************************
		      // Now copy stuff over
		      // ******************************************************
		      pfSumFile->CopyInAndWriteRunHeader(&fFileIn);
		      pfSumFile->CopyInAndWriteArrayInfo(&fFileIn);
		      pfSumFile->CopyInAndWriteQStatsData(&fFileIn);
		      pfSumFile->CopyInAndWritePixelStausData(&fFileIn);
		      pfSumFile->CopyInAndWriteRelGainData(&fFileIn);
		      
		      // ******************************************************
		      //Copy over Simulation header
		      // ******************************************************
		      if(pfKRootSimHead==NULL)
			{
			  std::cout<<"ksSumFiles: File "
				   <<fRootFileName
				   <<"has no VASimulationHeader record"
				   <<std::endl;
			  exit(1);
			}
		      if(pfRootSimHead->fSimulationPackage!=
		      	 VASimulationHeader::E_KASCADE)
		      	{
			  std::cout<<"ksSumFiles: Wrong simulation package "
		      	    "found. fSimulationPackage: "
		      		   << pfRootSimHead->fSimulationPackage
		      		   <<std::endl;
		      	  exit(1);
		      	}
		      // ******************************************************
		      // The following works for anything inherited from 
		      // VASimulationHeader since VASimulationHeader is a 
		      // TObject and thus VARootIO can write it out.
		      // ******************************************************
		      pfSumFile->setSimulationHeaderPtr(pfKRootSimHead); 
		      pfSumFile->writeSimulationHeader();
		      
		      //  *****************************************************
		      // Create a Calibrated event tree and a simulation event
		      // tree
		      // ******************************************************
		      pfSumFile->createTheCalibratedArrayEventTree();
		      pfSumCalEvent=pfSumFile->getCalibratedArrayEventPtr();
		      
		      std::cout<<"ksSumSiles: Creating Simulation Event "
			"Tree"<<std::endl;
		      
		      pfSumSimEvent= new VAKascadeSimulationData();
		      TTree* pfSimulationEventTree= 
			new TTree(gSimulatedEventsTreeName.c_str(),
				  "Simulation Parameters");
		      if(pfSimulationEventTree==NULL)
			{
			  std::cout<<"KSEvent: Problem creating  "
			    "pfSimulationEventTree"<<std::endl;
			  exit(1);
			}
		      pfSimulationEventTree->
			Branch(gSimulatedEventsBranchName.c_str(),
			       "VAKascadeSimulationData",&pfSumSimEvent,16000,
			       0);
		      pfSumFile->setSimulationPtr(pfSumSimEvent);
		      pfSumFile->setSimulationEventTree(pfSimulationEventTree);
		      fFirstFile=false;
		    } //End first root file
		  fFirstFile=false;
		  std::cout<<"ksSumFiles:Starting Summing Pass"
			   <<std::endl;
		}  //End First File
	      
	      float fWeight=1.0;
	      if(fWeightBySpectrum)
		{
		  fWeight=pfWeights->getWeight(fType,fEnergyGeV);
		}
	      for(int index=0;index<fNumArrayEvents;index++)//VBF Events start
		// at 1, Root at 0
		{
		  // *********************************************************
		  // Are we weighting by the spectrum? do we cut this event?
		  // Cut or keep for both ouput files
		  // *********************************************************
		  if((fWeightBySpectrum &&pran(&fXDummy)<fWeight) || 
		     !fWeightBySpectrum)
		    {
		      if(fOutputVBF)
			{
			  VPacket* pfWritePacket = new VPacket();
			  pfPacket=pfReader->readPacket(index+1); 
			  
			  // *************************************************
			  // Update event numbers here and maybe times
			  // *************************************************
			  
			  // *************************************************
			  // Fix up simulation data bank in this packet
			  // *************************************************
			  VSimulationData *pfSimData =
			    pfPacket->get< VSimulationData >
			    (VGetSimulationDataBankName());
			  // I think the following is ignored but do anyway
			  // If it is ignored then we didn't have to do this 
			  // section
			  pfSimData->fRunNumber=fRunNumber;
			  pfSimData->fEventNumber=fArrayEventNum;
			  
			  pfWritePacket->put(VGetSimulationDataBankName(),
					     pfSimData);  
			  // *************************************************
			  // Fix up Kascade simulation data bank in this packet
			  // *************************************************
			  VKascadeSimulationData *pfKSimData =
			    pfPacket->get< VKascadeSimulationData >
			    (VGetKascadeSimulationDataBankName());
			  // I think the following is ignored but do anyway
			  // If it is ignored then we didn't have to do this 
			  // section
			  pfKSimData->fRunNumber=fRunNumber;
			  pfKSimData->fEventNumber=fArrayEventNum;
			  
			  pfWritePacket->
			    put(VGetKascadeSimulationDataBankName(),
				pfKSimData);  
			  // *************************************************
			  // Now the ArrayEvents
			  // First fix times and event number in array Trigger
			  // *********************************************
			  VArrayEvent* pfAEOut  = new VArrayEvent();
			  
			  pfAEIn=pfPacket->getArrayEvent();
			  pfAT = pfAEIn->getTrigger();
			  // set the event number
			  pfAT->setEventNumber(fArrayEventNum);
			  uint8_t  fGPSYear;
			  uint16_t fGPSWords[5];
			  fEventTime.getForVBF(fGPSYear,6,fGPSWords);
			  // SEtGPS Time, I know it looks like a get but its 
			  //not
			  pfAT->getGPSTime()[0]=fGPSWords[0];
			  pfAT->getGPSTime()[1]=fGPSWords[1];
			  pfAT->getGPSTime()[2]=fGPSWords[2];
			  pfAT->getGPSTime()[3]=fGPSWords[3];
			  pfAT->getGPSTime()[4]=fGPSWords[4];
			  
			  pfAT->setGPSYear(fGPSYear);
			  if(pfAT->getEventType().trigger==
			     VEventType::PED_TRIGGER)
			    {
			      fNumPedEvents++;
			    }
			  else if(pfAT->getEventType().trigger==
				  VEventType::L2_TRIGGER)
			    {
			      fNumNormalEvents++;
			    }
			  // now put array trigger back into the array event
			  pfAEOut->setTrigger(pfAT);
			  
			  // **************************************************
			  // Now fix telescope events
			  // *************************************************
			  int fNumTriggeredTels = 
			    (int) pfAEIn->getNumEvents();
			  if(fNumTriggeredTels!=1)
			    {
			      std::cout<<"fNumTriggeredTels: "
				       <<fNumTriggeredTels
				       <<std::endl;
			    }
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
			      // add the event to the array event!
			      pfAEOut->addEvent(pfEvent);
			    }
			  // put the array event back into the packet
			  // I'm told this will be a replacement
			  pfWritePacket->putArrayEvent(pfAEOut);
			  
			  // finally, write the packet into the file
			  pfWriter->writePacket(fArrayEventNum,pfWritePacket);
			  delete pfWritePacket;
			}
		      if(fOutputRoot)
			{
			  fFileIn.readSimulationData(index);
			  
			  // *************************************************
			  // Update event numbers here and maybe times
			  // **************************************************
			  fFileIn.loadInArrayEvent(index);
			  
			  *pfSumCalEvent=*pfInCalEvent;  //copy over
			  
			  *pfSumSimEvent=*pfKInSimEvent;
			  
			  pfSumCalEvent->fArrayEventNum=fArrayEventNum;
			  
			  pfSumCalEvent->fArrayTime=fEventTime;
			  int fEventNumTels=pfSumCalEvent->fTelEvents.size();
			  for(int i=0;i<fEventNumTels;i++)
			    {
			      pfSumCalEvent->fTelEvents[i].fTelTime=fEventTime;
			    }
			  pfSumFile->writeCalibratedArrayEvent(fNumTels);
			  pfSumFile->writeSimulationData();
			}  //End root event write
		      
		      fLastValidEventTime=fEventTime;
		      double fEventTimeMJD=fEventTime.getMJDDbl();
		      double fTimeGapDay=
			Rexp(fMeanTimeBetweenEventsSec)/(60.*60.*24.);
		      
		      fEventTimeMJD+=fTimeGapDay;
		      fEventTime.setFromMJDDbl(fEventTimeMJD);
		      fArrayEventNum++;
		      fCountOut++;
		  
		    }   //end weight test
		}       //end event for loop
	    }          //End of test that we have events in file
	  fFileIn.Close();
	}              //End of while loop over input files from list

      // finish up.  
      if(fOutputVBF)
	{
	  //this creates the index and writes the checksum.
	  pfWriter->finish();
	  
	  std::cout<<"ksSumFiles: Output summary file closed with "
		   <<fArrayEventNum-1<<" events"<<std::endl;
	  std::cout<<"ksSumFiles: Number of Normal Events written:"
		   <<fNumNormalEvents<<std::endl;
	  std::cout<<"ksSumFiles: Number of Pedestal Events written:"
		   <<fNumPedEvents<<std::endl;
	}
      if(fOutputRoot)
	{
	  
	  VARunHeader* pfSumRunHeader=pfSumFile->getRunHeaderPtr();
	  pfSumRunHeader->pfRunDetails->fLastValidEventTime=
	    fLastValidEventTime;
	  pfSumFile->writeRunHeader();
	  pfSumFile->writeCalibratedEventTree();
	  pfSumFile->writeSimulationEventTree();
	  pfSumFile->Close();
	}
  
  
      std::cout<<"ksSumFiles: End of Run at: "<<fEventTime<<std::endl;
      std::cout<<"ksSumFiles: Normal end"<<std::endl;
      // ----------------------------------------------------------------------
      // Save the random number generator seeds.
      // ----------------------------------------------------------------------
      ranend(&printseeds,(char*)fRandomSeedFileName.c_str(),
	     (int)fRandomSeedFileName.length());
      return 0;
    }
 
  catch(VAException &ex)
    {
      std::cerr<<ex;
      return 1;
    }
  catch(...)
    {
      std::cout<<"ksSumFiles - Fatal--Unknown exception found."
	       <<std::endl;
      return 1;
    }
}
// **************************************************************************

