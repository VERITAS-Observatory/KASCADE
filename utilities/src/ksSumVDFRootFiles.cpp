//-*-mode:c++; mode:font-lock;-*-
/**
 * \class ksSumVDFRootFiles
 * \brief This code Appends all the VDF (stage2 or kasAomega output) root
 *  files into a single file cutting on spectrum is designated
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

#include "VAVDF.h"
#include "VAKascadeSimulationData.h"
#include "VAException.h"
#include "VSOptions.hpp"
#include "VACommon.h"

#include "KSEventWeights.h"

#include "TROOT.h"
#include "TTree.h"


const double kEventRateHZ=500.0;  //Allows for ~43*e6 event in a day(limit of
                                  //Qstats time etc).

extern "C" void ranstart(int* printseedflag, char* random_seed_file_name, 
			 int length);
extern "C" void ranend(int* printseedflag, char* random_seed_file_name, 
			 int length);
extern "C" float pran(float* dummy);

extern "C" double Rexp(double fScaleFactor);


void usage(const std::string& progname, const VAOptions& command_line)
{
  std::cout << std::endl;
  std::cout << "ksSumVDFRootFiles: Usage: " << progname 
	    << " [options]  <Input File List Name> <Output File Name>" 
	    << std::endl;
  std::cout <<" Input File List Name and Output File Name are required"
              "specified in command line"<<std::endl;
  std::cout<<"ksKascade: Options: "<<std::endl;
  command_line.printUsage(std::cout);
}

int main(int argc, char** argv)
{ 
  try
    {
      time_t thetime=time(NULL);
      std::cout<<"ksSumVDFRootFiles:  START------"<<ctime(&thetime)<<std::endl;
      
      TROOT root("ksSumVDFRootFiles","Sum (and cut on spectrum)VDFRoot files");
  
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
	  fRandomSeedFileName="ksSumVDFRootFiles.ran";
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
      argv++;
      std::string OutputVDFFileName=*argv;
      
      std::cout<<"ksSumVDFRootFiles:Input List File Name: "<<ListFileName
	       <<std::endl;
      std::cout<<"ksSumVDFRootFiles:Output VDF File Name: "<<OutputVDFFileName
	       <<std::endl;
      
      


      // ********************************************************************
      // Do we need weights, that is do we make spectrum cuts?
      // ********************************************************************

      KSEventWeights* pfWeights=NULL;
      float fXDummy=0;
      if(fWeightBySpectrum)
	{
	  std::cout<<"ksSumVDFRootFiles:Determining Spectrum weights"
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
	  // typedef std::map<int,int > fShwrMap_t;
	   std::map<int, fShwrMap_t > fTypes;   //Map of a map
	   std::map<int, fShwrMap_t >::iterator fTypesPos;
	   fShwrMap_t::iterator fShowerDataPos;

	  //Ready to read in showers
	  std::string fInputFileName;
	  while(getline(fListIn,fInputFileName))
	    {
	      // Open file from list
	      VAVDF fFileIn;
	      fFileIn.OpenForStage3(fInputFileName);
	      VASimulationHeader* pfSimHead=fFileIn.getSimulationHeaderPtr();
	      VAKascadeSimulationHead *pfKSimHead;
	      if(pfSimHead->fSimulationPackage==KASCADE)
		{
		  pfKSimHead = 
		    dynamic_cast< VAKascadeSimulationHead* >(pfSimHead);
		  if(pfKSimHead==NULL)
		    {
		      std::cout<<"ksSumVDFRootFiles: File "<<fInputFileName
			       <<"has no VASimulationHeader record"<<std::endl;
		      exit(1);
		    }
		}
	      else
		{
		  std::cout<<"ksSumVDFFile: Wrong simulation package found. "
		    "fSimulationPackage: "<< pfSimHead->fSimulationPackage
			   <<std::endl;
		  exit(1);
		}
	      //Now get stuff from the header
	      //Primary type first
	      int fType = pfKSimHead->fCORSIKAParticleID;
	      // Now get energy
	      int fEnergyGeV=(int)pfKSimHead->fEnergyGeV;

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


	      //fShwrMap_t fShowers = fTypes[fType];  //creates a fShwrMap_t if 
	                                     //one doesn't exist with this key 
	      //int fNumShwr=fShowers[fEnergyGeV];
		  //Incriment number of showers
	      //fShowers[fEnergyGeV]=fNumShwr+1;   //bump the shwer count
	      // fTypes[fType]=fShowers;       //restore back into the map of maps

	      fFileIn.Close();	            //Close up the root file
	    }
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
	}


      // ******************************************************************
      // Now we are ready to start. Begin by creating the output file;
      // We make the assumption that all the files in the file list have the
      // same ArrayInfo,RelGains PixelStatus,QStats. This is probably OK, but 
      // check pedvars someday. 
      // ******************************************************************
      // Use first file in file list to get these things
      VAVDF* pfSumFile=new VAVDF();
      VACalibratedArrayEvent* pfSumCalEvent=NULL;
      VAKascadeSimulationData* pfSumSimEvent=NULL;
      std::string fInputFileName;
      bool fFirstFile=true;
      int fNumTels=1;
      int fEventNum=0;
      VATime fFirstValidEventTime;
      VATime fLastValidEventTime;
      VATime fEventTime;
      double fMeanTimeBetweenEventsSec=1./kEventRateHZ;
      std::cout<<"ksSumVDFRootFiles:Merging Files.....Takes even longer"
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
      while(getline(fListIn,fInputFileName))
	{
	  // Open file from list
	  VAVDF fFileIn;
	  fFileIn.OpenForStage3(fInputFileName);
	  // ***********************************************************
	  // First input file? Use its various objects for the summary file
	  if(fFirstFile)
	    {
	      VARunHeader* pfInRunHeader=fFileIn.getRunHeaderPtr();
              if(pfInRunHeader==NULL)
		{
		  std::cout<<"File: "<<fInputFileName<<" has no RunHeader"
			   <<std::endl;
		  exit(1);
		}
	      fNumTels   = pfInRunHeader->pfRunDetails->fTels; 
	      fFirstValidEventTime = 
		               pfInRunHeader->pfRunDetails->fFirstValidEventTime;
	      std::cout<<"RunStart Time: "<< fFirstValidEventTime<<std::endl;
	      pfSumFile->createFile( OutputVDFFileName,fNumTels,
				                         fFirstValidEventTime);
	      fEventTime=fFirstValidEventTime;
	      // *************************************************************
	      // Now copy stuff over
	      // *************************************************************
	      pfSumFile->CopyInAndWriteRunHeader(&fFileIn);
	      pfSumFile->CopyInAndWriteArrayInfo(&fFileIn);
	      pfSumFile->CopyInAndWriteQStatsData(&fFileIn);
	      pfSumFile->CopyInAndWritePixelStausData(&fFileIn);
	      pfSumFile->CopyInAndWriteRelGainData(&fFileIn);

	      // *************************************************************
	      //Copy over Simulation header
	      // ************************************************************
	      VASimulationHeader* pfSimHead=fFileIn.getSimulationHeaderPtr();
	      VAKascadeSimulationHead *pfKSimHead;
	      if(pfSimHead->fSimulationPackage==KASCADE)
		{
		  pfKSimHead = 
		    dynamic_cast< VAKascadeSimulationHead* >(pfSimHead);
		  if(pfKSimHead==NULL)
		    {
		      std::cout<<"ksSumVDFRootFiles: File "<<fInputFileName
			       <<"has no VASimulationHeader record"<<std::endl;
		      exit(1);
		    }
		}
	      else
		{
		  std::cout<<"ksSumVDFFile: Wrong simulation package found. "
		    "fSimulationPackage: "<< pfSimHead->fSimulationPackage
			   <<std::endl;
		  exit(1);
		}
	      // *************************************************************
	      // The following works for anything inherited from 
	      // VASimulationHeader since VASimulationHeader is a TObject and
	      // thus VARootIO can write it out.
	      // *************************************************************
	      pfSumFile->setSimulationHeaderPtr(pfKSimHead); 
	      pfSumFile->writeSimulationHeader();

	      //  *************************************************************
	      // Create a Calibrated event tree and a simulation event tree
	      // **************************************************************
	      pfSumFile->createTheCalibratedArrayEventTree();
	      pfSumCalEvent=pfSumFile->getCalibratedArrayEventPtr();

	      std::cout<<"ksSumVDFSiles: Creating Simulation Event Tree"
		       <<std::endl;

	      pfSumSimEvent= new VAKascadeSimulationData();
	      TTree* pfSimulationEventTree= 
		new TTree(gSimulatedEventsTreeName.c_str(),
				       "Simulation Parameters");
	      if(pfSimulationEventTree==NULL)
		{
		  std::cout<<"KSEvent: Problem creating  pfSimulationEventTree"
			   <<std::endl;
		  exit(1);
		}
	      pfSimulationEventTree->Branch(gSimulatedEventsBranchName.c_str(),
			   "VAKascadeSimulationData",&pfSumSimEvent,16000,0);
	      pfSumFile->setSimulationPtr(pfSumSimEvent);
	      pfSumFile->setSimulationEventTree(pfSimulationEventTree);
	      fFirstFile=false;
	      std::cout<<"ksSumVDFFiles:Starting Summing Pass"<<std::endl;
	    }
	  // ******************************************************************
	  // Find number of Calibrated events (and thus simulated events) in
	  // the input file
	  // ******************************************************************
	  int fNumArrayEvents=fFileIn.getNumArrayEvents();
	  int fCountOut=0;
	  if(fNumArrayEvents>0)
	    {
	      VACalibratedArrayEvent* pfInCalEvent = 
	                               fFileIn.getCalibratedArrayEventPtr();
	      float fWeight=1.0;
	      if(fWeightBySpectrum)
		{
		  VASimulationHeader* pfSimHead=
		                              fFileIn.getSimulationHeaderPtr();
		  VAKascadeSimulationHead *pfKSimHead = 
		    dynamic_cast< VAKascadeSimulationHead* >(pfSimHead);
		  int fType = pfKSimHead->fCORSIKAParticleID;
		  int fEnergyGeV=(int)pfKSimHead->fEnergyGeV;
		  fWeight=pfWeights->getWeight(fType,fEnergyGeV);
		}
	      VASimulationData* pfInSimEvent = fFileIn.getSimulationDataPtr();
	      VAKascadeSimulationData* pfKInSimEvent = 
		       dynamic_cast< VAKascadeSimulationData* >(pfInSimEvent);
	      for(int index=0;index<fNumArrayEvents;index++)
		{
		  fFileIn.readSimulationData(index);

	      // ***********************************************************
	      // Are we weighting by the spectrum? do we cut this event?
	      // ***********************************************************
		  if((fWeightBySpectrum &&pran(&fXDummy)<fWeight) || 
		     !fWeightBySpectrum)
		    {
	      // *********************************************************
	      // Update event numbers here and maybe times
	      // *********************************************************
		      fFileIn.loadInArrayEvent(index);
		  
		      *pfSumCalEvent=*pfInCalEvent;  //copy over

		      *pfSumSimEvent=*pfKInSimEvent;

		      pfSumCalEvent->fArrayEventNum=fEventNum;
		      fEventNum++;

		      pfSumCalEvent->fArrayTime=fEventTime;
		      int fEventNumTels=pfSumCalEvent->fTelEvents.size();
		      for(int i=0;i<fEventNumTels;i++)
			{
			  pfSumCalEvent->fTelEvents[i].fTelTime=fEventTime;
			}

		      fLastValidEventTime=fEventTime;
		      double fEventTimeMJD=fEventTime.getMJDDbl();
		      double fTimeGapDay=
			Rexp(fMeanTimeBetweenEventsSec)/(60.*60.*24.);

		      fEventTimeMJD+=fTimeGapDay;
		      fEventTime.setFromMJDDbl(fEventTimeMJD);

		      pfSumFile->writeCalibratedArrayEvent(fNumTels);
		      pfSumFile->writeSimulationData();
 
		      fCountOut++;
		    }
		}
	    }
	  //std::cout<<"Read From Input file "<<fInputFileName<<" "
	  //	   <<fNumArrayEvents<<" events. Wrote: "
	  //	   <<fCountOut<<" Total so far:"<<fEventNum<<std::endl;
	  fFileIn.Close();
 	}
      VARunHeader* pfSumRunHeader=pfSumFile->getRunHeaderPtr();
      pfSumRunHeader->pfRunDetails->fLastValidEventTime=fLastValidEventTime;
      pfSumFile->writeRunHeader();
      pfSumFile->writeCalibratedEventTree();
      pfSumFile->writeSimulationEventTree();
      pfSumFile->Close();
      std::cout<<"ksSumVDFRootFiles:: Ouput summary file closed with "
	       <<fEventNum<<" events"<<std::endl;
      std::cout<<"ksSumVDFRootFiles:: Normal end"<<std::endl;
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
      std::cout<<"ksSumVDFRootFiles - Fatal--Unknown exception found."
	       <<std::endl;
      return 1;
    }
}
// **************************************************************************

