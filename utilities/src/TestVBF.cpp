// **************************************************
// Test program to create a vbf file with a single bank with only a 
// KascadeSimulationHead packet in it and then to try and read it.

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


#include "VAException.h"

// declare that we're using the VConfigMaskUtil namespace, which gives us
// easy access to parseConfigMask().
// include the configuration mask utilities, which give us parseConfigMask()
#include <VBF/VConfigMaskUtil.h>

using namespace VConfigMaskUtil;

int main(int argc, char** argv)
{ 
  try
    {
      std::string fVBFFileName("VBFTest.vbf");
      {
	int fRunNumber=90000;
	std::string fConfigMask("0");
	VBankFileWriter* pfWriter = 
	  new VBankFileWriter(fVBFFileName.c_str(),fRunNumber,
				 parseConfigMask(fConfigMask.c_str()));
	if(pfWriter==NULL)
	  {
	    std::cout<<"TestVBF:--Output VBF file failed to open"
		     <<std::endl;
	    return 1;
	    //throw exception
	  }
	else
	  {
	    std::cout<<"TestVBF: Ouput VBF file opened ok"<<std::endl;
	  }

	VPacket *packet=new VPacket();

	// *******************************************************************
	// Set up Simulation Header variables
	// *******************************************************************
	//Get a time
	uint32_t y=2006;
	uint32_t m=7;
	uint32_t d=26;
	uword32 fDateOfSimsUTC= y*10000ULL+ m*100ULL+ d;
	uword32 fSimulationPackage   = KASCADE;  //Purdue Kascade
	uword32 fSimulator           = SEMBROSKI;  //Purdue Sembroski
	uword32 fDateOfArrayForSims  = 0;
	uword32 fAtmosphericModel    = 0;  //no codes as of yet
	
	std::vector<VArrayConfiguration>  fArray;
	VArrayConfiguration fArConfig;
	fArConfig.fRelTelLocSouthM=0;
	fArConfig.fRelTelLocEastM=0;
	fArConfig.fRelTelLocUpM=0;
	for(int i=0;i<499;i++)
	  {
	    VPixelLocation fPixLoc;
	    fPixLoc.fPixLocEastAtStowDeg = i*.1;
	    fPixLoc.fPixLocUpAtStowDeg   = i*.2;
	    fPixLoc.fPixRadiusDeg        = .15;
	    fArConfig.fCamera.push_back(fPixLoc);
	  }
	fArray.push_back(fArConfig);
	
	float fObsAlt=1275;
	std::string fSimConfigFile = "Not Specified";
	uword32 fCORSIKAType=14;
	float   fEnergyGeV=20;
	uword32 fShowerID=1;
	// ******************************************************************
      
  
	// Fill a VKascadeSimulationHeader and write it out to event 0
	VSimulationHeader* pfSimHead = 
	  new VSimulationHeader(fDateOfSimsUTC, fSimulationPackage,
				       fSimulator, fDateOfArrayForSims,
				       fAtmosphericModel, fObsAlt, fArray,
				fSimConfigFile);
	packet->put(VGetSimulationHeaderBankName(), pfSimHead);
	if (!packet->has(VGetSimulationHeaderBankName())  )
	  {
	    std::cout<<"TestVBF: No SimulationHeader bank in packet "
	      "when we just put one in"<<std::endl;
	    return 1;
	  }
	else
	  {
	    std::cout<<"TestVBF: Packet does indeed have a "
	      "SimulationHeader bank in it"<<std::endl;
	  }

	VKascadeSimulationHeader* pfKSimHead = 
	  new VKascadeSimulationHeader( fCORSIKAType,
				       fEnergyGeV, fShowerID);
	packet->put(VGetKascadeSimulationHeaderBankName(), pfKSimHead);
	if (!packet->has(VGetKascadeSimulationHeaderBankName())  )
	  {
	    std::cout<<"TestVBF: No KascadeSimulationHeader bank in packet "
	      "when we just put one in"<<std::endl;
	    return 1;
	  }
	else
	  {
	    std::cout<<"TestVBF: Packet does indeed have a "
	      "KascadeSimulationHeader bank in it"<<std::endl;
	  }

	// finally, write the packet into the file
	uword32 fArrayEventNum=0;
	pfWriter->writePacket(fArrayEventNum, packet);
	
	// dispose of the packet, so that we don't leak memory
	delete packet;
	
	pfWriter->finish();
	std::cout<<"TestVBF: Ouput file Finished"<<std::endl;

      }

      // ******************************************************************
      // End of test file creation
      // ******************************************************************

      // ******************************************************************
      // Now reopen the file and try reading the first packet back in.
      // ******************************************************************
      
      {
	VBankFileReader reader(fVBFFileName.c_str());

	std::cout<<"TestVBF: Input file opened"<<std::endl;
	
	VPacket* packet;
	if(reader.hasPacket(0))
	  {
	    std::cout<<"TestVBF: hasPacket(0) returns: true"<<std::endl;
	    packet=reader.readPacket(0);  //0=Location of header
	    std::cout<<"TestVBF: Packet(0) read OK!"<<std::endl;
	  }
	else
	  {
	    std::cout<<"TestVBF: No header packet with index 0 "
	      "found in file: "<<fVBFFileName<<std::endl;
	    exit(1);
	  }
	VSimulationHeader *pfSimHead =
	  packet->get< VSimulationHeader >
	                               (VGetSimulationHeaderBankName());
	std::cout<<"fSimConfigFile:"<<pfSimHead->fSimConfigFile<<std::endl;

      }
    }

 catch(VAException &ex)
    {
      std::cerr<<ex;
      return 1;
    }
  catch(...)
    {
      std::cout<<"ksSumVBFFiles - Fatal--Unknown exception found."
	       <<std::endl;
      return 1;
    }
}
