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
#include <VBF/VSimulationData.h>
#include <VBF/VSimulationHeader.h>
#include <VBF/VKascadeSimulationData.h>
#include <VBF/VKascadeSimulationHeader.h>
#include <VBF/VEventType.h>

#include "VAException.h"
#include "VATime.h"
// declare that we're using the VConfigMaskUtil namespace, which gives us
// easy access to parseConfigMask().
// include the configuration mask utilities, which give us parseConfigMask()
#include <VBF/VConfigMaskUtil.h>

using namespace VConfigMaskUtil;

int main(int argc, char** argv)
{ 
  try
    {
      int fDebugLimit=100;
      VPacket*       pfPacket = NULL;
      VArrayEvent*   pfAEIn   = NULL;
      VEvent*        pfEvent  = NULL;
      VArrayTrigger* pfAT     = NULL;

      // *****************************************************************
      // Open file
      // ************************************************************
      //std::string fInputFileName("1V.vbf");
      //std::string fInputFileName("./protons/P1VGeV422.d1.vbf");
      std::string fInputFileName("./P1VGeV422.d1.vbf");
      VBankFileReader fReader(fInputFileName);
      if(fReader.hasPacket(0))
	{
	  std::cout<<"TestVBF: Found Packet 0 in file: "
		   <<fInputFileName<<std::endl;
	  pfPacket=fReader.readPacket(0);  //0=Location of header
	}
      else
	{
	  std::cout<<"TestVBF: No header packet with index 0 "
	    "found in file: "<<fInputFileName<<std::endl;
	  exit(1);
	}
      if (!pfPacket->has(VGetSimulationHeaderBankName())  )
      	{
      	  std::cout<<"TestVBF: No SimulationHeader bank "
      	    "in file: "<<fInputFileName<<std::endl;
      	  exit(1);
      	}
      VSimulationHeader *pfSimHead =
      	pfPacket->get< VSimulationHeader >(VGetSimulationHeaderBankName());
      //pfSimHead->Print();

      if (!pfPacket->has(VGetKascadeSimulationHeaderBankName())  )
	{
	  std::cout<<"TestVBF: No KascadeSimulationHeader bank "
	    "in file: "<<fInputFileName<<std::endl;
	  exit(1);
	}
      VKascadeSimulationHeader *pfKSimHead =
	              pfPacket->get< VKascadeSimulationHeader >
	                              (VGetKascadeSimulationHeaderBankName());
      //pfKSimHead->Print();

      // ******************************************************************
      // Find number of packets in the input file
      // ******************************************************************
      int fNumArrayEvents = fReader.numPackets();
      std::cout<<"TestVBF: fNumArrayEvents:"<<fNumArrayEvents-1<<std::endl;
      if(fNumArrayEvents>1)  //packet 0 (header) should always be there
	{
	  int fRunNumber = fReader.getRunNumber();
	  std::cout<<"TestVBF: fRunNumber: "<<fRunNumber<<std::endl;
	  
	  for(int index=1;index<fNumArrayEvents;index++)//Events start at 1
	    {
	      pfPacket=fReader.readPacket(index); 

	      if (!pfPacket->has(VGetSimulationDataBankName())  )
		{
		  std::cout<<"TestVBF: No SimulationData bank in file: "
			   <<fInputFileName<<" at paccketA# "<<index
			   <<std::endl;
		  exit(1);
		}
	      VSimulationData *pfSimData =
		pfPacket->get< VSimulationData >(VGetSimulationDataBankName());
	      if(index<fDebugLimit)
		{
		  //pfSimData->Print();
		}
	      if (!pfPacket->has(VGetKascadeSimulationDataBankName())  )
		{
		  std::cout<<"TestVBF: No KascadeSimulationData bank "
		    "in file: "<<fInputFileName<<std::endl;
		  exit(1);
		}
	      VKascadeSimulationData *pfKSimData =
	              pfPacket->get< VKascadeSimulationData >
		                       (VGetKascadeSimulationDataBankName());
	      if(index<fDebugLimit)
		{
		  //pfKSimData->Print();
		}
	      // ******************************************
	      // Now the ArrayEvents
	      // First fix times and event number in array Trigger
	      // *********************************************
	      pfAEIn=pfPacket->getArrayEvent();
	      int fNumTriggeredTels = (int) pfAEIn->getNumEvents();
	      pfAT = pfAEIn->getTrigger();
	      VATime fEventTime;
	      int fArrayEventNum=pfAT->getEventNumber();
	      fEventTime.setFromVBF(5,pfAT->getGPSTimeNumElements(),
					    pfAT->getGPSTime());
	      if(index<fDebugLimit)
		{
		  std::cout<<"ATEventNumber: "
			   <<pfAT->getEventNumber()<<std::endl;
		  std::cout<<"AT: fEventTime: "<<fEventTime<<std::endl;
		  std::cout<<"fNumTriggeredTels: "<<fNumTriggeredTels
			   <<std::endl;
		  if(pfAT->getEventType().trigger==
		     VEventType::L2_TRIGGER)
		    {
		      std::cout<<" AT: Found L2_trigger event at: "
			       <<fArrayEventNum<<" at: "<<fEventTime
			       <<std::endl;
		    }
		}
	      if(pfAT->getEventType().trigger==VEventType::PED_TRIGGER)
		{
		  std::cout<<" Found Pedestal ArrayTrigger event at: "
			   <<fArrayEventNum<<" at: "<<fEventTime
			   <<std::endl;
		}
	      
	      for(int i=0;i<fNumTriggeredTels;i++)
		{
		  pfEvent=pfAEIn->getEvent(i);
		  int fEventNum=pfEvent->getEventNumber();
		  fEventTime.setFromVBF(5,pfEvent->getGPSTimeNumElements(),
					    pfEvent->getGPSTime());
		  if(index<fDebugLimit)
		    {
		      std::cout<<"EventNumber: " <<fEventNum<<std::endl;
		      std::cout<<"Event: fEventTime: "<<fEventTime<<std::endl;
		      if(pfEvent->getEventType().trigger==
			 VEventType::L2_TRIGGER)
			{
			  std::cout<<" Found L2_trigger event at: "
				   <<fEventNum<<" at: "<<fEventTime
				   <<std::endl;
			}
		    }
		  if(pfEvent->getEventType().trigger==VEventType::PED_TRIGGER)
		    {
		      std::cout<<" Found Pedestal event at: "
			       <<fEventNum<<" at: "<<fEventTime
			       <<std::endl;
		    }
		}
	    }          //End of test that we have events in file
	}              //End of while loop over input files from list
    }

 catch(VAException &ex)
    {
      std::cerr<<ex;
      return 1;
    }
  catch(...)
    {
      std::cout<<"TestVBF - Fatal--Unknown exception found."
	       <<std::endl;
      return 1;
    }
}
