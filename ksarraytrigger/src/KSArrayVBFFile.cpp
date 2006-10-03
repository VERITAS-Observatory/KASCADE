//-*-mode:c++; mode:font-lock;-*-
/**
 * \class KSArrayVBFFile.cpp
 * \ingroup common 
 * \brief File of methods for KSArrayVBFFile.
 * This file has various methods to build the some of the various records 
 * needed for a VEGAS stage1 multi-telescope input VBF file for veritas
 * data.
 *
 * Original Author: Glenn H. Sembroski
 * $Author$
 * $Date$
 * $Revision$
 * $Tag$
 *
 **/
// This is where the code starts.
// ***********************************************************************
#include "KSArrayVBFFile.h"

KSArrayVBFFile::KSArrayVBFFile(std::vector< KSTelescope* >& pArray)
{
  pfArray=pArray;
  fNumTels=pfArray.size();
}
// ***********************************************************************

KSArrayVBFFile::~KSArrayVBFFile()
{
  //Nothing to do
}
// ***********************************************************************

void KSArrayVBFFile::CreateVBFFile(std::string fOutputVBFFileName)
// ***********************************************************************
// Create the Array VBF file
// ***********************************************************************
{
  uint fRunNumber=pfArray[0]->getRunNumber();
  std::string fConfigMask;
  for(int i=0;i<fNumTels;i++)  //brute force
    {
      if(pfArray[i]->fTelID==E_T1)fConfigMask+="0";
      else if(pfArray[i]->fTelID==E_T2)fConfigMask+="1";
      else if(pfArray[i]->fTelID==E_T3)fConfigMask+="2";
      else if(pfArray[i]->fTelID==E_T4)fConfigMask+="3";
      fConfigMask+=",";
    }
  std::cout<<"fConfigMask: "<<fConfigMask<<std::endl;

  
  pfWriter = new VBankFileWriter(fOutputVBFFileName,
				 fRunNumber,
				 parseConfigMask(fConfigMask.c_str()));
  
  std::vector< bool > fCMask= pfWriter->getConfigMask();
  //for(int i=0;i<(int)fCMask.size();i++)
  // {
  //   std::cout<<"i,cmask: "<<i<<": "<<fCMask[i]<<std::endl;
  //  }


 if(pfWriter==NULL)
    {
      std::cout<<"ksArrayTrigger--Output VBF file failed to "
	"open"<<std::endl;
      exit(1);
    }	      
  return;
}
void KSArrayVBFFile::CopyOutHeaderPacket()
// **********************************************************************
// Copy over header packet (0) to ouput file
{
  pfWriter->writePacket(0, pfArray[0]->getHeaderPacketPtr());
  return;
}
// ***********************************************************************


