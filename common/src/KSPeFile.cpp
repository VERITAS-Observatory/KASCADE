/**
 * \class KSPeFile
 * \ingroup common
 * \brief Methods for Kascade Pe file.
 
 * Original Author: Glenn H. Sembroski
 * $Author$
 * $Date$
 * $Revision$
 * $Tag$
 *
 **/

#include "KSPeFile.h"
#include "KSSegmentFile.h"

//I found the way to do this at:
//  http://www.angelfire.com/country/aldev0/cpphowto/cpp_BinaryFileIO.html

KSPeFile::KSPeFile()
{
  pfInFile=NULL;
  pfOutFile=NULL;
  fSegmentHeadWritten=false;
  fSegmentHeadRead=false;
  fPeHeadWritten=false;
  fPeHeadRead=false;
  fFoundEOF=false;
  fFoundError=false;
  fNumPe=0;
}

KSPeFile::~KSPeFile()
{
  Close();
}

// ***************************************************************************


bool KSPeFile::Create(std::string PeFileName)
// ***************************************************************************
//  Create an output binary file.
// ***************************************************************************

{
  if(pfOutFile!=NULL || pfInFile!=NULL)
    {
      std::cout<<"KSPeFile-- Output file already created or Input opened"
	       <<std::endl;
      //throw an exception here
    }
  pfOutFile=new std::ofstream(PeFileName.c_str(), 
			      std::ios::out | std::ios::binary);
  if(pfOutFile->fail())
    {
      std::cout<<"KSPeFile--Failed to Open a new output Pe file"
	       <<std::endl;
      exit(1);
      //Throw an exception here.
    }
  return pfOutFile->is_open();
}
// ***************************************************************************


bool KSPeFile::Open(std::string PeFileName)
// ***************************************************************************
//  Open for input an existing pe binary file.
// ***************************************************************************

{
  if(pfInFile!=NULL || pfOutFile!=NULL)
    {
      std::cout<<"KSPeFile-- Input file already opened or Output created"
	       <<std::endl;
      //throw an exception here
    }
  pfInFile=new std::ifstream(PeFileName.c_str(), 
			     std::ios::in | std::ios::binary);
  if(pfInFile->fail())
    {
      std::cout<<"KSPeFile--Failed to Open an existing input pe file"
	       <<std::endl;
      exit(1);
      //Throw an exception here.
    }
  return pfInFile->is_open();
}
// ***************************************************************************

void KSPeFile::Close()
// ***************************************************************************
//  Close the opened file.
// ***************************************************************************
{
  if(pfOutFile!=NULL)
    {
      pfOutFile->clear();
      pfOutFile->close();
      pfOutFile=NULL;
      fSegmentHeadWritten=false;
      fPeHeadWritten=false;
   //std::cout<<"KSPeFile: Number of Pe records written: "<<fNumPe<<std::endl;
    }
  if(pfInFile!=NULL)
    {
      pfInFile->clear();
      pfInFile->close();
      pfInFile=NULL;
      fSegmentHeadRead=false;
      fPeHeadRead=false;
   //std::cout<<"KSPeFile: Number of Pe records read: "<<fNumPe<<std::endl;
    }
  return;
}
// ***************************************************************************

void KSPeFile::WriteSegmentHead(KSSegmentHeadData* segHead)
// ***************************************************************************
// The the segment head data to the output file. Should be the first thing 
// written to the file. And it should only be written once.
// ***************************************************************************
{
  if(pfOutFile==NULL)
    {
      std::cout<<"KSPeFile--Output pe file is not yet opened"
	       <<std::endl;
      //throw exception
    }
  else if(fSegmentHeadWritten)
    {
      std::cout<<"KSSegmentFile--Segment Header already written."
	       <<std::endl;
      //throw exception
    }
  else
    {
      pfOutFile->write((char*)segHead, sizeof(KSSegmentHeadData));
      fSegmentHeadWritten=true;
    }
  return;
}
// ***************************************************************************



void KSPeFile::WritePeHead(KSPeHeadData* peHead)
// ***************************************************************************
// The the pe head data to the output file. Sould be the second thing written 
// to the file after the seg head. And it should only be written once.
// ***************************************************************************
{
  if(pfOutFile==NULL)
    {
      std::cout<<"KSPeFile--Output pe file is not yet opened"
	       <<std::endl;
      //throw exception
    }
  else if(!fSegmentHeadWritten)
    {
      std::cout<<"KSPeFile--Segment Header not yet written."
	       <<std::endl;
      //throw exception
    }
  else if(fPeHeadWritten)
    {
      std::cout<<"KSPeFile--Pe Header already written."
	       <<std::endl;
      //throw exception
    }
  else
    {
      pfOutFile->write((char*)peHead, sizeof(KSPeHeadData));
      fPeHeadWritten=true;
    }
  return;
}
// ***************************************************************************

void KSPeFile::WritePe(KSPeData* pe)
// ***************************************************************************
// The the pe head data to the output file. Segment head and pe head need to 
//be written before any pe's written out.
// ***************************************************************************
{
  if(pfOutFile==NULL)
    {
      std::cout<<"KSPeFile--Output pe file is not yet opened"
	       <<std::endl;
      //throw exception
    }
  else if(!fSegmentHeadWritten)
    {
      std::cout<<"KSPeFile--Segment Header Not yet written."
	       <<std::endl;
      //throw exception
    }
  else if(!fPeHeadWritten)
    {
      std::cout<<"KSPeFile--Pe Header Not yet written."
	       <<std::endl;
      //throw exception
    }
  else
    {
      pfOutFile->write((char*)pe, sizeof(KSPeData));
      fNumPe++;
    }
  return;
}
// ***************************************************************************


bool KSPeFile::ReadSegmentHead(KSSegmentHeadData* segHead)
// ***************************************************************************
// Read segment head data from the input file. Sould be the first thing read.
// And it should only be read once.
// ***************************************************************************
{
  if(pfInFile==NULL)
    {
      std::cout<<"KSPeFile--Input segment file is not yet opened"
	       <<std::endl;
      return false;
    }
  else if(fSegmentHeadRead)
    {
      std::cout<<"KSSegmentFile--Segment Header Already read."
	       <<std::endl;
      return false;
    }
  else
    {
      
      std::cout<<"KSPeFile: Reading Segment Head"<<std::endl;
      pfInFile->read((char*)segHead, sizeof(KSSegmentHeadData));
      if(!pfInFile->good())
	{
	  std::cout<<"KSPeFile--Failed to read Segment Header."
		   <<std::endl;
	}
      fSegmentHeadRead=true;
      return true;
    }
}
// ***************************************************************************


bool KSPeFile::ReadPeHead(KSPeHeadData* peHead)
// ***************************************************************************
// Read pe head data from the input file. Sould be second thing read.
// And it should only be read once.
// ***************************************************************************
{
  if(pfInFile==NULL)
    {
      std::cout<<"KSPeFile--Input pe file is not yet opened"
	       <<std::endl;
      return false;
    }
  else if(!fSegmentHeadRead)
    {
      std::cout<<"KSPeFile--Segment Header Not yet read."
	       <<std::endl;
      return false;
    }
  else if(fPeHeadRead)
    {
      std::cout<<"KSPeFile--Pe Header Already read."
	       <<std::endl;
      return false;
    }
  else
    {
      
     pfInFile->read((char*)peHead, sizeof(KSPeHeadData));
     if(!pfInFile->good())
       {
	 std::cout<<"KSPeFile--Failed to read Pe Header."
		  <<std::endl;
       }
     fPeHeadRead=true;
     return true;
    }
}
// ***************************************************************************

bool KSPeFile::ReadPe(KSPeData* pe)
// ***************************************************************************
// Read pe data from the input file. The segment head and the pe head need to be read before and pe's are read
// ***************************************************************************
{
  if(pfInFile==NULL)
    {
      std::cout<<"KSPeFile--Input pe file is not yet opened"
	       <<std::endl;
      return false;
    }
  else if(!fSegmentHeadRead)
    {
      std::cout<<"KSPeFile--Segment Head not yet read."
	       <<std::endl;
      return false;
    }
  else if(!fPeHeadRead)
    {
      std::cout<<"KSPeFile--Pe head not yet read."
	       <<std::endl;
      return false;
    }
  else
    {
     pfInFile->read((char*)pe, sizeof(KSPeData));
     if(pfInFile->eof())
       {
	 fFoundEOF=true;
	 return false;
       }
     if(!pfInFile->good())
       {
	 std::cout<<"KSPeFile--Failed to read Pe."
		  <<std::endl;
	 fFoundError=true;
	 return false;
       }
     fNumPe++;
     return true;
    }
}
// ***************************************************************************




