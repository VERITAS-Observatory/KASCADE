/**
 * \class KSSegmentFile
 * \ingroup common
 * \brief Methods forKascade Segment file.
 
 * Original Author: Glenn H. Sembroski
 * $Author$
 * $Date$
 * $Revision$
 * $Tag$
 *
 **/

#include "KSSegmentFile.h"

//I found the way to do this at:
//  http://www.angelfire.com/country/aldev0/cpphowto/cpp_BinaryFileIO.html


KSSegmentFile::KSSegmentFile()
{
  pfInFile=NULL;
  pfOutFile=NULL;
  fSegmentHeadWritten=false;
  fSegmentHeadRead=false;
}
KSSegmentFile::~KSSegmentFile()
{
  Close();
}

// ***************************************************************************


bool KSSegmentFile::Create(std::string SegFileName)
// ***************************************************************************
//  Create an output binary file.
// ***************************************************************************

{
  if(pfOutFile!=NULL || pfInFile!=NULL)
    {
      std::cout<<"KSSegmentFile-- Output file already created or Input Opened"
	       <<std::endl;
      //throw an exception here
    }
  pfOutFile=new std::ofstream(SegFileName.c_str(), 
			      std::ios::out | std::ios::binary);
  if(pfOutFile==NULL)
    {
      std::cout<<"KSSegmentFile--Failed to Open a new output segment file"
	       <<std::endl;
      //Throw an exception here.
    }
  return pfOutFile->is_open();
}
// ***************************************************************************


bool KSSegmentFile::Open(std::string SegFileName)
// ***************************************************************************
//  Open for input an existing segment binary file.
// ***************************************************************************

{
  if(pfInFile!=NULL || pfOutFile!=NULL)
    {
      std::cout<<"KSSegmentFile-- Input file already opened or Output created"
	       <<std::endl;
      //throw an exception here
    }
  pfInFile=new std::ifstream(SegFileName.c_str(), 
			     std::ios::in | std::ios::binary);
  if(pfInFile==NULL)
    {
      std::cout<<"KSSegmentFile--Failed to Open an existing input segment file"
	       <<std::endl;
      //Throw an exception here.
    }
  return pfInFile->is_open();
}
// ***************************************************************************

void KSSegmentFile::Close()
// ***************************************************************************
//  Close the opened file.
// ***************************************************************************
{
  if(pfOutFile!=NULL)
    {
      pfOutFile->close();
      pfOutFile=NULL;
      fSegmentHeadWritten=false;
    }
  if(pfInFile!=NULL)
    {
      pfInFile->close();
      pfInFile=NULL;
      fSegmentHeadRead=false;
    }
  return;
}
// ***************************************************************************


void KSSegmentFile::WriteSegmentHead(KSSegmentHeadData* segHead)
// ***************************************************************************
// The the segment head data to the output file. Sould be firt thing written to
// file. And it should only be written once.
// ***************************************************************************
{
  if(pfOutFile==NULL)
    {
      std::cout<<"KSSegmentFile--Output segment file is not yet opened"
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

void KSSegmentFile::WriteSegment(KSSegmentData* segment)
// ***************************************************************************
// The the segment head data to the output file. Sould be firt thing written to
// file. And it should only be written once.
// ***************************************************************************
{
  if(pfOutFile==NULL)
    {
      std::cout<<"KSSegmentFile--Output segment file is not yet opened"
	       <<std::endl;
      //throw exception
    }
  else if(!fSegmentHeadWritten)
    {
      std::cout<<"KSSegmentFile--Segment Header Not yet written."
	       <<std::endl;
      //throw exception
    }
  else
    {
      pfOutFile->write((char*)segment, sizeof(KSSegmentData));
    }
  return;
}
// ***************************************************************************

bool KSSegmentFile::ReadSegmentHead(KSSegmentHeadData* segHead)
// ***************************************************************************
// Read segment head data from the input file. Sould be first thing read.
// And it should only be read once.
// ***************************************************************************
{
  if(pfInFile==NULL)
    {
      std::cout<<"KSSegmentFile--Input segment file is not yet opened"
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
      
     pfInFile->read((char*)segHead, sizeof(KSSegmentHeadData));
     if(!pfInFile->good())
       {
	 std::cout<<"KSSegmentFile--Failed to read Segment Header."
		  <<std::endl;
       }
     fSegmentHeadRead=true;
     return true;
    }
}
// ***************************************************************************

bool KSSegmentFile::ReadSegment(KSSegmentData* segment)
// ***************************************************************************
// Read segment data from the input file. Segment head first needs to be read
// ***************************************************************************
{
  if(pfInFile==NULL)
    {
      std::cout<<"KSSegmentFile--Input segment file is not yet opened"
	       <<std::endl;
      return false;
    }
  else if(!fSegmentHeadRead)
    {
      std::cout<<"KSSegmentFile--Segment not yet read."
	       <<std::endl;
      return false;
    }
  else
    {
     pfInFile->read((char*)segment, sizeof(KSSegmentData));
     if(pfInFile->eof())
       {
	 return false;
       }
     if(!pfInFile->good())
       {
	 std::cout<<"KSSegmentFile--Failed to read Segment."
		  <<std::endl;
	 return false;
       }
     return true;
    }
}
// ***************************************************************************


