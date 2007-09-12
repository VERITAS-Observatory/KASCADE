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
  // ********************************************************************
  // Replace C++ Stlib  open for lower level c open. Do this for large file
  // support (Eliminates "File size limit exceeded" at 2.148 G bytes)
  // ********************************************************************
  //pfInFile=NULL;
  //pfOutFile=NULL;
  pfInFile=-1;
  pfOutFile=-1;
  fSegmentHeadWritten=false;
  fSegmentHeadRead=false;
  fPeHeadWritten=false;
  fPeHeadRead=false;
  fFoundEOF=false;
  fFoundError=false;
  fNumPe=0;
  fInFileName=" ";
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
  // ********************************************************************
  // Replace C++ Stlib  open for lower level c open. Do this for large file
  // support (Eliminates "File size limit exceeded" at 2.148 G bytes)
  // ********************************************************************
  //if(pfOutFile!=NULL || pfInFile!=NULL)
  if(pfOutFile!=-1 || pfInFile!=-1)
    {
      std::cout<<"KSPeFile-- Output file already created or Input opened"
	       <<std::endl;
      exit(1);
      //throw an exception here
    }
  //pfOutFile=new std::ofstream(PeFileName.c_str(), 
  //			      std::ios::out | std::ios::binary);
  //if(pfOutFile->fail())
  // {
  //   std::cout<<"KSPeFile--Failed to Open a new output Pe file"
  //	       <<std::endl;
  //   exit(1);
  //   //Throw an exception here.
  // }
  // ********************************************************************
  // Create a new file, Open for write only. Replace any existing file 
  // (O_TRUNC does this) set the permissions to 644
  // ******************************************************************* 
  pfOutFile=open(PeFileName.c_str(),O_CREAT|O_WRONLY|O_TRUNC,0644);

  //Check file opened ok
  if (pfOutFile<0) 
    {
      std::cout<<"KSPeFile--Failed to Open a new output Pe file"
  	       <<std::endl;
      exit(1);
    }
 
  //return  pfOutFile->is_open();
  return  true;
}
// ***************************************************************************


bool KSPeFile::Open(std::string PeFileName)
// ***************************************************************************
//  Open for input an existing pe binary file.
// ***************************************************************************

{
  // ********************************************************************
  // Replace C++ Stlib  open for lower level c open. Do this for large file
  // support (Eliminates "File size limit exceeded" at 2.148 G bytes)
  // ********************************************************************
  //  if(pfInFile!=NULL || pfOutFile!=NULL)
  if(pfInFile!=-1 || pfOutFile!=-1)
    {
      std::cout<<"KSPeFile-- Input file already opened or Output created"
	       <<std::endl;
      exit(1);
      //throw an exception here
    }

 
  fInFileName=PeFileName;

  //  pfInFile=new std::ifstream(PeFileName.c_str(), 
  //			     std::ios::in | std::ios::binary);

  pfInFile=open(PeFileName.c_str(),O_RDONLY);

  //if(pfInFile->fail())
  if(pfInFile<0)
   {
      std::cout<<"KSPeFile--Failed to Open an existing input pe file"
	       <<std::endl;
      exit(1);
      //Throw an exception here.
    }
  //return pfInFile->is_open();
  return true;
}
// ***************************************************************************

void KSPeFile::Close()
// ***************************************************************************
//  Close the opened file.
// ***************************************************************************
{
  // ********************************************************************
  // Replace C++ Stlib  open for lower level c open. Do this for large file
  // support (Eliminates "File size limit exceeded" at 2.148 G bytes)
  // ********************************************************************
  if(pfOutFile!=-1)
    {
      //pfOutFile->clear();
      //pfOutFile->close();
      //pfOutFile=NULL;
      close(pfOutFile);
      pfOutFile=-1;
      fSegmentHeadWritten=false;
      fPeHeadWritten=false;
   //std::cout<<"KSPeFile: Number of Pe records written: "<<fNumPe<<std::endl;
    }
  if(pfInFile!=-1)
    {
      //pfInFile->clear();
      //pfInFile->close();
      //pfInFile=NULL;
      close(pfInFile);
      pfInFile=-1;
      fSegmentHeadRead=false;
      fPeHeadRead=false;
   //std::cout<<"KSPeFile: Number of Pe records read: "<<fNumPe<<std::endl;
    }
  return;
}
// ***************************************************************************


void KSPeFile::Delete()
// ***************************************************************************
//  Delete the input file useing lower level  c type system call "unlink"
// ***************************************************************************
{
  if(pfInFile!=-1)
    {
      int fResult=unlink(fInFileName.c_str());  //Low level c file delete 
                                                //command. 
      if(fResult!=0)
	{
	  std::cout<<"KSPeFile: Failed to delete "<<fInFileName<<std::endl;
	  close(pfInFile);
	}
      pfInFile=-1;
    }
  fSegmentHeadRead=false;
  fPeHeadRead=false;
  return;
}

// **************************************************************************

void KSPeFile::WriteSegmentHead(KSSegmentHeadData* segHead)
// ***************************************************************************
// The the segment head data to the output file. Should be the first thing 
// written to the file. And it should only be written once.
// ***************************************************************************
{
  // ********************************************************************
  // Replace C++ Stlib  write for lower level c write. Do this for large file
  // support (Eliminates "File size limit exceeded" at 2.148 G bytes)
  // ********************************************************************
  //if(pfOutFile==NULL)
  if(pfOutFile==-1)
    {
      std::cout<<"KSPeFile--Output pe file is not yet opened"
	       <<std::endl;
      exit(1);
      //throw exception
    }
  else if(fSegmentHeadWritten)
    {
      std::cout<<"KSSegmentFile--Segment Header already written."
	       <<std::endl;
      exit(1);
      //throw exception
    }
  else
    {
      //pfOutFile->write((char*)segHead, sizeof(KSSegmentHeadData));
      write(pfOutFile,(char*)segHead, sizeof(KSSegmentHeadData));
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
  // ********************************************************************
  // Replace C++ Stlib  write for lower level c write. Do this for large file
  // support (Eliminates "File size limit exceeded" at 2.148 G bytes)
  // ********************************************************************
  //  if(pfOutFile==NULL)
  if(pfOutFile==-1)
    {
      std::cout<<"KSPeFile--Output pe file is not yet opened"
	       <<std::endl;
      exit(1);
      //throw exception
    }
  else if(!fSegmentHeadWritten)
    {
      std::cout<<"KSPeFile--Segment Header not yet written."
	       <<std::endl;
      exit(1);
      //throw exception
    }
  else if(fPeHeadWritten)
    {
      std::cout<<"KSPeFile--Pe Header already written."
	       <<std::endl;
      exit(1);
      //throw exception
    }
  else
    {
      //pfOutFile->write((char*)peHead, sizeof(KSPeHeadData));
      write(pfOutFile,(char*)peHead, sizeof(KSPeHeadData));
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
  // ********************************************************************
  // Replace C++ Stlib  write for lower level c write. Do this for large file
  // support (Eliminates "File size limit exceeded" at 2.148 G bytes)
  // ********************************************************************
  // if(pfOutFile==NULL)
  if(pfOutFile==-1)
    {
      std::cout<<"KSPeFile--Output pe file is not yet opened"
	       <<std::endl;
      exit(1);
      //throw exception
    }
  else if(!fSegmentHeadWritten)
    {
      std::cout<<"KSPeFile--Segment Header Not yet written."
	       <<std::endl;
      exit(1);
      //throw exception
    }
  else if(!fPeHeadWritten)
    {
      std::cout<<"KSPeFile--Pe Header Not yet written."
	       <<std::endl;
      exit(1);
      //throw exception
    }
  else
    {
      //pfOutFile->write((char*)pe, sizeof(KSPeData));
      write(pfOutFile,(char*)pe, sizeof(KSPeData));
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
  // ********************************************************************
  // Replace C++ Stdlib  read for lower level c read. Do this for large file
  // support (Eliminates "File size limit exceeded" at 2.148 G bytes)
  // ********************************************************************
  //if(pfInFile==NULL)
  if(pfInFile==-1)
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
      //pfInFile->read((char*)segHead, sizeof(KSSegmentHeadData));
      int fReadFlag=read(pfInFile, (char*)segHead, sizeof(KSSegmentHeadData));
      //if(!pfInFile->good())
      if(fReadFlag==-1)
	{
	  std::cout<<"KSPeFile--Failed to read Segment Header."
		   <<std::endl;
	  exit(1);
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
  // ********************************************************************
  // Replace C++ Stdlib  read for lower level c read. Do this for large file
  // support (Eliminates "File size limit exceeded" at 2.148 G bytes)
  // ********************************************************************
  //  if(pfInFile==NULL)
  if(pfInFile==-1)
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
      
      //pfInFile->read((char*)peHead, sizeof(KSPeHeadData));
      int fReadFlag=read(pfInFile,(char*)peHead, sizeof(KSPeHeadData));
      //if(!pfInFile->good())
      if(fReadFlag==-1)
	{
	  std::cout<<"KSPeFile--Failed to read Pe Header."
		   <<std::endl;
	  exit(1);
	}
      fPeHeadRead=true;
      return true;
    }
}
// ***************************************************************************

bool KSPeFile::ReadPe(KSPeData* pe)
// ***************************************************************************
// Read pe data from the input file. The segment head and the pe head need 
// to be read before and pe's are read
// ***************************************************************************
{
  // ********************************************************************
  // Replace C++ Stdlib  read for lower level c read. Do this for large file
  // support (Eliminates "File size limit exceeded" at 2.148 G bytes)
  // ********************************************************************
  //if(pfInFile==NULL)
  if(pfInFile==-1)
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
      //pfInFile->read((char*)pe, sizeof(KSPeData));
      int fReadFlag=read(pfInFile,(char*)pe, sizeof(KSPeData));
      //if(pfInFile->eof())
      if(fReadFlag==0)
	{
	  fFoundEOF=true;
	  return false;
	}
      //if(!pfInFile->good())
      if(fReadFlag==-1)
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




