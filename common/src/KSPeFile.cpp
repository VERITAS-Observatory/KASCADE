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
#include <stdio.h>
#include "KSPeFile.h"
#include "KSSegmentFile.h"

// ************************************************************************
//I found the way to do this at:
//  http://www.angelfire.com/country/aldev0/cpphowto/cpp_BinaryFileIO.html
// ************************************************************************
// The use of the low level c commands was to avoid 4.2 GByte file size limit
// This should be rectified with the later gcc LFS(largeFileSystem) stuff. 
// Also there are options that should gaurettee it woks
// ************************************************************************
// Test that we can use ofstream for reading and writing (and not have LFS 
// problems). ofstream may run faster due to its internal buffering.
// ************************************************************************




KSPeFile::KSPeFile()
{
  // ********************************************************************
  // Replace C++ Stlib  open for lower level c open. Do this for large file
  // support (Eliminates "File size limit exceeded" at 2.148 G bytes)
  // ********************************************************************
#ifdef EnableFileStream
  pfInFile=NULL;
  pfOutFile=NULL;
#else
  pfInFile=-1;
  pfOutFile=-1;
#endif
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
  // but new C++ may have LFS
#ifdef EnableFileStream
  if(pfOutFile!=NULL || pfInFile!=NULL){
#else
  if(pfOutFile!=-1 || pfInFile!=-1){
#endif
    std::cout<<"KSPeFile-- Output file already created or Input opened"
	     <<std::endl;
    exit(1);
    //throw an exception here
  }
#ifdef EnableFileStream
  pfOutFile=new std::ofstream(PeFileName.c_str(), 
  			      std::ios::out | std::ios::binary);
  if(pfOutFile->fail()){
    std::cout<<"KSPeFile--Failed to Open a new output Pe file"
	     <<std::endl;
    exit(1);
    //Throw an exception here.
  }
  return  pfOutFile->is_open();


#else
  // ********************************************************************
  // Create a new file, Open for write only. Replace any existing file 
  // (O_TRUNC does this) set the permissions to 644
  // ******************************************************************* 
  pfOutFile=open(PeFileName.c_str(),O_CREAT|O_WRONLY|O_TRUNC,0644);
  
  //Check file opened ok
  if (pfOutFile<0) {
    std::cout<<"KSPeFile--Failed to Open a new output Pe file"
	     <<std::endl;
    exit(1);
  }
 
  return  true;
#endif
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
#ifdef EnableFileStream
  if(pfInFile!=NULL || pfOutFile!=NULL){
#else
  if(pfInFile!=-1 || pfOutFile!=-1){
#endif
    std::cout<<"KSPeFile-- Input file already opened or Output created"
	     <<std::endl;
    exit(1);
    //throw an exception here
  }

 
  fInFileName=PeFileName;

#ifdef EnableFileStream
  pfInFile=new std::ifstream(PeFileName.c_str(), 
  			     std::ios::in | std::ios::binary);
  if(pfInFile->fail()){
#else
  pfInFile=open(PeFileName.c_str(),O_RDONLY);
  if(pfInFile<0){
#endif
    std::cout<<"KSPeFile--Failed to Open an existing input pe file"
	     <<std::endl;
    exit(1);
    //Throw an exception here.
  }
#ifdef EnableFileStream
  return pfInFile->is_open();
#else
  return true;
#endif
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
#ifdef EnableFileStream
  if(pfOutFile!=NULL){
    pfOutFile->clear();
    pfOutFile->close();
    pfOutFile=NULL;
    fSegmentHeadWritten=false;
    fPeHeadWritten=false;
  }
  if(pfInFile!=NULL){
    pfInFile->clear();
    pfInFile->close();
    pfInFile=NULL;
    fSegmentHeadRead=false;
    fPeHeadRead=false;
  }
#else
  if(pfOutFile!=-1){
    close(pfOutFile);
    pfOutFile=-1;
    fSegmentHeadWritten=false;
    fPeHeadWritten=false;
    //std::cout<<"KSPeFile: Number of Pe records written: "<<fNumPe<<std::endl;
  }
  if(pfInFile!=-1){
    close(pfInFile);
    pfInFile=-1;
    //std::cout<<"KSPeFile: Number of Pe records read: "<<fNumPe<<std::endl;
  }
#endif

  return;
}
// ***************************************************************************


void KSPeFile::Delete()
// ***************************************************************************
//  Delete the input file useing lower level  c type system call "unlink"
// ***************************************************************************
{
#ifdef EnableFileStream
  if(pfInFile!=NULL){
#else
  if(pfInFile!=-1){
#endif
    Close();                                  //Tidy up.
    int fResult=remove(fInFileName.c_str());  //Low level c file delete 
                                                //command. 
    if(fResult!=0){
      std::cout<<"KSPeFile: Failed to delete "<<fInFileName<<std::endl;
    }

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
#ifdef EnableFileStream
  if(pfOutFile==NULL){
#else
  if(pfOutFile==-1){
#endif
    std::cout<<"KSPeFile--Output pe file is not yet opened"
	     <<std::endl;
    exit(1);
    //throw exception
  }
  else if(fSegmentHeadWritten){
    std::cout<<"KSSegmentFile--Segment Header already written."
	     <<std::endl;
    exit(1);
    //throw exception
  }
  else{
#ifdef EnableFileStream
    pfOutFile->write((char*)segHead, sizeof(KSSegmentHeadData));
#else
    write(pfOutFile,(char*)segHead, sizeof(KSSegmentHeadData));
#endif
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
#ifdef EnableFileStream
  if(pfOutFile==NULL){
#else
  if(pfOutFile==-1){
#endif
    std::cout<<"KSPeFile--Output pe file is not yet opened"
	     <<std::endl;
    exit(1);
    //throw exception
  }
  else if(!fSegmentHeadWritten){
    std::cout<<"KSPeFile--Segment Header not yet written."
	     <<std::endl;
    exit(1);
    //throw exception
  }
  else if(fPeHeadWritten){
    std::cout<<"KSPeFile--Pe Header already written."
	     <<std::endl;
    exit(1);
    //throw exception
  }
  else{
#ifdef EnableFileStream
   pfOutFile->write((char*)peHead, sizeof(KSPeHeadData));
#else
   write(pfOutFile,(char*)peHead, sizeof(KSPeHeadData));
#endif
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
#ifdef EnableFileStream
  if(pfOutFile==NULL){
#else
  if(pfOutFile==-1){
#endif
    std::cout<<"KSPeFile--Output pe file is not yet opened"
	     <<std::endl;
    exit(1);
    //throw exception
  }
  else if(!fSegmentHeadWritten){
    std::cout<<"KSPeFile--Segment Header Not yet written."
	     <<std::endl;
    exit(1);
    //throw exception
  }
  else if(!fPeHeadWritten){
    std::cout<<"KSPeFile--Pe Header Not yet written."
	     <<std::endl;
    exit(1);
    //throw exception
  }
  else{
#ifdef EnableFileStream
    pfOutFile->write((char*)pe, sizeof(KSPeData));
#else
    write(pfOutFile,(char*)pe, sizeof(KSPeData));
#endif
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
#ifdef EnableFileStream
  if(pfInFile==NULL){
#else
  if(pfInFile==-1){
#endif
    std::cout<<"KSPeFile--Input segment file is not yet opened"
	     <<std::endl;
    return false;
  }
  else if(fSegmentHeadRead){
    std::cout<<"KSSegmentFile--Segment Header Already read."
	     <<std::endl;
    return false;
  }
  else{
    
    std::cout<<"KSPeFile: Reading Segment Head"<<std::endl;
#ifdef EnableFileStream
    pfInFile->read((char*)segHead, sizeof(KSSegmentHeadData));
    if(!pfInFile->good()){
#else
    int fReadFlag=read(pfInFile, (char*)segHead, sizeof(KSSegmentHeadData));
    if(fReadFlag==-1){
#endif
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
#ifdef EnableFileStream
  if(pfInFile==NULL){
#else
  if(pfInFile==-1){
#endif
    std::cout<<"KSPeFile--Input pe file is not yet opened"
	     <<std::endl;
    return false;
  }
  else if(!fSegmentHeadRead){
    std::cout<<"KSPeFile--Segment Header Not yet read."
	     <<std::endl;
    return false;
  }
  else if(fPeHeadRead){
    std::cout<<"KSPeFile--Pe Header Already read."
	     <<std::endl;
    return false;
  }
  else{
#ifdef EnableFileStream
    pfInFile->read((char*)peHead, sizeof(KSPeHeadData));
    if(!pfInFile->good()){
#else
    int fReadFlag=read(pfInFile,(char*)peHead, sizeof(KSPeHeadData));
    if(fReadFlag==-1){
#endif
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
#ifdef EnableFileStream
  if(pfInFile==NULL){
#else
  if(pfInFile==-1){
#endif
    std::cout<<"KSPeFile--Input pe file is not yet opened"
	     <<std::endl;
    return false;
  }
  else if(!fSegmentHeadRead){
    std::cout<<"KSPeFile--Segment Head not yet read."
	     <<std::endl;
    return false;
  }
  else if(!fPeHeadRead){
    std::cout<<"KSPeFile--Pe head not yet read."
	     <<std::endl;
    return false;
  }
  else{
#ifdef EnableFileStream
    pfInFile->read((char*)pe, sizeof(KSPeData));
    if(pfInFile->eof()){
#else
    int fReadFlag=read(pfInFile,(char*)pe, sizeof(KSPeData));
    if(fReadFlag==0){
#endif
      fFoundEOF=true;
      return false;
    }


#ifdef EnableFileStream
    if(!pfInFile->good()){
#else
    if(fReadFlag==-1){
#endif
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




