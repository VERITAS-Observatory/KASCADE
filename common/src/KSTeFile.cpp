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
#include "KSTeFile.h"
#include "KSTeDataClasses.h"


//  I found the way to do this at:
//  http://www.angelfire.com/country/aldev0/cpphowto/cpp_BinaryFileIO.html

KSTeFile::KSTeFile()
{
  pfInFile=NULL;
  pfOutFile=NULL;
  fSegmentHeadWritten=false;
  fSegmentHeadRead=false;
  fPeHeadWritten=false;
  fPeHeadRead=false;
  fTeHeadWritten=false;
  fTeHeadRead=false;

  fTeRead=false;
  fTeWritten=false;
  
  fFoundEOF=false;
  fFoundError=false;
  fNumTe=0;
}

KSTeFile::~KSTeFile()
{
  Close();
}

// ***************************************************************************


bool KSTeFile::Create(std::string TeFileName)
// ***************************************************************************
//  Create an output binary file.
// ***************************************************************************

{
  if(pfOutFile!=NULL || pfInFile!=NULL)
    { 
      std::cout<<"KSTeFile-- Output file already created or Input opened"
	       <<std::endl;
      fFoundError=true;
      //throw an exception here
    }
  pfOutFile=new std::ofstream(TeFileName.c_str(), 
			      std::ios::out | std::ios::binary);
  if(pfOutFile->fail())
    {
      std::cout<<"KSTeFile--Failed to Open a new output Te file"
	       <<std::endl;
      fFoundError=true;
      exit(1);
      //Throw an exception here.
    }
  return pfOutFile->is_open();
}
// ***************************************************************************


bool KSTeFile::Open(std::string TeFileName)
// ***************************************************************************
//  Open for input an existing pe binary file.
// ***************************************************************************

{
  if(pfInFile!=NULL || pfOutFile!=NULL)
    {
      std::cout<<"KSTeFile-- Input file already opened or Output created"
	       <<std::endl;
      fFoundError=true;
      //throw an exception here
    }
  pfInFile=new std::ifstream(TeFileName.c_str(), 
			     std::ios::in | std::ios::binary);
  if(pfInFile->fail())
    {
      std::cout<<"KSTeFile--Failed to Open an existing input pe file"
	       <<std::endl;
      fFoundError=true;
      exit(1);
      //Throw an exception here.
    }
  return pfInFile->is_open();
}
// ***************************************************************************

void KSTeFile::Close()
// ***************************************************************************
//  Close the opened file.
// ***************************************************************************
{
  if(pfOutFile!=NULL)
    {
      pfOutFile->close();
      pfOutFile=NULL;
      fSegmentHeadWritten=false;
      fPeHeadWritten=false;
      fTeHeadWritten=false;

      fTeWritten=false;
  
    }
  if(pfInFile!=NULL)
    {
      pfInFile->close();
      pfInFile=NULL;
      fSegmentHeadRead=false;
      fPeHeadRead=false;
      fTeHeadRead=false;
      fTeRead=false;

    }
  return;
}
// ***************************************************************************

void KSTeFile::WriteSegmentHead(KSSegmentHeadData* segHead)
// ***************************************************************************
// The the segment head data to the output file. Should be the first thing 
// written to the file. And it should only be written once.
// ***************************************************************************
{
  if(pfOutFile==NULL)
    {
      std::cout<<"KSTeFile--Output pe file is not yet opened"
	       <<std::endl;
      fFoundError=true;
      //throw exception
    }
  else if(fSegmentHeadWritten)
    {
      std::cout<<"KSSegmentFile--Segment Header already written."
	       <<std::endl;
      fFoundError=true;
      //throw exception
    }
  else
    {
      pfOutFile->write((char*)segHead, sizeof(KSSegmentHeadData));
      fSegmentHeadWritten=true;
      fFoundError=false;
    }
  return;
}
// ***************************************************************************



void KSTeFile::WritePeHead(KSPeHeadData* peHead)
// ***************************************************************************
// The the pe head data to the output file. Sould be the second thing written 
// to the file after the seg head. And it should only be written once.
// ***************************************************************************
{
  if(pfOutFile==NULL)
    {
      std::cout<<"KSTeFile--Output te file is not yet opened"
	       <<std::endl;
      fFoundError=true;
      //throw exception
    }
  else if(!fSegmentHeadWritten)
    {
      std::cout<<"KSTeFile--Segment Header not yet written."
	       <<std::endl;
      fFoundError=true;
      //throw exception
    }
  else if(fPeHeadWritten)
    {
      std::cout<<"KSTeFile--Pe Header already written."
	       <<std::endl;
      fFoundError=true;
      //throw exception
    }
  else
    {
      pfOutFile->write((char*)peHead, sizeof(KSPeHeadData));
      fPeHeadWritten=true;
      fFoundError=false;
    }
  return;
}
// ***************************************************************************

void KSTeFile::WriteTeHead(KSTeHeadData* teHead)
// ***************************************************************************
// The the te head data to the output file. Sould be the third thing written 
// to the file after the seg and pe head records. And it should only be 
// written once.
// ***************************************************************************
{
  if(pfOutFile==NULL)
    {
      std::cout<<"KSTeFile--Output te file is not yet opened"
	       <<std::endl;
      fFoundError=true;
      //throw exception
    }
  else if(!fPeHeadWritten)
    {
      std::cout<<"KSTeFile--Pe Header not yet written."
	       <<std::endl;
      fFoundError=true;
      //throw exception
    }
  else if(fTeHeadWritten)
    {
      std::cout<<"KSTeFile--Te Header already written."
	       <<std::endl;
      fFoundError=true;
      //throw exception
    }
  else
    {
      pfOutFile->write((char*)teHead, sizeof(KSTeHeadData));
      fTeHeadWritten=true;
      fFoundError=false;
    }
  return;
}
// ***************************************************************************

void KSTeFile::WriteTe(KSTeData* te)
// ***************************************************************************
// Write the te evnet tag data to the output file. Segment head, pe head , 
// and te head need to be written before any pe's written out.
// ***************************************************************************
{
  if(pfOutFile==NULL)
    {
      std::cout<<"KSTeFile--Output te file is not yet opened"
	       <<std::endl;
      fFoundError=true;
      //throw exception
    }
  else if(!fTeHeadWritten)
    {
      std::cout<<"KSTeFile--Te Header Not yet written."
	       <<std::endl;
      fFoundError=true;
      //throw exception
    }
  else if(fTeWritten)
    {
      std::cout<<"KSTeFile--Trying to write 2 Te tag records in a row"
	       <<std::endl;
      std::cout<<"KSTeFile--Must write a TePixelData record for every Te tag"
	       <<std::endl;
      fFoundError=true;
      //throw exception
    }
  else
    {
      pfOutFile->write((char*)te, sizeof(KSTeData));
      fTeWritten=true;
      fFoundError=false;
      fNumTe++;
    }
  return;
}
// ***************************************************************************
void KSTeFile::WriteTePixelData(std::vector<KSPixel>& fPixel)
// ***************************************************************************
// Write the te pixel data in compressed format with zero supression to the 
// output file. Must follow its Te tage record.
// ***************************************************************************
// Format of this record: First word is an int with the number of characters 
//                      that follow (doesn't include itself);
//                      Next follows data for first non zero pixel:
//                      First word is negative pixel index (pixel index starts
//                      at 1) Minus sign is flag that this is start of a 
//                      single pixels data. This is a float.
//                      This is followed by the pe arrival times (as floats) 
//                      of each pes that hits that pixel.
// ****************************************************************************
{
  int fLength=0;
  if(pfOutFile==NULL)
    {
      std::cout<<"KSTeFile--Output te file is not yet opened"
	       <<std::endl;
      fFoundError=true;
      //throw exception
    }
  else if(!fTeWritten)
    {
      std::cout<<"KSTeFile--Te Tag record must be written before each Pixels "
	         "record"<<std::endl;
      fFoundError=true;
      //throw exception
    }
  else
    {
      int fNumPixels=fPixel.size();
      std::vector<float> fCompressionBuffer;
      for(int i=0;i<fNumPixels;i++)
	{
	  int fNumTimes=fPixel[i].fTimePe.size();
	  if(fNumTimes>0)
	    {
	      float fIndex=-(float)(i+1); //- sign flags start of a pixels 
	                                  //time data. All times are positive
	                                  //Index starts at PixelIndex +1 
	                                  //(so the - sign can work as a flag)
	      fCompressionBuffer.push_back(fIndex);
	      for(int j=0;j<fNumTimes;j++)
		{
		  //Could put in a check here that time is indeed positive
		  float fPeTime=fPixel[i].fTimePe[j];
		  fCompressionBuffer.push_back(fPeTime);
		}
	    }
	}
      //Copy over to a float buffer.
      int fNumInBuffer=fCompressionBuffer.size();
      float* pfWriteBuffer= new float[fNumInBuffer];

      for(int i=0;i<fNumInBuffer;i++)  
	{
	  pfWriteBuffer[i]=fCompressionBuffer[i];
	}
      fLength=fNumInBuffer*sizeof(float);
      //std::cout<<"fLength: "<<fLength<<std::endl;
      pfOutFile->write((char*)&fLength, sizeof(int));
      pfOutFile->write((char*)pfWriteBuffer, fLength);
      fTeWritten=false;
      fFoundError=false;
    }
  return;
}
// ***************************************************************************


bool KSTeFile::ReadSegmentHead(KSSegmentHeadData* segHead)
// ***************************************************************************
// Read segment head data from the input file. Sould be the first thing read.
// And it should only be read once.
// ***************************************************************************
{
  if(pfInFile==NULL)
    {
      std::cout<<"KSTeFile--Input Te file is not yet opened"
	       <<std::endl;
      fFoundError=true;
      return false;
    }
  else if(fSegmentHeadRead)
    {
      std::cout<<"KSSegmentFile--Segment Header Already read."
	       <<std::endl;
      fFoundError=true;
      return false;
    }
  else
    {
      
      std::cout<<"KSTeFile: Reading Segment Head"<<std::endl;
      pfInFile->read((char*)segHead, sizeof(KSSegmentHeadData));
      if(!pfInFile->good())
	{
	  std::cout<<"KSTeFile--Failed to read Segment Header."
		   <<std::endl;
	  fFoundError=true;
	  return false;
	}
      fSegmentHeadRead=true;
      fFoundError=false;
      return true;
    }
}
// ***************************************************************************


bool KSTeFile::ReadPeHead(KSPeHeadData* peHead)
// ***************************************************************************
// Read pe head data from the input file. Sould be second thing read.
// And it should only be read once.
// ***************************************************************************
{
  if(pfInFile==NULL)
    {
      std::cout<<"KSTeFile--Input te file is not yet opened"
	       <<std::endl;
      fFoundError=true;
      return false;
    }
  else if(!fSegmentHeadRead)
    {
      std::cout<<"KSTeFile--Segment Header Not yet read."
	       <<std::endl;
      fFoundError=true;
      return false;
    }
  else if(fPeHeadRead)
    {
      std::cout<<"KSTeFile--Pe Header Already read."
	       <<std::endl;
      fFoundError=true;
      return false;
    }
  else
    {
      
     pfInFile->read((char*)peHead, sizeof(KSPeHeadData));
     if(!pfInFile->good())
       {
	 std::cout<<"KSTeFile--Failed to read Pe Header."
		  <<std::endl;
	 fFoundError=true;
	 return false;
       }
     fPeHeadRead=true;
     fFoundError=false;
     return true;
    }
}
// ***************************************************************************



bool KSTeFile::ReadTeHead(KSTeHeadData* teHead)
// ***************************************************************************
// Read te head data from the input file. Sould be third thing read.
// And it should only be read once.
// ***************************************************************************
{
  if(pfInFile==NULL)
    {
      std::cout<<"KSTeFile--Input te file is not yet opened"
	       <<std::endl;
      fFoundError=true;
      return false;
    }
  else if(!fPeHeadRead)
    {
      std::cout<<"KSTeFile--Pe Header Not yet read."
	       <<std::endl;
      fFoundError=true;
      return false;
    }
  else if(fTeHeadRead)
    {
      std::cout<<"KSTeFile--Te Header Already read."
	       <<std::endl;
      fFoundError=true;
      return false;
    }
  else
    {
      
     pfInFile->read((char*)teHead, sizeof(KSTeHeadData));
     if(!pfInFile->good())
       {
	 std::cout<<"KSTeFile--Failed to read Te Header."
		  <<std::endl;
	 fFoundError=true;
	 return false;
       }
     fTeHeadRead=true;
     fFoundError=false;
     return true;
    }
}
// ***************************************************************************

bool KSTeFile::ReadTe(KSTeData* te)
// ***************************************************************************
// Read pe tag data from the input file. The segment head, pe head and Te head
// need to be read before and pe's are read
// We must alternate reading TE tag records and te pixel data records
// ***************************************************************************
{
  if(pfInFile==NULL)
    {
      std::cout<<"KSTeFile--Input te file is not yet opened"
	       <<std::endl;
      fFoundError=true;
      return false;
    }
  else if(!fTeHeadRead)
    {
      std::cout<<"KSTeFile--Te Head not yet read."
	       <<std::endl;
      fFoundError=true;
      return false;
    }
  else if(fTeRead)
    {
      std::cout<<"KSTeFile--Need to do a TePixelData read after each Te read"
	       <<std::endl;
      fFoundError=true;
      return false;
    }
  else
    {
     pfInFile->read((char*)te, sizeof(KSTeData));
     if(pfInFile->eof())
       {
	 fFoundEOF=true;
	 fFoundError=false;
	 return false;
       }
     if(!pfInFile->good())
       {
	 std::cout<<"KSTeFile--Failed to read Pe."
		  <<std::endl;
	 fFoundError=true;
	 return false;
       }
     fTeRead=true;
     fFoundError=false;
     fNumTe++;
     return true;
    }
}
// ***************************************************************************

  bool KSTeFile::ReadTePixelData(std::vector<KSPixel>& fPixel)
// ***************************************************************************
// Read pe tag data from the input file. The segment head, pe head and Te head
// need to be read before and pe's are read
// We must alternate reading TE tag records and te pixel data records
// ***************************************************************************
{
  if(pfInFile==NULL)
    {
      std::cout<<"KSTeFile--Input te file is not yet opened"
	       <<std::endl;
      fFoundError=true;
      return false;
    }
  else if(!fTeRead)
    {
      std::cout<<"KSTeFile--Need to do a Te read before each TePixelData read"
	       <<std::endl;
      fFoundError=true;
      return false;
    }
  else
    {
      //Format is a little special here. See WriteTePixelData method above
      // First comes one word which is the total length in (char) of the 
      // remaing data in the TePixelData Record
      int fLength=0;
      pfInFile->read((char*)&fLength, sizeof(int));
      if(!pfInFile->good())
	{
	  std::cout<<"KSTeFile--Failed to read Te Pixel Data."
		   <<std::endl;
	  fFoundError=true;
	  return false;
	}
      int fNumWords     = fLength/sizeof(float);
      float* pfReadBuffer = new float[fNumWords];
      pfInFile->read((char*)pfReadBuffer, fLength);
      if(!pfInFile->good())
	{
	  std::cout<<"KSTeFile--Failed to read Te Pixel Data."
		   <<std::endl;
	  fFoundError=true;
	  return false;
	}
     fTeRead=false;
     //Now unpack the data
     //Zero TimePe data
     int fNumPixels=fPixel.size();
     for(int i=0;i<fNumPixels;i++)
       {
	 fPixel[i].fTimePe.clear();
       }

     int fPixelIndex=-1;
     for(int i=0;i<fNumWords;i++)
       {
	 if(pfReadBuffer[i]<0)
	   {
	     fPixelIndex = (-(int)pfReadBuffer[i])-1; //Back to Pixel index
	   }
	 else if(fPixelIndex<0)
	   {
	     std::cout<<"KSTeFile--Error Decoding Compressed TePixelData "
	       "Record"<<std::endl;
	     fFoundError=true;
	     return false;
	   }
	 else
	   {
	     fPixel[fPixelIndex].fTimePe.push_back(pfReadBuffer[i]);
	   }
       }
     fFoundError=false;
     return true;
    }
}
// ***************************************************************************




