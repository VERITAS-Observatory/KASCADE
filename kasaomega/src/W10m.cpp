//W10m.cpp
//Code to Open,Read and Write a binary file(streaming) for use as input to 
//qteventview.

//   G.H.Sembroski
//   Physics Dept.
//   Purdue Univ.
//   W.Lafayette, In 47907

//Modified:

// 10/06/02  GHS
//        Add contructor DisplayOut(filename) and DisplayIn(filename) where 
//        the filename can be specified. If no filename specified use 
//        "Display.dat"


#include "W10m.h"


//************************************************************************
//  DisplayOut member functions
//************************************************************************

DisplayOut::DisplayOut(int npmts)
{
  char* FileName="Display.dat";
  InitOutput(npmts,FileName);
  return;
}

DisplayOut::DisplayOut(int npmts, char* FileName)
{
  InitOutput(npmts,FileName);
  return;
}

void DisplayOut::InitOutput(int npmts, char* FileName)
{
  numberPmts=npmts;
  adcSize=numberPmts*sizeof(float);
 
  RecordSize=kHeaderSize+kHillasSize+kHillasMiscSize+kMiscSize+adcSize;

  Record= new char[RecordSize];//Points to header also(start of Record)
  HillasPtr=Record+kHeaderSize; //Pointer to Hillas data in Record.
  HillasMiscPtr=HillasPtr+kHillasSize;
  MiscPtr=HillasMiscPtr+kHillasMiscSize;
  AdcPtr=MiscPtr+kMiscSize;

  out=new ofstream(FileName);
  if(! out)
    {
      cerr<<"Can't open output file: "<<FileName<<endl;
      exit(EXIT_FAILURE);
    }
  return;
}

void DisplayOut::SetAdc(float* adc)
{
  //adc follow header.
  memcpy(AdcPtr,adc,adcSize);
  return;
}
 
void DisplayOut::SetHeader(float* tep, int* id, int* type, int* nx, int* ny,
		      int* itheta, int* iphi)
{
  memcpy(Record,nx,sizeof(int));
  memcpy(Record+sizeof(int),ny,sizeof(int));
  memcpy(Record+2*sizeof(int),itheta,sizeof(int));
  memcpy(Record+3*sizeof(int),iphi,sizeof(int));
  memcpy(Record+4*sizeof(int),tep,sizeof(float));
  memcpy(Record+4*sizeof(int)+sizeof(float),id,sizeof(int));
  memcpy(Record+5*sizeof(int)+sizeof(float),type,sizeof(int));
  return;
}

void DisplayOut::SetHillas(float* azwidth, float* length, float* width, 
			   float* miss,float* alpha, float* dist, float* size,
			   float* asym)
{
  memcpy(HillasPtr,azwidth,sizeof(float));
  memcpy(HillasPtr+sizeof(float),length,sizeof(float));
  memcpy(HillasPtr+2*sizeof(float),width,sizeof(float));
  memcpy(HillasPtr+3*sizeof(float),miss,sizeof(float));
  memcpy(HillasPtr+4*sizeof(float),alpha,sizeof(float));
  memcpy(HillasPtr+5*sizeof(float),dist,sizeof(float));
  memcpy(HillasPtr+6*sizeof(float),size,sizeof(float));
  memcpy(HillasPtr+7*sizeof(float),asym,sizeof(float));
  return;
}

void DisplayOut::SetHillasMisc(float* xmean, float* ymean, float* alph, 
			  float* sdevxy)
{
  memcpy(HillasMiscPtr,xmean,sizeof(float));
  memcpy(HillasMiscPtr+sizeof(float),ymean,sizeof(float));
  memcpy(HillasMiscPtr+2*sizeof(float),alph,sizeof(float));
  memcpy(HillasMiscPtr+3*sizeof(float),sdevxy,sizeof(float));
  return;
}

void DisplayOut::SetMisc(float* x, float* y, float* emmission_alt, 
			 float* muon_ratio)
{
  memcpy(MiscPtr,x,sizeof(float));
  memcpy(MiscPtr+sizeof(float),y,sizeof(float));
  memcpy(MiscPtr+2*sizeof(float),emmission_alt,sizeof(float));
  memcpy(MiscPtr+3*sizeof(float),muon_ratio,sizeof(float));
  return;
}

void DisplayOut::WriteRecord()
{
  streamsize i=RecordSize;
  out->write(Record,i);
  return;
}



//************************************************************************
//  DisplayIn member functions
//************************************************************************
DisplayIn::DisplayIn(int npmts)
{
  char* FileName="Display.dat";
  InitInput(npmts,FileName);
  return;
}

DisplayIn::DisplayIn(int npmts, const char* FileName)
{
  InitInput(npmts,FileName);
  return;
}

void DisplayIn::InitInput(int npmts,const char* FileName)
{
  eventNumber=0;   //Init event input counter.
  numberPmts=npmts;
  adcSize=numberPmts*sizeof(float);
  RecordSize=kHeaderSize+kHillasSize+kHillasMiscSize+kMiscSize+adcSize;

  Record = new char[RecordSize];//Points to header also(start of Record)
  HillasPtr=Record+kHeaderSize; //Pointer to Hillas data in record.
  HillasMiscPtr=HillasPtr+kHillasSize;
  MiscPtr=HillasMiscPtr+kHillasMiscSize;
  AdcPtr=MiscPtr+kMiscSize;
  in= new ifstream(FileName);
  if(! in)
    {
      cerr<<"Can't open input file: "<<FileName<<endl;
      exit(EXIT_FAILURE);
    }
  return;
}

void DisplayIn::GetAdc(float* adc)
{
  //adc follow header.
  memcpy(adc,AdcPtr,adcSize);
  return;
}

void DisplayIn::GetHeader(float* tep, int* id, int* type, int* nx, int* ny,
		      int* itheta, int* iphi)
{
  memcpy(nx,Record,sizeof(int));
  char* rtemp;
  rtemp=Record+sizeof(int);
  memcpy(ny,rtemp,sizeof(int));
  memcpy(itheta,Record+2*sizeof(int),sizeof(int));
  memcpy(iphi,Record+3*sizeof(int),sizeof(int));
  memcpy(tep,Record+4*sizeof(int),sizeof(float));
  memcpy(id,Record+4*sizeof(int)+sizeof(float),sizeof(int));
  memcpy(type,Record+5*sizeof(int)+sizeof(float),sizeof(int));
  return;
}

void DisplayIn::GetHillas(float* azwidth, float* length, float* width, 
			  float* miss,float* alpha, float* dist, float* size,
			  float* asym)
{
  memcpy(azwidth,HillasPtr,sizeof(float));
  memcpy(length,HillasPtr+sizeof(float),sizeof(float));
  memcpy(width,HillasPtr+2*sizeof(float),sizeof(float));
  memcpy(miss,HillasPtr+3*sizeof(float),sizeof(float));
  memcpy(alpha,HillasPtr+4*sizeof(float),sizeof(float));
  memcpy(dist,HillasPtr+5*sizeof(float),sizeof(float));
  memcpy(size,HillasPtr+6*sizeof(float),sizeof(float));
  memcpy(asym,HillasPtr+7*sizeof(float),sizeof(float));
  return;
}

void DisplayIn::GetHillasMisc(float* xmean, float* ymean, float* alph, 
			  float* sdevxy)
{
  memcpy(xmean,HillasMiscPtr,sizeof(float));
  memcpy(ymean,HillasMiscPtr+sizeof(float),sizeof(float));
  memcpy(alph,HillasMiscPtr+2*sizeof(float),sizeof(float));
  memcpy(sdevxy,HillasMiscPtr+3*sizeof(float),sizeof(float));
  return;
}

void DisplayIn::GetMisc(float* x, float* y, float* emmission_alt,
			float* muon_ratio)
{
  memcpy(x,MiscPtr,sizeof(float));
  memcpy(y,MiscPtr+sizeof(float),sizeof(float));
  memcpy(emmission_alt,MiscPtr+2*sizeof(float),sizeof(float));
  memcpy(muon_ratio,MiscPtr+3*sizeof(float),sizeof(float));
  return;
}

void DisplayIn::ReadRecord()
{
  streamsize i=RecordSize;
  in->read(Record,i);
  eventNumber++;
  return ;
}

bool DisplayIn::RecordRead()
{
  ReadRecord();
  return !(in->eof());
}
