//Modifed:
//10/06/02 GHS.
//Don't have a seperate pedvars argument. Just use the normal ADC data. Also
//don't convert to pedvars from high level. Let calling routine do it. Also 
//add provision for specifying a filename. Defualt (in DisplayOut) is 
//Display.dat.

#include "W10m.h"


extern "C" void DisplayW10m(int* Npmts, float* adc, float* tep, int* id, 
			    int* type, int* nx, int* ny, int* itheta, int*iphi,
			    float* x, float* y, float* emission_alt, 
			    float* azwidth, float* width, float* length, 
			    float* dist, float* miss, float* alpha, 
			    float* size, float* xmean, float* ymean, 
			    float* alph, float* sdevxy, float* asym,
			    char* filename, float* muon_ratio);
//			    float* pedvars);
                                
/****************************************************************************
 * DisplayW10m
 * Writes a binary event file for use with qteventview program for image 
 * displays.
 ****************************************************************************/
void DisplayW10m(int* Npmts, float* adc, float* tep, int* id, int* type, 
		 int* nx, int* ny, int* itheta, int*iphi, float* x, float* y, 
		 float* emission_alt, float* azwidth, float* width, 
		 float* length, float* dist, float* miss, float* alpha, 
		 float* size, float* xmean, float* ymean, float* alph,
		 float* sdevxy, float* asym, char* filename, float* muon_ratio)
  //,float* high)
{
static bool firstDisplayCall=true;
static DisplayOut* OutDisp;

  //First open output file.  Always called Display.dat. Also determine length 
  //of output event record.
  if(firstDisplayCall)
    {
      int npmt=*Npmts;
      if(filename!=0)
	{
	  OutDisp = new DisplayOut(npmt,filename);
	}
      else
	{
	  OutDisp = new DisplayOut(npmt);
	}
      firstDisplayCall=false;
     }

  //Now load up the record for writting.
  OutDisp->SetAdc(adc);
  OutDisp->SetHeader(tep,id,type,nx,ny,itheta,iphi);
  OutDisp->SetHillas(azwidth,length,width,miss,alpha,dist,size,asym);
  OutDisp->SetHillasMisc(xmean,ymean,alph,sdevxy);
  OutDisp->SetMisc(x,y,emission_alt,muon_ratio);
  //Now write it
  OutDisp->WriteRecord();
  return;
}
