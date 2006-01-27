//kastriggerMHDF5Subs.cpp
//	This file has has the C++ routines for use in kastrigger to write out
//      the MHDF5 file using the MHDF5SimDataFile classes.
//   G.H.Sembroski
//   Physics Dept.
//   Purdue Univ.
//   W.Lafayette, In 47907
//sembroski@physics.purdue.edu
//   01/11/04
//Modified:

#include "MHDF5SimDataFile.h"

extern "C" void MHDF5Init(char* filename, int* itype, float* tep, float* dli, 
			  float* dmi, char* magnet_field, float* etr,
			  float* depth, float* zobs, float* xinitial, 
			  float* yinitial, int* idfile, char* version_seg, 
			  float* dl, float* dm, float* xseg, float* yseg, 
			  float* hobs, float* x_offset, float* y_offset, 
			  char* petype, char* version_pe, int* noise, 
			  float* disc_width,float* rate_noise, 
			  float* reflectivity, float* full_aperture, 
			  float* pesum, float* step_size, int* itheta_max, 
			  int* phi_steps, char* version_m, float* adc_gate, 
			  float* hmult, float* concentration, char* options);



extern "C" void MHDF5Out(int *recnum, float *tep, int *idfile,
			 float *x_offset, float *y_offset, short* nx, 
			 short* ny, short* itheta,short* iphi, 
			 float* emission_alt, float* em_alt_sig,
			 float* muon_ratio, float* median_time, 
			 float* fp_median_time, float* time_cfd, 
			 float* fp_time_cfd, float* ithphi,
			 float* fp_threshold, float* i_mult, float* fp_mult, 
			 float *disc, float *times, int *npixels);

extern "C" void MHDF5Close();

// ***************************************************************

MHDF5SimDataFile* outputMHDF5;

// ***************************************************************


void MHDF5Init(char* filename, int* itype, float* tep, float* dli, 
			  float* dmi, char* magnet_field, float* etr,
			  float* depth, float* zobs, float* xinitial, 
			  float* yinitial, int* idfile, char* version_seg, 
			  float* dl, float* dm, float* xseg, float* yseg, 
			  float* hobs, float* x_offset, float* y_offset, 
			  char* petype, char* version_pe, int* noise, 
			  float* disc_width,float* rate_noise, 
			  float* reflectivity, float* full_aperture, 
			  float* pesum, float* step_size, int* itheta_max, 
			  int* phi_steps, char* version_m, float* adc_gate, 
			  float* hmult, float* concentration, char* options)
// ************************************************************************
//  Open a new MHDF5 file and write out the header datset
//  For use in kastrigger.
// ************************************************************************
//Modified:

{
  outputMHDF5 = new MHDF5SimDataFile();
  outputMHDF5->createFile(filename);       //Create and Open ouput file
  //  cout<<"MHDF5Init:Filename: "<<filename<<endl;


  MHDF5MHead* iHead = new MHDF5MHead(); //create and fill M header record,
  iHead->itype=*itype;            // Start of seg head stuff 
  iHead->tep=*tep;
  iHead->dli=*dli;
  iHead->dmi=*dmi;
  for(int i=0;i<4;i++)iHead->magnet_field[i]=magnet_field[i];
  iHead->etr=*etr;
  iHead->depth=*depth;
  iHead->zobs=*zobs;
  iHead->xinitial=*xinitial;
  iHead->yinitial=*yinitial;
  iHead->idfile=*idfile;
  for(int i=0;i<8;i++)iHead->version_seg[i]=version_seg[i];
  iHead->dl=*dl;               // Start of pe head stuff 
  iHead->dm=*dm;
  iHead->xseg=*xseg;
  iHead->yseg=*yseg;
  iHead->hobs=*hobs;
  iHead->x_offset=*x_offset;
  iHead->y_offset=*y_offset;
  for(int i=0;i<16;i++)iHead->petype[i]=petype[i];
  for(int i=0;i<16;i++)iHead->version_pe[i]=version_pe[i];
  iHead->noise=*noise;       // Start of m head stuff 
  iHead->disc_width=*disc_width;
  iHead->rate_noise=*rate_noise;
  iHead->reflectivity=*reflectivity;
  iHead->full_aperture=*full_aperture;
  iHead->pesum=*pesum;
  iHead->step_size=*step_size;
  iHead->itheta_max=*itheta_max;
  iHead->phi_steps=*phi_steps;
  for(int i=0;i<16;i++)iHead->version_m[i]=version_m[i];
  iHead->adc_gate=*adc_gate;
  iHead->hmult=*hmult;
  iHead->concentration=*concentration;
  for(int i=0;i<60;i++)iHead->options[i]=options[i];

  outputMHDF5->writeMHeadRec(iHead);

} 
// ******************************************************************


void MHDF5Out(int *recnum, float *tep, int *idfile, float *x_offset, 
	      float *y_offset, short* nx, short* ny, short* itheta,
	      short* iphi, float* emission_alt, float* em_alt_sig,
	      float* muon_ratio, float* median_time, float* fp_median_time,
	      float* time_cfd, float* fp_time_cfd, float* ithphi,
	      float* fp_threshold, float* i_mult, float* fp_mult, 
	      float *disc, float *times, int *npixels)
// ******************************************************************
//  Loadup MHDF5Rec record and write out to file{
// ******************************************************************
//ARGUMENTS:
//   recnum: place in array to be rwritten out of this record.
//   mrec:   header struff for this record.
//   disc:   Array of pixels with number of pes that hit each pixel.
//   time:   pes hit times for each pe. Times are in order of pes in disc.
{
  MHDF5EventData* MHDF5Ev = new MHDF5EventData();
	      

  //Load up the MHDF5 event record
  MHDF5Ev->dataHead.tep=*tep;
  MHDF5Ev->dataHead.idfile=*idfile;
  MHDF5Ev->dataHead.x_offset=*x_offset;
  MHDF5Ev->dataHead.y_offset=*y_offset;
  
  MHDF5Ev->rec.nx=*nx;
  MHDF5Ev->rec.ny=*ny;
  MHDF5Ev->rec.itheta=*itheta;
  MHDF5Ev->rec.iphi=*iphi;
  MHDF5Ev->rec.emission_alt=*emission_alt;
  MHDF5Ev->rec.em_alt_sig=*em_alt_sig;
  MHDF5Ev->rec.muon_ratio=*muon_ratio;
  MHDF5Ev->rec.median_time=*median_time;
  MHDF5Ev->rec.fp_median_time=*fp_median_time;
  MHDF5Ev->rec.time_cfd=*time_cfd;
  MHDF5Ev->rec.fp_time_cfd=*fp_time_cfd;
  MHDF5Ev->rec.ithphi=*ithphi;
  MHDF5Ev->rec.fp_threshold=*fp_threshold;
  MHDF5Ev->rec.i_mult=*i_mult;
  MHDF5Ev->rec.fp_mult=*fp_mult;

  //disc has number of pes in each pixels.
  //times has pes times in order of disc 
 
  int itimes=0;                   //Keeps track of location within times array
  for (int i=0; i<*npixels; i++)
    {
      int idisc=(int) disc[i];
      if(idisc>0) 
	{
	  MHDF5Ev->iPix.push_back(i+1);  //Pixels start at 1
	  int indx=MHDF5Ev->iPix.size();  //Index of last pixel loaded
	  MHDF5Ev->pixels.resize(indx);
	  for(int j=0; j<idisc; j++)
	    {
	      MHDF5Ev->pixels[indx-1].push_back(times[itimes]);
	      itimes++;
	    }
	}
    }

  //Write it out
  outputMHDF5->writeEvent(MHDF5Ev);
  delete MHDF5Ev;
}
// *************************************************************************

void MHDF5Close()
{
  outputMHDF5->closeFile();
  return;
}
// *************************************************************************


