//eventWeight class methods.
//Glenn Sembroski
//Physics Dept.
//Purdue Univ
//W.Lafayette, In 47906
//765-494-5172
//sembroski@physics.purdue.edu
//18/07/03

//Modified:


#include "eventWeights.h"
#include <cmath>                       //For pow(x,y)=x**y  function.
#include <iostream>
using namespace std;


eventWeights::eventWeights(float alpha, float* energiesGeV, int* nShowers,
                                         float sparseTeV,int nEnergies,
			                 bool quiet)
{

  //Algorithums Derived form michell_dist from kasaomega_cmn_sub.F90

  numberEnergies=nEnergies;
                      //First width is halfway up to second and the same down.
  fWeights = new float[numberEnergies];
  fEnergiesGeV = new float[numberEnergies];

  for(int i=0;i<numberEnergies;i++)
    {
      fEnergiesGeV[i]=energiesGeV[i];
    }
   
  if(numberEnergies==1)	       //single shower only.
    {
      fWeights[0]=1.0;
      return;
    }
  //	Since we now use delta(ln(E)) spaceing as a const, make for gap half 
  //	way to second energy and down the same.
  double wlow=energiesGeV[0]-(energiesGeV[1]-energiesGeV[0])*.5; 
  double whi=energiesGeV[0]+(energiesGeV[1]-energiesGeV[0])*.5; 
               
                      //Using C pow(x,y)=x**y  function
	             //integrel of E**alpha from wl to wh; constants removed
  double dAlpha=alpha;
  fWeights[0]=(pow((wlow),(dAlpha+1.0))-pow(whi,(dAlpha+1.0)));
  fWeights[0]=fWeights[0]/nShowers[0];


  for(int i=1;i<numberEnergies;i++)
    {
                      //Go down to match previous and go up by the same amount.
      wlow=whi;
      whi=energiesGeV[i]+(energiesGeV[i]-wlow);
      if((whi-wlow)<0)
	{
	  cout<<" --WARNING--(dist.ftn)--Bad Shower Energy Spacing"<<endl;
	  cout<<" --between energies"<<energiesGeV[i-1]<<"GeV and"<<
	    energiesGeV[i]<<" GeV"<<endl;
	  cout<<" --WARNING--To compensate for Bad spacing"<<endl;
	  cout<<" --Useing uncentered bin at shower energy"<<energiesGeV[i-1]<<
	    "GeV"<<endl;
	  //Have to first redo energy i-1. Find bin half width.
	  float w_half=wlow-energiesGeV[i-1];
	  double wl=energiesGeV[i-1]-w_half;
	    //High side if halfway between
	  double wh=energiesGeV[i-1]+((energiesGeV[i]-energiesGeV[i-1])/2);

	             //integrel of E**alpha from wl to wh; constants removed
	  fWeights[i-1]=pow(wl,(dAlpha+1.0))-pow(wh,(dAlpha+1.0));
	  wlow=wh;
	  whi=energiesGeV[i]+(energiesGeV[i]-wlow);
	}
       	             //integrel of E**alpha from wl to wh; constants removed
                     //This is weight as if we only had 1 shower at each energy
      fWeights[i]=pow(wlow,(dAlpha+1.0))-pow(whi,(dAlpha+1.0));
                     //Compensate for multiple showers at each energy
      fWeights[i] = fWeights[i]/nShowers[i];
     }

  //Now scale to the number of showers
  wmax=0.;

  float sparseGeV=sparseTeV*1000.;
  for(int i=0;i<numberEnergies;i++)
    {
                //Adjust weights for energies greater then sparse TeV
                //We use a  partial sampling above sparse to reduce file size.
      if(energiesGeV[i]>sparseGeV)
	{
	  fWeights[i]=4.*fWeights[i];
	}
      if(fWeights[i]>wmax)
	{
	  wmax=fWeights[i];
	}
    }             
                          //re-scale so maximum weight is 1.0
  float weight100=0;
  float weight294=0;
  if(!quiet)
    {
     for(int i=0;i<numberEnergies;i++)
	{
	  if(energiesGeV[i]==100)
	    {
	      weight100=fWeights[i]/wmax;
	    }
	  if(energiesGeV[i]==294)
	    {
	      weight294=fWeights[i]/wmax;
	    }
	}
    }
 
  for(int i=0;i<numberEnergies;i++)
    {
      fWeights[i]=fWeights[i]/wmax;
      if(!quiet)
	{
	  float for100at100=(fWeights[i]/weight100)*nShowers[i];
	  float for50at294=(fWeights[i]/weight294)*nShowers[i];
	  cout<<"E(GeV):#:Weight:#100@1.0%:#294@1.0:"<<
	    energiesGeV[i]<<","<<nShowers[i]<<","<<fWeights[i]<<","<<
	    (int)for100at100<<","<<(int)for50at294<<endl;
	}
    }
  return;
}

eventWeights::~eventWeights()
{
  delete [] fWeights;
  delete [] fEnergiesGeV;
  return;
}


float eventWeights::getWeight(float energyGeV)
{
  for(int i=0;i<numberEnergies;i++)
    {
      if(fabs(fEnergiesGeV[i]-energyGeV)<1)
	{
	  return fWeights[i];
	}
    }
  cout<<"energyWeight failed to find energyGev:"<<energyGeV<<endl;
  throw unknownEnergy();
}

 
