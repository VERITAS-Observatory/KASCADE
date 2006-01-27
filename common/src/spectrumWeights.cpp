//The spectrum weights routines.
//Extracted from Veritas.cpp for use in various other places.

//Modified:

#include "eventWeights.h"
#include <cstdlib>
#include <cmath>
#include <iostream>
using namespace std;

static eventWeights* weightsProton;
static eventWeights* weightsGamma;
static eventWeights* weightsHe4;

//These routines are to be callable from fortran, so all arguments must be 
//pointers

extern "C" void VeritasEventWeight(float* tep, float* type, float* weight,
				   bool* quiet);
extern "C" void WhippleEventWeight(float* tep, float* type, float* weight,
				   bool* quiet);


// ************************************************************************

void VeritasEventWeight(float* tep, float* type, float* weight, bool* q)

// ************************************************************************
//This reoutine returns the weight for a shower of type and energy tep.
//It uses the eventWeight class. It generates mebers of this class as needed
// ************************************************************************
{
#include "VSumInit.h"   //Specifies partical type input spectrum parameteres
                        //energies used, number of sheres each energy.
                        //Must be modified for each new shower database.
#include "eventWeights.h"
  bool quiet=*q;
  float GeV=(*tep)*1000.;
  if(*type==1)           //Gammas?
    {
      if(weightsGamma==0)   //Exist yet?
	{
	  int nEnergies=(sizeof gGeV)/(sizeof *gGeV);
	  weightsGamma = new eventWeights(gAlpha,gGeV,gNshowers,gSparseTeV,
					  nEnergies,quiet);
	}
      *weight=weightsGamma->getWeight(GeV);
      return;
    }
  if(*type==13)
    {
      if(weightsProton==0)  //Exist yet?
	{
	  int nEnergies=(sizeof pGeV)/(sizeof *pGeV);
	  weightsProton = new eventWeights(pAlpha,pGeV,pNshowers,pSparseTeV,
					   nEnergies,quiet);
	}
      *weight=weightsProton->getWeight(GeV);
      return;
    }
  cout<<"Error:Warning--VeritasEventWeight:No weights for type:"<<*type<<endl;
  return;
}

// ************************************************************************


void WhippleEventWeight(float* tep, float* type, float* weight,bool* q)

// ************************************************************************
//This reoutine returns the weight for a shower of type and energy tep.
//It uses the eventWeight class. It generates mebers of this class as needed
// ************************************************************************
{
#include "WSumInit.h"   //Specifies partical type input spectrum parameteres
                        //energies used, number of sheres each energy.
                        //Must be modified for each new shower database.
#include "eventWeights.h"
  bool quiet=*q;
  float GeV=(*tep)*1000.;
  if(*type==1)           //Gammas?
    {
      if(weightsGamma==0)   //Exist yet?
	{
	  int nEnergies=(sizeof gGeV)/(sizeof *gGeV);
	  weightsGamma = new eventWeights(gAlpha,gGeV,gNshowers,gSparseTeV,
					  nEnergies, quiet);
	}
      *weight=weightsGamma->getWeight(GeV);
    }
  else if(*type==13)
    {
      if(weightsProton==0)  //Exist yet?
	{
	  int nEnergies=(sizeof pGeV)/(sizeof *pGeV);
	  weightsProton = new eventWeights(pAlpha,pGeV,pNshowers,pSparseTeV,
					   nEnergies, quiet);
	}
      *weight=weightsProton->getWeight(GeV);
    }
  else if(*type==24)
    {
      if(weightsHe4==0)  //Exist yet?
	{
	  int nEnergies=(sizeof he4GeV)/(sizeof *he4GeV);
	  weightsHe4 = new eventWeights(he4Alpha,he4GeV,he4Nshowers,
					he4SparseTeV, nEnergies, quiet);
	}
      *weight=weightsHe4->getWeight(GeV);
    }
  else
    {
      cout<<"Error:Warning--WhippleEventWeight:No weights for type:"
	                                                     <<*type<<endl;
    }
  return;
}
