//-*-mode:c++; mode:font-lock;-*-
/**
 * \class VAKascadeSimulation Class methods
 * \ingroup common
 * \brief Data classes for holding Kascade simulation specific data
 *
 * Here is a tedious verbose multi-line description of all
 * the details of the code, more than you would
 * ever want to read. Generally, all the important documentation
 * goes in the .cpp files.
 *
 * Original Author: Glenn Sembroski
 * $Author$
 * $Date$
 * $Revision$
 * $Tag$
 *
 **/

#include "KSEventWeights.h"


KSEventWeights::KSEventWeights(float alpha, 
			       std::map<int,int,std::greater<int> >& fShowers) 
{
  //Algorithums Derived form michell_dist from kasaomega_cmn_sub.F90
  //Maps are screwy and you have to look very carefully at whats being done 

  // **********************************************************************
  // Get stuff out of the map and into some vectors for temporary ease of
  // use. Use map iterators. See pg 200 fo C++Standard Library book
  // **********************************************************************
  fEnergiesGeV.clear();
  fNumShowers.clear();
  for(pos=fShowers.begin();pos != fShowers.end();pos++)
    {
      fEnergiesGeV.push_back(pos->first);
      fNumShowers.push_back(pos->second);
    }
  int fNumEnergies=fShowers.size();
  std::vector<float> fWeights(fNumEnergies);
   
  if(fNumEnergies==1)	       //single shower only.
    {
      fWeightMap[fShowers.begin()->first]=1.0; //creates map entry key and sets 
      return;                         //associated value
    }
  // **********************************************************************
  // First width is halfway up to second and the same down.
  //	Since we now use delta(ln(E)) spaceing as a const, make for gap half 
  //	way to second energy and down the same.
  // **********************************************************************
  double fWidthLow=(double)fEnergiesGeV[0]-((double)fEnergiesGeV[1]-
					    (double)fEnergiesGeV[0])*.5; 
  double fWidthHigh=(double)fEnergiesGeV[0]+((double)fEnergiesGeV[1]-
					     (double)fEnergiesGeV[0])*.5; 
               
  // *************************************************************************
  // Using C pow(x,y)=x**y  function,  integrel of E**alpha from wl to wh; 
  // constants removed
  // ************************************************************************
  double fDAlpha=alpha;
  fWeights[0]=(pow(fWidthLow,(fDAlpha+1.0))-pow(fWidthHigh,(fDAlpha+1.0)));
  fWeights[0]=fWeights[0]/fNumShowers[0];

  for(int i=1;i<fNumEnergies;i++)
    {
                      //Go down to match previous and go up by the same amount.
      fWidthLow=fWidthHigh;
      fWidthHigh=(double)fEnergiesGeV[i]+((double)fEnergiesGeV[i]-fWidthLow);
      if((fWidthHigh-fWidthLow)<0)
	{
	  std::cout<<" --WARNING--(dist.ftn)--Bad Shower Energy Spacing"
	             " --between energies"<<fEnergiesGeV[i-1]<<"GeV and"
		   <<fEnergiesGeV[i]<<" GeV"<<std::endl;
	  std::cout<<" --WARNING--To compensate for Bad spacing--Useing "
	             "uncentered bin at shower energy"<<fEnergiesGeV[i-1]
		   <<"GeV"<<std::endl;
	  //Have to first redo energy i-1. Find bin half width.
	  double fWidthHalf=fWidthLow-(double)fEnergiesGeV[i-1];
	  double fWidthL=(double)fEnergiesGeV[i-1]-fWidthHalf;
	    //High side if halfway between
	  double fWidthH=(double)fEnergiesGeV[i-1] +
	              (((double)fEnergiesGeV[i]-(double)fEnergiesGeV[i-1])/2);
	  fWeights[i-1]=pow(fWidthL,(fDAlpha+1.0))-pow(fWidthH,(fDAlpha+1.0));
	  fWidthLow=fWidthH;
	  fWidthHigh=(double)fEnergiesGeV[i]+((double)fEnergiesGeV[i]-
					                            fWidthLow);
	}
      
      // *******************************************************************
      // integrel of E**alpha from fWidthL to fWidthH; constants removed
      // This is weight as if we only had 1 shower at each energy
      // *******************************************************************
      fWeights[i]=pow(fWidthLow,(fDAlpha+1.0))-pow(fWidthHigh,(fDAlpha+1.0));
                     //Compensate for multiple showers at each energy
      fWeights[i] = fWeights[i]/fNumShowers[i];
     }

  // **********************************************************************
  // Find the max weight
  fMaxWeight=0.;
  for(int i=0;i<fNumEnergies;i++)
    {
      if(fWeights[i]>fMaxWeight)
	{
	  fMaxWeight=fWeights[i];
	}
    }             
  // ***********************************************************************
  // Re-scale so maximum weight is 1.0 and load into the Weight map which we 
  // will use as an associative array
  // ***********************************************************************
  for(int i=0;i<fNumEnergies;i++)
    {
      fWeightMap[fEnergiesGeV[i]]=fWeights[i]/fMaxWeight;
    }
  return;
}
// **************************************************************************

KSEventWeights::~KSEventWeights()
{
  // Nothing to do
}
// **************************************************************************


float KSEventWeights::getWeight(int energyGeV)
{
  fPos=fWeightMap.find(energyGeV);
  if(fPos == fWeightMap.end())
    {
      std::cout<<"KSEventWeights: Failed to find energy(Gev):"<<energyGeV
	       <<std::endl;
      exit(1);
    }
  return fPos->second;
}
// **************************************************************************

void KSEventWeights::Print()
// **************************************************************************
{
  std::cout<<"Energy(GeV) Weight(Gev) #showers "<<std::endl;

  int i=0;
  for(fPos=fWeightMap.begin();fPos != fWeightMap.end();fPos++)
      {
	std::cout<<std::setw(11)<<fPos->first<<std::setw(12)<<fPos->second
		 <<std::setw(9)<<fNumShowers[i]<<std::endl;
	  i++;
      }
  return;
}
