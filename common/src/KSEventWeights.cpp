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

//	New gamma flux parameters from Dave Lewis 15/5/96
const float gGammaAlpha  = -2.45;
const float gGammaIPhi   =  7.16e-3;           //Spectral Amplitude for gammas
                                               //Units=/m**2/sec/GeV

//Update to lastest proton and He4 spectra from: Weibel-Smith et.al. 
//Astronomy and Astrophysics, 300(1), pg.389-398,Feb. 1, 1998
const float gProtonAlpha = -2.77;
const float gProtonIPhi  =  2.35e4;	       //Spectral Amplitude for protons
					       //Units=/m**2/sec/sr/GeV
const float gHe4Alpha    = -2.64;
const float gHe4IPhi     =  5.98e3;	       //Spectral Amplitude Helium
					       //Units=/m**2/sec/sr/GeV



KSEventWeights::KSEventWeights(std::map<int,fShwrMap_t>& fShowers) 
{
  // ********************************************************************** 
  //Maps are screwy and you have to look very carefully at whats being done 
  // We use map iterators a lot (probably should use them more). See pg 200 
  // of the C++Standard Library book
  // ********************************************************************** 
  //The fShowers argument is a map where the 'key' is the Corsika particle 
  //type and the value is a fShwrMap_t.
  //the fShwrMap_t are maps where the 'key' is the energy in GeV and the
  //value is the number of showers at that energy.
  // ********************************************************************** 

  // **********************************************************************
  // Iterate through the types.
  // **********************************************************************
  for(typePos=fShowers.begin();typePos != fShowers.end();typePos++)
    {
      int fShowerType=typePos->first;  //'first is the 'key'

      // Using the [] operator below will create an element entry if the 
      // 'key' doesn't exist yet.
      fShwrWeightMap_t fTypeWeightMap = fWeightMap[fShowerType];
      fShwrMap_t fTypeNumMap = fNumMap[fShowerType];

      // ******************************************************************
      // Energies will be sorted within the type map.
      // ******************************************************************
      int fNumEnergies=typePos->second.size();  //second is the 'value'  In 
                                                //this case a  fShwrMap_t map

      if(fNumEnergies==1)	       //single shower only.
	{
	  int fEnergy=typePos->second.begin()->first;
	  fTypeWeightMap[fEnergy]=1.0;
	  int fNum= typePos->second.begin()->second;

	  fTypeNumMap[fEnergy]=fNum;
	  //creates energy as map entry key and sets associated value
	}
      else
	{
	  float fDAlpha=0;
	  float fIPhi=0;
	  if(fShowerType==1)  //Check Corsika type
	    {
	      fDAlpha=gGammaAlpha;
	      fIPhi=gGammaIPhi;
	    }
	  else if(fShowerType==14)
	    {
	      fDAlpha=gProtonAlpha;
	      fIPhi=gProtonIPhi;
	    }
	  else if(fShowerType==402)
	    {
	      fDAlpha=gHe4Alpha;
	      fIPhi=gHe4IPhi;
	    }
	  else
	    {
	      std::cout<<"KSEventWeights: We are not set up for this Corsika "
		"Particle type: "<<fShowerType<<std::endl;
	      exit(1);
	    }
	  fEnergiesGeV.clear();   //get stuff into vectors just to maske the 
	  fNumShowers.clear();    //code clearer
 	  fWeightsVector.resize(fNumEnergies,0.0);
	  for(pos=typePos->second.begin();pos != typePos->second.end();pos++)
	    {
	      fEnergiesGeV.push_back(pos->first);
	      fNumShowers.push_back(pos->second);
	    }
	
	  // **************************************************************
	  // First width is halfway up to second and the same down.
	  // Since we now use delta(ln(E)) spaceing as a const, make for 
	  // gap half way to second energy and down the same.
	  // **************************************************************
	  double fWidthLow=(double)fEnergiesGeV[0]-((double)fEnergiesGeV[1]-
					     (double)fEnergiesGeV[0])*.5; 
	  double fWidthHigh=(double)fEnergiesGeV[0]+((double)fEnergiesGeV[1]-
					     (double)fEnergiesGeV[0])*.5; 
	  // ****************************************************************
	  // Using C pow(x,y)=x**y  function,  integrel of E**alpha from wl to
	  // wh; constants retained for different types
	  // *****************************************************************
	  double fFluxConst=(fIPhi/(fDAlpha+1.0));
	  float fW=fFluxConst*
	    (pow(fWidthLow,(fDAlpha+1.0))-pow(fWidthHigh,(fDAlpha+1.0)));
	  fWeightsVector[0]=(fW/fNumShowers[0]);
	  fTypeWeightMap[fEnergiesGeV[0]]=fWeightsVector[0];
	  fTypeNumMap[fEnergiesGeV[0]]=fNumShowers[0];

	  // ****************************************************
	  // Now the rest
	  // ****************************************************
	  for(int i=1;i<fNumEnergies;i++)
	    {
	      //Go down to match previous and go up by the same amount.
	      fWidthLow=fWidthHigh;
	      fWidthHigh=(double)fEnergiesGeV[i]+
		((double)fEnergiesGeV[i]-fWidthLow);
	      if((fWidthHigh-fWidthLow)<0)
		{
		  std::cout<<" --WARNING--(dist.ftn)-Bad Shower Energy Spacing"
		    " --between energies"<<fEnergiesGeV[i-1]<<"GeV and"
			   <<fEnergiesGeV[i]<<" GeV"<<std::endl;
		  std::cout<<" --WARNING--To compensate for Bad spacing--"
		    "Useing uncentered bin at shower energy"
			   <<fEnergiesGeV[i-1]<<"GeV"<<std::endl;
		  //Have to first redo energy i-1. Find bin half width.
		  double fWidthHalf=fWidthLow-(double)fEnergiesGeV[i-1];
		  double fWidthL=(double)fEnergiesGeV[i-1]-fWidthHalf;
		  //High side if halfway between
		  double fWidthH=(double)fEnergiesGeV[i-1] +
		    (((double)fEnergiesGeV[i]-(double)fEnergiesGeV[i-1])/2);
		  fWeightsVector[i-1]=fFluxConst*
		    (pow(fWidthL,(fDAlpha+1.0))-pow(fWidthH,(fDAlpha+1.0)));

		  fWeightsVector[i-1]=fWeightsVector[i-1]/fNumShowers[i-1];
		  fTypeWeightMap[fEnergiesGeV[i-1]]=fWeightsVector[i-1];

		  fWidthLow=fWidthH;
		  fWidthHigh=(double)fEnergiesGeV[i]+((double)fEnergiesGeV[i]-
						      fWidthLow);
		}
	    
	      // *************************************************************
	      // integrel of E**alpha from fWidthL to fWidthH; 
	      // *************************************************************
	      fWeightsVector[i]=fFluxConst*
		(pow(fWidthLow,(fDAlpha+1.0))-pow(fWidthHigh,(fDAlpha+1.0)));
		  //Compensate for multiple showers at each energy
	      fWeightsVector[i] = fWeightsVector[i]/fNumShowers[i];
	      // *************************************************************
	      // Now place this "weight" into the map. We will normalize later
	      //after all types are done.
	      // *************************************************************
	      fTypeWeightMap[fEnergiesGeV[i]]=fWeightsVector[i];
	      fTypeNumMap[fEnergiesGeV[i]]=fNumShowers[i];
	    }
	}
    }
  // *************************************************************
  // Find the max weight
  // *************************************************************
  fMaxWeight=0.;
  for(weightPos=fWeightMap.begin();weightPos != fWeightMap.end();weightPos++)
    {

    for(typeWeightPos=weightPos->second.begin();
	typeWeightPos!=weightPos->second.end();weightPos++)
      {
	if((typeWeightPos->second) > fMaxWeight)
	{
	  fMaxWeight=typeWeightPos->second;
	}
      }    
    }         
  // ***********************************************************************
  // Re-scale so maximum weight is 1.0: all types
  // ***********************************************************************
  for(weightPos=fWeightMap.begin();weightPos != fWeightMap.end();weightPos++)
    {

      for(typeWeightPos=weightPos->second.begin();
	  typeWeightPos!=weightPos->second.end();weightPos++)
	{

	  typeWeightPos->second=typeWeightPos->second/fMaxWeight;
	}
    }
    // And we are done.
  return;
}
// **************************************************************************

KSEventWeights::~KSEventWeights()
{
  // Nothing to do
}
// **************************************************************************


 float KSEventWeights::getWeight(int type, int energyGeV)
{
  weightPos=fWeightMap.find(type);
  if(weightPos == fWeightMap.end())
    {
      std::cout<<"KSEventWeights: Failed to find Corsika type: "
	       <<type<<std::endl;
      exit(1);
    }
  typeWeightPos=weightPos->second.find(energyGeV);
  if(typeWeightPos == weightPos->second.end())
    {
      std::cout<<"KSEventWeights: Failed to find Energy (Gev)"<<energyGeV
	       <<std::endl;
      exit(1);
    }
  return typeWeightPos->second;
}
// **************************************************************************

void KSEventWeights::Print()
// **************************************************************************
{
  std::cout<<"Type Energy(GeV) Weight(Gev) #showers "<<std::endl;

  for(weightPos=fWeightMap.begin();weightPos != fWeightMap.end();weightPos++)
    {

      fShwrMap_t fTypeNumMap=fNumMap[weightPos->first];
      for(typeWeightPos=weightPos->second.begin();
	  typeWeightPos!=weightPos->second.end();weightPos++)
	{
	  typeNumPos=fTypeNumMap.find(typeWeightPos->first);
	  std::cout<<std::setw(5)<< weightPos->first<<std::setw(11)
		   <<typeWeightPos->first<<std::setw(12)
		   <<typeWeightPos->second
		   <<std::setw(9)<<typeNumPos->second<<std::endl;
	}     
    }
  return;
}
