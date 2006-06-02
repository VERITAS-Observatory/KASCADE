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
const double gGammaAlpha  = -2.45;
const double gGammaIPhi   =  7.16e-3;           //Spectral Amplitude for gammas
                                               //Units=/m**2/sec/GeV

//Update to lastest proton and He4 spectra from: Weibel-Smith et.al. 
//Astronomy and Astrophysics, 300(1), pg.389-398,Feb. 1, 1998
const double gProtonAlpha = -2.77;
const double gProtonIPhi  =  2.35e4;	       //Spectral Amplitude for protons
					       //Units=/m**2/sec/sr/GeV
const double gHe4Alpha    = -2.64;
const double gHe4IPhi     =  5.98e3;	       //Spectral Amplitude Helium
					       //Units=/m**2/sec/sr/GeV



KSEventWeights::KSEventWeights(std::map<int,fShwrMap_t> Showers) 
// ********************************************************************** 
// Save the input map of maps
// ********************************************************************** 
{
  fShowers=Showers;
}
// ********************************************************************** 

KSEventWeights::~KSEventWeights()
{
  // Nothing to do
}
// ************************************************************************

void KSEventWeights::calculateWeights() 
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
  for(fShowersPos=fShowers.begin();fShowersPos != fShowers.end();fShowersPos++)
    {
      // ******************************************************************
      // Create entries in the ouput maps if we need to, and get
      // iterators to maps of this type
      // ******************************************************************
      int fShowerType=fShowersPos->first;  //'first' is the 'key'
      fWeightPos = fWeightMap.find(fShowerType);
      if(fWeightPos==fWeightMap.end())
	{
	  fWeightMap[fShowerType]; // Using the [] operator will create an 
	                           // element entry. see pg 202 C++ std book
	  fWeightPos = fWeightMap.find(fShowerType);
	}
      fNumPos = fNumMap.find(fShowerType);
      if(fNumPos==fNumMap.end())
	{
	  fNumMap[fShowerType];
	  fNumPos = fNumMap.find(fShowerType);
	}

      // ******************************************************************
      // Energies will be sorted within the type map.
      // ******************************************************************
      int fNumEnergies=fShowersPos->second.size(); //second is the 'value'  In 
                                                  //this case a  fShwrMap_t map
      if(fNumEnergies==1)	       //single shower only.
	{
	  int fEnergy=fShowersPos->second.begin()->first;
	  fWeightPos->second[fEnergy]=1.0;
	  int fNum= fShowersPos->second.begin()->second;
	  fNumPos->second[fEnergy]=fNum;
	}
      else
	{
	  double fIAlpha=0;  //Integrel index
	  double fIPhi=0;
	  if(fShowerType==1)  //Check Corsika type
	    {
	      fIAlpha=1.+gGammaAlpha;
	      fIPhi=gGammaIPhi;
	    }
	  else if(fShowerType==14)
	    {
	      fIAlpha=1.+gProtonAlpha;
	      fIPhi=gProtonIPhi;
	    }
	  else if(fShowerType==402)
	    {
	      fIAlpha=1.+gHe4Alpha;
	      fIPhi=gHe4IPhi;
	    }
	  else
	    {
	      std::cout<<"KSEventWeights: We are not set up for this Corsika "
		"Particle type: "<<fShowerType<<std::endl;
	      exit(1);
	    }
	  
	  // *****************************************************************
	  //get stuff into vectors just to maske the code clearer
	  // *****************************************************************
	  fEnergiesGeV.clear(); 
	  fNumShowers.clear();
 	  fWeightsVector.resize(fNumEnergies,0.0);
	  for(pos=fShowersPos->second.begin()
		;pos != fShowersPos->second.end();pos++)
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
	  // ****************************************************************
	  double fFluxConst=(fIPhi/(fIAlpha));
	  double fW=pow(fWidthHigh,(fIAlpha)) -  pow(fWidthLow, (fIAlpha));
	  fW=fFluxConst*fW/fNumShowers[0];
	  fWeightsVector[0]=fW;
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
		    (pow(fWidthH,(fIAlpha))-pow(fWidthL,(fIAlpha)));

		  fWeightsVector[i-1]=fWeightsVector[i-1]/fNumShowers[i-1];
		  fWidthLow=fWidthH;
		  fWidthHigh=(double)fEnergiesGeV[i]+((double)fEnergiesGeV[i]-
						      fWidthLow);
		}
	    
	      // *************************************************************
	      // integrel of E**alpha from fWidthL to fWidthH; 
	      // *************************************************************
	      fWeightsVector[i]=fFluxConst*
		(pow(fWidthHigh,(fIAlpha))-pow(fWidthLow,(fIAlpha)));
		  //Compensate for multiple showers at each energy
	      fWeightsVector[i] = fWeightsVector[i]/fNumShowers[i];
	      // *************************************************************
	      // Now place this "weight" into the map. We will normalize later
	      //after all types are done.
	      // *************************************************************
	    }

	  // ****************************************************************
	  // Now fill the Weight and Num maps

	  for(int i=0;i<fNumEnergies;i++)
	    {
	      fWeightPos->second[ fEnergiesGeV[i] ]=(float)fWeightsVector[i];
	      fNumPos->second[ fEnergiesGeV[i] ]=fNumShowers[i];
	    }
	}
    }
  // *************************************************************
  // Find the max weight
  // *************************************************************
  fMaxWeight=0.;
  for(fWeightPos=fWeightMap.begin();fWeightPos != fWeightMap.end();
      fWeightPos++)
    {

    for(fShowerWeightPos=fWeightPos->second.begin();
	fShowerWeightPos!=fWeightPos->second.end();fShowerWeightPos++)
      {
	if((fShowerWeightPos->second) > fMaxWeight)
	{
	  fMaxWeight=fShowerWeightPos->second;
	}
      }    
    }         
  // ***********************************************************************
  // Re-scale so maximum weight is 1.0: all types
  // ***********************************************************************
  for(fWeightPos=fWeightMap.begin();fWeightPos != fWeightMap.end()
;fWeightPos++)
    {

      for(fShowerWeightPos=fWeightPos->second.begin();
	  fShowerWeightPos!=fWeightPos->second.end();fShowerWeightPos++)
	{

	  fShowerWeightPos->second=fShowerWeightPos->second/fMaxWeight;
	}
    }
    // And we are done.
  return;
}
// **************************************************************************

float KSEventWeights::getWeight(int type, int energyGeV)
// **************************************************************************
// Find in our map of maps 
// **************************************************************************
{
  fWeightPos=fWeightMap.find(type);
  if(fWeightPos == fWeightMap.end())
    {
      std::cout<<"KSEventWeights: Failed to find Corsika type: "
	       <<type<<std::endl;
      exit(1);
    }
  fShowerWeightPos=fWeightPos->second.find(energyGeV);
  if(fShowerWeightPos == fWeightPos->second.end())
    {
      std::cout<<"KSEventWeights: Failed to find Energy (Gev)"<<energyGeV
	       <<std::endl;
      exit(1);
    }
  return fShowerWeightPos->second;
}
// **************************************************************************

void KSEventWeights::Print()
// **************************************************************************
{
  std::cout<<" Type Energy(GeV) Weight #showers"<<std::endl;

  for(fWeightPos=fWeightMap.begin();fWeightPos != fWeightMap.end();
      fWeightPos++)
    {
      
      int fShowerType=fWeightPos->first;
      fNumPos = fNumMap.find(fShowerType);
      for(fShowerWeightPos=fWeightPos->second.begin();
	  fShowerWeightPos!=fWeightPos->second.end();
	  fShowerWeightPos++)
	{
	  int fEnrgy=fShowerWeightPos->first;
	  pos=fNumPos->second.find(fEnrgy);
	  int fNum=pos->second;
	  float fWt=fShowerWeightPos->second;
	  std::cout<<std::setw(5)<<fShowerType<<std::setw(11)<<fEnrgy<<" "
		   <<std::setw(6)<<fWt<<std::setw(9)<<fNum<<std::endl;
	}     
    }
  return;
}
