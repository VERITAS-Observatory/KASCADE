//-*-mode:c++; mode:font-lock;-*-
/**
 * Original Author: GlennSembroski
 * $Author$
 * $Date$
 * $Revision$
 * $Tag$
 *
 **/

#ifndef KSEVENTWEIGHTS_H
#define KSEVENTWEIGHTS_H
// *******************************************************************
//Header file for eventWeight class
//This class used to generate weights for events in each shower depending on
//number of showers at that energy , energy width the energy represents, and
//primary spectrum. Weights will be mormalized so that the biggest is 1.0.
//This is reimplimentation of code from sum_init.kumac,dist.f, michelle_weight
//and michell_dist.
// ********************************************************************
//Exception
#include <vector>
#include <map>
#include <iostream>
#include <iomanip>
#include <cmath>                       //For pow(x,y)=x**y  function.


typedef std::map<int,int > fShwrMap_t;
typedef std::map<int,float > fShwrWeightMap_t;

class KSEventWeights
{
 public:
  KSEventWeights(std::map<int, fShwrMap_t> Showers); 
  ~KSEventWeights();
  void calculateWeights();
  float getWeight(int type, int fEnergyGeV);
  float getMaximumWeight(){return fMaxWeight;};
  float getWeightedDifferentialRateHzPerM2(int type, int energyGeV);
  int getNumShowers(int type, int fEnergyGeV);
  double getIPhi(int type);
  double getAlpha(int type);
  float  getDistributedEnergy(int type, int fEnergyGeV);
  void  Print();
 private:
  std::map<int, fShwrMap_t> fShowers;
  std::map<int, fShwrMap_t >::iterator fShowersPos;
  fShwrMap_t::iterator pos;
  fShwrWeightMap_t::iterator fPos;

  float fMaxWeight;

  std::vector<float> fWeightsVector;  //Temp vectors used to fill maps
  std::vector<float> fELowVector;
  std::vector<float> fEHighVector;
  std::vector<int> fEnergiesGeV;
  std::vector<int> fNumShowers;

  std::map<int, fShwrWeightMap_t > fWeightMap;   //Map of a map
  std::map<int, fShwrWeightMap_t >::iterator fWeightPos;
  fShwrWeightMap_t::iterator fShowerWeightPos;

  std::map<int, fShwrMap_t > fNumMap;   //Map of a map
  std::map<int, fShwrMap_t >::iterator fNumPos;

  std::map<int, fShwrWeightMap_t > fELowMap;   //Map of a map
  std::map<int, fShwrWeightMap_t >::iterator fELowPos;

  std::map<int, fShwrWeightMap_t > fEHighMap;   //Map of a map
  std::map<int, fShwrWeightMap_t >::iterator fEHighPos;
};

#endif

