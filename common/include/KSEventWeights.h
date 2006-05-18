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



class KSEventWeights
{
 public:
  KSEventWeights(float alpha, std::map<int,int,std::greater<int> >& fShowers); 
                                      //constructor will generate the weights 
  ~KSEventWeights();
  float getWeight(int fEnergyGeV);
  float getMaximumWeight(){return fMaxWeight;};
  void  Print();
 private:
  std::map<int,float,std::greater<int> > fWeightMap;
  std::map<int,float>::iterator fPos;
  std::map<int,int>::iterator pos;
  float fMaxWeight;
  std::vector<int> fEnergiesGeV;
  std::vector<int> fNumShowers;
};

#endif

