#ifndef EVENT_WEIGHTS_H
#define EVENT_WEIGHTS_H
//Header file for eventWeight class
//This class used to generate weights for events in each shower depending on
//number of showers at that energy , energy width the energy represents, and
//primary spectrum. Weights will be mormalized so that the biggest is 1.0.
//This is reimplimentation of code from sum_init.kumac,dist.f, michelle_weight
//and michell_dist.

//Exception
class unknownEnergy{};

class eventWeights
{
 public:
  eventWeights(float alpha, float* energiesGeV, int* nShowers,
	       float sparseTeV, int nEnergies, bool quiet); 
                                      //constructor will generate the weights 
  ~eventWeights();
  float getWeight(float GeV);
  float getMaximumWeight(){return wmax;};
 private:
  float* fWeights;
  float* fEnergiesGeV;
  float wmax;
  int numberEnergies;
};

#endif

