#ifndef W10M_H
#define W10M_H

//W10m.h
//Code to Open,Read and Write a binary file(streaming) for use as input to 
//qteventview.

//   G.H.Sembroski
//   Physics Dept.
//   Purdue Univ.
//   W.Lafayette, In 47907

//Modified:

// 10/06/02  GHS
//        Add contructor DisplayOut(filename) and DisplayIn(filename) where 
//        the filename can be specified. If no filename specified use 
//        "Display.dat"




//#include <cstdlib>
//#include <cstring>

#include <ios>
#include <fstream>
#include <iostream>

using namespace std;

const int kNumFltHeader=1;
const int kNumIntHeader=6;
const int kNumFltHillas=8;
const int kNumFltHillasMisc=4;
const int kNumFltMisc=4;

const int kHeaderSize=kNumFltHeader*sizeof(float) + kNumIntHeader*sizeof(int);
const int kHillasSize=kNumFltHillas*sizeof(float);
const int kHillasMiscSize=kNumFltHillasMisc*sizeof(float);
const int kMiscSize=kNumFltMisc*sizeof(float);

class DisplayOut
{
 private:
  int numberPmts;
  int adcSize;
  ofstream* out;
  int RecordSize;
  char* Record;
  char* AdcPtr;
  char* HillasPtr;
  char* HillasMiscPtr;
  char* MiscPtr;
  void  InitOutput(int npmt, char* filename);
 public: 
  DisplayOut(int Npmts);    //Opens new file(Display.dat)
                                // determens record size,
                                // Creates record array.
  DisplayOut(int Npmts,char* FileName);    //Opens new file(FileName)

  ~DisplayOut(){};       
  void SetAdc(float* adc); //Load ADC data into record array
  void SetHeader(float* tep, int* id, int* type, int* nx, int* ny, 
		 int* itheta, int* iphi); // Loads stuff for Array trigger in 
                                          //specific format
  void SetHillas(float* azwidth, float* length, float* width, float* miss, 
		 float* alpha, float* dist, float* size, float* asym);
  void SetHillasMisc(float* xmean, float* ymean, float* alph, float* sdevxy); 
  void SetMisc(float* x, float* y, float* emmission_alt, float* muon_ratio);
  void WriteRecord();
};

class DisplayIn
{
 private:
  int numberPmts;
  int adcSize;
  ifstream* in;
  int RecordSize;
  char* Record;
  char* AdcPtr;
  char* HillasPtr;
  char* HillasMiscPtr;
  char* MiscPtr;
  int eventNumber;
  void  InitInput(int npmt, const char* filename);
public:
  DisplayIn(int npmts);    //Opens old input file(Display.dat)
  DisplayIn(int npmts, const char* FileName); //Opens old input file(FileName))
  ~DisplayIn(){};   //Closes input file
  void GetAdc(float* adc); //Load ADC data into record array
  void GetHeader(float* tep, int* id, int* type, int* nx, int* ny, int* itheta,
		 int* iphi);// Loads stuff for Array trigger in specific format
  void GetHillas(float* azwidth, float* length, float* width, float* miss, 
		 float* alpha, float* dist, float* size, float* asym);
  void GetHillasMisc(float* xmean, float* ymean, float* alph, float* sdevxy); 
  void GetMisc(float* x, float* y, float* emmision_alt, float* muon_ratio);
  int GetEventNumber(){return eventNumber;};
  void ReadRecord();
  bool RecordRead();
};

#endif //W10M_H  
