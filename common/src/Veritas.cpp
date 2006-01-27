//Various routines for kasaomega to use with the VERITAS camera.
#include "VPulse.h"
#include "Veritas_defs.h"
#include "Random.h"
#include "VHDF5KascadeRunDataFile.h"
#include "VPattern.h"
#include <vector>
#include <cstdlib>
#include <cmath>
#include <iostream>
#include <iomanip>
using namespace std;

#define WHIPLONG          1.935190
#define WHIPLAT           0.552978


//Modifed:
//
//21/07/03 GHS Add weight to KASCADE tags. VHDF5KascadeMCOut
//             Add routien VeritasEventWeight to gfiver the weight for n
//             event.
//18/02/04 GHS Upgrade to latest VHDF5 stuff. Add WhippleEventWeight
//04/02/05 GHS In file Veritas.cpp, routine WhippleVHDF5Out add arguments to
//             routine for pedvars and a logical flag that indicates we are to
//             generate pedestal events. Add code to determine when ped 
//             events should be written, create them and write them.
//  29/03/05 GHS I'm going to fixup VHDF5KascadeMCOut and VHDF5Param for the 
//             2d stuff. 
//               1:First, add xo1,yo1 and xo2,yo2 for the 2 possible x,y 
//                 origen of 
//                 event (Lessard elongation method) xo1,yo1 is most 
//                 likly from asymmetry. (Note kasaomega asummetry cal may be 
//                 flawed)
//               2:Add xk1,yk1 and xk2,yk2 from 2d source location from 
//                 Mary's method. 
//               3:Add ra2d1,dec2d1 and ra2d2,dec2d2 for ra and dec of 
//                 xo1,yo1 abd xo2,yo2.(Not calculated in kasaomega as yet)


static VPulse *gPulse;
static bool* gPixelInit;
static float gTimeMin;
static float gTimeMax;
static float gTrigTimeMax;

static double UTCStartTime;
static double UTCTime;
static int NumberPedEvents;

static VHDF5KascadeRunDataFile output;
static VHDF5RunDataFile testInput(kNumberFADCSamples);
const float kWhippleVeritasPedestal=kPedOffset*kFadcPedc;
  
extern "C" void FADCPedWidths(int *nPMTs, float *meanTimeGap,float *activeHv,
			    float *pedWidths);

extern "C" void PePulseVeritas(int *timeIndex, float *times, int *Npmts, 
			       float *timePixel,float *activeHv,  float *disc,
			       float *efficency,float *meanTimeGap, 
			       float *timeTrig,int *NpmtTrigger,
			       float *threshold,int *nTrigger);
extern "C" void PePulseAdc(int *Npmts, int *NpmtTrigger,float *activeHv,
			   float *adc, float *MeanTimeGap, float *TriggerTime);
extern "C" void PePulsePrintParameters();

extern "C" void PePulsePrint(int *Npmts,float *timeTrig,float *pstTrigTime);
extern "C" void PeFadcPrint(int *Npmts,float *timeTrig);
extern "C" void VHDF5Open(char* filename,int*  TeleFlag);
extern "C" void VHDF5PEDSOut(float* pedvars, float* peds, float* pairpedvars,
			     float* gains, float* tubesoff, 
			     int* Flag);
extern "C" void VHDF5Out(int* eventNumber, int* patchTriggerPattern, 
			 int* patchesPST,int *Npmts, float *timeTrig, 
                         int* prototypeFlag);
extern "C" void WhippleVHDF5Out(int* eventNumber, int *Npmts, float* adc,
				bool* PedEventsEnabled, float* pedvars,
				int* pedNumber);

extern "C" void VHDF5KascadeAttributes(float* xseg, float* yseg);

extern "C" void VHDF5KascadeMCOut(int* eventNumber, float* tep, float* type,   
      float* id,       float* xOffset,   float* yOffset, float* x,
      float* y,        float* theta,     float* phi,     float* ithphi,
      float* azwidth,
      float* width,    float* length,    float* dist,    float* miss, 
      float* alpha,    float* size,      float* asym,    float* maxPixels,
      float* bright,   float* boundry,   float* xmean,   float* ymean,
      float* alph,     float* sdevxy,    float* psi,     float* emissionAlt, 
      float* emAltSig, float* muonRatio, float* timeCFD, float* xmpln,
      float* ympln,    float* emaltmpln, float* weight,  float* xo1,
      float* yo1,      float* xo2,       float* yo2,     float* xk1,
      float* yk1,      float* xk2,       float* yk2,     float* ra2d1,
      float* dec2d1,   float* ra2d2,     float* dec2d2);
extern "C" void VHDF5KascadeOutCleanup();
extern "C" void W10mPepulseSize(float* risetime,int* nBins);
extern "C" void W10mPepulseBuild(float* pepulse, float* risetime);

extern "C" void W10mGetRaDecFromVec(double* X, double* ra, double* dec);
extern "C" void W10mGetVecFromRaDec(double* ra, double* dec, double* X);

//Sla lib stuff
extern "C" void sla_dh2e_(double *,double *, double *, double *, double *);
extern "C" double sla_dranrm_(double *);
extern "C" void sla_de2h_(double *,double *, double *, double *, double *);

extern double Rexp(float rate);

void FillWhippleEvent(VHDF5Event& Event, float* adc, int nPMTs, int eventNum);

RandomNumbers ranp("fadc.ran");   //This definintion should happen only once
                                  //in a program.
                                  //All othere classes get ranp with extern 
                                  //declartions of ranp.
//If RANLUX is defined when Random.cpp is compiled then it will use calls to
//the F77 RAnlux package with calls to pran to get its random numbers. If
//RANLUX is defined then "fadc.ran" is a dummy string, not used by 
//RandomNumbers constructer.
                                 
// *************************************************************************
// Pattern class with added prototype methods. Inherits form VPattern
// *************************************************************************
const int prototypePixelAssignment[255]= 
{  0,  1,  2,  3,  6,  7,  8,  9, 10, 16, 17, 18, 19, 20, 21, 22, 23, 24, 33,
  34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 56, 57, 58, 59, 60, 61, 62, 63, 64,
  65, 66, 67, 68, 84, 85, 86, 87, 88, 89,100,118, 90, 91, 92, 93, 94, 95, 96,
  97, 98, 99,119,130,131,132,133,134,135,136,137,159,120,121,122,123,124,125,
 126,127,128,129,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,
 175,176,177,178,179,180,181,205,206,207,208,209,230,258,259,210,211,212,213,
 214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,260,261,262,
 263,264,265,266,267,268,269,270,271,272,273,274,275,276,277,278,279,280,281,
 282,283,284,285,286,317,318,319,320,321,322,323,324,325,326,327,328,329,330,
 331,332,333,334,335,336,337,338,339,340,341,342,343,344,345,346,347,381,382,
 383,384,385,386,387,388,389,410,411,412,390,391,392,393,394,395,396,397,398,
 399,400,401,402,403,404,405,406,407,408,409,413,447,448,449,470,471,450,451,
 452,453,454,455,456,457,458,459,460,461,462,463,464,465,466,467,468,469,490,
 491,492,493,494,495,496,497,498};

class VPatternProto : public VPattern
{
private:
  bool firstPrototype;
public:
  VPatternProto(int nChannels):VPattern(nChannels){};
  void toPrototype();
};

void VPatternProto::toPrototype()
{
  //reshuffles bits according to table.
  VPattern tempPattern(nChannels);
  for(int i=0;i<255;i++)
    {
      int j=prototypePixelAssignment[i]; //Pixel index start at 0
      if(isBitOn(j))
	{
	  tempPattern.setBit(i);
	}
    }
  for(int i=0;i<nWords;i++)
    {
      fPattern[i]=tempPattern.getPatternWord(i);
    }
return;
}



// ***************************************************************************
// FADCPedWidths
// Determined Pedestal widths in FADCs for sky shine noise (meanTimeGap)
//  Used to determine pixels hi/low cleanup values.
// ***************************************************************************
void FADCPedWidths(int *numberPMTs, float *meanTimeGap,float *activeHv,
			   float *pedWidths)
{
  int nPixels=*numberPMTs;
  int nFADCWindows=5000;
  float lengthNs=nFADCWindows*(kFadcBinSize*kFadcSamples);
  bool afterPulse=true;         //Turn on afterpulses in single pe 
                                //pulse height distribution.
  VPulse nPulse;
  nPulse.setLength(lengthNs); //This inits fPulse array(=0) and fPedestal (=0).
  nPulse.setSinglePeRiseTime(kSinglePePulseRise);

	           //Single pe in Fadc is sampled 1/8 of nPulse. So to get area
	           //of single pe in Fadc multiply single pe area by 1/8
  float sArea=nPulse.getSinglePeArea()*kPulseBinSize/kFadcBinSize;

                              //go through pixels
  for(int i=0;i<nPixels;i++)
    {                         
      if(activeHv[i]!=0)         //Pmt is to be used
	{
	  float noiseRate=(1./meanTimeGap[i]); //Noise rate this pixel in 
                                               //pe/ns
	  nPulse.InitPulse();   //zero the fPulse array.
	  nPulse.Noise(noiseRate,afterPulse);  //Load noise
	  float sumadc=0;
	  float sumadc2=0;
	  for(int j=0;j<nFADCWindows;j++)
	    {
	      nPulse.InitFadc();     // Init to make Fadc samples.
	      float startFadc=j*(kFadcBinSize*kFadcSamples);//starting time of
	                                // this FADC window

	      nPulse.Fadc(startFadc);//Converts nPulse fo Fadc samples
	                            //Multiplies by kFadcPedc
                                    //Sets hit flag if above threshold
                                    //Sets hi/lo flag if samples max above 256
                                    //If ho/lo set reduce samples by hi/lo gain
	      //And get Adc value. This is area in DC
	      float adc=0;
	      if(nPulse.getFadcHiLo()) //Remove Hi/lo attenuation
		{ 
		  adc=((float)nPulse.getFadcArea())/kFadcHiGain;
		}
	      else
		{
		  adc=((float)nPulse.getFadcArea());
		}
	      adc=(adc/kFadcPedc)/sArea;
	      sumadc+=adc;
	      sumadc2+=(adc*adc);
	    }
	  float pedMean=sumadc/nFADCWindows;
	  pedWidths[i]=sqrt((sumadc2/nFADCWindows)-(pedMean*pedMean));
	}
      else
	{
	  pedWidths[i]=0.; //This results in pptr%high and pptr&low=0
                           //I think thats used as flag in cleanup.
	}
    }   //do next pixel
  return;
};
 
/****************************************************************************
 * PePulseVeritas
 * Form for each pixel the waveform(in kPulseBinSize steps) that each pixel 
 * will see. Include noise and 'real' pes. Use individual pe timing 
 * information. Also determine Veritas Cfd response and get individual pixel
 * trigger  times.
 * Multiplicity and/or PST trigger.
 ****************************************************************************/
void PePulseVeritas(int *timeIndex, float *times, int *Npmts, float *timePixel,
		    float *activeHv,  float *disc,  float *efficency, 
		    float *meanTimeGap, float *timeTrig,int *NpmtTrigger,
		    float *threshold,int *nTrigger)
{
  //Setup things for the Pulse analysis for KASCADE.
  //First: Load up the PeTime array. Do this so we can use min/max functions on
  //time array.
  //  float adisc;
  int nPes=*timeIndex;
  //  Array<float,1> peTimes;
  //  peTimes.resize(nPes);


  //Second: Determine length of waveform window. Do this in ns.
  //Define the waveform array.
  
  int nPmts=*Npmts;  //Do this to make sure we get value.

  //Initalize pulse declaration. Assume Npmts never changes.
  static bool gFirstTime=true;
  if(gFirstTime)
    {
      gPulse= new VPulse[nPmts];
      for(int i=0;i<nPmts;i++)
	{
	  //Define rise time of single pe pulse shape
	  gPulse[i].setSinglePeRiseTime(kSinglePePulseRise);
	}
      //     gPixelInit.resize(nPmts);
     gPixelInit = new bool[nPmts];
      gFirstTime=false;
    }
  
  //For speed determine length Ns only once for event
  //We need size(in ns) of single pe pulse. Same for all gPulse[].
  //  gTimeMin=min(peTimes)-kPstPulseWidth;
  float peTimes[nPes];
  for(int i=0;i<nPes;i++)peTimes[i]=times[i];
  gTimeMin=peTimes[0]; //Use the first one as the inital.
  for(int i=1;i<nPes;i++)
    {
      if(gTimeMin>peTimes[i])gTimeMin=peTimes[i];
    }
  gTimeMin-=kPstPulseWidth;

  //gTrigTimeMax=max(peTimes)+kVcfdDelay+kVcfdTrigDelay+
  // gPulse[0].getSinglePeSizeNs();
  gTrigTimeMax=peTimes[0];
  for(int i=1;i<nPes;i++)
    {
      if(gTrigTimeMax<peTimes[i])gTrigTimeMax=peTimes[i];
    }
  gTrigTimeMax+=kVcfdDelay+kVcfdTrigDelay+gPulse[0].getSinglePeSizeNs();
  gTimeMax=gTrigTimeMax + kFadcBinSize*kFadcSamples - kAdcDelay +
    kPulseBinSize;

  float lengthNs=gTimeMax-gTimeMin;
                                     
  for(int i=0;i<nPmts;i++)
    {                                //Set the length of the fPulse arrays.
      gPulse[i].setLength(lengthNs);  //This inits fPulse arrays also.
    }

  //Init the pixel flag array.

  for(int i=0;i<nPmts;i++)gPixelInit[i]=false;


  //Go through the pes(that hit pixels). Collect pulses for all pixels at the 
  //same time. Do this for pulse height need.
  for (int i=0;i<nPes;i++)
    {
      int j=int(timePixel[i]-1);              //get pixel id. Remember C++
      // starts at 0 not 1.
      //      if(activeHv[j]==0)  
      if(activeHv[j]<1.0)         //Funy way to test for 0, since the ==0 test
	                          //wont work. I don't know why
	{
	  disc[j]=0;	                   //PMT is dead. Ignore it
	  continue;                        //Do next pmt
	}
      //      float xdummy=0;
      if(ranp.Uniform()>efficency[j])
	{
	  disc[j]=disc[j]-1;               //See if we should drop this pe.
	  continue;
	}

    //We have a good pe. Check wether this is the first one for this pixel
    //Generate noise and remove pedistal. This is VERITAS AC coupling
    //model where instantanious ped is removed.
    
      bool pInit=gPixelInit[j];
      if(!(pInit))
	{
	  float noiseRate=(1./meanTimeGap[j]); //Noise rate this pixel in 
	  //pe/ns
	  bool afterPulse=true;         //Turn on afterpulses in single pe 
                                         //pulse height distribution.
	  gPulse[j].Noise(noiseRate,afterPulse);  //Load noise
	  float acped=gPulse[j].FindPedestal();   //Find ped due to noise.
	  //float ac=gPulse[0].GetPedestal(); //Get the previously found ac 
	  gPulse[j].RemovePedestal(acped);     //Subtract out the ac pedistal.
	  gPixelInit[j]=true;                  //Flag noise done this pixel
	}
      float peTime=peTimes[i]-gTimeMin;	
      bool afterPulse=false;             //Turn off afterpulses in single pe 
                                         //pulse height distribution.
      if(peTime<0)
	{
	  cout<<"peTime:"<<peTime<<endl;
	}
      gPulse[j].AddPe(peTime,afterPulse); //Add in this pe.
    }

  //Init timeTrig to overflow value. 
  //Cover all PMTs's even those not used in trig
  for(int i=0;i<nPmts;i++)
    {
      timeTrig[i]=100001.;
    }

  *nTrigger=0;                //Counts triggered CFD channels
  //  int dntrig=*nTrigger;
  bool realPulse=false;       //Flag that at least one real pulse trigger a
  // CFD
  float startTime=0.;             //start at begining of waveform for CFD.
  int nPmtTrigger=*NpmtTrigger;
  for(int i=0;i<nPmtTrigger;i++) //Look for triggers
    {
      if(disc[i]!=0)
	{
	  //	  gPulse[i].PrintfPulse();
	  float tlevel=threshold[i];
	  timeTrig[i]=gPulse[i].Vcfd(tlevel,startTime,kVcfdOffset);
          if(timeTrig[i]<0)
	    {
	      cout<<"timeTrig[i]:"<<timeTrig[i]<<endl;
	    }
	  if(timeTrig[i]<gTrigTimeMax-gTimeMin)
	    {
	      realPulse=true;
	      *nTrigger=*nTrigger+1;
	      //dntrig=*nTrigger;
	      timeTrig[i]+=gTimeMin;
	      //cout<<i<<":"<<timeTrig[i]<<endl;
	      //gPulse[i].PrintfMainPulse;
	    }
	  else
	    {
	      timeTrig[i]=100001.;
	    }
	}
    }
  //Now do the noise triggers if we have any real hits.
  /***********************************************************************
   * This is a place where we can save time by reducing the length of the*
   *  waveform to only include the trigger(its also more accurate!)
   ***********************************************************************/
  if(realPulse)
    {
      for(int i=0;i<nPmtTrigger;i++)
	{
	  //      if(activeHv[i]==0)  
	  if(activeHv[i]<1.0)         //Funy way to test for 0, since the ==0 
	                              //test wont work. I don't know why
	    {
	      disc[i]=0;
	      continue;                   //Igonre hv off channels.
	    }
	  //	  if(disc[i]==0)
	  if(disc[i]<1)               //Funy way to test for 0, since the ==0 
	                              //test wont work. I don't know why
	    {
	      float noiseRate=(1./meanTimeGap[i]);     //Noise rate this pixel
	      // in pe/ns
	      bool afterPulse=true;       //Turn off afterpulses in single pe 
                                           //pulse height distribution.
	      gPulse[i].Noise(noiseRate,afterPulse);  //Load noise
	      float acped=gPulse[i].FindPedestal();   //Find ped due to noise.
	      gPulse[i].RemovePedestal(acped);  //Subtract out the ac pedistal.
	   //  float ac=gPulse[0].GetPedestal(); //Get the previously found ac 

	      float tlevel=threshold[i];
	      timeTrig[i]=gPulse[i].Vcfd(tlevel,startTime,kVcfdOffset);

	      if(timeTrig[i]<gTrigTimeMax-gTimeMin)
		{
		  *nTrigger=*nTrigger+1;
		  //dntrig=*nTrigger;
		  timeTrig[i]+=gTimeMin;
		}
	      else
		{
		  timeTrig[i]=100001.;
		}
	    }
	}
    }
}

/******************************************************************************
 *PePulseAdc:
 * This routine generates the pulse waveforms for those pixels outside the 
 * trigger area. It determines the start time for the Fadc. It also 
 * produces the summed charge values(like standard Adc) for each pixel for use
 * in Hillas parameter/event analysis code.
 * kPedOffset*kFadcPedc is added as a pedestal to each FADC sample.
 * There is no noise pedestal added (ac coupling assumed) 
 *****************************************************************************/
// Modified:
// 29/05/03 GHS
//          Include check of Hi/Lo flag when determining ADC value.

void PePulseAdc(int *Npmts, int *NpmtTrigger, float *activeHv, float *adc, 
		float *MeanTimeGap, float *TriggerTime)
{
      //From the trigger timeing determine the start time of the Fadc samples.
  float triggerTime=*TriggerTime;
  float startFadc=triggerTime-gTimeMin+kAdcDelay-kVcfdDelay-kVcfdTrigDelay;
                                                    //Adjust to start of pulse
                                                    //array

      //Itereate over pixels outside of trigger area to add noise to their 
      //waveforms
  int nPmts=*Npmts;  //Do this to make sure we get value.
  int nPmtTrigger=*NpmtTrigger;
  if(nPmtTrigger<nPmts)
    {
      for(int i=nPmtTrigger;i<nPmts; i++)
	{
	  bool pInit=gPixelInit[i];
	  if(!(pInit))
	    {
	      if(activeHv[i]!=0)         //Pmt is to be used
		{
		  float noiseRate=(1./MeanTimeGap[i]); //Noise rate this pixel
		  // in pe/ns
		  bool afterPulse=true;    //Turn on afterpulses in single pe 
		  //pulse height distribution.
		  gPulse[i].Noise(noiseRate,afterPulse);  //Load noise
		  float acped=gPulse[i].FindPedestal();//Find ped due to noise
		  gPulse[i].RemovePedestal(acped);//Subtract the ac pedistal.
		  gPixelInit[i]=true;           //Flag noise done this pixel
		}
	    }
	}
    }
       //Iterate through all pixels . Make Fadc samples.
       //Make adc values.(Sum of Fadc) Theoretically this should have no 
       //residual Pedistal except for the noise pedestal.

  //Offset pedestal in digital counts
  float fadcOffsetPedDC;

  for(int i=0;i<nPmts; i++)
    {
      if(activeHv[i]!=0)         //Pmt is to be used
	{
	  gPulse[i].InitFadc();
	  //30/07/03 GHS I'm pretty sure I don't want the ac pedestal in here
	  //float acped=gPulse[i].GetPedestal(); //Get the previously found ac 
          //acped=acped+kPedOffset;           //pedestal and Add in the offset
                                              // pedestal
	  float acped=kPedOffset;              //pedestal 
	  fadcOffsetPedDC=acped*kFadcSamples*kFadcPedc; //Added to 'charge'
	  gPulse[i].AddPedestal(acped);   //Add in the offset pedestal
	  gPulse[i].Fadc(startFadc);//Converts gPulse fo Fadc samples
	                            //Multiplies by kFadcPedc
                                    //Sets hit flag if above threshold
                                    //Sets hi/lo flag if samples max above 256
                                    //If ho/lo set reduce samples by hi/lo gain
	  //Single pe in Fadc is sampled 1/8 of gPulse. So to get area
	  // of single pe in Fadc multiply single pe area in gPulse by 1/8
	  float sArea=gPulse[i].getSinglePeArea()*kPulseBinSize/kFadcBinSize;
	  if(gPulse[i].getFadcHit())
	    {
	      //And get Adc value. This is area in DC
	      if(gPulse[i].getFadcHiLo())
		                    //Remove Hi/lo attenuation
		{ 
		  adc[i]=(float)gPulse[i].getFadcArea()/kFadcHiGain;
		}
	      else
		{
		  adc[i]=((float)gPulse[i].getFadcArea());
		}
	                           //REmove pedestal
	      adc[i]=adc[i]-fadcOffsetPedDC;
	                           //Convert to pe.
	      adc[i]=(adc[i]/kFadcPedc)/sArea;
	    }
	  else
	    {
	      adc[i]=0.;
	    }
	}
      else
	{
	  adc[i]=0.;  //Dead PMTS have no ADC values.
	}
    }
}
// *************************************************************************!

void PePulsePrintParameters()
{
  //Print out the various constats used in determining the FADC stuff.

  cout<<endl<<"!****************************V499 Pixel Camera Parameters**********************"<<endl;
  cout<<"!*******Pulse Parameters:"<<endl;
  cout<<"!                     Pulse bin width(ns):kPulseBinSize: "<<kPulseBinSize<<endl;
  cout<<"!Single Pe rise time at CFD/FADC(ns):kSinglePePulseRise: "<<kSinglePePulseRise<<endl;
  float peSizeNs=gPulse[0].getSinglePeSizeNs();
  cout<<"!                   Single Pe length(ns):SinglePeSizeNs: "<<peSizeNs<<endl;
  float sArea=gPulse[0].getSinglePeArea()*kFadcPedc*kPulseBinSize/kFadcBinSize;
  cout<<"!                            In Fadc Single Pe Area(dc): "<<sArea<<endl;
  cout<<"!"<<endl<<"!*******VCFD Parameters:"<<endl;
  cout<<"!VCFD                  Inverted amplified pulse delay time(ns):kVcfdDelay: "<<kVcfdDelay<<endl;
  cout<<"!VCFD     Gain for Amplified Pulse(inverse is trigger fraction):kVcfdGain: "<<kVcfdGain<<endl;
  cout<<"!VCFD Offset Level summed pulses cross (neg slope) to trigger:kVcfdOffset: "<<kVcfdOffset<<endl;
  cout<<"!"<<endl<<"!*******FADC Parameters:"<<endl;
  cout<<"!+++Timing:"<<endl;
  cout<<"!FADC Gate Timing=>"<<endl;
cout<<"!           Vcfd Trigger Time - kVCfdDelay-kVCfdTrigDelay+kAdcDelay-gTimeMin"<<endl;
  cout<<"!Standard offset to start of FADC data(ns):kAdcDelay: "<<kAdcDelay<<endl;
  cout<<"!"<<endl<<"!+++Pedestal:"<<endl;
  cout<<"!Added FADC pedestal(dc)(to make positive)=>"<<endl;
cout<<"                               kPedOffset*kFadcSamples*kFadcPedc"<<endl;
  cout<<"!Offset added to each sample channel(pe's):kPedOffset: "<<kPedOffset<<endl;
  cout<<"!        Number of samples per FADC gate:kFadcSamples: "<<kFadcSamples<<endl;
  cout<<"!    Pe to digital counts conversion factor:kFadcPedc: "<<kFadcPedc<<endl;
  cout<<"!"<<endl<<"!+++Sampling:"<<endl;
  cout<<"!Number pulse bins to ave for each sample channel:kFadcSampleBins: "<<kFadcSampleBins<<endl;
  cout<<"!     Hit Threshold(does not include pedestal)(pes):kHitThreshold: "<<kHitThreshold<<endl;
  cout<<"!              FADC Hi Range reduction over Low range:kFadcHiGain: "<<kFadcHiGain<<endl;
  if(kFadcHitThreshold==0)
    {
      cout<<"!                      No Zero Supression since kFadcHitThreshold: 0"<<endl;
    }
  cout<<"!*****************************************************************************"<<endl<<endl;
  return;
}

// *************************************************************************


void PePulsePrint(int *Npmts,float *timeTrig,float *pstTrigTime)
{
  int nPmts=*Npmts;
  for (int i=0;i<nPmts; i++)
    {
      if(timeTrig[i]<100000)
	{
	  float pulseTrigTime=timeTrig[i]-gTimeMin;
	  float pstTime=*pstTrigTime-gTimeMin+kAdcDelay-kVcfdDelay-
	    kVcfdTrigDelay;
	  if(abs(*pstTrigTime-gTimeMin-pulseTrigTime)<10.0)
	    {
	      // cout<<i<<":"<<pulseTrigTime<<','<<pstTime<<endl;
	      gPulse[i].PrintfPulse(pstTime);
	    }
	}
    }
}
void PeFadcPrint(int *Npmts,float *timeTrig)
{
  int nPmts=*Npmts;
  for (int i=0;i<nPmts; i++)
    {
      if(timeTrig[i]<100000)
	{
	  gPulse[i].PrintFadc();
	}
    }
}

// ************************************************************************


void VHDF5Open(char* filename,int* TeleFlag)
// ************************************************************************
//    Prepare for writting to a new VHDF5 run data file
// ************************************************************************
//    This works for both Whipple and VERITAS simulations. GHS 08/03/04 
// ************************************************************************
{
  //Open the file
  output.createFile(filename); //Creates the ouput file.
   //Define stuff
  VHDF5InfoHeader* iHead = new VHDF5InfoHeader();
  vector< VHDF5TelescopeInfo > tInfo;          //Need an array of these.

  //Now fill up the info record using the defult test data methods
  //Fill up standard stuff using the createTestInfo(called by readInfoRec
  //after initTestInput set flag to do so).
  //(Just being lazy here)
  testInput.initTestInput();
  testInput.readInfoRec(iHead,tInfo);//Among others things sets UTCSTart,
                                     //UTCTime,expected duration, actual 
                                     //duration,UTCEnd
  //correct a few things
  int nTel=1;
  iHead->nTelescopes=nTel;   //Single telescope operation
  tInfo.resize(iHead->nTelescopes); //This just shrinks us down. The test data
                                    //should already be there.
  if(*TeleFlag==1)
    {
      iHead->runNumber=70000;   //Whipple Simulated(MC) run number.
      tInfo[0].nTrigger=331;    }
  else if(*TeleFlag==2)
    {
      iHead->runNumber=527;   //Veritas Simulated run number.
    }
  else
    {
      iHead->runNumber=528;   //Unknown Simulated run number.
    }

  output.writeInfoRec(iHead,tInfo);
  
  //Save a few things 
  // ***********************************************************************
  //  UTCStartTime  
  //  We could define a different start time here . By defualt 
  //  (VHDF5RunDataFile::initTestInput) we use UTC=100days,2hrs,3min,.00045 sec
  // ***********************************************************************
  UTCStartTime=iHead->UTCStart;
  UTCTime=UTCStartTime;
  NumberPedEvents=0;
  delete iHead;
  return;
}

// ************************************************************************


void VHDF5PEDSOut(float* pedvars, float* peds, float* pairpedvars, 
		  float* gains, float* tubesoff,int* Flag)
{
// ************************************************************************
//  Write out this PEDS dataset in VHDF5 file.
// ************************************************************************
//  This works for both Whipple and VERITAS data. GHS 08/03/04 
//  Only questionalble thing was the use of kPedOffsetand kFadcPedc for the
//  Whipple peds calculation but it should be ok to use that for Whipple also.
// ************************************************************************
//Write out PEDS record(peds,pedvars,gains,pairpedvars,tubesoff
  //Used mostly for Display but maybe infuture used during analysis.
  //Defined for that reason in VHDF5RunDataFile class.

  //Fill in pedestal here. 
  // VERITAS: In PePulseADC we added kPedOffset*kFadcPedc to each sample of 
  // the FADC.
  //There is no ped from the noise (ac coupling assumed)

  for(int i=0;i<kMaxNumberOfChannels;i++)
    {
      peds[i]=kWhippleVeritasPedestal;   //Works for both Whipple and VERITAS
      if(*Flag!=0)
	{ //Only done for non Whipple
	  pedvars[i]=pedvars[i]*kFadcPedc;
	  pairpedvars[i]=pairpedvars[i]*kFadcPedc;
	}
    }

  //If this is the prototype(499->250) pixels remap the data.
  if(*Flag==1)
    {
      float* prtPedvars = new float[kMaxNumberOfChannels];
      float* prtPeds = new float[kMaxNumberOfChannels];
      float* prtPairPedvars = new float[kMaxNumberOfChannels];
      float* prtGains = new float[kMaxNumberOfChannels];
      float* prtTubesOff = new float[kMaxNumberOfChannels];
      for(int i=0;i<255;i++)
	{
	  int channelSrc=prototypePixelAssignment[i];//Pixel index start at 0
	  prtPedvars[i]=pedvars[channelSrc];
	  prtPeds[i]=peds[channelSrc];
	  prtPairPedvars[i]=pairpedvars[channelSrc];
	  prtGains[i]=gains[channelSrc];
	  prtTubesOff[i]=tubesoff[channelSrc];
	}
      output.writePEDS(0,prtPedvars);
      output.writePEDS(1,prtPeds);
      output.writePEDS(2,prtPairPedvars);
      output.writePEDS(3,prtGains);
      output.writePEDS(4,prtTubesOff);
      delete []prtPedvars;
      delete []prtPeds;
      delete []prtPairPedvars;
      delete []prtGains;
      delete []prtTubesOff;
    }
  else //Whipple/non prototype
    {
      output.writePEDS(0,pedvars);
      output.writePEDS(1,peds);
      output.writePEDS(2,pairpedvars);
      output.writePEDS(3,gains);
      output.writePEDS(4,tubesoff);
    }

  return;
}

// ************************************************************************

void VHDF5Out( int* eventNumber, int* patchTriggerPattern, int* patchesPST,
	       int *Npmts, float *timeTrig, int* prototypeFlag)
// ************************************************************************
//  Write out this event as a Varray event for a single telescope.
// ************************************************************************
//To add: Pedestal events. If enabled (add input argument: bool* peds)On every 
//GPS  second mark, generate a pedestal event.(How done? Can't tun off zero 
//supression!)

{

   int nPMTs=*Npmts;
  //Get index of next event to be written out.
  int eventNum=output.getNextIndex();
  *eventNumber=eventNum;  //Give back to calling routine.

   //Define the VHDF5 Event.
  VHDF5Event Event;
  // **************************
  //Now fill in the event. This steals alot from the 
  //VHDF5RunDataFill.cpp::createTestEvent method.

  Event.setTriggerMask(01);//octal(the leading 0 specifies that)
                     //lowest 1 bit set means this trigger 
                     //has data for only the first telescope.
  Event.setEventNumber(eventNum);  //Put into event


  double triggerRate=30.0;  //Psuedo trigger rate. units= Hz(= sec**-1)
  UTCTime=UTCTime+(double)Rexp(triggerRate);
    unsigned short GPSTim[5];
  VUTCGPS::UTC2GPS(UTCTime,GPSTim);
  Event.setTrigGPSTime(GPSTim);

  // **************************
  Event.setEventType(1);           //event type (normal)   
  Event.setTrigStatusFlags(0);     //Status bits for event.
  Event.setConfigurationMask(01);  //1 bit for telescope in sub array
  Event.setNumberTelescopes(1);   //Number in this sub array.
  int nTel=1;
  int iTel=nTel-1;
  Event.setNumberTelescopesData((unsigned char)nTel);//Number of telescopes 
                                                     //with data

  // **************************
  //Telescope data
  Event.deallocateAllVL();
  int nClkTrigBrd=testInput.getNumberOfClockTriggerBoards();
  int nSamp=testInput.getNumberOfFADCSamples();
  Event.allocateTelescopes(nTel);
  Event.allocateClockTriggerBoardData(iTel,nClkTrigBrd);

  int nChan=0;
  for(int i=0;i<nPMTs;i++)
    {
      if(gPulse[i].getFadcHit())
	{
	  nChan++;
	}
    }
  Event.allocateFADCData(iTel,nChan);
      //We are being tricky here. We would like to make an array of 
      //FDCSample arrays but we can't since C++ does not allow us to declare 
      //and array length dynamically. HDF5 just expects a pointer here to an 
      //array of arrays of data. 
  Event.allocateFADCSamples(iTel,nChan,nSamp); //Also sets FADCNumberSamples




  // **************************
  Event.setNodeID(iTel,0);                        //Telescope ID
  Event.setRightAscention(iTel,5*60*60.+34*60+32);//RA Crab(seconds)
  Event.setDeclination(iTel,(32-20)*60*60.);      //DEC Crab (seconds)
  Event.setTDCCounts(iTel,100);          //Relative time trigger in TDC counts
  Event.setDelay(iTel,15);               //Trig Dynamic delay correction 
  Event.setCFDScalerRate(iTel,20);                //Trigger rate of this 
  Event.setTenMHzClock(iTel,(int)((UTCTime-UTCStartTime)*10e6));
                                         //Elasped time scaler this telescope.
  Event.setVetoedClock(iTel,
		       (int)((UTCTime-UTCStartTime)*(10e6)-(.01*eventNum)));
                                         //Livetime scaler this telescope.
  // **************************
  //Load up PST patches hit pattern.(Ignore resuffling to macth PST for 
  //prototype. I don't know how its done,need another translation table
  //I guess).
  int numPSTPatches=*patchesPST;
  VPattern PSTPtrn(numPSTPatches); //Init patterns to 0
  for(int i=0;i<numPSTPatches;i++)
    {
      if(patchTriggerPattern[i]!=0)    //This patch fired. Set Bit.
	{
	  PSTPtrn.setBit(i);
	}

    }
   unsigned int PSTPat[3];         //PST trigger pattern bits. 
   for (int i=0;i<3;i++)
     {
       PSTPat[i]=PSTPtrn.getPatternWord(i);
     } 
   Event.setPSTPattern(iTel,PSTPat);
  
  // **************************
   VUTCGPS::UTC2GPS(UTCTime,GPSTim);
   Event.setTelescopeGPSTime(iTel,GPSTim);
   Event.setTelescopeStatusFlags(iTel,0);     //Status flags for this event.
   Event.setNumberChannels(iTel,nChan);//Number of channels read out
   Event.setFADCNumberSamples(iTel,nSamp);//Also done in allocateFADCSamples 
                                          //above
  // **************************
  //now we set up to get all patterns and  count the number of hit
  //FADC channels. SEt up for possibility this is the prototype
  //in which case we need to reorder things. patterns and FADC.
  //Don't do PST 'cause I don't know how.

  VPatternProto hitPtrn(nPMTs);
  VPatternProto CFDPtrn(nPMTs);
  for(int i=0;i<nPMTs;i++)
    {
      if(gPulse[i].getFadcHit())
	{
	  hitPtrn.setBit(i);
	}
      if(timeTrig[i]<100000.)
	{
	  CFDPtrn.setBit(i);
	}
    }

  //If this is the prototype, rearrainge hitPtrn and CFDPtrn
  static bool firstPrototype=true;
  if(*prototypeFlag==1)
    { 
      if(firstPrototype)
	{
	  cout<<"Prototype Pixel assignments being used in VHDF5 ouput file."
	      <<endl;
	  firstPrototype=false;
	}
      hitPtrn.toPrototype(); //This puts all hits bits from 
      CFDPtrn.toPrototype(); //0-255.
    }

  Event.allocateHitPattern(iTel,hitPtrn.getSize());
  Event.setHitPattern(iTel,hitPtrn.getSize(),hitPtrn);
  Event.allocateCFDPattern(iTel,CFDPtrn.getSize());
  Event.setCFDPattern(iTel,CFDPtrn.getSize(),CFDPtrn);

  // *******************
  //This clock stuff is all fake
  unsigned long clkDat[kNumberWordsClkTrigBoard]={1,2,3,4,5,6,7};
  for(int j=0;j<nClkTrigBrd;j++)
    {
      Event.setClockTriggerBoardData(iTel,j,clkDat);
    }  
 
 // ************************
  //Now the FADC Sample arrays.



  int j=0;  //Index for hit channels
  int channelSrc;
  for(int i=0;i<nPMTs;i++)
    {
      if(hitPtrn.isBitOn(i))  //If this is prototype we are using the
                              //rearrainged hit patterns
	{
	  if(*prototypeFlag==1)
	    {
	      channelSrc=prototypePixelAssignment[i];//Pixel index start at 0
	    }
	  else
	    {
	      channelSrc=i;
	    }
	  Event.setFADCChannelID(iTel,j,i);
	  //No pedistal in data for now. Just set HiLo bit
	  unsigned short ped=(1 << 15);
	  if(gPulse[channelSrc].getFadcHiLo())
	    {  //pedestalAndHiLo is unsigned SHORT!
	      Event.setFADCPedestalAndHiLow(iTel,j,ped);
	    }
	  else
	    {
	      Event.setFADCPedestalAndHiLow(iTel,j,0);
	    }
	  //Set 'charge'  Includes whatever pedestal etc.
	  int charge=gPulse[channelSrc].getFadcArea();
	  Event.setFADCCharge(iTel,j,charge); 
  // ************************
      //Fill the samples
	  unsigned char* samples=Event.getFADCSamplesPointer(iTel,j,nSamp);
	  for(int k=0;k<nSamp;k++)
	    {
	      samples[k]= gPulse[channelSrc].getFadcSample(k);
	    }
	  j++; //Bump output channel index.
	}
    }
  //Event is now filled. Write it out.
  output.writeEvent(Event);
  return;
}

// **************************************************************************


void WhippleVHDF5Out(int* eventNumber, int *Npmts, float* adc,
		     bool* PedEventsEnabled, float* pedvars, int* pedNumber)
// ************************************************************************
//  Write out this Whipple event  as a VHDF5 event
// ************************************************************************
//Modified:
//04/02/05 GHS Add arguments toroutine for pedvars and a logical flag that 
//             indicates we are to generate pedestal events.
//             Add code to generate and write a pedestal event when we cross
//             the second mark. (=> pedestal events once per second)

{
   //Define the VHDF5 Event.
  VHDF5Event Event;

  float triggerRate=24.0;  //Psuedo trigger rate. units= Hz(= /sec)
  int nPMTs=*Npmts;
  double UTCOld=UTCTime;
  UTCTime=UTCTime+(double)Rexp(triggerRate);
  if(*PedEventsEnabled)
    {
      int j = (int)UTCTime-(int)UTCOld; // Number of pedestal events we need  
                                        // to generate
      if(j>0)
	{
	  float pedadc[nPMTs];
	  for(int k=0;k<j;k++)
	    {
	      for(int i=0;i<nPMTs;i++)
		{
		  pedadc[i]=pedvars[i]*ranp.Normal(); //Pedestal added later
		}
	      
	      int eventNum=output.getNextIndex();
  	      FillWhippleEvent(Event, pedadc, nPMTs, eventNum);
	      Event.setEventType(2);       //Assume this is flag for ped event
	      output.writeEvent(Event);
	      NumberPedEvents++;
	    }
	}
    }
  //Now write out the event itself

  //Get index of next event to be written out(which will be index of last 
  //event when we write this one).
  int eventNum=output.getNextIndex();
  
  FillWhippleEvent(Event,adc,nPMTs,eventNum);

  //Event is now filled. Write it out.
  output.writeEvent(Event);

  *eventNumber=eventNum;  //Give back to calling routine.
  *pedNumber=NumberPedEvents;
return;
}

// **************************************************************************


void VHDF5KascadeAttributes(float* xseg, float* yseg)
// **************************************************************************
//  Add xseg and yseg as an attribute to the KASCADE DataSet;
// **************************************************************************
//Modified:
// 20/10/04 GHS Add ithphi to everything
{
  const int aLength=2;   //Length of the attribute array to write.
  float attribute[aLength];
  attribute[0]=*xseg;
  attribute[1]=*yseg;
  output.writeKASCADEAttribute(attribute,aLength);//VHDF5Kascade method for now
  return;
}

// **************************************************************************

void VHDF5KascadeMCOut(int* eventNumber, float* tep, float* type,   
      float* id,       float* xOffset,   float* yOffset, float* x,
      float* y,        float* theta,     float* phi,     float* ithphi,
      float* azwidth,
      float* width,    float* length,    float* dist,    float* miss, 
      float* alpha,    float* size,      float* asym,    float* maxPixels,
      float* bright,   float* boundry,   float* xmean,   float* ymean,
      float* alph,     float* sdevxy,    float* psi,     float* emissionAlt, 
      float* emAltSig, float* muonRatio, float* timeCFD, float* xmpln,
      float* ympln,    float* emaltmpln, float* weight,  float* xo1,
      float* yo1,      float* xo2,       float* yo2,     float* xk1,
      float* yk1,      float* xk2,       float* yk2,     float* ra2d1,
      float* dec2d1,   float* ra2d2,     float* dec2d2)
// **************************************************************************
// Writes out the KASCADE MonteCarlo tags for event eventNumber 
// **************************************************************************
//Modifed:
//
//21/07/03 GHS Add weight to KASCADE tags.
//18/02/04 GHS Update to latest VHDF5.
// **************************************************************************

{
  //Now fill up the structures.
  //Param data first
  VHDF5Param paramEvent;
  paramEvent.azwidth=*azwidth;
  paramEvent.width=*width;
  paramEvent.length=*length;
  paramEvent.dist=*dist;
  paramEvent.miss=*miss;
  paramEvent.alpha=*alpha;
  paramEvent.size=*size;
  paramEvent.asym=*asym;
  for(int i=0;i<8;i++)
    {
      paramEvent.maxPixels[i]=maxPixels[i];
    }
  paramEvent.bright=*bright;
  paramEvent.boundry=*boundry;
  paramEvent.xmean=*xmean;
  paramEvent.ymean=*ymean;
  paramEvent.alph=*alph;
  paramEvent.sdevxy=*sdevxy;
  paramEvent.psi=*psi;
  paramEvent.xo1=*xo1;
  paramEvent.yo1=*yo1;
  paramEvent.xo2=*xo2;
  paramEvent.yo2=*yo2;
  paramEvent.xk1=*xk1;
  paramEvent.yk1=*yk1;
  paramEvent.xk2=*xk2;
  paramEvent.yk2=*yk2;
  paramEvent.ra2d1=*ra2d1;
  paramEvent.dec2d1=*dec2d1;
  paramEvent.ra2d2=*ra2d2;
  paramEvent.dec2d2=*dec2d2;
  
 //Now Kascade tags
  VHDF5Kascade kascadeEvent;

  kascadeEvent.tep=*tep;
  kascadeEvent.type=*type;
  kascadeEvent.id=*id;
  kascadeEvent.xOffset=*xOffset;
  kascadeEvent.yOffset=*yOffset;
  kascadeEvent.x=*x;
  kascadeEvent.y=*y;
  kascadeEvent.theta=*theta;
  kascadeEvent.phi=*phi;
  kascadeEvent.ithphi=*ithphi;
  kascadeEvent.emissionAlt=*emissionAlt;
  kascadeEvent.emAltSig=*emAltSig;
  kascadeEvent.muonRatio=*muonRatio;
  kascadeEvent.timeCFD=*timeCFD;
  kascadeEvent.xmpln=*xmpln;
  kascadeEvent.ympln=*ympln;
  kascadeEvent.emaltmpln=*emaltmpln;
  //Adding 'weight' to tags. This weight is the normalized weight each event 
  //would have for a crab spectrum(E**alpha)*deltaE(E)/nshowers(E))
  //Max weight is 1.0
  kascadeEvent.weight=*weight;

  //And write it out
  int eventNum=*eventNumber;
  output.writeParam(eventNum,paramEvent);
  output.writeKascade(eventNum,kascadeEvent);
  return;
}
// **************************************************************************

void VHDF5KascadeOutCleanup()
  // **************************************************************************
  //Cleanup after writing out the Vevent simulated data file.
  // **************************************************************************
{
  output.closeFile();
  testInput.closeFile();
  return;
}
// **************************************************************************

void  W10mPepulseSize(float* risetime, int* nBins)
  // **************************************************************************
  //Create a single pepulse with the requested risetime. Determine its size
  // **************************************************************************
  //GHS 29-march-2004
{
  float rTime=*risetime; 
  VSinglePe  singlePepulse(rTime);
  *nBins =int(singlePepulse.getSize()/kPulseBinSize); //Number of bins
  return;
}
// **************************************************************************

void W10mPepulseBuild(float* pePulse, float* risetime)
  // **************************************************************************
  //Create a single pepulse with the requested risetime. 
  // **************************************************************************
  //GHS 29-march-2004
{
  float rTime=*risetime; 
  VSinglePe  singlePepulse(rTime);
  int nBins =int(singlePepulse.getSize()/kPulseBinSize); //Number of bins

  for(int i=0;i<nBins; i++)
    {
      pePulse[i]=singlePepulse.fSingPulse[i]; 
    }
  return;
}

// **************************************************************************


void W10mGetRaDecFromVec(double* X, double* ra, double* dec)
  // **************************************************************************
  //   Get the Ra and Dec of a vector X at sideraltime=12:00(chosen arbitray 
  //   time)
  // **************************************************************************
{
  double pi=3.141592654;
  double elevation=pi/2-(acos(abs(X[2])));
  double az;
  if(X[1]==0 && X[0]==0)
    {      //At zenith
      az=0.0;
    }
  else if(X[1]==0 && X[0]>0)    //along + x axis  (270 deg)
    {
      az=3*pi/2;
    }
  else if(X[1]==0 && X[0]<0)    //along - x axis (90 deg)
    {
      az=pi/2;
    }
  else if(X[1]>0 && X[0]<=0.0)     //Quadrant 1 (0 to 90 deg)
    {
      az=-atan(X[0]/X[1]);
    }
  else if(X[1]<0 && X[0]<=0.0)     //Quadrant 2 (90 to 180 deg)
    {
      az=pi/2+atan(X[0]/X[1]);
    }
  else if(X[1]<0 && X[0]>=.0)      //Quadrant 3 (180 to 270 deg)
    {
      az=pi-atan(X[0]/X[1]);
    }
  else if(X[1]>0 && X[0]>0.0)       //Quadrant 4 (270 to 360 deg)
    {
      az=2*pi-atan(X[0]/X[1]);
    }

  //Determine ra and dec from az,elev  
  double hourangle;
  double decd;

  //Convert az and elevation to hourangle and dec    
  double latitude = WHIPLAT;
  sla_dh2e_(&az, &elevation, &latitude, &hourangle, &decd); 
  //cout<<"hourangle,decd,latitude,az,elevation: "<<hourangle<<","<<
  //                 decd<<","<<latitude<<","<<az<<","<<elevation<<endl; 

  //Convert hour angle back to ra
  double rad=((pi/2)-hourangle); //Assumes sideraltime is 6:00 
  *ra= sla_dranrm_(&rad);  
  *dec= decd;
  //cout<<"ra,dec, (1,1): "<<*ra<<","<<*dec<<endl;
  return;
}

void W10mGetVecFromRaDec(double* ra, double* dec, double* X)
// **************************************************************************
//   Using an  Ra and Dec and a sideral time of 12:00 get position vector 
// **************************************************************************
{
  double pi=3.141592654;
  double elevation, azimuth;
  double latitude = WHIPLAT;
  double decd=*dec;
  double rad= *ra;
  double hourangle = (pi/2) - rad; //sidereal time is 6:00

  sla_de2h_(&hourangle,&decd,&latitude,&azimuth,&elevation);
  //cout<<"rad,hourangle,decd,latitude,azimuth,elevation: "<<rad<<","<<
  //  hourangle<<","<<
  //  decd<<","<<latitude<<","<<azimuth<<","<<elevation<<endl;
// convert to a vector
  X[2] = -abs(sin(elevation));
  double length=sqrt(1-X[2]*X[2]);
  if(length==0)
    {
      X[0]=0;
      X[1]=0;
    }
  else
    {
      X[0]= -sin(azimuth)*length;
      X[1]= cos(azimuth)*length;
    }
  return;
}

void FillWhippleEvent(VHDF5Event& Event, float* adc, int nPMTs, int eventNum)
{
  // **************************
  //Now fill in the event. This steals alot from the 
  //VHDF5RunDataFill.cpp::createTestEvent method.
  int nTel=1;
  int nChan=492;
  unsigned int eNum=eventNum;
  int nClkTrigBrd=testInput.getNumberOfClockTriggerBoards();
  int nPatWrd=(kMaxNumberOfChannels+31)/32;
  int nSamp=testInput.getNumberOfFADCSamples();

  Event.createTestEvent(nTel,nChan,eNum,UTCTime,UTCStartTime,
			   nClkTrigBrd,nPatWrd,nSamp,
			   kMaxNumberOfChannels);

  //Now the ADC Data (no FADC Samples for now)
  for(int i=0;i<nPMTs;i++)
    {
      Event.setFADCChannelID(0,i,i);
	  //No pedistal in data for now. Just set HiLo bit to low
      Event.setFADCPedestalAndHiLow(0,i,0);
      //Set 'charge'  to ADC value. Add in a pedestal 
      //(ADC[i] may be negative from noise fluxtuations).
      int charge=(int)(adc[i]+kWhippleVeritasPedestal);
      if(charge<0)
	{
	  charge=1;
	}
      Event.setFADCCharge(0,i,charge); 
    }
  return;
}

