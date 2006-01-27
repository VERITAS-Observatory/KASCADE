#include "Random.h"
#include <cmath>
#include "VPulse.h"
#include <iostream>



extern RandomNumbers ranp;

double Rexp(float rate)
{
  /*
    This function randomly picks from an exponatial distribution using
    rate as the scale factor.
    Rexp is distributed as exp(-q/rate)
  */
	
  
  //       This produces the required random distribution.
	double r = ranp.Exponential()/rate;
	return r;
}


               //VSinglePe class Method defns.

 void  VSinglePe::setRiseTime(float single_pulse_rise)
{
  /*
    This method builds the single pe pulse shape. Max value is normalized 
    to 1. Scale the size of the pulse to the risetime using interpolation.
    Inital pulse has 2.0 ns risetime.
  */
  float base_rise=2.0;  //ns
  const int base_size=67;
  //  Array<float,1> base_pulse(base_size);
  //base_pulse=
 static float base_pulse[base_size]=
    {
      0.0,  .011,  .022,  .033,  .044,  .067,  .089,   .15,  .22,  
    .34,   .46,   .58,   .69,   .80,   .91,   .96,   1.0,  .99,  
    .98,   .97,   .96,   .94,   .91,   .89,   .86,   .81,  .78,  
    .76,   .73,   .69,   .66,   .63,   .60,   .56,   .51,  .47,  
    .43,   .39,   .37,   .36,   .33,   .31,   .30,   .28,  .27,  
    .26,   .24,   .22,   .21,   .20,   .18,   .17,   .15,  .13,  
    .12,   .12,   .11,  .089,  .078,  .060,  .044,  .038, .033, 
    .022, .011,  .005,   0.0
    };
  float ratio=(single_pulse_rise/base_rise);    //Ratio of pulse widths.
  int size=int(ratio*base_size);
  if(size<1)
    {
      cout<<"Size is too small.size:"<<size<<endl;
      exit(1);
    }

  //  fSingPulse.resize(size);
  fSingPulse=new float[size];
  if(size==base_size)
    {                                   //No change from base.
      //      for(int i=0;i<size;i++)fSingPulse(i)=base_pulse(i);
      for(int i=0;i<size;i++)fSingPulse[i]=base_pulse[i];
    }
  else
    {
      fSingPulse[0]=base_pulse[0];    //Starts at same place
	for (int j=1;j<size-1; j++)    //different from base: interpolate.
	  {
	    float aindex=(j/ratio);    // fractional index within base_pulse.
	    int k=int(aindex);

	    if(k>base_size-2)
	      {
		cout<<"Index out of range for base.k:"<<k<<endl;
		exit(1);
	      }
	    fSingPulse[j]=base_pulse[k]+
	      (aindex-float(k))*(base_pulse[k+1]-base_pulse[k]);
	  }
	fSingPulse[size-1]=base_pulse[base_size-1];  //ends at same place.
    }
    fSize=size*kPulseBinSize;
    fArea=0;
    for(int i=0;i<size;i++)fArea+=fSingPulse[i];
}

VSinglePe::VSinglePe()
{
  setRiseTime(2.0);
}

VSinglePe::VSinglePe(float singlePulseRise)
{
  setRiseTime(singlePulseRise);
}


float Pulse_height(bool after_pulse)
{
//	Models Chucks measurment of single pe pulse height distribution	for
//	a r1398 pmt.
//	Mean of distribution is set to 1.0 (chucks channel 139)
//	The distribution is modeled in two parts. 
//	Area 1:Below height=.68 (channel 95) down to .158 its constant.0 below
//	       .158 (channel 22)
//	Area 2:Above .68 its a gaussian, with a mean of 1.09(channel 152) and
//		a sigma of .2865 (40 channels)
//	Area 1 is .2865 of the total area under the curve. Throw over that
//		to pick our areas.
//	Written by:	
//		Glenn Sembroski
//		Purdue Univ.

//	Modified:

//	28/9/98 GHS
//		Add a gaussian for after pulsing.
//		Choose some percent of the time (AFTER_PULSE_FRAC) to geneate
//		our pulse height value from the afterpulse spectrum. Assume
//		this spectrum is gaussian with mean AFTER_PULSE_MEAN and width
//		AFTER_PULSE_SIGMA.
//	25/2/99 GHS
//		Add this after pulseing version to w10m_pepulse file. Add a
//		flag, AFTER_PULSE,  to indicate whether to add after pulseing
//		or not. Use this to turn off afterpulses when we have true
//		shower pe's. Afterpulses only show up in noise


const float kSinglePeArea1=.08;	  //probability in area1
const float kSinglePeMin=.462;	  //lowest non 0 value
const float kSinglePeEdge=.62; 	  //dividing point area1->area2
const float kSinglePeGaussMean=1.0;    //area2 gaussian mean
const float kSinglePeGaussSigma=.275;  //area2 gaussian width
const float kAfterPulseFrac=3.e-4;      //Fraction of afterpulses
const float kAfterPulseMean=2.0;        //Mean of afterpulses
const float kAfterPulseSigma=6.0;       //Width of afterpulse spectrum
	
                             	//see if this is an after pulse.
  if(after_pulse)
    {
      double y=ranp.Uniform();
      if(y<kAfterPulseFrac)
	{
	  for(;;)
	    {
	      float height = ranp.Normal()*kAfterPulseSigma + 
		kAfterPulseMean;
	      if(height>kSinglePeEdge) 
		{
		  return height;
		}
	    }
	}
    }
  
  //	If we are not to make afterpulses or if this pulse wasn't chosen as
  //	and afterpulse, pick from standard distribution.
  //	Find which area we are in.

  float y =ranp.Uniform();
  if(y<kSinglePeArea1)
    {
      //Area 1
      float height = ranp.Uniform()*(kSinglePeEdge-kSinglePeMin) + 
	kSinglePeMin;
      return height;
	}
    else
      {
	// Area 2: Pick from a gaussian with mean=0 and sigma=1
	//modify result to mean=kSinglePeGaussMean and 
	//sigma=kSinglePeGaussSigma
	//if result is below .68 try again.
	for(;;)
	  {
	    float height=ranp.Normal()*kSinglePeGaussSigma +
	      kSinglePeGaussMean;
	    if(height>kSinglePeEdge)
	      {
		return height;
	      }
	  }
      }
}


//VPulse::VPulse()  Default constructor, no lenght. Use setLenght to do that.
VPulse::VPulse()                        
{
  fNbins=1;
  fFadcPulse = new (unsigned char)[kFadcSamples];
  for(int i=0;i<kFadcSamples;i++) fFadcPulse[i]=0;
  fFadcHiLo=false;
  fFadcHit=false;
  fSinglePe.setRiseTime(2.0);
  fSinglePeSizeNs=fSinglePe.getSize();
  fSinglePeSizeBins=int(fSinglePeSizeNs/kPulseBinSize); //number of bins
  fSinglePeArea=fSinglePe.getArea();
  fPedestal=0;
  fPulse=0;  //This flags that we shouldn't try tp delete this guy if memory 
             //not allocated to it through new yet.
  fMainPulse=0;
  fDpulse=0;
  fNPulse=0;
  fNFadc=0;
}
float VPulse::getSinglePeSizeNs()
{
  return fSinglePeSizeNs;
}


void VPulse::setSinglePeRiseTime(float riseTime)
{
  fSinglePe.setRiseTime(riseTime);
  fSinglePeSizeNs=fSinglePe.getSize();
  fSinglePeSizeBins=int(fSinglePeSizeNs/kPulseBinSize); //number of bins
  fSinglePeArea=fSinglePe.getArea();
}


void VPulse::setLength(float lenNs)
{
  fNbins=int(lenNs/kPulseBinSize)+1;   // Get number of bins in waveform
                                        // array
  if(fNbins<1)
    {
      cout<<"fNbins is too small.fNbins:"<<fNbins<<endl;
      exit(1);
    }
  //  fPulse.resize(fNbins);
  if(fPulse)delete[] fPulse; //in constructor fPulse set to 0.(false). Don't 
                             //delete unless its be allocated memory by the 
                             //following new.
  fPulse = new float[fNbins];
  for(int i=0;i<fNbins;i++)
    {
      fPulse[i]=0.0;
    }
  fPedestal=0.0;
}

void VPulse::InitPulse()
{
  for (int i=0;i<fNbins;i++)
    {
      fPulse[i]=0.0;
    }
  fPedestal=0.0;
}

void VPulse::InitFadc()
{
  for(int i=0;i<kFadcSamples;i++) fFadcPulse[i]=0;
  fFadcHiLo=false;
  fFadcHit=false;
}

void VPulse::AddPe(float time_ns, bool after_pulse)
{                                     // Add a pe to the waveform array 
                                      //starting at time_ns
int pulse_index=int(time_ns/kPulseBinSize); 
                                     // Position in pulse array of start of pe
    if(pulse_index<(-(fSinglePeSizeBins-1))||pulse_index>(fNbins-1)) return;
                                      //Make sure at least part of pulse is 
                                      //within the waveform array.

  int jstart_pe=0;                      //Starting point of portion of single
                                        //pe we will load
  int jend_pe=fSinglePeSizeBins-1;           //End point.

  if(pulse_index<0)                //See if starting point is before waveform
  {
    jstart_pe=-pulse_index;
    pulse_index=0;
  }
  else
    if(pulse_index+fSinglePeSizeBins-1>fNbins-1)
    {
      jend_pe=fNbins-pulse_index-1;
    }

  float p_height=Pulse_height(after_pulse);        //Get pulse height.
  //  int pend=pulse_index+jend_pe-jstart_pe;
  for(int i=jstart_pe;i<=jend_pe;i++)
    {
      int k=i-jstart_pe+pulse_index;
      fPulse[k]+=p_height*fSinglePe.fSingPulse[i];
    }
  //fPulse(Range(pulse_index,pend))+=
  //  p_height*fSinglePe.fSingPulse(Range(jstart_pe,jend_pe));
  return;
}

float VPulse::getArea(float single_area)  //Area is in units of average pes.
{      
  float a=0;
  for(int i=0;i<fNbins;i++)
    {
      a+=fPulse[i];
    }
  a=a/single_area;
  return a;
}

float VPulse::getFadcArea(float single_area)  //Fadc Area is in units of 
                                              //average pes.
{      
  float a=float(getFadcArea())/single_area;
  return a;
}
// **************************************************************************

unsigned short int VPulse::getFadcArea()  //area is in units of dc
{                                         //Not checked for hi/lo
  //  unsigned short int a=int(sum(fFadcPulse));
  unsigned short a=0;
  for(int i=0;i<kFadcSamples;i++)a+=(int)fFadcPulse[i];
  return a;
}
// **************************************************************************

  void VPulse::Noise(float pe_rate_ns, bool after_pulse)
{
  float pulse_time_ns=-fSinglePeSizeNs;
                                                // start early to get partial
  pulse_time_ns += float (Rexp(pe_rate_ns));
  while(pulse_time_ns<fNbins*kPulseBinSize)
   {
     AddPe(pulse_time_ns,after_pulse);
     pulse_time_ns += float(Rexp(pe_rate_ns));
   }
  return;
}
// **************************************************************************

int VPulse::NoisePedestal(float pe_rate_ns, int bins)
    //NOTE:bins must be multiple of kFadcSamples
    //*********************************************************************
    // Determine average fadc pedestal for a random noise waveform. Mimics
    // Wash U Fadc use of non signal bins to generate pedestal.
    //********************************************************************
{                                            //Length in pulse samples bins
  int nsize=(int) ((float)bins*kFadcBinSize/kPulseBinSize);
  if(fNPulse)delete[] fNPulse; //in constructor fNPulse set to 0.(false). 
                             //Don't 
                             //delete unless its be allocated memory by the 
                             //following new.
  fNPulse = new float[nsize];
  for(int i=0;i<nsize;i++)
    {
      fNPulse[i]=0.0;
    }

  float pulse_time_ns=-fSinglePeSizeNs;  // start early to get partial
  pulse_time_ns += float (Rexp(pe_rate_ns));
  bool afterPulse=true;
  while(pulse_time_ns<nsize*kPulseBinSize)
    {
      int pulse_index=int(pulse_time_ns/kPulseBinSize); 
                                   // Position in pulse array of start of pe
      if(pulse_index>=(-(fSinglePeSizeBins-1))&&pulse_index<=(nsize-1))
	{ 
               //Make sure at least part of pulse is within the waveform array.
               //Starting point of portion of single pe we will load
	  int jstart_pe=0; 
	  int jend_pe=fSinglePeSizeBins-1;           //End point.
	  if(pulse_index<0)          //See if starting point is before waveform
	    {
	      jstart_pe=-pulse_index;
	      pulse_index=0;
	    }
	  else
	    {
	      if(pulse_index+fSinglePeSizeBins-1>nsize-1)
		{
		  jend_pe=nsize-pulse_index-1;
		}
	    }
	  float p_height=Pulse_height(afterPulse);        //Get pulse height.
	  for(int i=jstart_pe;i<=jend_pe;i++)
	    {
	      int k=i-jstart_pe+pulse_index;
	      fNPulse[k]+=p_height*fSinglePe.fSingPulse[i];
	    }
	}
      pulse_time_ns += float(Rexp(pe_rate_ns));
    }
  //Now convert to a Fadc sample waveform
  if(fNFadc)delete[] fNFadc; //in constructor fNFadc set to 0.(false). 
                             //Don't 
                             //delete unless its be allocated memory by the 
                             //following new.
  fNFadc = new float[bins];
  int pedsum=0;
  for( int ifadc=0;ifadc<bins; ifadc++)  //
    {
//********************************************************************
//  Fadc electronics averages over 1 nsec.
//********************************************************************
      int jpulse=ifadc*kBinRatio;
      fNFadc[ifadc]=0.0;
      for(int i=jpulse;i<jpulse+kFadcSampleBins;i++)
	{
	  float a=fNPulse[i];
	  fNFadc[ifadc]=fNFadc[ifadc]+a;
	}
      fNFadc[ifadc]=fNFadc[ifadc]/kFadcSampleBins;  
//********************************************************************
      fNFadc[ifadc]=fNFadc[ifadc]*kFadcPedc;  //Amplify to digital counts
      pedsum+=(int)fNFadc[ifadc];  //This is where Jim B would round down.
    }
  pedsum=pedsum/(bins/kFadcSamples); //This mimics way Jim B will deterine.
                                        //sum and shift (divide by power of 2)
  return pedsum;
}
//********************************************************************



void VPulse::Fadc(float start_time)
// ********************************************************************
//  Determine FADC from gpulse
// ********************************************************************
//Modified:
//  22/07/03 GHS if kHitThreshold is zero then we have no zero supression
// ********************************************************************

{
  int pulse_index=int(start_time/kPulseBinSize);

  fFadcHit=false;
  if(kFadcHitThreshold==0)
    {
      fFadcHit=true;              //0 means zero supression turned off
                                  //All channels hit!
    }
 
  float gfadc[kFadcSamples];
  for( int ifadc=0; ifadc<kFadcSamples; ifadc++)  //
    {
//********************************************************************
//  Fadc electronics averages over 1 nsec.
//********************************************************************
      int jpulse=pulse_index+ifadc*kBinRatio;
      gfadc[ifadc]=0.0;
      for(int i=jpulse;i<jpulse+kFadcSampleBins;i++)
	{
	  gfadc[ifadc]+=fPulse[i];
	}
      gfadc[ifadc]=gfadc[ifadc]/kFadcSampleBins;  
      if(gfadc[ifadc]<0)gfadc[ifadc]=0; //Since we may have subtracted a 
                                        //pedistal this may be negative. If so,
                                        //set it to 0, as it would be in the 
                                        //electronics.
// ********************************************************************

      gfadc[ifadc]=gfadc[ifadc]*kFadcPedc;  //Amplify to digital counts

// *****************************************************************
//Check to see if we don't Zero Supress this channel
// *****************************************************************
      if(!fFadcHit)
	{
	  if((int)gfadc[ifadc]>kFadcHitThreshold)  //Note cast to int here.
	    {
	      fFadcHit=true;
	    }
	}

// *****************************************************************
//Hi/lo
// *****************************************************************
      if((int)gfadc[ifadc]>256)                  //Look for high/low
	{
	  fFadcHiLo=true;  //set HI flag.
	  break;
	  }
    }
  for( int ifadc=0; ifadc<kFadcSamples; ifadc++)  //
    {
      if(fFadcHiLo)                  //Look for high/low
	{
	  gfadc[ifadc]=gfadc[ifadc]*kFadcHiGain;   //Reduce by gain factor.
	  fFadcPulse[ifadc]=(unsigned char)gfadc[ifadc];
	}
      else
	{                           //convert to digital counts.
	  fFadcPulse[ifadc]=(unsigned char)gfadc[ifadc];

	}
// *****************************************************************
                                    //Lots of casting here and above
      fFadcSamples[ifadc]=(long)fFadcPulse[ifadc]; 
    }
}

float VPulse::Cfd(float threshold,float start_time)
/***************************************************************************
  Model CFD on waveform. Returns CFD trigger time or 100001 ns overflow
  time if no trigger found.
****************************************************************************/
{
                                        //Find if we go over threshold.
  int ithresh;
  int istart=int(start_time/kPulseBinSize);   //Time to start looking.
  if(istart>fNbins-1)
    return 100001.;
  if(istart<kCfdDelayBins)                   //Force minimum.
    istart=kCfdDelayBins;                    //May want error message here

  for(ithresh=istart;ithresh<fNbins; ithresh++)
    {
      if(fPulse[ithresh]>=threshold)
	{
	                                   //determine number of bins to delay
	  //cout<<"ithresh "<<ithresh<<"istart"<<istart<<endl;
	  //cout<<"delay "<<kCfdDelayBins<<endl;
	  int lsize=kCfdDelayBins+fNbins-ithresh;

              // Allocate the delayed waveform array and the summing array.
	  if(fMainPulse)delete[] fMainPulse;
	  fMainPulse=new float[lsize];

	  if(fDpulse)delete[] fDpulse;
	  fDpulse=new float[lsize];

	  for(int i=0;i<lsize;i++)fDpulse[i]=0;   //Init to zero
               // Delay and amplify and negate waveform(this is what CFD's do)
                                                      //Vector arith
	  
	  //fDpulse=(-fPulse(Range(ithresh-kCfdDelayBins,fNbins-1))*kCfdGain);
	  for(int i=0;i<lsize;i++)
	    {
	      int j=i+ithresh-kCfdDelayBins;
	      fDpulse[i]=(-fPulse[j]*kCfdGain);
	    }
	  for(int i=0;i<lsize;i++)fMainPulse[i]=0;
	  
	  for(int i=0;i<fNbins-ithresh;i++)
	    {
	      int j=i+ithresh;
	      fMainPulse[i]=fPulse[j];
     //	fMainPulse(Range(0,fNbins-ithresh-1))=fPulse(Range(ithresh,fNbins-1));
	    }

	  //Add delayed,amplified and negated fPulse to original
 	  for(int i=0;i<lsize;i++)fMainPulse[i]+=fDpulse[i];
	  for(int j=0;j<lsize-kCfdDelayBins;j++) //look for zero crossing.
	    {
	      if(fMainPulse[j]<=0)
		{
		  float trigger_time=(j+ithresh)*kPulseBinSize;
		  return trigger_time; //Normal return(trigger time)
		  //cout<<"itrig:"<<(j+ithresh)<<endl;
		  // return float(ithresh); //Figure 10 return only. Debug.
		}
	    }
	}
    }
  return 100001.;            // No trigger: CFD overflow time.
}

float VPulse::Vcfd(float threshold,float start_time, float vcfd_offset)
/***************************************************************************
  Model VERITAS CFD on waveform. Returns CFD trigger time or 100001 ns 
  overflow time if no trigger found.
  This uses VVV's description of the VERITAS CFD. 2 Main differences from
  standard CFD:
  1: CFD signal (pos->neg transition) used to 'clock' Delayed discriminated 
  signal. Requires looking for all transisitions and checking DISC 3ns later
  2: CFD zero crossing replaced with rate adjustable offset level(instead of 0)
  ***********Impliment 1: only for now Leave OFFSET at 0******************
****************************************************************************/
{
  int istart=int(start_time/kPulseBinSize);   //Time to start looking.
  if(istart>fNbins-1)    //Check max.
    return 100001.;
  if(istart<kVcfdDelayBins)                   //Force minimum.
    istart=kVcfdDelayBins;                    //May want error message here
                          //Look for zero cross first, then look at threshold.
                          //Only want positive to neg crossings.

  //  if(max(fPulse(Range(istart,fNbins-1)))<threshold)
  //  return 100001.;
  float maxPulse=0;
  for(int i=istart;i<fNbins;i++)
    {
      if(maxPulse<fPulse[i])maxPulse=fPulse[i];
      }
  if(maxPulse<threshold)return 100001.;  //Quick way to reject.


//determine number of bins in summing arrays.
  int lsize=kVcfdDelayBins+fNbins;
  // Allocate the delayed waveform array and the summing array.

  if(fMainPulse)delete[] fMainPulse;
  fMainPulse = new float[lsize];

  if(fDpulse)delete[] fDpulse;
  fDpulse = new float[lsize];

  for(int i=0;i<lsize;i++)fDpulse[i]=0;      //init to 0
               // Delay and amplify and negate waveform(this is what CFD's do)
  for(int i=0;i<lsize-kVcfdDelayBins;i++)
    {
      //fDpulse(Range(kVcfdDelayBins,lsize-1))=(-fPulse*kVcfdGain);
      int j=i+kVcfdDelayBins;
      fDpulse[j]=(-fPulse[i]*kVcfdGain);
    }
  for(int i=0;i<lsize;i++)fMainPulse[i]=0;
  for(int i=0;i<fNbins;i++)fMainPulse[i]=fPulse[i];
      //Add delayed,amplified and negated pulse to original
  for(int i=0;i<lsize;i++)fMainPulse[i]+=fDpulse[i]; 
  bool vcfd_fired=false;  //Flag that vcfd has fired(if true) and has not yet
                          //reset(gone positive)
  if(fMainPulse[istart]<vcfd_offset)  //See if we start fired. set flag.
    vcfd_fired=true;
                           //look for pos-> negative zero(or offset) crossing.
  for(int j=istart+1;j<fNbins-1-kVcfdTrigDelayBins;j++)
    {
      if(vcfd_fired)
	{
	  if(fMainPulse[j]>vcfd_offset)  //neg ->positive crossing. reset
	    vcfd_fired=false;
	}
      else
	if(fMainPulse[j]<=vcfd_offset)   //pos -> crossing. CFD fires?
	  {                              //Do we trigger. Check disc
	    if(fPulse[j+kVcfdTrigDelayBins]>=threshold) 
	      {         //WE TRIGGER! Determine when and return
		        //j+kVcfdTrigDelayBins is trigger bin in fMainPulse. 
	       float trigger_time=float(j+kVcfdTrigDelayBins)*kPulseBinSize;
		                //This is trigger time
	       //cout<<j<<","<<kVcfdTrigDelayBins<<endl;
	       return trigger_time; //Normal return(trigger time)
	       //cout<<"itrig:"<<(j+ithresh)<<endl;
	       // return float(ithresh); //Figure 10 return only. Debug.
	     }
	    vcfd_fired=true;
	  }

    }
  return 100001.;            // No trigger: CFD overflow time.
} 

//Return the preesent pedestal
float VPulse::FindPedestal()
{                         //Find present average value per bin of waveform. 
  float ped=0;
  for(int i=0;i<fNbins;i++)
    {
      ped+=fPulse[i];
    }
  return ped/fNbins;
}


//Remove the average value from the pulse array.
void VPulse::RemovePedestal(float ped)
{  //Don't init fPedestal. Just add whatever we take out.
  for(int i=0;i<fNbins;i++)
    {
      fPulse[i]-=ped;
    }
  fPedestal+=ped;
  return;
}

//Add a pedestal to the pulse.
void VPulse::AddPedestal(float ped)
{
  for(int i=0;i<fNbins;i++)
    {
      fPulse[i]+=ped;
    }
  return;
}




void VPulse::PrintfPulse(float startTime)
{
  char * filename;
  filename="pulse.dat";
  ofstream out(filename
);
  int startBin=int(startTime/kPulseBinSize);

  int endBin=int(startBin+((kFadcSamples*kFadcBinSize)/kPulseBinSize));
//  for(int i=startBin;i<fNbins;i++)
  for(int i=startBin;i<endBin;i++)
    {
      int j=i-startBin;
      cout<<j*kPulseBinSize<<','<<fPulse[i]<<endl;;
      out<<j*kPulseBinSize<<','<<fPulse[i]<<",\n";
    }
}

 void VPulse::PrintfDpulse()
{
  ofstream out("dpulse.dat");
  for(int i=0;i<(int) sizeof(fDpulse);i++)
    {
      cout<<i*kPulseBinSize<<','<<fDpulse[i]<<endl;
      out<<i*kPulseBinSize<<','<<fDpulse[i]<<",\n";
    }
}

 void VPulse::PrintfMainPulse()
{
  ofstream out("mainpulse.dat");
  for(int i=0;i< (int) sizeof(fMainPulse);i++)
    {
      cout<<i*kPulseBinSize<<','<<fMainPulse[i]<<endl;
      out<<i*kPulseBinSize<<','<<fMainPulse[i]<<",\n";
    }
}
 void VPulse::PrintFadc()
{
  //ofstream out("mainpulse.dat");
  float fadcPulseHeight;
  for(int i=0;i<kFadcSamples;i++)
    {
      if(fFadcHiLo)
	{
	  fadcPulseHeight=int(fFadcPulse[i])/kFadcHiGain;
	}
      else
	{
	  fadcPulseHeight=int(fFadcPulse[i])/kFadcHiGain;
	}
      cout<<i*kFadcBinSize<<','<<fadcPulseHeight<<endl;
      //out<<i*kFadcBinSize<<','<<fFadcPulse[i]<<",\n";
    }
}


    
