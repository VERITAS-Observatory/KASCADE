/**
 * \class KSPST
 * \ingroup common
 * \brief File of methods for KSPST
 * Original Author: Glenn H. Sembroski
 * $Author$
 * $Date$
 * $Revision$
 * $Tag$
 *
 **/
// This is where the code starts.

#include "KSPST.h"

KSPixelTimes::~KSPixelTimes()
{
  // Nothing to do
}
// *********************************************************************


// In the constructor is where most of the work gets done.
KSPST::KSPST(KSCameraTypes CameraType, int TriggerMultiplicity,
	     int NumPixelsTrigger)
{
  fCameraType           = CameraType;
  fTriggerMultiplicity  = TriggerMultiplicity;

  fNumPatches=kNumPatches[fCameraType];
  fNumPixelsCamera=gNumPixelsCamera[fCameraType];
  fNumPixelsTrigger= NumPixelsTrigger;
  pfPatchTriggerTimes.resize(fNumPatches);       //times patches trigger
  pfPatchTriggerPattern = new int[fNumPatches];  //patttern that caused trig

  fL2TriggerPixels.resize(fNumPixelsCamera);

  fNullDoubleVector.clear();
  fPatchCFDTriggerTimes.clear();
  fPatchCFDTriggerTimes.resize(kNumPixelsPerPatch,fNullDoubleVector);

  pfPixelsInPatch.resize(fNumPatches);
  //pfPixelsInPatchTimes.resize(fNumPatches);

  for(int i=0;i<fNumPatches;i++)
    {
      pfPixelsInPatch.at(i).resize(kNumPixelsPerPatch);
  //    pfPixelsInPatchTimes.at(i).resize(kNumPixelsPerPatch);
    }
  FillPixelsInPatch();

  // **************************************************************
  //Debug dump
  // *************************************************************
  //for(int i=0;i<fNumPatches;i++)
  // {
  //   std::cout<<"Patch: "<<i<<std::endl;
  //   for(int j=0;j<kNumPixelsPerPatch;j++)
  //	{
  //	  // ***************************************************************
  //	  // Note: Pixels start at 1, pixel index's start at 0
  //	  //       Missing patch pixels flaged as -1
  //	  // *****************************************************************
  //	  int fPixelIndex=pfPixelsInPatch[i][j];  //Get a pixel index 
  //	  std::cout<<"j,pix: "<<j<<" "<<fPixelIndex<<std::endl;
  //	}
  // }
  // ***********************************************************************
 
  FillPSTPatterns();

}
// ********************************************************************

KSPST::~KSPST()
{
  //nothing to do
}
// ********************************************************************

bool KSPST::isTriggered(std::vector< std::vector < double > >& CFDTriggerTimes, 
		   double& fImageTriggerTime)
// ****************************************************************************
// 	Determine if we have a trigger in the pst. If we do put the trigger
// 	time into fImageTriggerTime. Return true if we have a trigger. 
// ****************************************************************************
{
//The PST model is as follows:
// 1:Find patches that have multiplicity trigger(include timing.
// 2:Using only those channels in a multiplicity trigger patch that are on 
//   PST_STROBE_DELAY after the trigger(models internal delays for address 
//   strobe inside PST) to form address to check PST_PATTERNS
//   Use first patch to have a good pattern to get trigger time.

// **********************************************************************
// Note: A time of >gOverflowTime  is an overflow! 
// **********************************************************************
// Note: Pixels start at 1, pixel index's start at 0
// **********************************************************************

  bool fTriggered=false;
  fImageTriggerTime=gOverflowTime;    // Default: no trigger

  // **********************************************************************
  // PST first finds patches that have a multiplicity trigger.
  // **********************************************************************
  // CFDTriggerTimes is a vector indexed by channels. The contents for for each
  //  channel it is a list of times that channel's CFD Triggered.
  // **********************************************************************

  bool weHaveAMultTrigger=false;
  pfPatchTriggerTimes.clear();

  for(int i=0;i<fNumPatches;i++)
    {
      int gotNumTimes=0;
      for(int j=0;j<kNumPixelsPerPatch;j++)
	{
	  // *************************************************************
	  // Note: Pixels start at 1, pixel index's start at 0
	  //       Missing patch pixels flaged as -1
	  // *************************************************************
	  int fPixelIndex=pfPixelsInPatch.at(i).at(j);  //Get a pixel index 
	  if(fPixelIndex<fNumPixelsTrigger)   //This is a check for diminished
                                              // pixel trigger range
	    {
	      
	      if(fPixelIndex>=0 && CFDTriggerTimes.at(fPixelIndex).size()>0)
		{
		  fPatchCFDTriggerTimes.at(j)=CFDTriggerTimes.at(fPixelIndex);
		  gotNumTimes++;
		}
	      else
		{
		  fPatchCFDTriggerTimes.at(j).clear();
		}
	    }
	}
      // *******************************************************************
      // First level check is to see if we have at least enough pixels
      // firing at any time to possibly cause a trigger
      // *******************************************************************
      if(gotNumTimes>=fTriggerMultiplicity)
	{
	  // ************************************************************
	  // Build the CFD summed pulse wave for this patch (in a funny way)
	  // ************************************************************
	  // buildSummedCFD builds a list waveform in the private 
	  // fSummedCFDWaveForm. fSummedCFDWaveForm is a vector indexed by 
	  // times (.25 Ns steps typically) and whose contents 
	  // at each time bin is a  list of which channle's CFD's are high at 
	  // that time. Looking at the size of the list in any time bin will 
	  // tell you the multiplcity. Since we restict fPatchCFDTriggerTimes 
	  // to only those channels in a particular patch, the size tells us 
	  // the multiplicity at any time in this patch.
      // ***************************************************************
	  buildSummedCFD( fPatchCFDTriggerTimes, 
			                       gPSTPulseWidthNS[fCameraType]);

	  // ************************************************************
	  // Find the first time that has required multiplcity. Starting at 
	  // timestartTriggerTime. Only retrigger if the pattern changes from 
	  // the previous pattern.  May be overflow time
	  // ************************************************************
	  // ***************************************************************
	  // We are going to loop over the different configurations of pixels
	  // and get all possible triggers with their times and configurations.
	  // Flag the we quit is to get the overflow time.
	  // ***************************************************************
	  pfCFDTriggerChannels=NULL;  //Flag to check for ignorable 
	                              //patterns (NULL means don't check)
                                      // Replaced by first success
                                      // afterwhich this pattern is ignored
                                      // until a new one is found.
	  double startTriggerTime= fSummedCFDWaveFormStartNS;
	  KSPixelTimes multiplicityTriggerTime;  
	  while(1)
	    {
	      //startTriggerTime is set to next time in waveform.
	      multiplicityTriggerTime.fTime = findSummedCFDTriggerTime( 
							startTriggerTime,
							fTriggerMultiplicity);
	      if(multiplicityTriggerTime.fTime==gOverflowTime)
		{
		  break;
		}
	      else
		{
		  weHaveAMultTrigger=true;
		  multiplicityTriggerTime.fIndex=i;
		  pfPatchTriggerTimes.push_back(multiplicityTriggerTime);
		}   
	    }
	}
    }
  // *****************************************************************
  // Now see if we have a multiplcity trigger. Create the full CFD Summed 
  // Waveform we will use it to find the list of all pixels that fired at a 
  // specific time in the camera. Not just the ones in a particular 
  // patch.
  // ****************************************************************
	
  if(weHaveAMultTrigger)
    {
      // *****************************************************************
      // We have a multiplcity trigger. Create the full CFD Summed Waveform
      // We will use it to find the list of all pixels that fired at a 
      // specific time in the camera. Not just the ones in a particular 
      // patch.
      // ****************************************************************
      buildSummedCFD(CFDTriggerTimes,gPSTPulseWidthNS[fCameraType]);
      
      // *****************************************************************
      // now order the multiplicity patch trigger times, and start checking
      // from the earliest to see if we have an acceptable pattern trigger 
      // *****************************************************************
      std::sort(pfPatchTriggerTimes.begin(),pfPatchTriggerTimes.end());
      
      for(int i=0;i<(int)pfPatchTriggerTimes.size();i++)
	{
	  double timeTriggerNS=pfPatchTriggerTimes.at(i).fTime;
	  //Get time this patch 
	  //triggered.
	  double timeStrobeNS=timeTriggerNS+gPSTStrobeDelayNS;
	  //Get time patch bits are 
	  //strobed into address. If 
	  //this patch triggers this 
	  //will be its trigger time
	  int fPatchIndex=pfPatchTriggerTimes.at(i).fIndex;
	  
	  
	  // **********************************************************
	  // Get a list of channels (full camera) whose CFD's are fireing
	  // at timeStro e and get the patch pattern from this list

	  // **********************************************************
	  // Note: This use of fTimeStrobe may cause us to loose pixels 
	  // from the start of the gPSTPulseWidthNS window but on the 
	  // other hand we may gain some after the strobe delay.
	  // **************************************************************
	  std::set<int>* pStrobeTimeCFDs=getSummedCFDSet(timeStrobeNS);
	  
	  unsigned long fMemoryAddress=getPatchPattern(fPatchIndex,
						       pStrobeTimeCFDs);
	  
	  if(pfPSTPatterns[fMemoryAddress]!=0)
	    {
	      fTriggered=true;
	      fImageTriggerTime=timeTriggerNS;
	      // **********************************************************
	      // Now fill in a whole buch of stuff for the VBF file.
	      // *********************************************************
	      for(int j=0;j<fNumPatches;j++)
		{
		  fMemoryAddress=getPatchPattern(j,pStrobeTimeCFDs);
		  if(pfPSTPatterns[fMemoryAddress]!=0)
		    {
		      pfPatchTriggerPattern[j]=fMemoryAddress;
		    }
		  else
		    {
		      pfPatchTriggerPattern[j]=0;
		    }
		}
	      for(int j=0;j<fNumPixelsCamera;j++)
		{
		  if(pStrobeTimeCFDs->find(j)!=pStrobeTimeCFDs->end())
		    {
		      fL2TriggerPixels.at(j)=true;
		    }
		  else
		    {
		     fL2TriggerPixels.at(j)=false;
		    }  
		}
	      return fTriggered;
	    }
	}
    }
  return fTriggered;
}
// **************************************************************************



void KSPST::FillPSTPatterns()
// *************************************************************************
// Read PST pattern array from binary file
// *************************************************************************
{
// *************************************************************************
//   First determine which data set to read. Level already checked by
//   KSTriggerDataIn to be 2,3, or 4        
// *************************************************************************

  std::string fPSTPatternFileName;
  if (fTriggerMultiplicity==2)
    {
      fPSTPatternFileName="PSTMultiplicity2.bin";
}
  else 	if (fTriggerMultiplicity==3)
    {
      fPSTPatternFileName="PSTMultiplicity3.bin";
    }
  else 	if (fTriggerMultiplicity==4)
    {
      fPSTPatternFileName="PSTMultiplicity4.bin";
    }
  else
    {                      //Should never get here
      std::cout<<"KSPST--Illegal PST Multiplicity specified"<<std::endl;
      exit(1);
    }

  std::ifstream fPSTPatternFile(fPSTPatternFileName.c_str(),
		                              std::ios::in | std::ios::binary);
  if(fPSTPatternFile.fail())
    {
      std::cout<<"KSPST--Failed to Open PST pattern file "<<fPSTPatternFileName
	       <<std::endl;
      exit(1);
      //Throw an exception here.
    }
  double fPSTPatternSize=pow(2.0,kNumPixelsPerPatch);   // 2**19 I hope
  pfPSTPatterns = new short[(int)fPSTPatternSize]; 
  fPSTPatternFile.read((char*)pfPSTPatterns,
			                  (int)fPSTPatternSize*sizeof(short)); 

  std::cout<<"Using binary version of input PST Pattern file:"
	   <<fPSTPatternFileName<<std::endl;
  fPSTPatternFile.close();
  return;
}
// ****************************************************************************

void KSPST::FillPixelsInPatch()
// **********************************************************************
// Fill in the pfPixelsInPatch array.
// **********************************************************************
// Note: Pixels start at 1, pixel index's start at 0
// **********************************************************************
{
  if(fCameraType==VERITAS499)
    {                     // Makes and fills pst_pixels, pst-patches
      pfA=(int*)k499A;          // Assign pointers
      pfB=(int*)k499B;
      pfC=(int*)k499C;
      pfD=(int*)k499D;
      pfU=(int*)k499U;
      pfV=(int*)k499V;
    }                     
  else if(fCameraType==WHIPPLE490)
    {                     // Makes and fills pst_pixels,pst-patches 
      pfA=(int*)k331A;          // Assign pointers
      pfB=(int*)k331B;
      pfC=(int*)k331C;
      pfD=(int*)k331D;
      pfU=(int*)k331U;
      pfV=(int*)k331V;
    }				
  
  // ***********************************************************************
  // Define the pixel coordinates in U,V,X,Y.
  //   **The whole purpose of the following is to produce pfPixelUV**
  // ***********************************************************************
  int fNumOfRings=gNumLines[fCameraType];
  KSCameraGrid fGrid(fNumOfRings);    //This gets the XIndex and Yindex
  //Convert the grid to xdeg ydeg (sort of)
  int fNumPixels= ((fNumOfRings+1)*fNumOfRings/2)*6+1;
  double* fXDeg= new double[fNumPixels];
  double* fYDeg= new double[fNumPixels];
  double fScale=1.0;

  for(int i=0;i<fNumPixels;i++)
    {
      fYDeg[i] = (fScale/2)*fGrid.pfYIndex[i]*cos(gDeg2Rad*30.);
      fXDeg[i] = (fScale/2)*fGrid.pfXIndex[i];
    }
 

  // ************************************************************************
  // Now for VERITAS499 we have to remove 6 of the pixels of ring 13(the 
  //corners)which have no PMT's in the veritas 499 pixel camera.( and then 
  //compress stuff)
  // *************************************************************************
   if(fCameraType==VERITAS499)
     {
       int fId=397;    // (destination)First corner pixel index
       int fISource=398;
       for(int k=0;k<6;k++)  // six corners 
	 {
	   for(int j=0;j<11;j++)
	     {
             fXDeg[fId]=fXDeg[fISource+j];
             fYDeg[fId]=fYDeg[fISource+j];
             fId++;
  	    }
         fISource+=12;
  	}
     fNumPixels=fId; // # of pixels(should be 463)
     }
  //  ************************************************************************


  //  ************************************************************************
  //     find (u,v) pixel coordinates  
  //  ************************************************************************
  int* fUPosition = new int[fNumPixels];
  int* fVPosition = new int[fNumPixels];

  // ************************************************************************
  //  The U axis is along X and the V axis is +60 deg from X.
  //  Its wierd how this works out but it does.
  // ************************************************************************

  for(int i=0;i<fNumPixels;i++)
    {
      fVPosition[i] = nearestInt((fYDeg[i]/sin(gDeg2Rad*60.0))/fScale );
 
      fUPosition[i] = nearestInt((fXDeg[i]-(fYDeg[i]/tan(gDeg2Rad*60.0)))/
				                                      fScale);
    }

  KSPixelUV* pfPixelUV=NULL;

  if(fCameraType==WHIPPLE490)
    {
      pfPixelUV= new  KSPixelUV(-12,25); //Special class to make an 2 dim 
                                         //array that has lower bounds of -12 
                                         //each dimension and is 25 by 25 big. 
    }
  else if(fCameraType==VERITAS499)
    {
      pfPixelUV= new  KSPixelUV(-14,29); //Special class to make an 2 dim 
                                         //array that has lower bounds of -14 
                                         //each dimension and is 29 by 29 big. 
    }
 
  for(int i=0;i<fNumPixels;i++)
    {
      pfPixelUV->set(fUPosition[i],fVPosition[i],i);
    }

  // ***********************************************************************
  // Finally we can start filling pfPixelsInPatch (the whole point of all this)
  // ***********************************************************************

  // ***********************************************************************
  // Module 1 has only 1 patch used. Do it first
  // ***********************************************************************
  int fModule=0;
  int fPatch=0;				// Number patches
  int fPatchNumber=0;
  for(int j=0;j<kNumPixelsPerPatch;j++) // Go through pixels
    {
      int fX = kPatchU[j]+kMiddlePatchU[fPatch];// x-position within a module  
      int fY = kPatchV[j]+kMiddlePatchV[fPatch];// y-position within a module  
      int fU = pfA[fModule]*fX + pfB[fModule]*fY + pfU[fModule];//rotate,module
      int fV = pfC[fModule]*fX + pfD[fModule]*fY + pfV[fModule];//rotate,module
      pfPixelsInPatch.at(fPatchNumber).at(j)=pfPixelUV->get(fU,fV);
    }

  // ***********************************************************************
  // now do the rest
  // ***********************************************************************
    
  for(fModule=1;fModule<kNumModules[fCameraType];fModule++)
    {
      for(fPatch=0;fPatch<kNumPatchesPerModule;fPatch++)//iterate over patches 
	{                                               // within modules.
	  fPatchNumber++;  //Note use of fPatch and fPatchNumber below!!!!!!

	  for(int j=0;j<kNumPixelsPerPatch;j++) // Go through pixels
	    {
	      int fX = kPatchU[j]+kMiddlePatchU[fPatch];// x-position
	      int fY = kPatchV[j]+kMiddlePatchV[fPatch];// y-position
	      int fU = pfA[fModule]*fX + pfB[fModule]*fY + pfU[fModule];
	      int fV = pfC[fModule]*fX + pfD[fModule]*fY + pfV[fModule];
	      pfPixelsInPatch.at(fPatchNumber).at(j)=pfPixelUV->get(fU,fV);
	    }
	}
    }
// done
 return;
}
// ****************************************************************************

KSPixelUV::KSPixelUV(int StartIndex, int fNumUVIndex)
{
  fStartIndex=StartIndex;
  fPixelUV.resize(fNumUVIndex);
  for(int i=0;i<fNumUVIndex;i++)
    {
      fPixelUV.at(i).resize(fNumUVIndex);
    }
}
// *****************************************************************

int KSPST::nearestInt(double fX)
{
  // Return nearest integer
  int fN=(int)floor((double)fX);
  if((fX-fN)<(fN+1-fX))
    {
      return fN;
    }
  else
    {
      return fN+1;
    }
}
// ********************************************************************

void KSPST::buildSummedCFD(
			std::vector< std::vector < double > >&CFDTriggerTimes, 
			double CFDPulseWidthNS)
// ********************************************************************
// To look for overlapping multiplicity in the CFD triggers build a waveform.
// Do it cleverly. Start with a vector whose indices are time in
// whatever step sizes weve have been using (usuall.25 NS). The contents at 
// each index is another vector of all the channels that are high at that 
// time. To check for a multiplicity we only need to look at the size of the 
// channel vector. For the PST trigger check that  shuch channels 
// are on at the trigger time. See the findSummedCFDTriggerTime method for 
// more info. on this scheme
// **********************************************************************
{
  // *****************************************************************
  // Create the fSummedCFDWaveForm vector. Find its start and end time and 
  // the number of bins needed.
  // ******************************************************************
  bool minMaxInitalize=false;
  
  for(int i=0;i<(int)CFDTriggerTimes.size();i++)
    {
      int numTimes=CFDTriggerTimes.at(i).size();
      if(numTimes>0)
	for(int j=0;j<numTimes;j++)
	  {
	    if(!minMaxInitalize)
	      {
		fSummedCFDWaveFormStartNS=CFDTriggerTimes.at(i).at(j);
		fSummedCFDWaveFormEndNS=CFDTriggerTimes.at(i).at(j);
		minMaxInitalize=true;
	      }
	    else
	      {
		if(CFDTriggerTimes.at(i).at(j)<fSummedCFDWaveFormStartNS)
		  {
		    fSummedCFDWaveFormStartNS=CFDTriggerTimes.at(i).at(j);
		  }
		if(CFDTriggerTimes.at(i).at(j)>fSummedCFDWaveFormEndNS)
		  {
		    fSummedCFDWaveFormEndNS=CFDTriggerTimes.at(i).at(j);
		  }
	      }
	  }
    }
  fSummedCFDWaveFormEndNS=fSummedCFDWaveFormEndNS+CFDPulseWidthNS+
                                                           gPSTStrobeDelayNS;
  // *********************************************************************  // Size up the waveForm. Init to zero size vectors
  // ********************************************************************
  int numCFDWaveFormBins=
    (int)((fSummedCFDWaveFormEndNS-fSummedCFDWaveFormStartNS)/
	                                               gWaveFormBinSizeNS) +1;
  //std::cout<<"numCFDWaveFormBins :"<<numCFDWaveFormBins<<std::endl;
  std::vector<int> nullVector;
  nullVector.clear();
  fSummedCFDWaveForm.clear();
  fSummedCFDWaveForm.resize(numCFDWaveFormBins, nullVector);

  int numBinsPulseWidth=(int)(CFDPulseWidthNS/gWaveFormBinSizeNS);

  
  // **********************************************************************
  // Now fill it up.
  // **********************************************************************
  for(int i=0;i<(int)CFDTriggerTimes.size();i++)
    {
      int numTimes=CFDTriggerTimes.at(i).size();
      if(numTimes>0)
	for(int j=0;j<numTimes;j++)
	  {
	    int CFDTimeIndex=(int) ((CFDTriggerTimes.at(i).at(j)-
			   fSummedCFDWaveFormStartNS)/gWaveFormBinSizeNS);
	    for(int k=CFDTimeIndex;k<CFDTimeIndex+numBinsPulseWidth;k++)
	      {
		if(k<numCFDWaveFormBins)
		  {
		    fSummedCFDWaveForm.at(k).push_back(i);
		  }

	      }
	  }
    }
  return;
}

// ***************************************************************************
double KSPST::findSummedCFDTriggerTime(double& startTimeNS, int Multiplicity)
// *************************************************************************
// Find the first instance after time startTimeNS in the fSummedCFDWaveForm 
// when we have a multiplicity of Multiplicity or more. If the 
// pfCFDTriggerChannels pointer is not NULL ignore triggers that have the 
// same channels as  pfCFDTriggerChannels does.
// Returns the next start time.
// ************************************************************************
{
  // Index to start at
  int startIndex=0;
  if(startTimeNS>fSummedCFDWaveFormStartNS)
    {
      startIndex=(int)((startTimeNS-fSummedCFDWaveFormStartNS)/
		                                          gWaveFormBinSizeNS);
    }
  // ******************************************************************
  // Search for the multiplicity
  // ******************************************************************
  for(int i=startIndex;i<(int)fSummedCFDWaveForm.size();i++)
    {
      if((int)fSummedCFDWaveForm.at(i).size()>=Multiplicity)
	{
	  double multiplicityTriggerTime=i*gWaveFormBinSizeNS + 
	                                            fSummedCFDWaveFormStartNS; 
	  startTimeNS= multiplicityTriggerTime+gWaveFormBinSizeNS;
	  if(pfCFDTriggerChannels==NULL)
	    {
	      pfCFDTriggerChannels=&fSummedCFDWaveForm.at(i);
	      return multiplicityTriggerTime;
	    }
	  else if(fSummedCFDWaveForm.at(i)!=*pfCFDTriggerChannels)
	    {
	      pfCFDTriggerChannels=&fSummedCFDWaveForm.at(i);
	      return multiplicityTriggerTime;
	    }
	}
    }
  return gOverflowTime;   //This is the no-trigger-found exit.
}
// ***************************************************************

unsigned long KSPST::getPatchPattern(int fPatchIndex,
				     std::set<int>* pStrobeTimeCFDs)
// *****************************************************************
// Using the full camera CFD stobe time list (list of on pixels in 
// camera) find which pixels in this pathch were on.
// ******************************************************************
{
  fMemoryAddressBits.reset();
  
  for(int j=0;j<kNumPixelsPerPatch;j++)
    {				// test pixel is on at strobe time.
      int fPixelIndex=pfPixelsInPatch.at(fPatchIndex).at(j);
      if(fPixelIndex<fNumPixelsTrigger) //This is a check for diminished
                                              // pixel trigger range
	{
	  if(pStrobeTimeCFDs->find(fPixelIndex)!=pStrobeTimeCFDs->end())
	    {
	      fMemoryAddressBits.set(j);
	    }	    
	}
    }
  unsigned long fMemoryAddress=fMemoryAddressBits.to_ulong();
  return fMemoryAddress;
}
// ************************************************************************


std::set<int>* KSPST::getSummedCFDSet(double time)
  // *********************************************************************
  // Get the list from out wave form of the channels that were high at 
  // timeStrobe. Output is a "set" not a "vector" so we can do "find" with it.
  // *********************************************************************
  {
    int timeIndex=(int)((time-fSummedCFDWaveFormStartNS)/gWaveFormBinSizeNS);
    if(timeIndex<0)
      {
	std::cout<<"KSPST - CFDWaveForm TimeIndex out of range:time, "
	  "fSummedCFDWaveFormStartNS,timeIndex: :"<<time<<" "
		 <<fSummedCFDWaveFormStartNS<<" "<<timeIndex<<std::endl;
	exit(1);
      }
    if(timeIndex>=(int)fSummedCFDWaveForm.size())
      {
	std::cout<<"KSPST - CFDWaveForm TimeIndex out of range:time, "
	  "fSummedCFDWaveFormEndNS,timeIndex: :"<<time<<" "
		 <<fSummedCFDWaveFormEndNS<<" "<<timeIndex<<std::endl;
	exit(1);
      }
    fCFDSet.clear();
    for(int i=0;i<(int)fSummedCFDWaveForm.at(timeIndex).size();i++)
      {
	fCFDSet.insert(fSummedCFDWaveForm.at(timeIndex).at(i));
      }
    return &fCFDSet;
  }
