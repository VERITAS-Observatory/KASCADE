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


// In the constructor is where most of the work gets done.
KSPST::KSPST(KSCameraTypes CameraType, int TriggerMultiplicity)
{
  fCameraType           = CameraType;
  fTriggerMultiplicity  = TriggerMultiplicity;

  fNumPatches=kNumPatches[fCameraType];
  pfPatchTriggerTimes.resize(fNumPatches);          //time a patch triggers
  pfPatchTriggerPattern = new int[fNumPatches];  //patttern that caused trig

  pfPixelsInPatch.resize(fNumPatches);
  pfPixelsInPatchTimes.resize(fNumPatches);

  for(int i=0;i<fNumPatches;i++)
    {
      pfPixelsInPatch[i].resize(kNumPixelsPerPatch);
      pfPixelsInPatchTimes[i].resize(kNumPixelsPerPatch);
    }
  FillPixelsInPatch();
 
  FillPSTPatterns();

}
// ********************************************************************

KSPST::~KSPST()
{
  //nothing to do
}
// ********************************************************************

bool KSPST::isTriggered(double* pfTimeTrigger, double& fImageTriggerTime)
// ****************************************************************************
// 	Determine if we have a trigger in the pst. If we do put the trigger
// 	time into fImageTriggerTime. Return true if we have a trigger. 
// ****************************************************************************
{
//The PST model is as follows:
// 1:Find patches that have multiplicity trigger(iclude timing.
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
  for(int i=0;i<fNumPatches;i++)
    {
      for(int j=0;j<kNumPixelsPerPatch;j++)
	{
// **********************************************************************
// Note: Pixels start at 1, pixel index's start at 0
//       Missing patch pixels flaged as -1
// **********************************************************************
	  int fPixelIndex=pfPixelsInPatch[i][j];  //Get a pixel index 
	  if(fPixelIndex>=0)
	    {
	      pfPixelsInPatchTimes[i][j].fTime=pfTimeTrigger[fPixelIndex]; 
	      pfPixelsInPatchTimes[i][j].fIndex=j; //do this for sort
	    }
	  else
	    {
	      pfPixelsInPatchTimes[i][j].fTime=gOverflowTime;
	      pfPixelsInPatchTimes[i][j].fIndex=-1; 
	    }
	}
      // ****************************************************************
      // Order patch pixel times. Since we defined < operator for KSPixelsTimes
      // this will for by fTime and bring the fIndex along. Tricky!
      // ****************************************************************
      std::sort(pfPixelsInPatchTimes[i].begin(),pfPixelsInPatchTimes[i].end());
					
      // ****************************************************************
      // Look for multiplicity trigger. Slide window returns with 100001. 
      // if no trigger
      // ****************************************************************
      // Note:With this scheme we can only get one tigger time per patch. Does
      // this mean that once the first multiplcity state is achieved we are 
      // dead for a second even if the first doesn't have a good pattern?
      // *****************************************************************
      pfPatchTriggerTimes[i].fTime = SlideWindow(pfPixelsInPatchTimes[i],
					 kPSTPulseWidth,fTriggerMultiplicity);
      pfPatchTriggerTimes[i].fIndex=i;
    }
	
  // ***********************************************************************
  // now order the multiplicity patch trigger times, and start  checking
  // from the earliest to see if we have an acceptable pattern trigger 
  // ***********************************************************************
  std::sort(pfPatchTriggerTimes.begin(),pfPatchTriggerTimes.end());
  for(int i=0;i<fNumPatches;i++)
    {
      if(pfPatchTriggerTimes[i].fTime >= gOverflowTime)
	{
	  break;      // Last trigger has been checked.
	}
      double fTimeTrigger=pfPatchTriggerTimes[i].fTime;//Get time this patch 
                                                     //triggered.
      double fTimeStrobe=fTimeTrigger+kStrobeDelay;// Get time patch bits are 
	                                          //strobed into address. If 
	                                          //this patch triggers this 
	                                          //will be trigger time
	// *****************************************************************
	// Go through patch pixel trigger times and form address of pattern of
	// patch with those pixels that are high at fTIMESTROBE.
	// *****************************************************************
	//Note: This use of fTimeStrobe may cause us to loose pixels from
	//the start of the kPSTPulseWidth window but on the other hand we may
	//gain some after the window.
	// ******************************************************************
      fMemoryAddressBits.reset();
                       //Cause the 2nd sort rearrainged things
      int fPatchIndex=pfPatchTriggerTimes[i].fIndex;

      for(int j=0;j<kNumPixelsPerPatch;j++)
	{				// test pixel is on at strobe time.

	  if(pfPixelsInPatchTimes[fPatchIndex][j].fTime>fTimeStrobe)
	    {
	      break;  //We are past the gate. Don't need to check anymore.
	    }
	  if(pfPixelsInPatchTimes[fPatchIndex][j].fTime+kPSTPulseWidth > 
	                                                           fTimeStrobe)
	    {
	      //Cause the 1st sort rearrainged things
	      int idx=pfPixelsInPatchTimes[fPatchIndex][j].fIndex;
	      //std::cout<<" "<<idx;
	      if(idx>=0)           //ignore missing pixels (but above should 
		{		   //have done that)
		  fMemoryAddressBits.set(idx);
		}	    
	    }
	}
				// Check to see if pattern good.
      unsigned long fMemoryAddress=fMemoryAddressBits.to_ulong();
      if(pfPSTPatterns[fMemoryAddress]!=0)
	{
	  fTriggered=true;
	  pfPatchTriggerPattern[i]=(int)fMemoryAddress;
	  if(fImageTriggerTime==gOverflowTime)
	    {
	      fImageTriggerTime=fTimeTrigger;
	    }
	  //std::cout<<" T:"<<std::oct<<fMemoryAddress<<std::dec
	  //     <<" "<<pfPSTPatterns[fMemoryAddress]<<std::endl;
	}
      else
	{
	  pfPatchTriggerPattern[i]=0;
	  //std::cout<<" NT:"<<std::oct<<fMemoryAddress<<std::dec
	  //     <<" "<<pfPSTPatterns[fMemoryAddress]<<std::endl;
	}
    }
  return fTriggered;
}
// **************************************************************************

double KSPST::SlideWindow(std::vector<KSPixelTimes>& pfTimes, 
			 double fWindowWidth, int fMultiplicity)
// ****************************************************************************
// Search with a sliding WINDOW over KSTimes.fTime. pfTimes vector
// has been ordered by fTime. Search until we get Multiplicity in window. 
// Return the time of the trigger.
// ****************************************************************************
{
  int fNumSearch=pfTimes.size();
  for(int i=0;i<fNumSearch-fMultiplicity+1;i++)
    {
      double fStartWindow=pfTimes[i].fTime;
      if(fStartWindow>=gOverflowTime)
	{
	  break;
	}
      double fEndWindow=fStartWindow+fWindowWidth;
      for(int j=i+1;j<kNumPixelsPerPatch;j++)
	{
	  double fThisPixelTime=pfTimes[j].fTime;
	  if(fThisPixelTime>=gOverflowTime)
	    {
	      return gOverflowTime;
	    }
	  else if(fThisPixelTime>fEndWindow)
	    {
	      break;      //try next start time
	    }
	  else if((j-i+1)>=fMultiplicity)
	    {
	      return fThisPixelTime;
	    }
	}
    }
  return gOverflowTime;
}
// ****************************************************************************

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
  //   **The whole purpose of the following is to produce fPixelUV**
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
      int fISource=399;
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

  KSPixelUV fPixelUV(-12,25); // Special class to make an 2 dim array that
                              // has lower bounds of -12 each dimension and is
                              // 25 by 25 big. 

  for(int i=0;i<fNumPixels;i++)
    {
      fPixelUV.set(fUPosition[i],fVPosition[i],i);
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
      pfPixelsInPatch[fPatchNumber][j]=fPixelUV.get(fU,fV);
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
	      pfPixelsInPatch[fPatchNumber][j]=fPixelUV.get(fU,fV);
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
      fPixelUV[i].resize(fNumUVIndex);
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

