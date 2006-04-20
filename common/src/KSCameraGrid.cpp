/**
 * \class KSCameraGrid
 * \ingroup common
 * \brief File of methods for KSCameraGrid.
 
 * Original Author: Glenn H. Sembroski
 * $Author$
 * $Date$
 * $Revision$
 * $Tag$
 *
 **/
// This is where the code starts.

#include <iostream>

#include "KSCameraGrid.h"

// In the constructor is where most of the work gets done.
KSCameraGrid::KSCameraGrid(int fNumOfRings)
// ****************************************************************************
// 	Get PMT locations in a standard Generic (hex pattern) 
// ****************************************************************************
{
                                  // Allocate grid arrays.
  fSize=(3*fNumOfRings*(fNumOfRings+1)+1);	
  //std::cout<<"KSCameraGrid:fSize: "<<fSize<<std::endl;

  pfXIndex = new int[fSize];
  pfYIndex = new int[fSize];
  // ************************************************************************
  //	Fill the integer arrays, pfXIndex,pfYIndex, with photomultiplier
  //	X and Y lattice indexs. The  PMT labeling convention is
  //	assumed. The number of rings to be filled is specified by fNumOfRings.
  // ************************************************************************
  //  Since each ring is actually a hexagon, we have 6 sides to step through.
  // Modified:
  //    13/03/06 SRC Converted to c++, comments added
  // The way this works is that the program starts at a position to the right 
  // of the    center PMT.  It then skips this and moves according to the 
  // program (follow the code to see how it works).  When it gets back to the 
  // PMT(the one it started with), which   is right of center, it will then 
  // read in its value and then jump over one to the right.  This then starts 
  // the code again and does the next ring until it reaches the last PMT's in 
  // the outer most ring.
  // ************************************************************************  
  //	Coords of center, not considered a 'ring'.
  int fXStep=0;  //this is the step in the x-direction
  int fYStep=0;  //this is the step in the y-direction
  //	Set values of tube #1
  pfXIndex[0]=fXStep;            //sets up array for location of X
  pfYIndex[0]=fYStep;            //sets up array for location of Y
  if (fNumOfRings >0)
    {
      //  Move to start of first ring at pmt index #2
      int fPMTIndex=2;
      //	Go through the rings.
      for(int l=1; l<=fNumOfRings; l++)    
	{
	  //  Save pointer to start(sort of, this is a funny case, see last
	  // 'side' at end of side for loop).
	  int fFirstIndexInRing=fPMTIndex;
	  
	  fXStep=fXStep+2;      //  Each ring has 1 more pmt per 'side'Ring #1 
	  // has 1, ring#2 has 2 etc.
	  // Step up 1 row from end of last ring.
	  fPMTIndex=fPMTIndex+1;                   //Bump pmt index.
	  
	  //Side #1
	  for(int i=0;i<l;i++)
	    {
	      fXStep=fXStep-1;	//Side #1 steps down half a tub dia and to the
	      fYStep=fYStep-2;	//left a complete tube dia.
	      pfXIndex[fPMTIndex-1]=fXStep;
	      pfYIndex[fPMTIndex-1]=fYStep;
	      fPMTIndex=fPMTIndex+1;
	    }
	  
	  //Side #2
	  for(int i=0;i<l;i++)	 //Move to the left along a row.
	    {
	      fXStep=fXStep-2;
	      pfXIndex[fPMTIndex-1]=fXStep;
	      pfYIndex[fPMTIndex-1]=fYStep;
	      fPMTIndex=fPMTIndex+1;
	    }
	  
	  //Side #3
	  for(int i=0;i<l;i++)	 //Step up 1 row each time and 1/2 pmt to left.
	    {
	      fXStep=fXStep-1;
	      fYStep=fYStep+2;
	      pfXIndex[fPMTIndex-1]=fXStep;
	      pfYIndex[fPMTIndex-1]=fYStep;
	      fPMTIndex=fPMTIndex+1;
	    }
	  
	  //Side #4
	  for(int i=0;i<l;i++)
	    {                 
	      fXStep=fXStep+1;        // Move up to the top of the 'ring'.
	      fYStep=fYStep+2;	  // step up a row each time and to the right
	      pfXIndex[fPMTIndex-1]=fXStep;	 // 1/2 pmt each time.
	      pfYIndex[fPMTIndex-1]=fYStep;
	      fPMTIndex=fPMTIndex+1;
	    }
	  
	  //Side #5
	  for(int i=0;i<l;i++)
	    {
	      fXStep=fXStep+2;	   //Along the top of the 'ring now. Step to
	      pfXIndex[fPMTIndex-1]=fXStep;	  //the right 1 pmt each time.
	      pfYIndex[fPMTIndex-1]=fYStep;
	      fPMTIndex=fPMTIndex+1;
	    }
	  
	  //Side #6
	  for(int i=0;i<l;i++) //Going back towards the starting point of this
	    {	               //'ring'. Step down 1 row each time and 1/2 to
	      fXStep=fXStep+1; //the right each time.	
	      fYStep=fYStep-2;   
	      
	      // This is funny and I haven't quit figured it out but it does 
	      // work.  I think the start of the first ring is weird and all 
	      // the others start 1 over, so except for the first one, finish 
	      // up the original PMT (in fFirstIndexInRing) for this ring.
	      if (fYStep != 0)  	//First ring only?(I may be wrong!)
		{
		  pfXIndex[fPMTIndex-1]=fXStep;
		  pfYIndex[fPMTIndex-1]=fYStep;
		  fPMTIndex=fPMTIndex+1;
		}
	      else	        //Do first PMT of this ring which was
		{               //skipped(fFirstIndexInRing)
		  pfXIndex[fFirstIndexInRing-1]=fXStep;
		  pfYIndex[fFirstIndexInRing-1]=fYStep;
		}
	    }
	}
    }
}                              
// *************************************************************************


KSCameraGrid::~KSCameraGrid()
{
  //nothing to do
}
// *************************************************************************
