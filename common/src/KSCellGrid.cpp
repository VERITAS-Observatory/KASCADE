/**
 * \class KSCellGrid 
 * \ingroup common
 * \brief File of methods for KSCellGrid. Adaptaed from algoritm due to 
 *  Charlie Duke.
 *  
 
 * Original Author: Glenn H. Sembroski
 * $Author$
 * $Date$
 * $Revision$
 * $Tag$
 *
 **/
// This is where the code starts.

#include "KSCellGrid.h"

KSCellGrid::KSCellGrid(float* pCellXLocations, float* pCellYLocations, float* pCellRadius, int NumCells)
// ***************************************************************************
// First we save the cell loacitions and radii. Convert to double (just 
// because everything else is double) and save locally (probably for no good 
// reason) 
// ***************************************************************************
{
  fNumCells=NumCells;
  pfCellX.clear();
  pfCellX.resize(fNumCells);

  pfCellY.clear();
  pfCellY.resize(fNumCells);

  pfCellRadius.clear();
  pfCellRadius.resize(fNumCells);
  for(int i=0;i<fNumCells;i++)
    {
      pfCellX.at(i)      = (double)pCellXLocations[i];
      pfCellY.at(i)      = (double)pCellYLocations[i];
      pfCellRadius.at(i) = (double)pCellRadius[i];
    }
  // *************************************************************************
  // Now create the grid first index is x, second is y, third will be position 
  // in list. 
  // *************************************************************************
  // This will be a square grid(can be changed in future). Size of grid is 
  // always kGridSize by kGridSize
  // ******************************************************************* 
  fNumGridAreasX=kGridSize;
  fNumGridAreasY=kGridSize;
  pfCellGrid.resize(fNumGridAreasX);
  for(int i=0;i<fNumGridAreasX;i++)
    {
      pfCellGrid.at(i).resize(fNumGridAreasY);
      for(int j=0;j<fNumGridAreasY;j++)
	{
	  pfCellGrid.at(i).at(j).clear();   //Just to be sure.
	}
    }
  // **********************************************************************
  // Find minimum and max X and Y of grid.
  // **********************************************************************
  double fGridMin=pfCellX.at(0)-2*pfCellRadius.at(0);  //starting place  
  double fGridMax=pfCellX.at(0)+2*pfCellRadius.at(0);  //starting place  
  for(int i=0;i<fNumCells;i++)
    {
      if(pfCellX.at(i)-2*pfCellRadius.at(i)<fGridMin)
	{
	  fGridMin=pfCellX.at(i)-2*pfCellRadius.at(i);
	}
      if(pfCellY.at(i)-2*pfCellRadius.at(i)<fGridMin)
	{
	  fGridMin=pfCellY.at(i)-2*pfCellRadius.at(i);
	}
      if(pfCellX.at(i)+2*pfCellRadius.at(i)>fGridMax)
	{
	  fGridMax=pfCellX.at(i)+2*pfCellRadius.at(i);
	}
      if(pfCellY.at(i)+2*pfCellRadius.at(i)>fGridMax)
	{
	  fGridMax=pfCellY.at(i)+2*pfCellRadius.at(i);
	}
    }
  
  fGridXMin=fGridMin;//This makes it square
  fGridYMin=fGridMin;
  fGridXMax=fGridMax;
  fGridYMax=fGridMax;
  fGridAreaWidthX=(fGridXMax-fGridXMin)/fNumGridAreasX;
  fGridAreaWidthY=(fGridYMax-fGridYMin)/fNumGridAreasY;

  // *************************************************************************
  // Determine Max File-Of_veiw dist sqared (from center 0,0)
  // *************************************************************************
  if(fabs(fGridMin)>fabs(fGridMax))
    {
      fMaxFOV2=(2*fGridMin*fGridMin);  //2 is so we get diagonal
    }
  else
    {
      fMaxFOV2=(2*fGridMax*fGridMax);
    }

  // *************************************************************************
  // Now we can fill up the grid lists 
  // *************************************************************************
  double fGridAreaDiagonal=sqrt(fGridAreaWidthX*fGridAreaWidthX +
				fGridAreaWidthY*fGridAreaWidthY);
  KSCellSeperation fCellSep;
  for(int i=0;i<fNumGridAreasX;i++)
    {
      double fGridX=fGridXMin+i*fGridAreaWidthX+fGridAreaWidthX/2.;
      for(int j=0;j<fNumGridAreasY;j++)
	{
	  double fGridY=fGridYMin+j*fGridAreaWidthY+fGridAreaWidthY/2.;
	  //std::cout<<i<<" "<<j<<" "<<fGridX<<" "<<fGridY<<std::endl;
	  fCellSeperation.clear();
	  for(int k=0;k<fNumCells;k++)
	    {
	      double fCenterSeperation=sqrt(
			      (fGridX-pfCellX.at(k))*(fGridX-pfCellX.at(k)) +
			      (fGridY-pfCellY.at(k))* (fGridY-pfCellY.at(k)) );
            // The .87 is round up of .866=cos(30 deg) to get 
	    // max Hex cell(light cone) size.
	      if(fCenterSeperation<(fGridAreaDiagonal/2.0)+
		                                       (pfCellRadius.at(i)/.87) )
		{
		  // This Cell may have area in this GridArea
		  fCellSep.fSeperation=fCenterSeperation;
		  fCellSep.fIndex=k;
		  fCellSeperation.push_back(fCellSep);
		}
	    }
	  // **************************************************************
	  // Now sort by size of Center seperation. From smallest to largest
	  // Might do bettrer if we include raidus but this is good enough to
	  // greatly improve efficiency.
	  // ***************************************************************
	  if(fCellSeperation.size()==1)
	    {
	      pfCellGrid.at(i).at(j).push_back(fCellSeperation.at(0).fIndex);
	    }
	  else if(fCellSeperation.size()>1)
	  // ****************************************************************
	  // Since we defined the < operator for class KSCellSeperation to
	  // be a test of fSeperation
	  // this will sort by fSeperation and bring the fIndex along. Tricky!
	  // ****************************************************************
	    {
	      std::sort(fCellSeperation.begin(),fCellSeperation.end());
	      
	      // *************************************************************
	      // Now place sorted indices into our grid list
	      // *************************************************************
	      for(int m=0;m<(int)fCellSeperation.size();m++)
		{
		  pfCellGrid.at(i).at(j).push_back(fCellSeperation.at(m).fIndex);
		}
	    }
	}
    }
}
// ************************************************************************

KSCellGrid::~KSCellGrid()
{
  //nothing to do
}
// ************************************************************************


bool KSCellGrid::GetCellIndex(double XLocation, double YLocation, 
			                                    int& fCellIndex)
// ************************************************************************
// Get the cell index for this X,Y location
//  ************************************************************************
// This needs to be very fast. See if we are possibly in the field of view.
{
  double fDist2=(XLocation*XLocation+YLocation*YLocation);
  if(fDist2>fMaxFOV2)
    {
      fCellIndex=-1;
      return false;
    }
  
  // *****************************************************************
  // Find which grid area we are in.
  // *****************************************************************
  int fXIndex=(int)floor((XLocation-fGridXMin)/fGridAreaWidthX); 
                                       //Note we want a round down here
  if(fXIndex>=fNumGridAreasX || fXIndex<0)
    {
      fCellIndex=-1;
      return false;
    }

  int fYIndex=(int)floor((YLocation-fGridYMin)/fGridAreaWidthY);
  if(fYIndex>=fNumGridAreasY || fYIndex<0)
    {
      fCellIndex=-1;
      return false;
    }
  

  int fNumCellsToCheck=pfCellGrid.at(fXIndex).at(fYIndex).size();
  if(fNumCellsToCheck==0)
    {
      fCellIndex=-1;
      return false;
    }
  else
    {
      for(int i=0;i<fNumCellsToCheck;i++)
	{
	  fCellIndex=pfCellGrid.at(fXIndex).at(fYIndex).at(i);
	  double fX=XLocation-pfCellX.at(fCellIndex);
	  double fY=YLocation-pfCellY.at(fCellIndex);
	  double fLightConeWidth=2*pfCellRadius.at(fCellIndex);
	  bool fInCell = IsInCell(fX,fY,fLightConeWidth);
	  if(fInCell)
	    {
	      return true;
	    }
	}
      
      fCellIndex=-1;
      return false;  //No hits
    }
}
// ************************************************************************

bool KSCellGrid::IsInCell(double fCellXDeg,double fCellYDeg, double fCellWidth)
// ******************************************************************
// See if fCellXdeg,fCellYDeg is inside a light cone cell of 'width' 
// fCellWidth (this is full seperation width between two parallel sides of 
// hexagon, NOT seperation between opposite 'points' of the hexagon))
// **********************************************************************
//  0,0 of fCellXDeg,fCellYDeg is center of cell
// Cell volume can be divided into 6 quadrants but we only need angle to 
// quadrant bisector. This is simular to getPixelIndex but the cell is rotated 
// by 30 deg to camera rings.
{
  double fThetaDeg=0;
  if(fCellXDeg==0)
    {
      fThetaDeg=0;
    }
  else if(fCellYDeg==0)
    {
      fThetaDeg=30.;
    }
  else
    {
      fThetaDeg=atan(fabs(fCellYDeg/fCellXDeg))*gRad2Deg;
    }
  if(fThetaDeg>30)
    {
      fThetaDeg=fabs(60.-fThetaDeg);
    }
  double fPerpDistanceDeg=sqrt(fCellXDeg*fCellXDeg+fCellYDeg*fCellYDeg)*
                                                     cos(fThetaDeg*gDeg2Rad);
  if(fPerpDistanceDeg<=fCellWidth/2.)
    {
      return true;
    }
  else
    {
      return false;
    }
}
// ************************************************************************
