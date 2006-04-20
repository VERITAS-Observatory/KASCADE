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
  pfCellX= new double[fNumCells];
  pfCellY= new double[fNumCells];
  pfCellRadius= new double[fNumCells];
  for(int i=0;i<fNumCells;i++)
    {
      pfCellX[i]      = (double)pCellXLocations[i];
      pfCellY[i]      = (double)pCellYLocations[i];
      pfCellRadius[i] = (double)pCellRadius[i];
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
      pfCellGrid[i].resize(fNumGridAreasY);
      for(int j=0;j<fNumGridAreasY;j++)
	{
	  pfCellGrid[i][j].clear();   //Just to be sure.
	}
    }
  // **********************************************************************
  // Find minimum and max X and Y of grid.
  // **********************************************************************
  double fGridMin=pfCellX[0]-2*pfCellRadius[0];  //starting place  
  double fGridMax=pfCellX[0]+2*pfCellRadius[0];  //starting place  
  for(int i=0;i<fNumCells;i++)
    {
      if(pfCellX[i]-2*pfCellRadius[i]<fGridMin)
	{
	  fGridMin=pfCellX[i]-2*pfCellRadius[i];
	}
      if(pfCellY[i]-2*pfCellRadius[i]<fGridMin)
	{
	  fGridMin=pfCellY[i]-2*pfCellRadius[i];
	}
      if(pfCellX[i]+2*pfCellRadius[i]>fGridMax)
	{
	  fGridMin=pfCellX[i]+2*pfCellRadius[i];
	}
      if(pfCellY[i]+2*pfCellRadius[i]>fGridMax)
	{
	  fGridMin=pfCellY[i]+2*pfCellRadius[i];
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
      fMaxFOV2=(2*fGridMin*fGridMin);
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
	  fCellSeperation.clear();
	  for(int k=0;k<fNumCells;k++)
	    {
	      double fCenterSeperation=sqrt(
				    (fGridX-pfCellX[k])*(fGridX-pfCellX[k]) +
				    (fGridY-pfCellY[k])*(fGridY-pfCellY[k]) );
	              // the .87 is round up of .866=cos(30 deg) to get 
	              // max Hex cell(light cone) size.
	      if(fCenterSeperation<fGridAreaDiagonal+pfCellRadius[i]/.87)
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
	      pfCellGrid[i][j].push_back(fCellSeperation[0].fIndex);
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
		  pfCellGrid[i][j].push_back(fCellSeperation[m].fIndex);
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
  int fYIndex=(int)floor((YLocation-fGridYMin)/fGridAreaWidthY);
  
  int fNumCellsToCheck=pfCellGrid[fXIndex][fYIndex].size();
  if(fNumCellsToCheck==0)
    {
      fCellIndex=-1;
      return false;
    }
  else
    {
      for(int i=0;i<fNumCellsToCheck;i++)
	{
	  fCellIndex=pfCellGrid[fXIndex][fYIndex][i];
	  double fX=XLocation-pfCellX[fCellIndex];
	  double fY=YLocation-pfCellY[fCellIndex];
	  double fLightConeWidth=pfCellRadius[fCellIndex];
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
