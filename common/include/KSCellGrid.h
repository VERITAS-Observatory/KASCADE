/**
 * Original Author: Glenn H. Sembroski
 * $Author$
 * $Date$
 * $Revision$
 * $Tag$
 *
 **/
// This is a class that holds all the stuff needed to create and then access a 
// Cell array lookup grid. Can be used for both pixels and mirror facets. 
// Main use is to find cell(pixel or facet) for a particular pe location (in 
// a focal plane for pixels or the mirror plane for facets)
// This is C++ implimentation of an algorthm originally from Charlie Duke.


#ifndef KSCELLGRID_H
#define KSCELLGRID_H

#include <iostream>
#include <vector>
#include <cmath>

#include "KSCommon.h"


const int kGridSize = 20;  //Charlie Duke says he uses '10 or so' here.

// ***********************************************************************
// Little class to use when sorting through Cell seperartions from center of 
// areas
// ***********************************************************************
class KSCellSeperation
{
 public:
  double fSeperation;
  int fIndex;
  inline bool operator < (const KSCellSeperation& o) const;//This for use with sort
};
inline bool KSCellSeperation::operator < (const KSCellSeperation& o) const
{
  return fSeperation<o.fSeperation;
}
// ***********************************************************************



class KSCellGrid
// *******************************************************
// ** CellGrid class for KASACADE
// *******************************************************
// Nomenclature: Cell=single pixel or facet area(usually hexagonal in shape)
//               Grid=An array of square(or possibly rectangular for unknown 
//                    reasons in the future) areas that lays over the Cells.
// pfGrid will be a 2 dim array(as a vector of vectors) of lists of cell 
// indices( as a vector)for Cells with area in each grid square.
{
 public:
  KSCellGrid(float* pCellXLocations, float* pCellYLocations, 
	                                   float* pCellRadius, int NumCells);
  virtual ~KSCellGrid();
  bool GetCellIndex(double XLocation, double YLocation, int& fCellIndex);
  bool IsInCell(double XLocation,double YLocation, double CellRadius);

  double fMaxFOV2;

	
 private:
  std::vector< std::vector< std::vector<int> > >  pfCellGrid;
  std::vector< KSCellSeperation> fCellSeperation;
  double* pfCellX;
  double* pfCellY;
  double* pfCellRadius;

  int fNumCells;

  double fGridXMin;
  double fGridYMin;
  double fGridXMax;
  double fGridYMax;
  int fNumGridAreasX;
  int fNumGridAreasY;
  double fGridAreaWidthX;
  double fGridAreaWidthY;
};
// ***************************************************************************
#endif
