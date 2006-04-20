/**
 * Original Author: Glenn H. Sembroski
 * $Author$
 * $Date$
 * $Revision$
 * $Tag$
 *
 **/
// This is a class that holds all the stuff needed by ksTrigger and ksAomega
// for a camera


#ifndef KSCAMERAGRID_H
#define KSCAMERAGRID_H

#include "KSCommon.h"

class KSCameraGrid
// *******************************************************
// ** CameraGrid class for KASACADE
// *******************************************************
{
 public:
  KSCameraGrid(int fNumOfRings);
  virtual ~KSCameraGrid();

 public:
  int* pfXIndex;
  int* pfYIndex;
  int fSize;
};
// ***************************************************************************


#endif
