//-*-mode:c++; mode:font-lock;-*-

/** \file VAAlgorithm.h
 *  \ingroup common
 *
 * Original Author: Stephen Fegan
 * $Author$
 * $Date$
 * $Revision$
 * $Tag$
 *
 **/

// This is where the code starts.

#ifndef VAALGORITHM_H
#define VAALGORITHM_H

#include "VAConfigurationData.h"

//! Base class for algorithms which can supply their configuarions for storage
class VAAlgorithm
{
public:
  virtual ~VAAlgorithm();
  //! Return the configuration of the algorithm
  virtual VAConfigurationData getConfig() const = 0;
};  

#endif 
