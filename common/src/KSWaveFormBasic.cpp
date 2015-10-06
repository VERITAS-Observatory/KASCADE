/**
 * \class KSWaveForm
 * \ingroup common
 * \brief Class to hold and minupulate a single pixel waveform.
 *  
 * Original Author: Glenn H. Sembroski
 * $Author$
 * $Date$
 * $Revision$
 * $Tag$
 *
 **/
// This is where the code starts.

#include "KSWaveFormBasic.h"


KSWaveFormBasic::KSWaveFormBasic()  //Mainly for basic stuff. like when used as a 
{                         // holder of templates
  //nothing to do
}
// *********************************************************************

KSWaveFormBasic::~KSWaveFormBasic()
{
  //nothing to do
}
// *********************************************************************

double KSWaveFormBasic::GetWaveFormSum()
{
  int    endIndex     = fWaveForm.size()-1;
  double waveFormsum  = GetWaveFormSum(0, endIndex);
  return waveFormsum;
}

double KSWaveFormBasic::GetWaveFormSum(int StartBin, int EndBin)
{
  double waveFormSum = 0.;
  for( int i = StartBin; i<EndBin+1; i++) {
    waveFormSum += fWaveForm.at( i );
  }
  return  waveFormSum;
}
// *********************************************************************

void KSWaveFormBasic::ScaleWaveForm(double scaleFactor)
{
  for( int i=0; i< (int) fWaveForm.size(); i++) {
    fWaveForm.at(i) *= scaleFactor;
  }
  return;
}
// ***************************************************************

double KSWaveFormBasic::GetWaveFormMax()
{
  std::vector< double >::iterator it;   //for pulse array
  // ******From max of waveFrom
  it = max_element(fWaveForm.begin(), fWaveForm.end());
  return  *it;
}
// *********************************************************************


double KSWaveFormBasic::GetWaveFormFWHMns()
{
  // ********************************************************************
  // This very crude FWHM. Only use for debugging
  // ********************************************************************
  double halfMax = GetWaveFormMax() / 2.0;
  int lowEdgeBin  = 0;
  int highEdgeBin = 0;
  for( int i=0; i< (int) fWaveForm.size(); i++) {
    if (fWaveForm.at(i) > halfMax ) {
      lowEdgeBin=i;
      break;
    }
  }
  //+8 just to make sure we are not fooled by local noise
  for( int i=lowEdgeBin+8 ; i< (int) fWaveForm.size(); i++) {
    if (fWaveForm.at(i) < halfMax ) {
      highEdgeBin=i;
      break;
    }
  }
  double FWHMns = ( highEdgeBin -lowEdgeBin ) * gWaveFormBinSizeNS;
  return FWHMns;
}
// ******************************************************************

