//-*-mode:c++; mode:font-lock;-*-
/**
 * \class ksSumVDFRootFiles
 * \brief This code Appends all the VDF (stage2 or kasAomega output) root
 *  files into a single file cutting on spectrum is designated
 *
 * Original Author: Glenn H. Sembroski 
 * $Author$
 * $Date$
 * $Revision$
 * $Tag$
 *
 **/

//Written by:
// G.H.Sembroski
//Physics Dept.
//Purdue Univ.
//West Lafayette, In. 479096
//sembrosk@physics.purdue.edu
//765-494-5172

// 15-May-2005
//Modified:


#include <iostream>
#include <vector>
#include <fstream>
#include <string>
#include <cmath>
#include <time.h>

int main(int argc, char** argv)
{
  argv++;
  std::string fInFileName=*argv;
  argv++;
  std::string fOutFileName=*argv;
  std::ifstream fIn;
  std::ofstream fOut;
  fIn.open(fInFileName.c_str());
  fOut.open(fOutFileName.c_str());
  std::vector<float> fWave;
  std::vector<float> fData;
  float fWaveLength;
  float fValue;
  while(1)
    {
      fIn>>fWaveLength>>fValue;
      if(!fIn.good())
	{
	  break;
	}
      fWave.push_back(fWaveLength);
      fData.push_back(fValue);
    }
  int fLast=fWave.size()-1;
  // Now interpolate
  for(float fW=180.;fW<705.;fW=fW+5.)
    {
      float fV=0;
      if(fW<fWave[0])
	{
	  fOut<<fW<<" "<<fV<<std::endl;
	}
      else if(fW>fWave[fLast])
	{
	  fOut<<fW<<" "<<fV<<std::endl;
	}
      else if(fW==fWave[fLast])
	{
	  fOut<<fW<<" "<<fData[fLast]<<std::endl;
	}
      else
	{
	  int fLow=0;
	  for(int i=0;i<fWave.size()-1;i++)
	    {
	      if(fW==fWave[i])
		{
		  fOut<<fW<<" "<<fData[i]<<std::endl;
		  break;
		}
	      else if(fW>fWave[i] && fW<fWave[i+1])
		{
		  fLow=i;
		  float fFraction=(fW-fWave[fLow])/(fWave[fLow+1]-fWave[fLow]);
		  float fV=fData[fLow]+fFraction*(fData[fLow+1]-fData[fLow]);
		  fOut<<fW<<" "<<fV<<std::endl;
		  break;
		}
	    }
	}
    }
  return 0;
}
      
