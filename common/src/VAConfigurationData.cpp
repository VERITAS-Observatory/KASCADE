//-*-mode:c++; mode:font-lock;-*-

/**
 * \class VAConfigurationData
 * \ingroup common
 * \brief Class which contains algorithm name and configuration values
 *
 * Here is a tedious verbose multi-line description of all
 * the details of the code, more than you would
 * ever want to read. Generally, all the important documentation
 * goes in the .cpp files.
 *
 * Original Author: Stephen Fegan
 * $Author$
 * $Date$
 * $Revision$
 * $Tag$
 *
 **/
// This is where the code starts.

// These deinitions tell the makefile which library the cpp file
// should be included in
// VA_LIBRARY_TAG: libSP24common.a
// VA_LIBRARY_TAG: libSP24commonLite.a

#include "VAConfigurationData.h"

#ifndef _NOROOT
   ClassImp(VAConfigurationData)
#endif

VAConfigurationData::~VAConfigurationData()
{
  // nothing to see here
}

void VAConfigurationData::
mergeConfigurationData(const std::string& key, const VAConfigurationData& data)
{
  fConfig[key]=data.fName;
  for(std::map<std::string, std::string>::const_iterator ipair = 
	data.fConfig.begin(); ipair != data.fConfig.end(); ipair++)
    {
      std::string out_key = key + std::string("->") + ipair->first;
      fConfig[out_key]=ipair->second;
    }
}

std::ostream& operator<< (std::ostream& stream, 
			  const VAConfigurationData& data)
{
  stream << data.fName << std::endl;
  for(std::map<std::string, std::string>::const_iterator ipair = 
	data.fConfig.begin(); ipair != data.fConfig.end(); ipair++)
    stream << ipair->first << '=' << ipair->second << std::endl;
  return stream;
}

#ifdef TEST_MAIN

int main(int argc, const char** argv)
{
  VAConfigurationData config("VAPedCalcSimple");
  config.setValue("PedCutLevel",float(35.0));
  config.setValue("PedCutMaxChannels",int(10));
  
  VAConfigurationData config2("VATraceAnalyzerFFT");
  config2.mergeConfigurationData("BaselinePedAlgorithm",config);
  config2.setValue("FilterWidth",float(45.0));

  std::cout << config2;
}

#endif
