//-*-mode:c++; mode:font-lock;-*-

/**
 *
 * Original Author: Stephen Fegan
 * $Author$
 * $Date$
 * $Revision$
 * $Tag$
 *
 **/

#ifndef VACONFIGURATIONDATA_H
#define VACONFIGURATIONDATA_H

#include<ostream>
#include<string>
#include<map>
#include<vector>

#ifndef _NOROOT
  #include <TROOT.h> 
  #include <TObject.h>
#endif

#include<VSDataConverter.hpp>

// ----------------------------------------------------------------------------
// Algorithm configuation data
// ----------------------------------------------------------------------------

//#ifdef _NOROOT
class VAConfigurationData
//#else
// class VAConfigurationData: public TObject
//#endif
{
public:
  VAConfigurationData(const std::string& name=""): fName(name), fConfig() { }
  VAConfigurationData(const std::string& name, 
		      const std::map<std::string, std::string> config):
    fName(name), fConfig(config) { }
  virtual ~VAConfigurationData();

  //! Integrate the sub-class configuration data
  void mergeConfigurationData(const std::string& key,
			      const VAConfigurationData& data);
  
  //! Add a key/value pair to the configuration. Convert value to string.
  template<typename T> void setValue(const std::string& key, const T& val);
  //! Get a value from the configuration. Convert string back to value.
  template<typename T> bool getValue(const std::string& key, T& val);

  //! Name of the algorithm
  std::string                         fName;
  //! Set of Name/Value pairs which describe configuration of algorithm
  std::map<std::string, std::string>  fConfig;

  //#ifndef _NOROOT
  //C l a s s D e f (VAConfigurationData,1); // Class which contains algorithm name and configuration values
  //#endif
};

std::ostream& operator<< (std::ostream& stream, 
			  const VAConfigurationData& data);

template<typename T> void VAConfigurationData::
setValue(const std::string& key, const T& val)
{
  std::string str = VSDataConverter::toString(val);
  fConfig[key]=str;
}

template<typename T> bool VAConfigurationData::
getValue(const std::string& key, T& val)
{
  val = T();
  if(fConfig.find(key) == fConfig.end())return false;
  return VSDataConverter::fromString(val,fConfig[key]);
}

#endif
