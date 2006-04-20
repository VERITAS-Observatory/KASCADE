//-*-mode:c++; mode:font-lock;-*-

// This file is shared between the Simulations and OAWG. To avoid a
// fork in development, please coordinate changes with Stephen Fegan.
// email: sfegan@astro.ucla.edu

// Old header for consistency with Simulations package

/*! \file VSOptions.hpp
  Class for processing of command line options

  \author     Stephen Fegan               \n
              UCLA                        \n
              sfegan@astro.ucla.edu       \n        

  \version    0.1
  \date       03/02/2005
*/

// New header for consistency with OAWG package

/**
 *
 * Original Author: Stephen Fegan
 * $Author$
 * $Date$
 * $Revision$
 * $Tag$
 *
 **/

#ifndef VSOPTIONS_HPP
#define VSOPTIONS_HPP

#include<ostream>
#include<sstream>
#include<vector>
#include<list>
#include<map>
#include<string>
#include<memory>
#include<cassert>

#include<VSDataConverter.hpp>

#ifndef _OAWG
namespace VERITAS
{
#endif

  //! Class encapsulating extraction of data type from option string
  template<typename T> class VSOptionValueExtractor
  {
  public:
    static inline bool extract(const std::string& buffer, T& value);
    static inline std::string typeName();
  };
  
  //! Command line options processor
  class VSOptions
  {
  public:
    enum FindStatus { FS_NOT_FOUND, FS_FOUND, 
		      FS_FOUND_WITH_UNDESIRED_VALUE,
		      FS_FOUND_BUT_WITHOUT_VALUE, 
		      FS_FOUND_BUT_COULD_NOT_CONVERT_VALUE };

    VSOptions(int& argc, char**argv, bool print_default = false);
    virtual ~VSOptions();
    
    void addOption(const std::string& opt, bool override = false);
    void addOptionWithValue(const std::string& opt, const std::string& val,
			    bool override = false);

    const std::string& arg0() const { return fArg0; }
    int argC() const { return fArgC; }
    char** argV() const { return fArgV; };
    
    inline FindStatus find(const std::string& key, const std::string& help="",
			   bool option_is_simple = true);
    
    template<typename T> inline FindStatus 
    findWithValue(const std::string& key, T& value, 
		  const std::string& help="", bool option_is_simple = true);
    
    bool assertNoOptions() const;

    void printUsage(std::ostream& stream, bool print_only_simple_options=false)
      const;

  private:
    struct OptionInfo
    {
      std::string                               fVal;
      bool                                      fValAttached;
      std::vector<unsigned>                     fArgBlank;
      std::vector<unsigned>                     fValBlank;
      OptionInfo(): fVal(), fValAttached(false), fArgBlank(), fValBlank() {}
    };

    struct OptionHelp
    {
      std::string                               fOption;
      bool                                      fArgRequired;
      std::string                               fArgTypeName;
      std::string                               fText;
      bool                                      fIsSimple;
      OptionHelp(std::string option, bool arg_required, 
		 std::string arg_type_name, std::string text,
		 bool option_is_simple = true):
	fOption(option), fArgRequired(arg_required), 
	fArgTypeName(arg_type_name), fText(text), 
	fIsSimple(option_is_simple) { }
    };

    VSOptions();
    VSOptions(const VSOptions&);
    const VSOptions& operator =(const VSOptions&);
    
    FindStatus doFind(const std::string& key, std::string* value,
		      const std::string& help, bool option_is_simple,
		      const std::string& type);

    std::map<std::string, OptionInfo> fOptions;
    std::string                       fArg0;
    std::vector<char*>                fArgs;
    int&                              fArgC;
    char**                            fArgV;

    bool                              fPrintDefault;
    std::list<OptionHelp>             fUsageInfo;
  };

  template<typename T> inline bool 
  VSOptionValueExtractor<T>::extract(const std::string& buffer, T& value)
  {
    return(VSDatumConverter<T>::fromString(value,buffer.c_str()));
  }

  template<typename T> inline std::string
  VSOptionValueExtractor<T>::typeName()
  {
    return(VSDatumConverter<T>::typeName());
  }
  
  inline VSOptions::FindStatus VSOptions::find(const std::string& key,
					       const std::string& help,
					       bool option_is_simple)
  {
    return doFind(key,0,help,option_is_simple,"");
  }
  
  template<typename T> inline VSOptions::FindStatus 
  VSOptions::findWithValue(const std::string& key, T& value, 
			   const std::string& help, bool option_is_simple)
  {
    std::string val_string;
    std::string type = VSOptionValueExtractor<T>::typeName();
    if(fPrintDefault)
      {
	std::string default_value;
	VSDataConverter::toString(default_value,value,true);
	if(default_value.empty())default_value="\"\"";
	type+="=";
	type+=default_value;
      }
    FindStatus status = doFind(key, &val_string, help, option_is_simple, type);

    if((status==FS_NOT_FOUND)||(status==FS_FOUND_BUT_WITHOUT_VALUE))
      return status;
    if(!VSOptionValueExtractor<T>::extract(val_string,value))
      return FS_FOUND_BUT_COULD_NOT_CONVERT_VALUE;
    return FS_FOUND;
  }
  
#ifndef _OAWG
}
#endif

#endif // VSOPTIONS_HPP
