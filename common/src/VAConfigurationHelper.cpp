//-*-mode:c++; mode:font-lock;-*-

/** \file VAConfigurationHelper.cpp
 * \brief Helper functions to clean up the configuration functions
 * \see ConfigInfo
 * \see CommandHandler
 * \see VAConfigInfo.hpp
 *
 * Original Author: Stephen Fegan
 * $Author$
 * $Date$
 * $Revision$
 * $Tag$
 *
 **/

// These deinitions tell the makefile which library the cpp file
// should be included in
// VA_LIBRARY_TAG: libSP24common.a
// VA_LIBRARY_TAG: libSP24commonLite.a

#include <VAConfigurationHelper.h>

#ifdef TEST_MAIN

// ----------------------------------------------------------------------------
// File: VASomeClass.h
// ----------------------------------------------------------------------------

class VASomeClass
{
public:
  VASomeClass(std::string some_string = gDefaultString, 
	      uint32_t some_uint32 = gDefaultUint32,
	      double some_double = gDefaultDouble):
    fString(some_string), fUint32(some_uint32), fDouble(some_double) { }

  void doSomething();

  static void configure(VAConfigInfo& file, VAOptions& command_line);
private:
  // Member variables of this instance
  std::string          fString;
  uint32_t             fUint32;
  double               fDouble;

  // Static
  static std::string   gDefaultString;
  static uint32_t      gDefaultUint32;
  static double        gDefaultDouble;
};

// ----------------------------------------------------------------------------
// File: VASomeClass.cpp
// ----------------------------------------------------------------------------

std::string   VASomeClass::gDefaultString = "Hello there";
uint32_t      VASomeClass::gDefaultUint32 = 42;
double        VASomeClass::gDefaultDouble = 12.5;

void VASomeClass::doSomething()
{
  std::cout << "String:  " << fString << std::endl;
  std::cout << "Uint32:  " << fUint32 << std::endl;
  std::cout << "Double:  " << fDouble << std::endl;
}

void VASomeClass::configure(VAConfigInfo& file, VAOptions& command_line)
{
  doVAConfiguration(file, command_line, 
		    "VASC_String", gDefaultString, "SomeClass",
		    "Set the string member variable of the class to some "
		    "value that will be used by the class in some way. This "
		    "text is intentionally very long to see how the options "
		    "processing package prints it.");
  doVAConfiguration(file, command_line, 
		    "VASC_Uint32", gDefaultUint32, "SomeClass",
		    "Set the Uint32 variable");
  doVAConfiguration(file, command_line, 
		    "VASC_Double", gDefaultDouble, "SomeClass",
		    "Set the Double variable");
}

// ----------------------------------------------------------------------------
// File: stage1.cpp
// ----------------------------------------------------------------------------

void usage(const std::string& progname, const VAOptions& command_line)
{
  std::cerr << "Usage: " << progname << " [options]" << std::endl;
  std::cerr << std::endl;
  std::cerr << "Options:" << std::endl;
  command_line.printUsage(std::cerr);
}

int main(int argc, char** argv)
{
  std::string progname = *argv;

  VAOptions command_line(argc,argv);
  VAConfigInfo config_file;

  // --------------------------------------------------------------------------
  // Configure ALL classes (usually through factories)
  // --------------------------------------------------------------------------

  VASomeClass::configure(config_file,command_line);

  // --------------------------------------------------------------------------
  // Test for "help" options
  // --------------------------------------------------------------------------
 
  bool print_usage = false;
  if(command_line.find("help","Print this message") 
     != VAOptions::FS_NOT_FOUND)print_usage = true;
  if(command_line.find("h","Print this message") 
     != VAOptions::FS_NOT_FOUND)print_usage = true;

  // --------------------------------------------------------------------------
  // Test for "load configuration file" options
  // --------------------------------------------------------------------------

  bool load_config = false;
  std::string load_filename;
  if(command_line.findWithValue("config",load_filename,
				"Load configuration file.")
     == VAOptions::FS_FOUND)
    load_config = true;

  // --------------------------------------------------------------------------
  // Test for "save configuration file" options
  // --------------------------------------------------------------------------

  bool save_config = false;
  bool only_save_config = false;
  std::string save_filename;  
  if(command_line.findWithValue("save_config",save_filename,
				"Save a configuration file with all "
				"configuration values before processing.") 
     == VAOptions::FS_FOUND)
    save_config=true;
  if(command_line.findWithValue("save_config_and_exit",save_filename,
				"Save a configuration file with all "
				"configuration values and immediately exit.")
     == VAOptions::FS_FOUND)
    only_save_config=true;

  // --------------------------------------------------------------------------
  // All the command line options that the program is able to  handle have been
  // processed, so make sure there are no more command lines options available.
  // --------------------------------------------------------------------------

  if(!command_line.assertNoOptions())
    {
      std::cerr << progname << ": unknown options: ";
      for(unsigned i=1;i<argc;i++)std::cerr << argv[i];
      std::cerr << std::endl;
      std::cerr << std::endl;
      usage(progname, command_line);
      exit(EXIT_FAILURE);
    }
  
  if(print_usage)
    {
      usage(progname, command_line);
      exit(EXIT_SUCCESS);
    }

  // --------------------------------------------------------------------------
  // Load the configuration file if we have been asked to 
  // --------------------------------------------------------------------------

  if(load_config)config_file.loadConfigFile(load_filename);

  // --------------------------------------------------------------------------
  // Save the configuration file if we have been asked to 
  // --------------------------------------------------------------------------
  
  if((save_config)||(only_save_config))
    config_file.saveConfigFile(save_filename);
  if(only_save_config)exit(EXIT_SUCCESS);

  // --------------------------------------------------------------------------
  // Begin normal processing...
  // --------------------------------------------------------------------------

  VASomeClass x;
  x.doSomething();
}

#endif
