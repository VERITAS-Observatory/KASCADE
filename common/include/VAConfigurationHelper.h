//-*-mode:c++; mode:font-lock;-*-

/** \file VAConfigurationHelper.h
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

#ifndef VACONFIGURATIONHELPER_H
#define VACONFIGURATIONHELPER_H

#include <VAOptions.h>
#include <VAConfigInfo.h>
#include <VADataConverter.h>

template<typename T> inline void
doVAConfiguration(VAConfigInfo& file, VAOptions& command_line, 
		  const std::string& key, 
		  T* variable,
		  const std::string& category, 
		  const std::string& help="", bool option_is_simple=true)
{
  bool readonly = 
    command_line.findWithValue(key,*variable,help,option_is_simple)
    ==  VAOptions::FS_FOUND;
  file.setHandler(key, new VATemplateHandler<T>(variable, readonly),category);
  if(!help.empty())file.setHelpText(key,help);
}

template<typename T> inline void
doVAConfiguration(VAConfigInfo& file, VAOptions& command_line, 
		  const std::string& key, 
		  std::vector<T>* variable,
		  const std::string& category, 
		  const std::string& help="", bool option_is_simple=true)
{
  bool readonly = 
    command_line.findWithValue(key,*variable,help,option_is_simple)
    ==  VAOptions::FS_FOUND;
  file.setHandler(key, new VAVectorHandler<T>(variable, readonly),category);
  if(!help.empty())file.setHelpText(key,help);
}

template<> inline void
doVAConfiguration<std::string>(VAConfigInfo& file, VAOptions& command_line, 
			       const std::string& key, 
			       std::string* variable,
			       const std::string& category,
			       const std::string& help, 
			       bool option_is_simple)
{
  bool readonly = 
    command_line.findWithValue(key,*variable,help,option_is_simple)
    ==  VAOptions::FS_FOUND;
  VAStringHandler* handler = new VAStringHandler(variable);
  handler->setReadOnly(readonly);
  file.setHandler(key, handler,category);
  if(!help.empty())file.setHelpText(key,help);
}

template<typename T> inline void 
doVAConfiguration(VAConfigInfo& file, VAOptions& command_line, 
		  const std::string& key, 
		  T& variable,
		  const std::string& category,
		  const std::string& help="", bool option_is_simple=true)
{
  doVAConfiguration(file, command_line, key, &variable, category, help,
		    option_is_simple);
}

#endif // VACONFIGURATIONHELPER_H
