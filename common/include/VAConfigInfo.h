//-*-mode:c++; mode:font-lock;-*-

/** \file VAConfigInfo.h
 * \brief Extension to Karl's ConfigInfo package
 * \see CommandHandler
 *
 * Original Author: Stephen Fegan
 * $Author$
 * $Date$
 * $Revision$
 * $Tag$
 *
 **/

#ifndef VACONFIGINFO_H
#define VACONFIGINFO_H

#include <vector>

#include <VSDataConverter.hpp>

#include <ConfigInfo.h> // Include Karl's code
#include <CommandHandlers.h> // Include Karl's code

//! Typedef of ConfigInfo into VA convention
typedef ConfigInfo VAConfigInfo;
//! Typedef of BoolHandler into VA convention
typedef BoolHandler VABoolHandler;
//! Typedef of FloatHandler into VA convention
typedef FloatHandler VAFloatHandler;
//! Typedef of DoubleHandler into VA convention
typedef DoubleHandler VADoubleHandler;
//! Typedef of IntHandler into VA convention
typedef IntHandler VAIntHandler;
//! Typedef of StringHandler into VA convention
typedef StringHandler VAStringHandler;
//! Typedef of RangeHandler into VA convention
typedef RangeHandler VARangeHandler;
//! Typedef of EnumHandler into VA convention
typedef EnumHandler VAEnumHandler;

template<typename T> class VATemplateHandler: public CommandHandler
{
public:
  VATemplateHandler(T* variable, bool read_only = false): 
    CommandHandler(), fVariable(variable) { setReadOnly(read_only); }
  virtual ~VATemplateHandler() { /* nothing to see here */ }

  virtual void set(std::vector<std::string> &arg)
  {
    if(arg.size() != 1)
      {
	std::string message = 
	  std::string("A single ")
	  + VSDataConverter::typeName<T>()
	  + std::string(" was expected");
	throw ConfigSyntaxError(message);
      }
    
    if(!VSDataConverter::fromString(*fVariable,arg[0]))
      {
	std::string message = 
	  std::string("Could not convert configuration argument \"")
	  + arg[0] 
	  + std::string("\" to a ")
	  + VSDataConverter::typeName<T>();
	throw ConfigSyntaxError(message);
      }
  }

  virtual std::string toString()
  {
    return VSDataConverter::toString(*fVariable,true);
  }

private:
  T* fVariable;
};

template<typename T> class VAVectorHandler: public CommandHandler
{
public:
  VAVectorHandler(std::vector<T>* variable, bool read_only = false): 
    CommandHandler(), fVariable(variable) { setReadOnly(read_only); }
  virtual ~VAVectorHandler() { /* nothing to see here */ }

  virtual void set(std::vector<std::string> &arg)
  {
    fVariable->clear();
    for(std::vector<std::string>::const_iterator iarg=arg.begin();
	iarg!=arg.end(); iarg++)
      {
	T variable;
	if(!VSDataConverter::fromString(variable,*iarg))
	  {
	    std::string message = 
	      std::string("Could not convert configuration argument \"")
	      + *iarg
	      + std::string("\" to a ")
	      + VSDataConverter::typeName<T>();
	    throw ConfigSyntaxError(message);
	  }
	fVariable->push_back(variable);
      }
  }

  virtual std::string toString()
  {
    return VSDataConverter::toString(*fVariable,true);
  }

private:
  std::vector<T>* fVariable;
};

#endif // VACONFIGINFO_H
