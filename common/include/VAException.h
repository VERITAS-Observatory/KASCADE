/**
 * \file VAException.h
 * 
 * Original Author: John Quinn
 * $Author$
 * $Date$
 * $Revision$
 * $Tag$
 *
 **/

#ifndef VAEXCEPTION_H
#define VAEXCEPTION_H

#include <vector>
#include <string>
#include <sstream>

#include <errno.h>
#include "VException.h"





//--------------------------------------------------------------------------------
// VABackTrace
//--------------------------------------------------------------------------------
class VABackTrace
{
 public:
  virtual ~VABackTrace(){};
  
  void push(const char* fn) { fBackTrace.push_back(fn); }
  void pop() { fBackTrace.pop_back(); }
  std::vector<std::string> backTrace() const;
  std::string backTraceString() const;
  
  // Singleton
  inline static VABackTrace* instance();
  
 private:
  VABackTrace(): fBackTrace() { }; 
  std::vector<const char*> fBackTrace;
  
  static VABackTrace* pfgInstance;
};

inline VABackTrace* VABackTrace::instance()
{
  if(pfgInstance == 0)pfgInstance = new VABackTrace;
  return pfgInstance;
}


//--------------------------------------------------------------------------------
// VARegisterThisFunction
//--------------------------------------------------------------------------------
/*
A guard-like function which when created registers a string with VABackTrace
and when destructed removes the string. 
*/


class VARegisterThisFunction
{
 public:
  VARegisterThisFunction(const char* fn);
  virtual ~VARegisterThisFunction();
 private:
  inline VARegisterThisFunction(const VARegisterThisFunction&);
  inline VARegisterThisFunction operator=(const VARegisterThisFunction&);
};

inline VARegisterThisFunction::VARegisterThisFunction(const char* fn)
{
  VABackTrace::instance()->push(fn);
}

inline VARegisterThisFunction::~VARegisterThisFunction()
{
  VABackTrace::instance()->pop();
}


//--------------------------------------------------------------------------------
// VAException
//--------------------------------------------------------------------------------

class VAException: public VException
{
 public:
  VAException(const std::string& type, 
	      const std::string& message="",
	      const std::string& file="",
	      const unsigned line=0)  throw();

  VAException(const VAException& e) throw();
    
  virtual ~VAException() throw() {};

  const std::vector<std::string> backTrace() const { return fBackTrace; }

 protected:
  void printBackTrace(std::ostream& os) const throw();

  // Reimplement this instead of VException::printTypeSpecificExceptionDetails()! 
  virtual void printVAExceptionDetails(std::ostream & out) const {};

 private:
  void printTypeSpecificExceptionDetails(std::ostream &out) const {
    printBackTrace(out);
    printVAExceptionDetails(out);
  }
  

  std::vector<std::string> fBackTrace;
};


//--------------------------------------------------------------------------------



//--------------------------------------------------------------------------------
// VAFatalException
//--------------------------------------------------------------------------------

class VAFatalException: public VAException
{
 public:
  VAFatalException(const std::string& message="", const std::string& file="",
		   unsigned line=0)  throw():
    VAException("VAFatalException", message, file, line) {};
      
  virtual ~VAFatalException() throw() {};
};

//--------------------------------------------------------------------------------


//--------------------------------------------------------------------------------
// VAConfigurationException
//--------------------------------------------------------------------------------
class VAConfigurationException: public VAException
{
 public:
  VAConfigurationException(const std::string& message="", 
			   const std::string& file="",
			   unsigned line=0)  throw():
    VAException("VAConfigurationException", message, file, line) {};
      
  virtual ~VAConfigurationException() throw() {};
};

//--------------------------------------------------------------------------------




//--------------------------------------------------------------------------------
// VASystemException
//--------------------------------------------------------------------------------
/*
An exception class to log system errors where errno is set.
*/

class VASystemException: public VAException
{
 public:
  VASystemException(const std::string& message="", int errorNum=errno,
		    const std::string& file="", unsigned line=0)  throw():
    VAException("VASystemException", message, file, line), fErrorNum(errorNum)
    {};

  virtual void printVAExceptionDetails(std::ostream &out) const
    {
      out<<"System Error: "<<fErrorNum<<": "<<strerror(fErrorNum)<<std::endl;
    }
      
  virtual ~VASystemException() throw() {};
  int errorNum() const { return fErrorNum; }
 private:
  int fErrorNum;
};

//--------------------------------------------------------------------------------



//--------------------------------------------------------------------------------
// VAAssertionFailedException
//--------------------------------------------------------------------------------

class VAAssertionFailedException: public VAException {
 public:
  VAAssertionFailedException(std::string exp,
			     std::string file="",
			     unsigned line=0) throw()
    : VAException("VAAssertionFailedException","", file, line),
    fExpression(exp)
    {};

  virtual void printVAExceptionDetails(std::ostream& out) const {
      out<<"Assertion "<<fExpression<<" failed."<<std::endl;
  }

  virtual ~VAAssertionFailedException() throw() {};

  std::string getExpression() const throw() { return fExpression;}
  std::string getFile() const throw() { return fFile;}
  unsigned getLinNo() const throw() { return fLineNo;}

 private:
  std::string fExpression;
  std::string fFile;
  unsigned fLineNo;
};


#define VA_ASSERT(exp) ((void)((exp)?0: \
    (throw VAAssertionFailedException(#exp,__FILE__,__LINE__), 0)))
   


//--------------------------------------------------------------------------------
// VARangeException
//--------------------------------------------------------------------------------

class VARangeException: public VAException
{
 public:
  VARangeException(std::string msg, double rangeMin, double rangeMax, double val,
		   const std::string& file="", unsigned line=0)  throw():
    VAException("VRangeException", msg, file, line),
    fRangeMin(rangeMin),
    fRangeMax(rangeMax),
    fVal(val)
    {};
      
  virtual void printVAExceptionDetails(std::ostream &out) const {      
    out<<fVal<<" not in allowed range: "<<fRangeMin<<" to "
       <<fRangeMax<<std::endl;
  }

  virtual ~VARangeException() throw() {};
  double getRangeMin() const { return fRangeMin; }
  double getRangeMax() const { return fRangeMax; }
  double getVal() const { return fVal; }

 private:
  double fRangeMin;
  double fRangeMax;
  double fVal;
};

//--------------------------------------------------------------------------------


#endif // VAEXCEPTION_H
