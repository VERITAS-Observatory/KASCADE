/**
 * \file VAException.cpp
 * \brief Contains the VABackTrace, VARegisterThisFunction and 
 * VAEXception classes
 *
 * Original Author: John Quinn
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

/**
 * \class VAException
 * 
 * The exception classes used in the off-line analysis project. All
 * other OAWG exception classes should inherit from the VAException
 * base class, which inherits from the official VERITAS exception 
 * class VException.
 *
 * The main feature added in VAException is a backtrace facility
 * which provides a trace of which functions had been called at the
 * time the exception was thrown. This is achieved using a singleton
 * class VABackTrace, which stores the function names as a vector of
 * strings, and class VARegisterThisFunction which acts as a \em guard
 * class and removes the function name it added automatically when it
 * goes out of scope.
 * 
 * A VAException (and VException) may be created and thrown in 
 * one command or be constructed, have information added to it
 * (cia the stream() member function) and then thrown (see examples below)
 * The VAException may also be caught, have information
 * added to it (via the stream() method which provides access to the 
 * comment ostringstream, and be re-thrown. \warning if you want to catch
 * and rethrow a VAException derived class you must catch the specific
 * class in question rather than the base class. However, if you simply
 * want to catch the exception and print out the information in it
 * then you can catch the base class (either VAException or VException)
 * and print it to a stream, or catch a std::exception and use the 
 * what() member function. Always catch by reference.
 *
 * VAException and all derived classes (so far) allow the filename and
 * line number of the place where the exception was originally thrown
 * to be included in the exception. The recommended way to do this
 * to get the filename and line number via __FILE__ and __LINE__ 
 * (see examples below). 
 * In general VAException will not be directly used but a derived class
 * used instead. In particular, the virtual function
 * virtual void printVAExceptionDetails(std::ostream & out) const {}; 
 * should be re-implemented to print details specific to the derived
 * class. This is a change from VException. See VAFatalException, 
 * VAConfigurationException, VARangeException, VASystemException 
 * for example implementations. 
 *
 * Silly example:
 * 
 * Throwing a VARangeException (code excerpt)
 * \code
 *
 * #include "VAException.h"
 *
 * class VAArray
 * {
 *   public:
 *   VAArray(int numElements);
 *   float getElement(int element) { 
 *     VARegisterThisFunction fn("VAArray::getElement()");
 *     if (element > fMaxElement)
 *       throw(VRangeException("Element requested does not exist!",
 *                             0, fMaxElement, element));
 *   }
 * 
 *   private:
 *    int fMaxElement;
 * }
 *
 * 
 * class VASummer
 * {
 *   public:
 *   float sumElements(const VAArray &array, int numToSum){
 *     VARegisterThisFunction fn("VASummer::sumElements()");
 *     float sum=0;
 *     for(int i=0; i<numToSum ;i++) 
 *       sum+=array.getElement(i);
 *     return sum;
 *   }
 *
 *
 * int main()
 * {
 *   VAArray array(10);
 *   VASummer summer;
 *   
 *   try{
 *     summer.sum(array,100);
 *   }
 *   catch (const VAException &ex){
 *     cerr<<ex;
 *   }
 * }
 * \endcode
 * which produces output:
 * \verbatim
 Exception: VRangeException: 
 Function back trace: VAArray::getElement()
                      VASummer::sumElements()
 10 not in allowed range: 0 to 9
 Element requested does not exist!
 \endverbatim
 * Alternatively, if the exception had been thrown as:
 * \code
 *   throw(VRangeException("Element requested does not exist!",
 *                         0, fMaxElement, element, __FILE__, __LINE__)); 
 * \endcode
 * the output would be:
 * \verbatim
Exception: VRangeException: 
Code location: VAArray.cpp:18
Function back trace: VAArray::getElement()
                     VASummer::sumElements()
10 not in allowed range: 0 to 9
Element requested does not exist!
 \endverbatim
 * If no function names had been registered with VARegisterThisFunction then
 * then the backtrace would not be printed.
 *
 *
 * If one wants to add more detailed information to a VException (rather
 * than the simple one-line message included in the constructor) then
 * one can use the stream() member function to obtain access to the 
 * internal ostringstream and write to it. Example:
 * \code
 * VAFatalException ex("really bad!",__FILE__,__LINE__);
 * ex.stream()<< "add more information here ";
 * throw(ex);
 * \endcode
 *\sa VABAckTrace, VARegisterThisFunction
**/



#include<iostream>
#include<sstream>
#include"VAException.h"


// ----------------------------------------------------------------------------
// VABackTrace
// ----------------------------------------------------------------------------


/**
 * \class VABackTrace
 * \brief Singleton class which provides stack-like mechanism for storing 
 * names of functions for tracing where exceptions originated.
 *
 * Needed by VAException. Developed originally by Stephen Fegan.
 *
 * \sa VAException, VARegisterThisFunction
 **/


VABackTrace* VABackTrace::pfgInstance=0;

/**
 * Returns a vector of strings containing all of the strings stored
 * in the class, separated by new lines.
 **/

std::vector<std::string> VABackTrace::backTrace() const
{
  std::vector<std::string> vs;
  vs.reserve(fBackTrace.size());
  for(std::vector<const char*>::const_iterator i = fBackTrace.begin();
      i != fBackTrace.end(); i++)vs.push_back(std::string(*i));
  return vs;
}

/**
 * Returns a single string containing all of the strings stored
 * in the class, separated by new lines.
 **/

std::string VABackTrace::backTraceString() const
{
  std::ostringstream oss;
  for(std::vector<const char*>::const_iterator i = fBackTrace.begin();
      i != fBackTrace.end(); i++)
    oss << *i << std::endl;
  return oss.str();
}



// Documentation for other member functions:

/**
 * \function void VABackTrace::push(const char*)
 *
 * Appends a string to the string list.
 **/


/**
 * \function void VABackTrace::pop()
 *
 * Remove the last string from the list - the string is discarded..
 **/

/**
 * \function std::vector<std::string> backTrace() const
 *
 * Returns a vector containg all the strings currently stored.
 *
 */

// ----------------------------------------------------------------------------
// RegisterThisFunction
// ----------------------------------------------------------------------------

/** 
 * \class VARegisterThisFunction
 * \brief Adds a string to the VABackTrace class and automatically removes
 * it when the class is destructed.
 *
 * Used as a \em guard type class so that when the class goes out of
 * scope the string is automatically removed from the VABackTrace class.
 *
 * \sa VAException, VABackTrace
 **/


// ----------------------------------------------------------------------------
// VAException
// ----------------------------------------------------------------------------


VAException::VAException(const VAException& e) throw():
  VException(e), fBackTrace(e.fBackTrace)
{}



VAException::VAException(const std::string& title, const std::string& message,
			 const std::string& file, unsigned line) throw():
  VException(), fBackTrace(VABackTrace::instance()->backTrace())
{
  setType(title);
  addComment(message);
  setThrowLocation(file.c_str(),line);
}


/**
 * Print BackTrace to a stream.
 **/

void VAException::printBackTrace(std::ostream& os) const throw()
{
 if(fBackTrace.size()>0)
    for(unsigned i=0; i<fBackTrace.size(); i++)
      os << (i==0 ? "Function back trace: " : "                     ") 
	 << fBackTrace[fBackTrace.size()-i-1] << std::endl;

}



// ----------------------------------------------------------------------------
// VAFatalException
// ----------------------------------------------------------------------------

/**
 * \class VAFatalException
 * \brief An exception for cases from which recovery is impossible.
 **/                                                                            


// ----------------------------------------------------------------------------
// VAConfigurationException
// ----------------------------------------------------------------------------
/**
 * \class VAConfigurationException
 * \brief An exception class for cases where there are errors in the 
 * file or command-line options.
 **/ 


// ----------------------------------------------------------------------------
// VASystemException
// ----------------------------------------------------------------------------
/**
 * \class VASystemException
 * \brief An exception class for cases where an error occurred in a system call.
 *
 * The error number \em errno can be passed to the constructor but it
 * should pick it up by defualt in any case. The error message (text)
 * is automatically looked up and included in the exception..
 **/ 


// ----------------------------------------------------------------------------
// VAAssertionFailedException
// ----------------------------------------------------------------------------
/**
 * \class VAAssertionFailedException
 * \brief As the name implies, an assertion failed!
 *
 * Use this with the VA_ASSERT() macro (thank to Filip Pizlo!)
 *
 **/ 




// ----------------------------------------------------------------------------
// VARangeException
// ----------------------------------------------------------------------------
/**
 * \class VARangeException
 * \brief For cases where a parameter is not in the allowed range.
 *
 * Examples of where to use: index out of range, requested time not
 * in allowed range etc.
 *
 **/ 







