#ifndef COMMANDHANDLERS_H
#define COMMANDHANDLERS_H

/** 
 * Implementations of the various command handlers.  Most are simple
 * enough to do inline, so no .cpp file is needed, but if they are
 * more complicated the code should be separated.
 */

#include <string>
#include <sstream>
#include <vector>
#include <iostream>
#include <stdexcept>
#include <exception>
#include <iomanip>

#include "TextHelpers.h"

using std::vector;
using std::string;

/**
 * Base exception for syntax errors
 */
class ConfigSyntaxError : public std::runtime_error {
 public:
    ConfigSyntaxError(std::string what): std::runtime_error(what){;}
};

/**
 * Thrown when the requested configuration key is not found
 */
class ConfigKeyNotFoundException : public ConfigSyntaxError {
 public:
    ConfigKeyNotFoundException(std::string what)
	: ConfigSyntaxError("ConfigKeyNotFoundException: "+what){;}
};

/**
 * Thrown when one tries to register a key which already is registered.
 */
class ConfigKeyAlreadyUsedException : public ConfigSyntaxError {
 public:
    ConfigKeyAlreadyUsedException(std::string what)
	: ConfigSyntaxError("ConfigKeyAlreadyUsedExcpetion: "+what){;}
};


/**
 * CommandHandlers are objects which parse a list of configuration key
 * arguments and put their values into the correct place in the
 * RunInfo structure.
 */
class CommandHandler {

 public:

    /// connects command handler to variable and sets help text, etc.
    CommandHandler() :fIsReadOnly(false){ ;}
    virtual ~CommandHandler(){;}

    /**
     *  This function takes a vector os string arguments, checks
     * them, converts them to the appropriate format and and sets the
     *  variables which are connected to this object 
     */
    virtual void set( vector<string> &args ) {;}  
    
    /// convert the command into a config-file string with the
    /// current values 
    virtual string toString() {return "UNDEFINED";} 

    void setReadOnly(bool val=true) { fIsReadOnly=val; }
    bool isReadOnly() { return fIsReadOnly; }

 private:
    bool fIsReadOnly;

};


/** 
 * Handler for config keys which have boolean (true/false) arguments
 */
class BoolHandler : public CommandHandler {

public:
    BoolHandler( bool *variable ) 
	: CommandHandler(), fVariable(variable) {;}
    
    void set( vector<string> &arg ) {

	if (arg.size() != 1) {
	    throw ConfigSyntaxError("Booleans should have only 1 argument");
	}	   

	if (arg[0] == "true" || arg[0] == "on")
	    *fVariable = true;
	else if (arg[0] == "false" || arg[0] == "off")
	    *fVariable = false;
	else 
	    throw ConfigSyntaxError("Booleans should be true/false or on/off");

    }
    
    string toString() {
	if (*fVariable == true) return string("true");
	else return string("false");
    }

private:
    bool *fVariable;   // pointer to the internal variable to set

};

/** 
 * handler for config keys which have single floating-point (float) arguments
 */
class FloatHandler : public CommandHandler {

public:
    FloatHandler( float *variable ) 
	: CommandHandler(), fVariable(variable) {;}
    
    void set( vector<string> &arg ) {

	if (arg.size() != 1) {
	    throw ConfigSyntaxError("A single float was expected");
	}	   

	*fVariable = atof(arg[0].c_str());

    }
    
    string toString() {
	std::stringstream s;
	s.precision(6);
	s.width(10);
	s << *fVariable;
	return s.str();
    }

private:
    float *fVariable;   // pointer to the internal variable to set

};


/** 
 * handler for config keys which have single floating-point (float) arguments
 */
class DoubleHandler : public CommandHandler {

public:
    DoubleHandler( double *variable ) 
	: CommandHandler(), fVariable(variable) {;}
    
    void set( vector<string> &arg ) {

	if (arg.size() != 1) {
	    throw ConfigSyntaxError("A single floating-point"
				    " number was expected");
	}	   

	*fVariable = atof(arg[0].c_str());

    }
    
    string toString() {
	std::stringstream s;
	s.precision(6);
	s.width(10);
	s << *fVariable;
	return s.str();
    }

private:
    double *fVariable;   // pointer to the internal variable to set

};




/** 
 * handler for config keys which have single integer arguments
 */
class IntHandler : public CommandHandler {

public:
    IntHandler( int *variable ) 
	: CommandHandler(), fVariable(variable) {;}
    
    void set( vector<string> &arg ) {

	if (arg.size() != 1) {
	    throw ConfigSyntaxError("A single integer was expected");
	}	   

	*fVariable = atoi(arg[0].c_str());

    }
    
    string toString() {
	std::stringstream s;
	s << *fVariable;
	return s.str();
    }

private:
    int *fVariable;   // pointer to the internal variable to set

};


/** 
 * handler for config keys which have single quoted string arguments
 */
class StringHandler : public CommandHandler {

public:
    StringHandler( std::string *variable ) 
	: CommandHandler(), fVariable(variable) {;}
    
    void set( vector<string> &arg ) {

	if (arg.size() != 1) {
	    throw ConfigSyntaxError("One quoted string expected "
				    "(use double quotes)");
	}	   

	try {
	    *fVariable = getStringLiteral( arg, 0 );
	}
	catch(std::runtime_error &e) {
	    throw ConfigSyntaxError( e.what() );
	}

    }
    
    string toString() {
	return "\"" + *fVariable + "\"";
    }

private:
    std::string *fVariable;   // pointer to the internal variable to set

};

/**
 * Handles commands which expect a range (lower and upper bound)
 */
class RangeHandler : public CommandHandler {

public:
    RangeHandler( double *minvariable, double *maxvariable  ) 
	: CommandHandler(), fMinVariable(minvariable),
	fMaxVariable(maxvariable) {;}
    
    void set( vector<string> &arg ) {

	if (arg.size() != 2) {
	    throw ConfigSyntaxError("Expected lower and upper bounds");
	}	   

	*fMinVariable = atof(arg[0].c_str());
	*fMaxVariable = atof(arg[1].c_str());
	
	// check bounds
	if (*fMinVariable > *fMaxVariable) {
	    throw ConfigSyntaxError("Lower bound exceeds upper bound! "
				    "Should be: min max");
	}

    }
    
    string toString() {
	std::stringstream s;
	s.precision(8);
	s << std::setw(10) <<*fMinVariable << " "
	  << std::setw(10) << *fMaxVariable;
	return s.str();
    }

private:
    double *fMinVariable;  // pointer to the internal LOW variable to set
    double *fMaxVariable; // pointer to the internal HIGH variable to set

};



/**
 * Handles commands which expect an enumeration of strings.  This is
 * useful for options which should have descriptive values.  For
 * example, if you have more than one method for performing some
 * analysis function, you could register a command called
 * "AnalysisMethod" which is handled by an EnumHandler with the
 * enumeration string set to "quick,complete,experimental", which
 * would only accept those three possibilities.
 */
class EnumHandler : public CommandHandler {

public:
    /**
     * \param enumeration: comma-separated list of acceptible values
     * \param variable: string where the user-selected value is stored
     */
    EnumHandler(string enumeration, string *variable  ) 
	: CommandHandler(), fVariable(variable), fEnumString(enumeration) {
	
	// break list of values into vector:
	tokenize( enumeration, fEnumValues, ", " );
	
    }
    ~EnumHandler(){;}

    void set( vector<string> &arg ) {

	if (arg.size() != 1) {
	    throw ConfigSyntaxError("Expected one of the following: "
				    +fEnumString);
	}	   

	// check that the value is one of the possible options:
	bool isok=false;
	for (vector<string>::iterator v=fEnumValues.begin();
	     v != fEnumValues.end(); v++) {
	    if (arg[0] == *v) {
		isok = true;
		break;
	    }
	}

	if (isok == false) {
	    throw ConfigSyntaxError("'"+arg[0]+"'"
				    +" should be one of the following: '"
				    +fEnumString+"'");
	}

	*fVariable = arg[0];

	
    }
    
    string toString() {
	return *fVariable;
    }

private:
    string *fVariable;   // pointer to the internal string to store result in
    string fEnumString;  // original comma-separated list of values
    vector<string> fEnumValues; // list of acceptible values

};



#endif
