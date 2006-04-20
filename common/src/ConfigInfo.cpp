/**
 * \class ConfigInfo.cpp
 * \ingroup common
 * \brief This is a one-line description of this cpp file.
 *
 * ConfigInfo allows one to easily implement a user-friendly
 * configuration file format which can be automatically read and
 * written, complete with help documentation.  One can either use the
 * base ConfigInfo class from a calling program, or more typically,
 * make a sub-class which implements a specific configuration
 * language (an example of this is VConfigInfo).
 *
 * ConfigInfo reads a writes a text-based, user editable format.  On
 * each line, there is a command keyword, followed by one or more
 * arguments.  Comments start with a '#' character, and can be placed
 * anywhere. Whitespace is ignored unless it is inside a quoted
 * string. Other command files may be included (recursively) by using
 * the \c include \c "filename" command.
 *
 * ConfigInfo works as follows:
 *
 * - First, a CommandHandler must be registered for each keyword that
 * is expected. The CommandHandler is responsible for parsing the
 * arguments, and connecting them to a variable. This is done by
 * calling the ConfigInfo's setHandler() method, and passing in a new
 * CommandHandler of the appropriate type.  A set of useful command
 * handlers is provided, and more can be implemented by extending the
 * base CommandHandler class. For example, if you wanted to implement
 * a command called 'filename' that takes one string argument (stored
 * in a \c std::string variable called \c fname), you would call:
 *
 * \code 
 *    setHandler( "filename", new StringHandler( &fname ) );
 * \endcode
 *
 * - Then, if a configuration file is loaded (using
 * ConfigInfo::loadConfigFile()), when the parser encounters the
 * keyword 'filename', it will put the argument (which it verifies is
 * a quoted string) into the variable fname.
 *
 * - When the ConfigInfo::saveConfigFile() or the
 * ConfigInfo::dumpConfigFile() method is called, all of the
 * registered keywords will be written out in the correct format and
 * their arguments will be the current value of the connected
 * variable.  If any help text has been associated with the command
 * (via the ConfigInfo::setHelpText() method), it will be included in
 * the written file. If it is short, it will be placed after the
 * command, otherwise it precedes it and is word wrapped accordingly.
 *
 *  <h2> Implementing a Configuration File interface </h2>
 * 
 * A simple example of a configuration file interface is shown below.
 * In addition, I have implemented VConfigInfo, which is a class to
 * read and write VERITAS offline-analysis config files.  It is not
 * complete (more options will need to be added), but should serve as
 * a good starting point. See the VConfigInfo description for more
 * information.
 *
 * Here's an example: \include ConfigInfo-example1.cpp
 *
 * Which would write and read a configuration file like:
 * \include example1.conf
 *
 * <h2> Other features </h2>
 *
 * <h3> Includes </h3> 
 * 
 * You can nest configuration files by using the \c include \c
 * "filename" keyword.  This is useful for providing default options
 * for a group of config files, or for specifying a particular set of
 * cuts which is to be reused.  ConfigInfo searches for included in
 * the current working directory and from the include path, which can
 * be set by calling \c ConfigInfo::addToIncludePath().
 *
 * <h3> Categories </h3>
 *
 * When calling the setHandler() method, you can optionally specify a
 * category string.  Normally, command keys are written in
 * alphabetical order when saving a config file, but if a category is
 * specified, they are grouped. You can also specify category help
 * text using the ConfigInfo::setCategoryHelpText() method. See
 * VConfigInfo for an example.
 *
 *
 * <h3> Read-only variables </h3>
 *
 * A command can be marked as "read-only", meaning that the variables
 * associated with it will not be overwritten when loadConfigFile() is
 * called. This is useful for commands that are overridden by a
 * command-line argument.  You can mark a command as read-only in two ways:
 *
 * 1. Call ConfigInfo::setReadOnly() as follows:
 * \code
 * config.setReadOnly("mycommand");  
 * config.setReadOnly("mycommand",false); // to make it read/write again
 * \endcode
 *
 * 2. call the setReadOnly() member function of the CommandHandler itself:
 * \code
 * CommandHandler *h = new FloatHandler( &myvariable );
 * h->setReadOnly();
 * config.setHandler( "mycommand", h );
 * \endcode
 *
 * <h3> Handling errors </h3>
 *
 * When reading a config file, a ConfigSyntaxError is thrown when
 * there is a problem with the command or arguments.  If a command
 * keyword is encountered that is not registered with a
 * CommandHandler, a warning is printed by default and the line is
 * skipped.  If you want an exception to be thrown, call the \c
 * ConfigInfo::throwExceptionOnUnknownKey() method before reading the
 * config file. If an unknown key is encountered, a
 * ConfigKeyNotFoundException will be thrown.
 *
 * Original Author: Ozlem Celik
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

#include <iostream>
#include <iomanip>
#include <fstream>
#include <map>
#include <stdexcept>
#include <exception>
#include <ctime>
#include "ConfigInfo.h"


using namespace std;

ConfigInfo::ConfigInfo() {
    
    fExceptionOnUnknownKey = false;
    
}

/**
 * Clean up and free the memory used by all the registered command
 * handlers.
 */
ConfigInfo::~ConfigInfo() {


    for (map< string, CommandHandler*>::iterator iter = fHandlerMap.begin();
	 iter != fHandlerMap.end(); iter++ ) {
	if (iter->second != NULL) delete iter->second;
    }

}

/**
 * Register a specific configuration keyword, and set up a
 * CommandHandler for it. The constructor for the CommandHandler
 * typically connects the handler to a user-specified variable.
 *
 * \param key keyword to register 
 * \param handler a new CommandHandler
 * which handles the particular type of arguments you expect for the
 * keyword.
 * \param optional category name (default is "other") for sorting purposes
 */
void 
ConfigInfo::
setHandler(  std::string key, CommandHandler *handler, string category) {

    if (fHandlerMap.count(key) != 0) {
	throw ConfigKeyAlreadyUsedException(key);
    }

    fHandlerMap[key] = handler;
    fCategory[category].push_back(key);
       
}

/**
 * Specify a string of help text that should accompany the keyword
 * when a configuration file is written.  The help text is
 * automatically word-wrapped, and if short enough, placed on the same
 * line as the command.  
 *
 * \param key keyword to add help text for
 * \param text text string, automatically word-wrapped.
 */
void 
ConfigInfo::
setHelpText( std::string key, std::string text ){
    
    fHelpText[key] = text;

}

string
ConfigInfo::getHelpText(string key) {
    if (fHelpText.count(key) != 1) 
	return "";
    else
	return fHelpText[key];
}



/**
 * Set help text for a particular category
 */
void 
ConfigInfo::setCategoryHelpText( string category, string text ) {

    fCategoryHelpText[category] = text;

}

CommandHandler*
ConfigInfo::getHandler(string key) {
    if (fHandlerMap.count(key) != 1) 
	throw ConfigKeyNotFoundException("Unknown configuration command: "
					 +key);
    return fHandlerMap[key];
}


/**
 * Parse the given file and put its values into the internal structures
 */
void
ConfigInfo::
parse( string filename ) {
    
    ifstream conffile;
    int linenum = 0;
    string line;
    string key;
    vector<string> tokens;
    vector<string> arg;
    int k;

    // =====================================================================
    // open file - if you can't find it, look in include path
    // =====================================================================
    conffile.open( filename.c_str() );
    
    if (conffile.fail()) {
        conffile.close();
        conffile.clear();
	// TODO: look in include path if you can't find it
	
	if (!conffile) {
	    // print out open failed message
	    
	    throw runtime_error("couldn't open config file " +
					filename) ;
	}
    }

    cout << "Reading configuration from: "<<filename<<"..."<<endl;
    
    // =====================================================================
    // read each line, tokenize, and determine what to do
    // =====================================================================
    while (conffile && !conffile.eof()) {

	linenum++;

	getline( conffile, line, '\n' );
	
	try {

	    // ------------------------------------------------------------
	    // remove white space and comments
	    
	    k = line.find("#");
	    line = line.substr(0,k);
	    removeWhiteSpace(line);

	    // ------------------------------------------------------------
	    // ensure that quoted strings take up exactly one token
	    // (convert whitespace into special characters). Later, quoted
	    // strings must be read back with getStringLiteral to remove
	    // the excess formatting and quote characters.

	    prepareQuotedStrings( line );
    
	    // ------------------------------------------------------------
	    // tokenize, and skip if line is empty
	    
	    tokenize( line, tokens, " \t," );
	    if (tokens.size() < 1 ) continue;
	    
	    // ------------------------------------------------------------
	    // put the key and arguments into nice places:

	    key = tokens[0];
	    arg.clear();
	    for (int i=1; i<(int)tokens.size(); i++)
		arg.push_back( tokens[i] );
	    
	    //------------------------------------------
	    // process the command:
	    
	    // special case is an include file, need to recurse
	    if (tokens[0] == "include")  {
		string incfile = getStringLiteral( tokens,1 );
		parse(incfile);
	    }
	    else {
		// if the key is valid, and the key is not marked as
		// read-only, call the handler for the key. Otherwise
		// produce syntax error.

		CommandHandler *handler=NULL;
		
		try {

		    if (isReadOnly(key)) {
			cout << "Skipping command '"
			     << key <<"', which is marked as read-only"<<endl; 
		    }
		    else {
			handler = getHandler(key);
			handler->set(arg);
		    }

		}
		catch (ConfigKeyNotFoundException &e) {
		    // If key wasn't found, then either re-throw the
		    // exception or output a warning.

		    if (fExceptionOnUnknownKey==true) throw e;
		    else {
			cout << "WARNING: skipping unknown command '"
			     <<key<<"' at line "<<linenum<<endl;
		    }
		}

	    }
	}
	catch(ConfigSyntaxError &e) {
	    cout << "*** Syntax error at line " << linenum 
		 << " of "<<filename<<":"<<endl;
	    cout << ">>" <<line <<endl;
	    cout << e.what() << endl;
	    throw runtime_error("Please fix error in config file"); 
	}

    }

}


/**
 * Write out all the keys and their current values in the current
 * configuration to a stream, complete with help text.  The specified
 * file is APPENDED to if it exists already, so no information is
 * overwritten.
 */
void
ConfigInfo::dumpConfigFile( std::ostream& outstream ) {

    const int HELPCOL=40;
    const int COLWIDTH=80;
    string text, helptext;
    
    printHeader(outstream); // contains time stamp too

    // ==============================================================
    // write the data: loop over each category:
    
    for (map< string, vector<string> >::iterator cat = fCategory.begin();
	 cat != fCategory.end(); cat++) {
      
      // write out the category help text, if provided:
      
      if (fCategoryHelpText.count(cat->first) == 1) {
	outstream << endl<< "# -------------------------------"
		  << "------------------------------------------" << endl;
	outstream << wordWrap(fCategoryHelpText[cat->first],"# ",70) <<endl;
	outstream << "# -------------------------------"
		  << "------------------------------------------" << endl;
      }
      
      // now, cat->first is the category name, cat->second is the
      // vector of keys in that category. Loop over the keys in this
      // category:
      
      for (vector<string>::iterator ikey = cat->second.begin();
	   ikey != cat->second.end(); ikey++ ) {
	
	// *ikey is the key string
	
	text = *ikey + " " + getHandler(*ikey)->toString();
	helptext = getHelpText(*ikey);
	
	
	if ((int)helptext.length() < COLWIDTH-HELPCOL-2 
	    && (int)text.length() < HELPCOL) {
	  // print help after the command
	  if ((int)text.length() < HELPCOL ) {
	    outstream << text; 
	    outstream << setw(HELPCOL-text.length()) 
		      <<"# "<< helptext
		      << endl;
	  }
	  
	}
	else {
	  // print help before the command as a block (word wrapped)
	  outstream <<endl<<wordWrap( helptext, "# ",60) <<endl;
	  outstream << text <<endl;
	}
	
      }
      
    }
}

/**
 * Write out all the keys and their current values in the current
 * configuration to a file, complete with help text.  The specified
 * file is APPENDED to if it exists already, so no information is
 * overwritten.
 */
void
ConfigInfo::saveConfigFile( string filename ) {

    cout << "Writing configuration to: '"<<filename<<"'..."<<endl;

    // ==============================================================
    // open the file.  The files are appended by default - that way
    // you retain a history of config changes, which is a good
    // idea. Newer values will override old ones anyway.
    
    ofstream outfile(filename.c_str(), ios::app);
    dumpConfigFile(outfile);
    outfile.close();    
}


/**
 * Read a config file and store configuration internally
 */
void
ConfigInfo::loadConfigFile( string filename ) {

    // ==============================================================
    // check if it exists

    ifstream infile;
    infile.open(filename.c_str());

    if (infile.fail()) 
	throw runtime_error("Couldn't open '"+filename+"'");
    else 
	infile.close();
    
    // ==============================================================
    // parse the data

    parse(filename);

}



/**
 * Prints header of the configuration file. Override this function to
 * write out a less general header.
 */
void 
ConfigInfo::printHeader( std::ostream &s ) {

    char *thetime;
    time_t lt;

    lt = time( NULL );
    thetime = ctime( &lt );
    thetime[strlen(thetime)-1] = '\0';

    s <<endl
      << "## ==================================================="<<endl
      << "## Configuration File - " << thetime <<endl
      << "## ==================================================="<<endl
      <<endl;
    
}



/**
 * Add the specified directory to the include path.  ConfigInfo will
 * look for Files specified using the \c include keyword in these
 * paths.
 *
 * \todo implement this
 */
void 
ConfigInfo::addToIncludePath( std::string path ) {


}


/**
 * Mark the specified command as "read-only", meaning do not overwrite
 * it when a config file is loaded.  This is useful for keys which get
 * specified on the command line.
 */
void 
ConfigInfo::setReadOnly( std::string key,bool val) {
    
    getHandler(key)->setReadOnly(val);
       
}

/**
 * Check if the specified keyword is marked as read-only.
 */
bool
ConfigInfo::isReadOnly( std::string key ) {
    
    return getHandler(key)->isReadOnly();
    
}


/**
 * Tell ConfigInfo to throw an excpeption when an unknown command
 * keyword is encountered.  Otherwise, a warning is sent to the
 * console and parsing continues.
 */
void ConfigInfo::throwExceptionOnUnknownKey( bool val ) {
    fExceptionOnUnknownKey=val;
}
