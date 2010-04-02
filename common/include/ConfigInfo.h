
#ifndef CONFIGINFO_H
#define CONFIGINFO_H

#include <map>
#include <string>
#include <iostream>
#include <exception>
#include <stdexcept>
#include "stdlib.h"
#include <stdio.h>
#include <string.h>
#include "CommandHandlers.h"
#include "TextHelpers.h"


class ConfigInfo {

public:

    ConfigInfo();
    ConfigInfo( std::string filename );
    virtual ~ConfigInfo();

    void setHandler(  std::string key, CommandHandler *handler, 
		      std::string category=std::string("other") );
    void setHelpText( std::string key, std::string text );
    void setCategoryHelpText( std::string category, std::string text );

    void dumpConfigFile( std::ostream& stream );
    void saveConfigFile( std::string filename );
    void loadConfigFile( std::string filename );

    void addToIncludePath( std::string path );

    void setReadOnly( std::string key,bool val=true );
    bool isReadOnly( std::string key );

    void throwExceptionOnUnknownKey( bool val=true );

    virtual void printHeader( std::ostream &s );

private:
    ConfigInfo(const ConfigInfo&);
    ConfigInfo& operator= (const ConfigInfo&);

    CommandHandler* getHandler( std::string key );
    std::string getHelpText( std::string key );
    void parse( std::string filename );

    std::map< std::string, CommandHandler* > fHandlerMap;
    std::map< std::string, std::string > fHelpText;

    // maps category to list of keys:
    std::map< std::string, std::vector<std::string> > fCategory; 
    std::map< std::string, std::string > fCategoryHelpText;

    bool fExceptionOnUnknownKey;

};

#endif



