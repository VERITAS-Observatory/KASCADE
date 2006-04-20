/**
 * \class TextHelpers
 * \ingroup common
 * \brief This is a one-line description of this cpp file.
 *
 * Here is a tedious verbose multi-line description of all
 * the details of the code, more than you would
 * ever want to read. Generally, all the important documentation
 * goes in the .cpp files.
 *
 * Original Author: Peter Cogan
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

// helper functions that are useful. TODO: put in their own namespace
#include <iostream>
#include <string>
#include <vector>
#include <stdexcept>

using namespace std;

namespace Text {
    const char SPACE_PLACEHOLDER = (char)254;
};


/**
 * Returns string that was enclosed in quotes and which may span
 * multiple tokens. This is really just a hack so i don't have to
 * write a proper parser.
 */
string 
getStringLiteral( vector<string> &v, int start ) {
    
    string s;
    int  q2;
    
    for (int i=start; i<(int)v.size(); i++) {
        s.append(v[i]);
        if (i!= (int)v.size()-1) s.append(" ");
    }
    
    if (s[0] != '"') {
        throw runtime_error("Expected a quoted string");
        return "";
    }
    
    q2 = s.find_first_of('"',1);
    
    string literal =  s.substr(1,q2-1);

    for (int i=0; i<(int)literal.length(); i++) 
        if (literal[i]==Text::SPACE_PLACEHOLDER) literal[i]=' ';

    return literal;
 
} 

void
prepareQuotedStrings( string &line ) {
    
    vector<int> quotes;

    // find quotes

    for (int i=0; i<(int)line.length(); i++) {
        if (line[i] == '"') quotes.push_back(i);
    }
    
    if (quotes.size() % 2 != 0) {
        throw runtime_error("Missing quotation mark");
    }

    for (int i=0; i<(int)quotes.size(); i+=2) {
        for (int j=quotes[i]; j<=quotes[i+1]; j++) {
            if (line[j] == ' ') {
                line[j]=Text::SPACE_PLACEHOLDER;
            }
        }
    }
    
}


void tokenize(const string& str, vector<string>& tokens,
              const string& delimiters) {
    
    tokens.clear();  //reset token list
    
    // Skip delimiters at beginning.
    string::size_type lastPos = str.find_first_not_of(delimiters, 0);
    // Find first "non-delimiter".
    string::size_type pos     = str.find_first_of(delimiters, lastPos);

    while (string::npos != pos || string::npos != lastPos)
    {
        // Found a token, add it to the vector.
        tokens.push_back(str.substr(lastPos, pos - lastPos));
        // Skip delimiters.  Note the "not_of"
        lastPos = str.find_first_not_of(delimiters, pos);
        // Find next "non-delimiter"
        pos = str.find_first_of(delimiters, lastPos);
    }
}

void
removeWhiteSpace( string &str ) {

    // first convert tabs to spaces:
    
    string::iterator i;

    for( i=str.begin(); i != str.end(); i++) {
        if (*i == '\t') *i = ' ';
    }

    // now chop off white space at beginning and end 

    string::size_type where = str.find_first_not_of(' ');
    
    if (where != string::npos && where != 0)            
        str = str.substr(where);
    
    where = str.find_last_not_of(' ');
  
    if (where != string::npos && where != (str.length() - 1))
        str = str.substr(0, where + 1);

    if (str==" ") str = "";

    
}

/**
 * \param text text string to wrap
 * \param width max number of columns you want in the text block (default 80)
 * \param prefix optional prefix string (for indentation) applied to each line
 * \returns word-wrapped string.
 */
string 
wordWrap( string& text,  string prefix,int width ) {

    vector<string> tokens;
    int numchars;
    string wrappedtext = prefix;

    tokenize(text, tokens, " \n");
    
    numchars = 0;
    for (vector<string>::iterator token=tokens.begin(); 
	 token != tokens.end(); token++) {
	
	numchars += token->length() +1;
	if (numchars >= width) {
	    wrappedtext += "\n" + prefix;
	    numchars = token->length()+1;
	}

	wrappedtext += *token + " ";
	
    }

    return wrappedtext;

}

















