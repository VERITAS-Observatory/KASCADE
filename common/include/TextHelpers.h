
#ifndef TEXTHELPERS_H
#define TEXTHELPERS_H
#include <string>
#include <iostream>

using std::string;
using std::vector;

void prepareQuotedStrings( string &line );
std::string  getStringLiteral( std::vector<std::string> &v, int start );
void tokenize(const string& str, vector<string>& tokens,const string& delimiters);

void removeWhiteSpace( string &str );
string wordWrap( string& text,  string prefix="",int width=80 );



#endif














