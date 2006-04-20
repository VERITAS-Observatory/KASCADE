/*
 * $Id$
 *
 * VException.cpp -- base-class for all VERITAS exceptions, implementation
 * by Filip Pizlo, 2002, 2003
 */

// This file is an import of the VAException into the OAWG so that VBF is
// not requires. Every effort should be made to keep it in sync with Filip's
// version

// These deinitions tell the makefile which library the cpp file
// should be included in
// VA_LIBRARY_TAG: libSP24commonLite.a

#include "VException.h"

void VException::makeTotal() const {
    std::ostringstream ret;
    
    ret<<desc.c_str();
    
    if (msg.length()!=0) {
        ret<<": "<<msg.c_str();
    }
    
    if (hasThrowLocation()) {
        ret<<": thrown in "<<throw_file<<" on line "<<throw_line;
    }
    
    for (std::vector< std::string >::const_iterator
         i=comments.begin();
         i!=comments.end();
         ++i)
    {
        ret<<": "<<*i;
    }
    
    std::string last_cmnt_str=last_comment.str();
    if (last_cmnt_str.length()>0) {
        ret<<": "<<last_cmnt_str;
    }
    
    total=ret.str();
}

VException::VException(const std::string &exception_type,
                       const std::string &thrown_by):
    desc(exception_type),
    throw_line(0)
{
    msg.append("In ");
    msg.append(thrown_by);
}

VException::VException():
    throw_line(0)
{
}

VException::VException(const VException &other):
    desc(other.desc),
    msg(other.msg),
    throw_file(other.throw_file),
    throw_line(other.throw_line),
    comments(other.comments)
{
    last_comment.str(other.last_comment.str());
    endComment();
}

VException::~VException() throw() {
}

void VException::endComment() throw() {
    std::string last_cmnt_str=last_comment.str();
    last_comment.str("");
    if (last_cmnt_str.length()>0) {
        comments.push_back(last_cmnt_str);
    }
}

void VException::addComment(const std::string &comment) {
    endComment();
    comments.push_back(comment);
}

void VException::addComment(const char *comment) {
    endComment();
    if (comment!=NULL) {
        comments.push_back(std::string(comment));
    }
}

void VException::setThrowLocation(const char *filename,
                                  unsigned line_no)
{
    throw_file=filename;
    throw_line=line_no;
}

void VException::addRethrowLocation(const char *filename,
                                    unsigned line_no)
{
    // do nothing just yet.
}

const std::vector< std::string > VException::getComments() const {
    std::vector< std::string > ret(comments);
    std::string last_cmnt_str=last_comment.str();
    if (last_cmnt_str.length()>0) {
        ret.push_back(last_cmnt_str);
    }
    return ret;
}

std::ostream& operator<<(std::ostream& stream,const VException &x) {
    stream<<"Exception: "<<x.getType()<<": "<<x.getMessage()<<std::endl;
    
    if (x.hasThrowLocation()) {
        stream<<"Code location: "<<x.getThrowFile()
              <<":"<<x.getThrowLine()<<std::endl;
    }
    
    x.printTypeSpecificExceptionDetails(stream);
    
    std::vector< std::string > comments=x.getComments();
    for (std::vector< std::string >::iterator
         i=comments.begin();
         i!=comments.end();
         ++i)
    {
        stream<<*i<<std::endl;
    }
    
    return stream;
}



