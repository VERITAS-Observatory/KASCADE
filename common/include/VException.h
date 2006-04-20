/*
 * $Id$
 *
 * VException.h -- base-class for all VERITAS exceptions
 * by Filip Pizlo, 2002, 2003, 2004
 *
 * With input from Marty Olevich and John Quinn
 *
 * For documentation: first read the VERITAS Error Handling document.  Then,
 * read the comments in this file.  At the bottom of this header, you will
 * find further documentation and usage examples.
 */

// This file is an import of the VAException into the OAWG so that VBF is
// not requires. Every effort should be made to keep it in sync with Filip's
// version

#ifndef V_EXCEPTION_H
#define V_EXCEPTION_H

#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <exception>
#include <sstream>
#include <vector>

/**
 * The official VERITAS exception class.  Subclass this to produce your
 * own exception classes, or use the ones provided in this file.
 * <p>
 * Subclassing example #1:
 * <pre>
 * class VMyException: public VException {
 *   public:
 *     VMyException(const char *message=NULL) {
 *       setStrings("Something bad happened",  // unique type string
 *                  message);                  // user-supplied message
 *     }
 * };
 * </pre>
 * Subclassing example #2:
 * <pre>
 * class VMyException: public VException {
 *   private:
 *     std::string _other_info
 *   public:
 *     VMyException(const std::string &throw_by,
 *                  const std::string &other_info):
 *       VException("Something bad happened",  // unique type string
 *                  thrown_by),  // description of throw location,
 *                               // will end up as a user-supplied message
 *       _other_info(other_info)
 *     {}
 *     void printTypeSpecificExceptionDetails(ostream &out) {
 *       out<<"Some other information: "<<_other_info<<std::endl;
 *     }
 * };
 * </pre>
 *
 * @author Filip Pizlo
 */
class VException:
    public std::exception
{
    private:
        std::string desc,msg;
        
        mutable std::string total;
        
        std::string throw_file;
        unsigned throw_line;
        
        std::vector< std::string > comments;
        
        std::ostringstream last_comment;
        
        void makeTotal() const;

    protected:
        
        /**
         * Compatibility constructor.  The thrown_by string is dropped into
         * the message string with a prefix "In ".
         * <p>
         * This is equivalent to using the default constructor and then
         * doing:
         * <pre>
         * setType(exception_type);
         * setMessage(std::string("In ")+thrown_by);
         * </pre>
         */
        VException(const std::string &exception_type,
                   const std::string &thrown_by);
        
        /**
         * A direct way of setting the type.
         */
        void setType(std::string _desc) {
            desc=_desc;
            makeTotal();
        }
        
        /**
         * A direct way of setting the type using C-strings.  If the
         * supplied string is NULL, the type will not be set.
         */
        void setType(const char *_desc) {
            if (_desc!=NULL) {
                setType(std::string(_desc));
            }
        }
        
        /**
         * A direct way of setting the user-supplied message.  If you
         * want to allow users of this class to set the user-supplied
         * messages, you should create your own method to do so,
         * or provide a mechanism via the constructor.
         */
        void setMessage(std::string _msg) {
            msg=_msg;
            makeTotal();
        }
        
        /**
         * A direct way of setting the user-supplied message using
         * C-strings.  If the supplied string is NULL, the user-supplied
         * message will not be set.
         */
        void setMessage(const char *_msg) {
            if (_msg!=NULL) {
                setMessage(std::string(_msg));
            }
        }
        
        /**
         * A short-cut that sets both the type and message.
         */
        void setStrings(const char *type,
                        const char *msg)
        {
            setType(type);
            setMessage(msg);
        }
        
        /**
         * A short-cut that sets both the type and message.
         */
        void setStrings(const std::string &type,
                        const std::string &msg)
        {
            setType(type);
            setMessage(msg);
        }
        
    public:
        
        VException();
        
        VException(const VException &other);
        VException &operator=(const VException &other);
        
        virtual ~VException() throw();
        
        /**
         * Returns a user-readable string describing this exception.  No
         * new-lines or carriage-returns or other special characters will
         * be present in the returned string, so this method is a suitable
         * way of getting a loggable message.
         * <p>
         * Be careful about the ownership of the const char* that is returned
         * here.  This const char* points to data that will live only as
         * long as the exception object.  So, for example, the following
         * code would almost certainly lead to problems:
         * <pre>
         * const char *exception_string;
         * try {
         *   // do some stuff...
         * } catch (const VException &e) {
         *   exception_string=e.what();
         * }
         * cout<<exception_string<<endl; // may crash here!
         * </pre>
         */
        virtual const char *what() const
            throw()
        {
            makeTotal();
            return total.c_str();
        }
        
        /**
         * Returns an ostream that may be used to compose a comment.  Repeated
         * calls to this method return a stream into the same very last comment.
         * To get a stream to the next comment, you may
         * <ul>
         * <li>Rethrow the exception,</li>
         * <li>Copy-construct a new exception,</li>
         * <li>Call endComment(), or<li>
         * <li>Call addComment() with the start of the new comment.</li>
         * </ul>
         * This means that if the exception traverses a sequence of
         * catch...rethrow blocks, where each block calls stream() and adds
         * comment text, then the text added in each such block will become
         * its own comment.
         */
        std::ostream& stream() throw() {
            return last_comment;
        }
        
        /**
         * End the currently edited comment, if one exists.
         */
        void endComment() throw();
        
        /**
         * End the currently edited comment, if one exists, and then
         * start a new one with the text provided.
         */
        void addComment(const std::string &comment);
        
        /**
         * Same as addComment(), but uses a C-string as an argument.
         * If the string is NULL, then the currently edited comment is
         * ended but not new comment is added.
         */
        void addComment(const char *comment);
        
        /**
         * Sets the throw location.  You should call this like so:
         * <pre>
         * exception.setThrowLocation(__FILE__,__LINE__);
         * </pre>
         * However, you shouldn't have to call this method if you use
         * the V_THROW macro.
         */
        void setThrowLocation(const char *filename,
                              unsigned line_no);
        
        /**
         * Adds a rethrow location.  Same semantics as setThrowLocation(),
         * except that this method is called for you in V_RETHROW.
         * Note that this method currently does nothing.
         */
        void addRethrowLocation(const char *filename,
                                unsigned line_no);
        
        const char *getType() const
            throw()
        {
            return desc.c_str();
        }
        
        const char *getMessage() const
            throw()
        {
            return msg.c_str();
        }
        
        bool hasThrowLocation() const
            throw()
        {
            return throw_file.length()>0 &&
                   throw_line!=0;
        }
        
        const char *getThrowFile() const
            throw()
        {
            return throw_file.c_str();
        }
        
        unsigned getThrowLine() const
            throw()
        {
            return throw_line;
        }
        
        /**
         * Returns a vector of strings that contains all of the currently
         * available comments.  This includes the currently edited comment.
         */
        const std::vector< std::string > getComments() const;
        
        /**
         * Each exception may override this method to print additional
         * information.  This method is called whenever you invoke
         * operator<<(ostream&,VException&).
         */
        virtual void printTypeSpecificExceptionDetails(std::ostream &output)
            const
        {
        }
        
};

/**
 * Convenience operator for printing an exception to a stream.  This method
 * will print all of the information available via the what() method,
 * with the addition of information supplied by the
 * printTypeSpecificExceptionDetails() method.  Also note that while what()
 * returs a string that is just one line, this method will print multiple
 * lines, and assumes that you have just started a new line prior to
 * calling it.
 */
std::ostream& operator<<(std::ostream& stream,const VException &x);

/**
 * An exception class suitable for wrapping errno's from libc.
 */
class VSystemException: public VException {
    private:
        
        int sys_errno;
        
        void composeError(const std::string &msg) {
            std::ostringstream buf;
            buf<<"System error: "<<strerror(sys_errno);
            setType(buf.str());
            setMessage(msg);
        }
        
    public:
        
        VSystemException(const std::string &msg=""):
            sys_errno(errno)
        {
            composeError(msg);
        }
        
        VSystemException(int _errno,const std::string &msg=""):
            sys_errno(_errno)
        {
            composeError(msg);
        }
        
        int getErrno() const throw() { return sys_errno; }
        
};

/**
 * A suitable exception class for use whenever a user passes an invalid
 * argument.
 */
class VInvalidArgumentException: public VException {
    public:
        VInvalidArgumentException(const std::string &msg="") {
            setStrings("Invalid Argument",msg);
        }
};

/**
 * An exception used by the V_ASSERT macro.
 */
class VAssertionFailedException: public VException {
    public:
        VAssertionFailedException(const char *msg,
                                  const char *file,
                                  unsigned line)
        {
            setType("Assertion failed");
            setMessage(msg);
            setThrowLocation(file,line);
        }
};

// helpful stuff

#define V_THROW(e_class,msg) do {                       \
    e_class __e_to_throw(msg);                          \
    __e_to_throw.setThrowLocation(__FILE__,__LINE__);   \
    throw __e_to_throw;                                 \
} while (false)

#define V_RETHROW(e_to_rethrow) do {                    \
    e_to_rethrow.addRethrowLocation(__FILE__,__LINE__); \
    throw;                                              \
} while (false)

// assert some expression to be true; if not true, a
// VAssertionFailedException will be thrown.
#define V_ASSERT(exp) ((void)((exp)?0: \
    (throw VAssertionFailedException("For \"" #exp "\"", \
    __FILE__,__LINE__), 0)))

// kindof like assert, except that you're saying that one of your own
// assertions has already failed and you wish to crap out with a semi-
// helpful message.
#define V_FAIL(msg) \
    (throw VAssertionFailedException(msg,__FILE__,__LINE__))
   
#endif

/******************************************************************************

Further VException Documentation
by Filip Pizlo

INTRODUCTION

In this comment block I will attmept to describe how one might subclass
VException, how one might throw a subclass of VException, how one
might catch VExceptions, and finally how one might rethrow VExceptions.
The remainder of this comment block is split into the sections SUBCLASSING,
THROWING, CATCHING, and RETHROWING.

SUBCLASSING

I will start this section off with an example:

class VDatabaseException:
    public VException
{
    public:
        VDatabaseException(const std::string &msg="") {
            setStrings("Something bad happened while talking to the "
                       "database",msg);
        }
};

Shown above is a realistic example of how a lot of currently existing
code defines exceptions.  Notice the use of the setStrings() method
to short-cut the constructor.  Equivalently, one could just as easily do:

class VDatabaseException:
    public VException
{
    public:
        VDatabaseException(const std::string &msg="") {
            setType("Something bad happened while talking to the "
                    "database");
            setMessage(msg);
        }
};

The above would have the same effect, but may require additional typing.

So what does such a declaration do?  In both cases, the exception class
sets its 'type' to be a string that uniquely describes the exception.  In
addition to this, the VDatabaseException constructor takes an optional
'msg' argument.  This would be supplied by the thrower as a way of indicating
any details about why the exception was thrown.

An alternate, sometimes simpler way of subclassing the VException is like
so:

class VDatabaseException:
    public VException
{
    public:
        VDatabaseException(const std::string &thrownBy):
            VException("VDatabaseException",thrownBy) {}
};

Here, the exception's type string is set to "VDatabaseException", and
the message string is set to "In " followed by thrownBy.  The thrownBy
argument is typically an abreviated signature of the method that threw
the exception.

Now, consider that you wanted to supply additinal information about why
the exception was thrown that went above and beyond a single message
string or a thrownBy string.  Below is an example of how you can do
that:

class VDatabaseException:
    public VException
{
    private:
        std::string f_dbFunction;
        std::string f_dbQuery;
        std::string f_dbError;
    
    public:
        VDatabaseException(const std::string &thrownBy,
                           const std::string &dbFunction,
                           const std::string &dbQuery,
                           const std::string &dbError):
            VException("VDatabaseException",thrownBy),
            f_dbFunction(dbFunction),
            f_dbQuery(dbQuery),
            f_dbError(dbError)
        {
        }
        
        void printTypeSpecificExceptionDetails(std::ostream &out) {
            out<<"Database API reported an error after a call to "
            out<<f_dbFunction<<std::endl<<"while working on the "
            out<<"following query:"<<std::endl;
            out<<f_dbQuery<<std::endl;
            out<<"The error was: "<<f_dbError<<std::endl;
        }
};

Any of the above methods of subclassing VException are acceptable and
almost fully interchangable: once thrown, it does not matter how the
VException subclass was defined so long as it contains all of the
pertinant information.

Because of this property of the VException, it is perfectly acceptable
for your own code to define VException subclasses in just about any
way that you think is optimal for your own error handling needs, so
long as these subclasses can be caught and rethrown just like the
VException itself.

Now that you know how to subclass a VException, I will move on to...

THROWING

Suppose you have a subclass of VException called VMyException that has a
constructor that takes no arguments.  In this case, you may throw the
VException as follows:

    throw VMyException();

And that's all it takes.  Now consider the more likely scenario, that you
have a VException subclass, again called VMyException, that takes one
argument, a string describing the circumstances under which the exception
was thrown.  Now you might do:

    throw VMyException("Some stuff went wrong");

Easy enough, but what if you wanted to specify where in your code the
exception was thrown?  That's simple, you would just have to do:

    VMyException e("Some stuff went wrong");
    e.setThrowLocation("my_file.cpp", 198);
    throw e;

OK., so maybe that's not so simple.  It is truly annoying to hard-code
source line numbers and file names into that same source.  So, C++
offers a lazy way out:
    
    VMyException e("Some stuff went wrong");
    e.setThrowLocation(__FILE__,__LINE__);
    throw e;

However, in my humble opinion, this is still too messy.  So, I have
created a macro that will make this even easier:
    
    V_THROW(VMyException, "Some stuff went wrong");

This will translate into the same code as I listed previously, but will
require much less typing.  The only constraint for using V_THROW() is
that it assumes that your exception's constructor takes one argument.
If your own exception subclasses use a different number of arguments, feel
free to implement your own V_THROW macro(s) that take the appropriate
number of arguments.

Any of the above methods for throwing an exception are perfectly
acceptable, but you are urged to supply the throw location data somehow,
either by using __FILE__ and __LINE__ directly or by using the V_THROW()
macro.

CATCHING

So now that you've thrown an exception, someone somewhere has to catch it or
else your program will crash.  VException is designed so as to make catching
easy.  If you're extremely lazy, you can catch a VException as an
std::exception.  This saves effort, because you can take care of exceptions
coming from standard C++ libraries as well as many third-party codes in
addition to VERITAS exceptions all in one catch block:

try {
    // do stuff...
} catch (std::exception &e) {
    cerr<<"Error: "<<e.what()<<endl;
}

However, the what() method of the VException will not print all of the
information; it will only generate a one-line summary.  To get all of
the information, you'll have to catch the VException separately:

try {
    // do stuff...
} catch (VException &e) {
    cerr<<e;
}

Now, the downside here is that you'll have to have another catch block for
std::exception somewhere down the line, but by catching VException, you
are given access to all of its wonderful methods, such as the << operator.

But say that you've caught an exception and then you decided to rethrow it.
The next section tells you how...

RETHROWING

The most basic use of rethrowing in C++ is for un-initializing state once an
exception is live.  To do that, you only need to write:

// init some state
try {
    // do stuff
} catch (...) {
    // un-init the state
    throw;
}

This is a common thing to do.  But now say that you know that you're catching
and rethrowing a VException.  Well, then, you might as well take advantage of
this knowledge in order to add some information to the exception:

try {
    // do stuff
} catch (VException &e) {
    e.stream()<<"I'm now in some method doing some stuff and x = "<<x;
    V_RETHROW(e);
}

This information, called a 'comment', will then be added to the output of
both the what() and operator<<() methods' output.  Adding comments is useful
for debugging, because you end up getting a trace of how your exceptions
travel through your code.

Notice the use of the V_RETHROW() instead of just doing 'throw' directly, or
even 'throw e'.  The reason is that doing 'throw' by itself would result in
the comment that you added being lost, since 'throw' rethrows the exception
as it was caught by you.  Doing 'throw e' would be wrong because it would
result in the exception's dynamic type being lost.  The V_RETHROW() macro
takes care of these problems for you: any comments that are added prior to
the call to V_RETHROW() will make it into the exception that is later
caught, and the dynamic type of the exception will be preserved.

In the future, V_RETHROW() will also be able to add the line number of where
the rethrow happened.

******************************************************************************/



