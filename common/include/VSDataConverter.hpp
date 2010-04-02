//-*-mode:c++; mode:font-lock;-*-

// This file is shared between the Simulations and OAWG. To avoid a
// fork in development, please coordinate changes with Stephen Fegan.
// email: sfegan@astro.ucla.edu

// Old header for consistency with Simulations package

/*! \file VSDataConverter.hpp
  Class encapsulating wisdom of encoding and decoding of data into string

  \author     Stephen Fegan               \n
              UCLA                        \n
              sfegan@astro.ucla.edu       \n        

  \version    0.1
  \date       03/23/2005
*/

// New header for consistency with OAWG package

/**
 *
 * Original Author: Stephen Fegan
 * $Author$
 * $Date$
 * $Revision$
 * $Tag$
 *
 **/

#ifndef VSDATACONVERTER_HPP
#define VSDATACONVERTER_HPP

#ifndef __STDC_LIMIT_MACROS
#define __STDC_LIMIT_MACROS
#endif

#ifndef __STDC_CONSTANT_MACROS
#define __STDC_CONSTANT_MACROS
#endif

#include <iostream>
#include <iomanip>
#include <sstream>
#include <string>
#include <vector>
#include <cstdlib>
#include <limits>
#include <stdint.h>
#include <cstring>

#ifndef INT8_MIN

#if 0

#if __WORDSIZE == 64
#define __INT64_C(c)  c ## L
#define __UINT64_C(c) c ## UL
#else
#define __INT64_C(c)  c ## LL
#define __UINT64_C(c) c ## ULL
#endif

/* Limits of integral types.  */

/* Minimum of signed integral types.  */
#define INT8_MIN               (-128)
#define INT16_MIN              (-32767-1)
#define INT32_MIN              (-2147483647-1)
#define INT64_MIN              (-__INT64_C(9223372036854775807)-1)
/* Maximum of signed integral types.  */
#define INT8_MAX               (127)
#define INT16_MAX              (32767)
#define INT32_MAX              (2147483647)
#define INT64_MAX              (__INT64_C(9223372036854775807))
/* Maximum of unsigned integral types.  */
#define UINT8_MAX              (255)
#define UINT16_MAX             (65535)
#define UINT32_MAX             (4294967295U)
#define UINT64_MAX             (__UINT64_C(18446744073709551615))

#else

#error __STDC_LIMIT_MACROS and __STDC_CONSTANT_MACROS must be defined before stdint is included for the first time

#endif

#endif

#ifndef _OAWG
namespace VERITAS
{
#endif

  template<typename T> class VSDatumConverter
  {
  public:
    static inline void toString(std::string& s, const T& x, 
				bool low_precision=false);
    static inline bool fromString(T& x, const char* s);
    static inline std::string typeName();
  };

  template<> class VSDatumConverter<bool>
  {
  public:
    static inline void toString(std::string& s, const bool& x,
				bool low_precision=false);
    static inline bool fromString(bool& x, const char* s);
    static inline std::string typeName();
  };

  template<> class VSDatumConverter<uint8_t>
  {
  public:
    static inline void toString(std::string& s, const uint8_t& x, 
				bool low_precision=false);
    static inline bool fromString(uint8_t& x, const char* s);
    static inline std::string typeName();
  };

  template<> class VSDatumConverter<int8_t>
  {
  public:
    static inline void toString(std::string& s, const int8_t& x, 
				bool low_precision=false);
    static inline bool fromString(int8_t& x, const char* s);
    static inline std::string typeName();
  };

  template<> class VSDatumConverter<uint16_t>
  {
  public:
    static inline void toString(std::string& s, const uint16_t& x, 
				bool low_precision=false);
    static inline bool fromString(uint16_t& x, const char* s);
    static inline std::string typeName();
  };

  template<> class VSDatumConverter<int16_t>
  {
  public:
    static inline void toString(std::string& s, const int16_t& x, 
				bool low_precision=false);
    static inline bool fromString(int16_t& x, const char* s);
    static inline std::string typeName();
  };

  template<> class VSDatumConverter<uint32_t>
  {
  public:
    static inline void toString(std::string& s, const uint32_t& x, 
				bool low_precision=false);
    static inline bool fromString(uint32_t& x, const char* s);
    static inline std::string typeName();
  };
  
  template<> class VSDatumConverter<int32_t>
  {
  public:
    static inline void toString(std::string& s, const int32_t& x, 
				bool low_precision=false);
    static inline bool fromString(int32_t& x, const char* s);
    static inline std::string typeName();
  };

  template<> class VSDatumConverter<uint64_t>
  {
  public:
    static inline void toString(std::string& s, const uint64_t& x, 
				bool low_precision=false);
    static inline bool fromString(uint64_t& x, const char* s);
    static inline std::string typeName();
  };
  
  template<> class VSDatumConverter<int64_t>
  {
  public:
    static inline void toString(std::string& s, const int64_t& x, 
				bool low_precision=false);
    static inline bool fromString(int64_t& x, const char* s);
    static inline std::string typeName();
  };

  template<> class VSDatumConverter<float>
  {
  public:
    static inline void toString(std::string& s, const float& x, 
				bool low_precision=false);
    static inline bool fromString(float& x, const char* s);
    static inline std::string typeName();
  };
  
  template<> class VSDatumConverter<double>
  {
  public:
    static inline void toString(std::string& s, const double& x, 
				bool low_precision=false);
    static inline bool fromString(double& x, const char* s);
    static inline std::string typeName();
  };

  template<> class VSDatumConverter<long double>
  {
  public:
    static inline void toString(std::string& s, const long double& x, 
				bool low_precision=false);
    static inline bool fromString(long double& x, const char* s);
    static inline std::string typeName();
  };

  template<> class VSDatumConverter<std::string>
  {
  public:
    static inline void toString(std::string& s, const std::string& x,
				bool low_precision=false);
    static inline bool fromString(std::string& x, const char* s);
    static inline std::string typeName();
  };

  template<typename T> class VSDatumConverter<std::vector<T> >
  {
  public:
    static inline void toString(std::string& s, const std::vector<T>& x, 
				bool low_precision=false);
    static inline bool fromString(std::vector<T>& x, const char* s);
    static inline std::string typeName();
  };

  template<typename T1, typename T2> class VSDatumConverter<std::pair<T1,T2> >
  {
  public:
    static inline void toString(std::string& s, const std::pair<T1,T2>& x, 
				bool low_precision=false);
    static inline bool fromString(std::pair<T1,T2>& x, const char* s);
    static inline std::string typeName();
  };

  template<typename T1, typename T2, typename T3> class triple
  {
  public:
    triple(): first(), second(), third() { /* nothing to see here */ }
    triple(const T1& _first, const T2& _second, const T3& _third)
      : first(_first), second(_second), third(_third) { }
    T1 first;
    T2 second;
    T3 third;
  };

  template<typename T1, typename T2, typename T3> 
  class VSDatumConverter<triple<T1,T2,T3> >
  {
  public:
    static inline void toString(std::string& s, const triple<T1,T2,T3>& x, 
				bool low_precision=false);
    static inline bool fromString(triple<T1,T2,T3>& x, const char* s);
    static inline std::string typeName();
  };

  template<typename T1, typename T2, typename T3, typename T4> class quad
  {
  public:
    quad(): first(), second(), third(), fourth() { /* nothing to see here */ }
    quad(const T1& _1st, const T2& _2nd, const T3& _3rd, const T4& _4th)
      : first(_1st), second(_2nd), third(_3rd), fourth(_4th) { }
    T1 first;
    T2 second;
    T3 third;
    T4 fourth;
  };

  template<typename T1, typename T2, typename T3, typename T4> 
  class VSDatumConverter<quad<T1,T2,T3,T4> >
  {
  public:
    static inline void toString(std::string& s, const quad<T1,T2,T3,T4>& x, 
				bool low_precision=false);
    static inline bool fromString(quad<T1,T2,T3,T4>& x, const char* s);
    static inline std::string typeName();
  };
  
  class VSDataConverter
  {
  public:
    template<typename T> static void toString(std::string& s, const T& x,
					      bool low_precision=false)
    { VSDatumConverter<T>::toString(s,x,low_precision); }
    template<typename T> static std::string toString(const T& x,
						     bool low_precision=false)
    { std::string s; VSDatumConverter<T>::toString(s,x,low_precision);
      return s; }
    template<typename T> static bool fromString(T& x, const char* s)
    { return VSDatumConverter<T>::fromString(x,s); }
    template<typename T> static bool fromString(T& x, const std::string& s)
    { return VSDatumConverter<T>::fromString(x,s.c_str()); }
    template<typename T> static std::string typeName()
    { return VSDatumConverter<T>::typeName(); }
    template<typename T> static std::string typeNameOf(const T& __attribute((unused)) x)
    { return VSDatumConverter<T>::typeName(); }
  };

#if 1
  template<typename T> 
  inline void VSDatumConverter<T>::toString(std::string& s, const T& x,
					    bool low_precision)
  {
    std::ostringstream _stream;
    _stream << x;
    s=_stream.str();
  }

  template<typename T> 
  inline bool VSDatumConverter<T>::fromString(T& x, const char* s)
  {
    std::string _s(s);
    std::istringstream _stream(_s);
    return _stream >> x;
  }

  template<typename T> 
  inline std::string VSDatumConverter<T>::typeName()
  {
    return std::string("unknown");
  }
#endif

  inline void VSDatumConverter<bool>::
  toString(std::string& s, const bool& x,
	   bool low_precision)
  {
    if(x)s.assign("1");
    else s.assign("0");
  }
  
  inline bool VSDatumConverter<bool>::
  fromString(bool& x, const char* s)
  {
    switch(*s)
      {
      case '0':
	if(s[1]=='\0')x=false;
	else return false;
	break;
      case '1':
	if(s[1]=='\0')x=true;
	else return false;
	break;
      case 'T':
      case 't':
	if((s[1]=='\0')||
	   ((tolower(s[1])=='r')&&
	    (s[2]!='\0')&&(tolower(s[2])=='u')&&
	    (s[3]!='\0')&&(tolower(s[3])=='e')&&(s[4]=='\0')))
	  x=true;
	else 
	  return false;
	break;
      case 'F':
      case 'f':
	if((s[1]=='\0')||
	   ((tolower(s[1])=='a')&&
	    (s[2]!='\0')&&(tolower(s[2])=='l')&&
	    (s[3]!='\0')&&(tolower(s[3])=='s')&&
	    (s[4]!='\0')&&(tolower(s[4])=='e')&&(s[5]=='\0')))
	  x=false;
	else 
	  return false;
	break;
      default:
	return false;
      }
    return true;
  }

  inline std::string VSDatumConverter<bool>::typeName()
  {
    return std::string("bool");
  }
  
  inline void VSDatumConverter<uint8_t>::
  toString(std::string& s, const uint8_t& x,
	   bool low_precision)
  {
    uint16_t y=x;
    char buffer[4];
    sprintf(buffer,"%hu",y);
    s.assign(buffer);
  }

  inline bool VSDatumConverter<uint8_t>::
  fromString(uint8_t& x, const char* s)
  {
    char* e;
    unsigned long y=strtoul(s,&e,10);
    if((e==s)||(*e!='\0')||(y>(unsigned long)UINT8_MAX))return false;
    x=y;
    return true;
  }

  inline std::string VSDatumConverter<uint8_t>::typeName()
  {
    return std::string("uint8_t");
  }

  inline void VSDatumConverter<int8_t>::
  toString(std::string& s, const int8_t& x,
	   bool low_precision)
  {
    int16_t y=x;
    char buffer[5];
    sprintf(buffer,"%hd",y);
    s.assign(buffer);
  }
  
  inline bool VSDatumConverter<int8_t>::
  fromString(int8_t& x, const char* s)
  {
    char* e;
    long y=strtol(s,&e,10);
    if((e==s)||(*e!='\0')||(y>(long)INT8_MAX)||(y<(long)INT8_MIN))
      return false;
    x=y;
    return true;
  }

  inline std::string VSDatumConverter<int8_t>::typeName()
  {
    return std::string("int8_t");
  }

  inline void VSDatumConverter<uint16_t>::
  toString(std::string& s, const uint16_t& x,
	   bool low_precision)
  {
    char buffer[6];
    sprintf(buffer,"%hu",x);
    s.assign(buffer);
  }

  inline bool VSDatumConverter<uint16_t>::
  fromString(uint16_t& x, const char* s)
  {
    char* e;
    unsigned long y=strtoul(s,&e,10);
    if((e==s)||(*e!='\0')||(y>(unsigned long)UINT16_MAX))return false;
    x=y;
    return true;
  }

  inline std::string VSDatumConverter<uint16_t>::typeName()
  {
    return std::string("uint16_t");
  }

  inline void VSDatumConverter<int16_t>::
  toString(std::string& s, const int16_t& x,
	   bool low_precision)
  {
    char buffer[7];
    sprintf(buffer,"%hd",x);
    s.assign(buffer);
  }

  inline bool VSDatumConverter<int16_t>::
  fromString(int16_t& x, const char* s)
  {
    char* e;
    long y=strtol(s,&e,10);
    if((e==s)||(*e!='\0')||(y>(long)INT16_MAX)||(y<(long)INT16_MIN))
      return false;
    x=y;
    return true;
  }

  inline std::string VSDatumConverter<int16_t>::typeName()
  {
    return std::string("int16_t");
  }

  inline void VSDatumConverter<uint32_t>::
  toString(std::string& s, const uint32_t& x,
	   bool low_precision)
  {
    char buffer[11];
    sprintf(buffer,"%u",x);
    s.assign(buffer);
  }

  inline bool VSDatumConverter<uint32_t>::
  fromString(uint32_t& x, const char* s)
  {
    char* e;
    unsigned long y=strtoul(s,&e,10);
    if((e==s)||(*e!='\0')||(y>(unsigned long)UINT32_MAX))return false;
    x=y;
    return true;
  }
  
  inline std::string VSDatumConverter<uint32_t>::typeName()
  {
    return std::string("uint32_t");
  }

  inline void VSDatumConverter<int32_t>::
  toString(std::string& s, const int32_t& x,
	   bool low_precision)
  {
    char buffer[12];
    sprintf(buffer,"%d",x);
    s.assign(buffer);
  }
      
  inline bool VSDatumConverter<int32_t>::
  fromString(int32_t& x, const char* s)
  {
    char* e;
    long y=strtol(s,&e,10);
    if((e==s)||(*e!='\0')||(y>(long)INT32_MAX)||(y<(long)INT32_MIN))
      return false;
    x=y;
    return true;
  }

  inline std::string VSDatumConverter<int32_t>::typeName()
  {
    return std::string("int32_t");
  }

  inline void VSDatumConverter<uint64_t>::
  toString(std::string& s, const uint64_t& x,
	   bool low_precision)
  {
    char buffer[19];
#if __WORDSIZE == 64
    sprintf(buffer,"%lu",x);
#else
    sprintf(buffer,"%llu",x);
#endif
    s.assign(buffer);
  }

  inline bool VSDatumConverter<uint64_t>::
  fromString(uint64_t& x, const char* s)
  {
    char* e;
    x=strtoull(s,&e,10);
    return((e!=s)&&(*e=='\0'));    
  }
  
  inline std::string VSDatumConverter<uint64_t>::typeName()
  {
    return std::string("uint64_t");
  }

  inline void VSDatumConverter<int64_t>::
  toString(std::string& s, const int64_t& x,
	   bool low_precision)
  {
    char buffer[20];
#if __WORDSIZE == 64
    sprintf(buffer,"%ld",x);
#else
    sprintf(buffer,"%lld",x);
#endif
    s.assign(buffer);
  }
 
  inline bool VSDatumConverter<int64_t>::
  fromString(int64_t& x, const char* s)
  {
    char* e;
    x=strtoll(s,&e,10);
    return((e!=s)&&(*e=='\0'));    
  }

  inline std::string VSDatumConverter<int64_t>::typeName()
  {
    return std::string("int64_t");
  }

  inline void VSDatumConverter<float>::
  toString(std::string& s, const float& x,
	   bool low_precision)
  {
    std::ostringstream stream;
    if(!low_precision)
      stream << std::setprecision(std::numeric_limits<float>::digits10)
	     << std::scientific ;
    stream << x;
    s=stream.str();
  }

  inline bool VSDatumConverter<float>::
  fromString(float& x, const char* s)
  {
    char* e;
    x=strtof(s,&e);
    return((e!=s)&&(*e=='\0'));
  }
  
  inline std::string VSDatumConverter<float>::typeName()
  {
    return std::string("float");
  }

  inline void VSDatumConverter<double>::
  toString(std::string& s, const double& x,
	   bool low_precision)
  {
    std::ostringstream stream;
    if(!low_precision)
      stream << std::setprecision(std::numeric_limits<double>::digits10)
	     << std::scientific;
    stream << x;
    s=stream.str();
  }
   
  inline bool VSDatumConverter<double>::
  fromString(double& x, const char* s)
  {
    char* e;
    x=strtod(s,&e);
    return((e!=s)&&(*e=='\0'));
  }

  inline std::string VSDatumConverter<double>::typeName()
  {
    return std::string("double");
  }

  inline void VSDatumConverter<long double>::
  toString(std::string& s, const long double& x,
	   bool low_precision)
  {
    std::ostringstream stream;
    if(!low_precision)
      stream << std::setprecision(std::numeric_limits<long double>::digits10)
	     << std::scientific;
    stream << x;
    s=stream.str();
  }

  inline bool VSDatumConverter<long double>::
  fromString(long double& x, const char* s)
  {
    char* e;
    x=strtold(s,&e);
    return((e!=s)&&(*e=='\0'));
  }
  
  inline std::string VSDatumConverter<long double>::typeName()
  {
    return std::string("long double");
  }

  inline void VSDatumConverter<std::string>::
  toString(std::string& s, const std::string& x,
	   bool low_precision)
  {
    s=x;
  }

  inline bool VSDatumConverter<std::string>::
  fromString(std::string& x, const char* s)
  {
    x=s;
    return true;
  }

  inline std::string VSDatumConverter<std::string>::typeName()
  {
    return std::string("string");
  }

  template<typename T1, typename T2> 
  inline void VSDatumConverter<std::pair<T1,T2> >::
  toString(std::string& s, const std::pair<T1,T2>& x,
	   bool low_precision)
  {
    static const std::string sep("/");
    VSDatumConverter<T1>::toString(s,x.first,low_precision);
    s+=sep;
    std::string s2;
    VSDatumConverter<T2>::toString(s2,x.second,low_precision); 
    s+=s2;
  }

  template<typename T1, typename T2> inline bool 
  VSDatumConverter<std::pair<T1,T2> >::
  fromString(std::pair<T1,T2>& x, const char* s)
  {
    static const char sep = '/';
    const char* find = s;
    while((*find!='\0')&&(*find!=sep))find++;
    if(*find=='\0')return false;
    std::string val_string1(s,find);
    if(VSDatumConverter<T1>::fromString(x.first,val_string1.c_str())==false)
      return false;
    std::string val_string2(find+1,s+strlen(s));
    if(VSDatumConverter<T2>::fromString(x.second,val_string2.c_str())==false)
      return false;
    return true;
  }

  template<typename T1, typename T2> 
  inline std::string VSDatumConverter<std::pair<T1,T2> >::typeName()
  {
    return 
      std::string("pair<")+VSDatumConverter<T1>::typeName()
      +std::string(",")+VSDatumConverter<T2>::typeName()+std::string(">");
  }

  template<typename T1, typename T2, typename T3> 
  inline void VSDatumConverter<triple<T1,T2,T3> >::
  toString(std::string& s, const triple<T1,T2,T3>& x,
	   bool low_precision)
  {
    static const std::string sep("/");
    VSDatumConverter<T1>::toString(s,x.first,low_precision);
    s+=sep;
    std::string s2;
    VSDatumConverter<T2>::toString(s2,x.second,low_precision); 
    s+=s2;
    s+=sep;
    std::string s3;
    VSDatumConverter<T3>::toString(s3,x.third,low_precision); 
    s+=s3;
  }

  template<typename T1, typename T2, typename T3> inline bool 
  VSDatumConverter<triple<T1,T2,T3> >::
  fromString(triple<T1,T2,T3>& x, const char* s)
  {
    static const char sep = '/';
    const char* find1 = s;
    while((*find1!='\0')&&(*find1!=sep))find1++;
    if(*find1=='\0')return false;
    std::string val_string1(s,find1);
    if(VSDatumConverter<T1>::fromString(x.first,val_string1.c_str())==false)
      return false;
    const char* find2 = find1+1;
    while((*find2!='\0')&&(*find2!=sep))find2++;
    std::string val_string2(find1+1,find2);
    if(VSDatumConverter<T2>::fromString(x.second,val_string2.c_str())==false)
      return false;
    std::string val_string3(find2+1,s+strlen(s));
    if(VSDatumConverter<T3>::fromString(x.third,val_string3.c_str())==false)
      return false;
    return true;
  }

  template<typename T1, typename T2, typename T3> 
  inline std::string VSDatumConverter<triple<T1,T2,T3> >::typeName()
  {
    return 
      std::string("triple<")+VSDatumConverter<T1>::typeName()
      +std::string(",")+VSDatumConverter<T2>::typeName()
      +std::string(",")+VSDatumConverter<T3>::typeName()
      +std::string(">");
  }

  template<typename T1, typename T2, typename T3, typename T4> 
  inline void VSDatumConverter<quad<T1,T2,T3,T4> >::
  toString(std::string& s, const quad<T1,T2,T3,T4>& x,
	   bool low_precision)
  {
    static const std::string sep("/");
    VSDatumConverter<T1>::toString(s,x.first,low_precision);
    s+=sep;
    std::string s2;
    VSDatumConverter<T2>::toString(s2,x.second,low_precision); 
    s+=s2;
    s+=sep;
    std::string s3;
    VSDatumConverter<T3>::toString(s3,x.third,low_precision); 
    s+=s3;
    s+=sep;
    std::string s4;
    VSDatumConverter<T4>::toString(s4,x.fourth,low_precision); 
    s+=s4;
  }

  template<typename T1, typename T2, typename T3, typename T4> inline bool 
  VSDatumConverter<quad<T1,T2,T3,T4> >::
  fromString(quad<T1,T2,T3,T4>& x, const char* s)
  {
    static const char sep = '/';
    const char* find1 = s;
    while((*find1!='\0')&&(*find1!=sep))find1++;
    if(*find1=='\0')return false;
    std::string val_string1(s,find1);
    if(VSDatumConverter<T1>::fromString(x.first,val_string1.c_str())==false)
      return false;

    const char* find2 = find1+1;
    while((*find2!='\0')&&(*find2!=sep))find2++;
    std::string val_string2(find1+1,find2);
    if(VSDatumConverter<T2>::fromString(x.second,val_string2.c_str())==false)
      return false;
    const char* find3 = find2+1;
    while((*find3!='\0')&&(*find3!=sep))find3++;
    std::string val_string3(find2+1,find3);
    if(VSDatumConverter<T3>::fromString(x.third,val_string3.c_str())==false)
      return false;
    std::string val_string4(find3+1,s+strlen(s));
    if(VSDatumConverter<T4>::fromString(x.fourth,val_string4.c_str())==false)
      return false;
    return true;
  }

  template<typename T1, typename T2, typename T3, typename T4> 
  inline std::string VSDatumConverter<quad<T1,T2,T3,T4> >::typeName()
  {
    return 
      std::string("quad<")+VSDatumConverter<T1>::typeName()
      +std::string(",")+VSDatumConverter<T2>::typeName()
      +std::string(",")+VSDatumConverter<T3>::typeName()
      +std::string(",")+VSDatumConverter<T4>::typeName()
      +std::string(">");
  }

  template<typename T> inline void VSDatumConverter<std::vector<T> >::
  toString(std::string& s, const std::vector<T>& x,
	   bool low_precision)
  {
    static const std::string sep(",");
    s.clear();
    bool first = true;
    for(typename std::vector<T>::const_iterator ival=x.begin();
        ival!=x.end();ival++)
      {
        std::string sval;
        VSDatumConverter<T>::toString(sval,*ival,low_precision);
        if(!first)s += sep;
        else first=false;
        s += sval;
      }
  }

  template<typename T> inline bool VSDatumConverter<std::vector<T> >::
  fromString(std::vector<T>& x, const char* s)
  {
    x.clear();
    const std::string csl(s);
    static const char sep[] = ", \t\r\n";
    std::string::size_type next = 0;
    bool result = true;
    while((next != std::string::npos)&&(next < csl.length()))
      {
        std::string::size_type here = next;
        next = csl.find_first_of(sep,here);
        std::string val_string;
        if(next == std::string::npos)val_string = csl.substr(here);
        else val_string = csl.substr(here,(next++)-here);
        if(1) // !val_string.empty()) -- better to allow empty strings ?
          {
            T val;
	    if(VSDatumConverter<T>::fromString(val,val_string.c_str()))
	      x.push_back(val);
	    else
	      result = false;
          }
      }
    return result;
  }

  template<typename T> inline std::string VSDatumConverter<std::vector<T> >::
  typeName()
  {
    return
      std::string("vector<")+VSDatumConverter<T>::typeName()+std::string(">");
  }
  
#ifndef _OAWG
}
#endif

#endif // VSDATACONVERTER_HPP
