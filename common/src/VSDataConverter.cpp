//-*-mode:c++; mode:font-lock;-*-

// This file is shared between the Simulations and OAWG. To avoid a
// fork in development, please coordinate changes with Stephen Fegan.
// email: sfegan@astro.ucla.edu

// Old header for consistency with Simulations package

/*! \file VSDataConverter.cpp
  Class encapsulating wisdom of encoding and decoding of data into string

  \author     Stephen Fegan               \n
              UCLA                        \n
              sfegan@astro.ucla.edu       \n        

  \version    0.1
  \date       03/23/2005
*/

// New header for consistency with OAWG package

/**
 * \class VSDataConverter
 * \ingroup common
 * \brief Class encapsulating wisdom of encoding and decoding of data into string
 *
 * Here is a tedious verbose multi-line description of all
 * the details of the code, more than you would
 * ever want to read. Generally, all the important documentation
 * goes in the .cpp files.
 *
 * Original Author: Stephen Fegan
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

#include "VSDataConverter.hpp"

#ifdef TEST_MAIN

#include <iostream> 

using namespace VERITAS;

int main(int argc, char** argv)
{
  bool x=true;
  std::cout << "A: " << VSDataConverter::fromString(x,"0");
  std::cout << ' ' << x << std::endl;
  std::cout << "B: " << VSDataConverter::fromString(x,"1");
  std::cout << ' ' << x << std::endl;
  std::cout << "C: " << VSDataConverter::fromString(x,"2");
  std::cout << ' ' << x << std::endl;
  std::cout << "D: " << VSDataConverter::fromString(x,"10");
  std::cout << ' ' << x << std::endl;
  std::cout << "E: " << VSDataConverter::fromString(x,"01");
  std::cout << ' ' << x << std::endl;
  std::cout << "F: " << VSDataConverter::fromString(x,"t");
  std::cout << ' ' << x << std::endl;
  std::cout << "G: " << VSDataConverter::fromString(x,"T");
  std::cout << ' ' << x << std::endl;
  std::cout << "H: " << VSDataConverter::fromString(x,"f");
  std::cout << ' ' << x << std::endl;
  std::cout << "I: " << VSDataConverter::fromString(x,"F");
  std::cout << ' ' << x << std::endl;

  std::cout << "J: " << VSDataConverter::fromString(x,"tr");
  std::cout << ' ' << x << std::endl;
  std::cout << "K: " << VSDataConverter::fromString(x,"tru");
  std::cout << ' ' << x << std::endl;
  std::cout << "L: " << VSDataConverter::fromString(x,"truely");
  std::cout << ' ' << x << std::endl;
  std::cout << "M: " << VSDataConverter::fromString(x,"true");
  std::cout << ' ' << x << std::endl;

  std::cout << "N: " << VSDataConverter::fromString(x,"fa");
  std::cout << ' ' << x << std::endl;
  std::cout << "O: " << VSDataConverter::fromString(x,"fal");
  std::cout << ' ' << x << std::endl;
  std::cout << "P: " << VSDataConverter::fromString(x,"fals");
  std::cout << ' ' << x << std::endl;
  std::cout << "Q: " << VSDataConverter::fromString(x,"fasse");
  std::cout << ' ' << x << std::endl;
  std::cout << "R: " << VSDataConverter::fromString(x,"false");
  std::cout << ' ' << x << std::endl;
}

#endif
