//-*-mode:c++; mode:font-lock;-*-
/** 
 * \class ksSumFiles * \brief This code reads in a .vbf file, gets the 
 * GPSYear from the ArrayTrigger event in the first packet (1) and exits with 
 * the returns value as the GPSYear.
 *
 * Original Author: Glenn H. Sembroski 
 * $Author$
 * $Date$
 * $Revision$
 * $Tag$
 *
 **/

//Written by:
// G.H.Sembroski
//Physics Dept.
//Purdue Univ.
//West Lafayette, In. 479096
//sembrosk@physics.purdue.edu
//765-494-5172

// 06-April-2007
//Modified:


#include <iostream>
#include <iomanip>
#include <fstream>
#include <string>
#include <cmath>1

#include <time.h>
#include <exception>
#include <algorithm>

#include <VBF/VBankFileReader.h>
#include <VBF/VBankFileWriter.h>
#include <VBF/VPacket.h>
#include <VBF/VArrayEvent.h>
#include <VBF/VDatum.h>
#include <VBF/VKascadeSimulationData.h>
#include <VBF/VKascadeSimulationHeader.h>
#include <VBF/VEventType.h>

// includ e the simulation data structure
#include <VBF/VSimulationData.h>

#include "VAException.h"
#include "VAOptions.h"
#include "VSOptions.hpp"
// *************************************************************************

void usage(const std::string& progname,
           const VAOptions& command_line)
{
  std::cout << std::endl;
  std::cout <<  progname << "- Usage: " << progname 
            << " [options] "
            << std::endl;
  std::cout << progname << "- Options: "<<std::endl;
  command_line.printUsage(std::cout);
}
// ************************************************************************
 
int main(int argc, char** argv)
{ 
  try {
    time_t thetime=time(NULL);
    // **********************************************************************
    // Pick up command line arguments
    // **********************************************************************
    std::string progname = *argv;
    VAOptions command_line(argc,argv);
 
    bool goodOptions = true;
   
   std::string fSourceFileName;
    if(! (command_line.findWithValue("SourceFile",fSourceFileName,
                                     "Input file ") 
          != VAOptions::FS_NOT_FOUND) ) {
      
      std::cout<< progname << " -- Source file must be specified"
               <<std::endl;
      goodOptions = false;
      
    }
    // -------------------------------------------------------------------
    // All the command line options that the program is able to  handle 
    // have been processed, so make sure there are no more command lines
    // options available.
    // -------------------------------------------------------------------

    if(!command_line.assertNoOptions()) {
      std::cerr << progname << ": unknown options: ";
      for(int i=1;i<argc;i++)std::cerr << argv[i];
      std::cerr << std::endl;
      std::cerr << std::endl;
      usage(progname, command_line);

      exit(EXIT_FAILURE);
    }
    argv++;
    argc--;
    if(argc!=0) {
      usage(progname, command_line);
      exit(EXIT_FAILURE);
    }
    
    // ******************************************************************
    // Now we are ready to start. Open the input files;
    // ******************************************************************
    VBankFileReader* pfReader = new VBankFileReader(fSourceFileName);

    // ******************************************************
    // Read the first data packet (1)
    // ******************************************************
    VPacket* pfPacket = pfReader->readPacket(1);
    VArrayEvent*  pfAEIn = pfPacket->getArrayEvent();
    VEvent* pfEvent = pfAEIn->getEvent(0);

    unsigned int year = (unsigned)pfEvent->getGPSYear() + 2000;
   
    std::cout << year << std::endl;
    return 0;
  }
  catch (const std::exception &e) {
    std::cerr<<"Error: "<<e.what()<<std::endl;
    return 1;
  }
  catch(...) {
    std::cout<<"ksFixYear - Fatal--Unknown exception found."
             <<std::endl;
    return 1;
  }
}
// ***********************************************************************
