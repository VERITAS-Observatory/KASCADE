//randomCreateRanluxSeed.cpp
//This files only purpose is to create a unique ranlux seed file. 
// it does this by using the number of seconds from jan 0 1970.
//This is done in the calling script file with seed=$(date +%s) 
//This string comes in as the argument to the -s parameter where it is 
//converted to a number (int_32) which is used as the a 32 bit 
//seed value in a call to RLUXGO (entry point in ranlux.for) which will 
//initalize the 24 word seed array. 
//We then write this array a file specified by the -o parameter.

#include <getopt.h>
#include <stdio.h>
#include <stdlib.h>
#include <float.h>
#include <iostream>
#include <iomanip>
#include <cstring>
using namespace std;

extern "C" float ranend(int*,char *,int);
extern "C" void rluxgo(int* LUX,int* primeSeed,int* K1,int* K2);



int main( int argc, char **argv )
 {
   bool seedSupplied=false;
   int primeSeed;
   //char* oFilename="";
   char* oFilename;
   bool outID=false;

   cout<<"randomCreateRanluxSeed:Creating new RanLux seed file values"<<endl;



  int c;   //c MUST be declared outside the while
           //(gcc compiler 'feature' 3.0.4)
  while ((c = getopt(argc, argv, "o:s:")) != -1)
//Note for Myself:In the getopt call, in the allowed options list, an option 
//followed by a ':' is required to be followed by an option argument. An 
//option not folowed by a ':' has no following option argumnet. Thus in 
//"i:to:" above, options -i and -o must have arguments, option -f must not!

    // followed by a : means it expects an argument to follow
    {
      switch (c)
	{
	case 'o':
	   cout<<"Output Randomized Seed file (option -o): "<<optarg<<endl;
	  oFilename=optarg;
          outID=true;
	  break;                 //This break just gets us out of the switch
                                 //not the while!
	case 's':
	  cout<<"Integer Supplied Primary seed(option -s):"<<optarg<<endl;
	  primeSeed=atoi(optarg);
 	  seedSupplied=true;
	  break;                 //This break just gets us out of the switch
	case 'h':
	  cout<<" *****************************************************"<<endl;
	  cout<<" * Command line input options:"<<endl;
	  cout<<" *****************************************************"<<endl;
	  cout<<" * -o Specify Output RanLux Seed file name "<<endl;
	  cout<<" * -t Specify primary integer seed (< 32 bits)"<<endl;
	  cout<<" * -h Print this options summary"<<endl;
	  cout<<" *****************************************************"<<endl;
	  return 0;
 	case '?':
	  cout<<"Unknown option:"<<c<<endl;
	  return 1;
	}
    }
  if(!seedSupplied)
    {
      cout<<"Must supply 32 bit positive integer for primary seed (option-s)"
	  <<endl;
      return 0;
    }
  if(!outID)
    {
      cout<<"Output RanLux Seed  file name not specified (option -o)!"<<
	endl;
      return 0;
    }


  //Initalize the seed:
  int k1=0;
  int k2=0;
  int lux=3;
  cout<<primeSeed<<endl;
  rluxgo(&lux,&primeSeed,&k1,&k2);

  //Now write oput the file
  int printseeds=1;   //Flag to also print seed values 
  ranend(&printseeds,oFilename,strlen(oFilename));

  return 0;
 }
