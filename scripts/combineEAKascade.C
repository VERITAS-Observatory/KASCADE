
#include "aclicPreProcCommands.h"
#include <Riostream.h>

#include <VARootIO.h>
#include "../include/VAEffectiveAreaCommon.h"
#include "../include/VAEffectiveArea.h"
#include <TList.h>
 
void combineEAK(string outFile, string sourcefile)
// **********************************************************************
// Add the EA tables in the SubEA sourcefile to the outFile
// Check that if the out file already has such a table it is only
// replaced if the new table has non 0 entries in it
// If no table exists in the Output file then add the new one even if its empty
// **********************************************************************
{
  // ************************************
  // first open the out and source files
  // ************************************

  VARootIO ioDestination(outFile);
  ioDestination.loadTheRootFile();
  cout<<"outFile: "<<outFile<<endl;

  VARootIO ioSource(sourcefile, true);
  ioSource.loadTheRootFile();

 
  // **************************************
  // get all the lookup table keys for the source file
   // **************************************
  TList *keysSource = ioSource.getDirectoryKeys("effective_areas", true);
  if(!keysSource){
    cout<<"Couldn't load the keys for "<<sourcefile<<endl;
    return;
  }

  cout << "Found " << keysSource->GetEntries() << " keys to check." << endl;
  int oneTenth = (int)(keysSource->GetEntries()/10);

  VAEffectiveArea *tableDestination;
  VAEffectiveArea *tableSource;
  TObject *objectSource = NULL;
  TObject *objectDestination = NULL;

  for (Int_t i=0; i<keysSource->GetEntries(); i++){
    if ( i%oneTenth == 0 ) {
      cout << "On key " << i << " of " << keysSource->GetEntries() << " ("
	   << (int)(100.*i/keysSource->GetEntries()) << "%)" << endl;
    }
    // *******************************************************************
    // Found a small problem. It looks like some times there are tables that should be
    // empty that are not.Change things to only replace a table in 
    // the destination file only if the new one is bigger
    // *******************************************************************

    // Step 1
    objectSource = ioSource.loadAnObject(keysSource->At(i)->GetName(), 
					 "effective_areas", true);
    int numEntriesSource=0;
    // **********************************************************************
    // Make sure this object exists and is a VAEffectiveArea
   // If so get the number of entries in the table
    // otherwise ignote it
    // **********************************************************************
    if(objectSource==NULL){
      cout<<"Fatal--Should have found: "<<keysSource->At(i)->GetName()
	  <<" in "<<sourcefile<<", but its not there!!!"<<endl;
      exit(EXIT_FAILURE);
    }

    // Step 2
    if(strcmp(objectSource->ClassName(), "VAEffectiveArea")==0){
      tableSource = (VAEffectiveArea*)objectSource;
      numEntriesSource=tableSource->pfEnergyBias_2D->GetEntries();
    }    
    else{
      cout<<"Ignoring: "<< keysSource->At(i)->GetName()<< " in source file: "
	  <<sourcefile<<endl;
      continue;   //not a VAEffectiveArea, ignore it.
    }


    // Step 3
    // ***********************************************************************
    // Now look for the same object in the destination file. If its there
    // and is a VAEffectiveArea get the mnumber of entries it has
    // ***********************************************************************
    objectDestination = NULL;
    objectDestination = ioDestination.loadAnObject(keysSource->At(i)->GetName(), 
						   "effective_areas", true );

    int numEntriesDestination=0;
    bool gotDestinationTable=false;
    if(objectDestination!=NULL){
      if(strcmp(objectDestination->ClassName(), "VAEffectiveArea")!=0){
	// *************************************************
	// Opps , not a VAEffectiveArea
	// *************************************************
	//cout<<"FATAL--Object: "<<keysSource->At(i)->GetName()<<" in file "<<outfile
	//    <<" is a: "<<objectDestination->ClassName()<<" not a VALookupTable"
	//    <<endl;
	exit(EXIT_FAILURE);
      }
      else{
	tableDestination = (VAEffectiveArea*)objectDestination;
	numEntriesDestination=tableDestination->pfEnergyBias_2D->GetEntries();
	gotDestinationTable=true;
      }
    }

    // ***********************************************************************
    // Step 4
    // If no such table exist in the destination file than add to the destination 
    // file a copy of the one in the source file
    // ***********************************************************************
    if(!gotDestinationTable){
      ioDestination.writeAnObject(objectSource, keysSource->At(i)->GetName(),
				                                 "effective_areas");
      cout<<"Added   : "<< keysSource->At(i)->GetName()<<endl;
    }
    else{
      // **********************************************************************
      // Step 5
      // If table in source is bigger than table in destination, replace destination
      // table
      // **********************************************************************
      if(numEntriesSource>numEntriesDestination){
	ioDestination.writeAnObject( objectSource, keysSource->At(i)->GetName(),
				     "effective_areas");
	cout<<"Replaced: "<< keysSource->At(i)->GetName()<<endl;
      }
    }

    if(objectDestination!=NULL){
      delete objectDestination; //Need to be careful about cleaning up
      objectDestination = NULL;
    }
    if(objectSource!=NULL){
      delete objectSource;    //Need to be careful about cleaning up
      objectSource = NULL;
    }
  }


  // And done!
  //cout<<"close Destination"<<endl;
  ioDestination.closeTheRootFile();
  //cout<<"close Source"<<endl;
  ioSource.closeTheRootFile();
  return;
}

void copyTree(string outFile, string sourcefile){

  VARootIO ioDestination(outFile);
  ioDestination.loadTheRootFile();

  VARootIO ioSource(sourcefile, true);
  ioSource.loadTheRootFile();


  TTree *tree = (TTree*)ioSource.loadAnObject("paramTree", "effective_areas",
					      true);
  if(!tree){
    cout<<"Couldn't load the tree"<<endl;
    return;
  }

  ioDestination.writeAnObject(tree, "paramTree", "effective_areas");

  ioDestination.closeTheRootFile();
  ioSource.closeTheRootFile();
}

void combineEAKFromList(string listOfFiles, string eaFile)
// *********************************************************************
// combine all the tables from a list of sub LT files into one  EA file.
// *********************************************************************
{
  ifstream infile;
  infile.open(listOfFiles.c_str(), ifstream::in);
  if(!infile.is_open()){
    cerr<<"Couldn't open "<<listOfFiles<<endl;
    return;
  }

  bool firstPass=true;
  while(true){
    // *******************************************************
    //First line in List file is the  first SubEA file.
    //Init eaFile by copying that to the ea combined file
    // *******************************************************
    char buffer[200];
    infile.getline(buffer, 200);
    //Lines  are the input sub EA files.
    if(infile.eof()){
      break;
    }
    string source(buffer);
    cout<<"Operating on "<<source<<endl;

    if(firstPass){
      string copyCommand="cp -v " + source + " " + eaFile;
      system(copyCommand.c_str());

      //      ifstream f1(source.c_str(), fstream::binary);
      //ofstream f2(eaFile.c_str(), fstream::trunc|fstream::binary);
      //f2 << f1.rdbuf(); 
 
      cout<<"Destination EA Lookup Table File: "<<eaFile<<endl;
      firstPass=false;
    }
    else{
      // Add in the new subEA tables. Replace table only if replacement is non-0
      combineEAK(eaFile, source);
    }
  }

 
  cout<<"All tables have been combined into "<<eaFile<<endl;
  cout<<"Now you need to run the buildTree program in resultsExtractor/bin"
      <<endl;
  return;
}
  
