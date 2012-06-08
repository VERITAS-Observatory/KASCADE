
#include "aclicPreProcCommands.h"
#include <Riostream.h>

#include <VARootIO.h>
#include "../include/VALookupTable.h"
#include <TList.h>



void combineLTK(string outFile, string sourcefile)
// **********************************************************************
// Add the LT tables in the sub lT sourcefile to the outFile
// Check that if the out file already has such a table it is only
// replaced if the new table has non 0 entries in it
// If no table exists in the Output file then add the new one even if its empty
// **********************************************************************
{
  
  // ************************************
  // first open the out and source files
  // ************************************
  VARootIO ioSource(sourcefile, true);
  ioSource.loadTheRootFile();

  cout<<"outFile: "<<outFile<<endl;
  VARootIO ioDestination(outFile);
  ioDestination.loadTheRootFile();


  // **************************************
  // get all the lookup table keysSource for the source file
  // **************************************
  TList *keysSource = ioSource.getDirectoryKeys("tables", true);
  if(!keysSource){
    cout<<"Couldn't load the keys for "<<sourcefile<<endl;
    return;
  }
  cout << "Found " << keysSource->GetEntries() << " keys to check." << endl;
  int oneTenth = (int)(keysSource->GetEntries()/10);

  // **************************************
  TObject *objectSource = NULL;
  TObject *objectDestination = NULL;
  VALookupTable *tableSource;
  VALookupTable *tableDestinatiuon;

  for (Int_t i=0; i<keysSource->GetEntries(); i++){
    if ( i%oneTenth == 0 ) {
      cout << "On key " << i << " of " << keysSource->GetEntries() << " ("
	   << (int)(100.*i/keysSource->GetEntries()) << "%)" << endl;
    }
    // *******************************************************************
    // Found a small problem. It looks like some times there are tables that should be
    // empty that are not. Don't know why. Change things to only replace a table in 
    // the destination file only if the new one is bigger
    // *******************************************************************
    // Procedure is as follows:
    // 1:Get the next table from the source file
    // 2:If it is not empty write it IF ITS BIGGER than existing table
    // 3:if it is empty check to see if a table of the same name already exists
    //   in the Desrtination file.
    // 4: If not write the empty table into the destination file
    // ********************************************************************

    // Step 1
    objectSource = ioSource.loadAnObject(keysSource->At(i)->GetName(), "tables", true);
    int numEntriesSource=0;
    // **********************************************************************
    // Make sure this object exists and is a VALookupTable
    // If so get the number of entries in the table
    // otherwise ignote it
    // **********************************************************************
    if(objectSource==NULL){
      cout<<"Fatal--Should have found: "<<keysSource->At(i)->GetName()
	  <<" in "<<sourcefile<<", but its not there!!!"<<endl;
      exit(EXIT_FAILURE);
    }
    
    if(strcmp(objectSource->ClassName(), "VALookupTable")==0){
      tableSource = (VALookupTable*)objectSource;
      numEntriesSource=tableSource->pfTableHistogram->GetEntries();
    }
    else{
      cout<<"Ignoring: "<< keysSource->At(i)->GetName()<< " in source file: "
	  <<sourcefile<<endl;
      continue;   //not a VALookupTable, ignore it.
    }

    // ***********************************************************************
    // Now look for the same object in the destination file. If its there
    // and is a VALookupTable get the mnumber of entries it has
    // ***********************************************************************
    objectDestination = NULL;
    objectDestination = ioDestination.loadAnObject(keysSource->At(i)->GetName(), 
						   "tables", true );
    int numEntriesDestination=0;
    bool gotDestinationTable=false;
    if(objectDestination!=NULL){
      if(strcmp(objectDestination->ClassName(), "VALookupTable")!=0){
	// *************************************************
	// Opps , not a VALookupTable
	// *************************************************
	//cout<<"FATAL--Object: "<<keysSource->At(i)->GetName()<<" in file "<<outfile
	//    <<" is a: "<<objectDestination->ClassName()<<" not a VALookupTable"
	//    <<endl;
	exit(EXIT_FAILURE);
      }
      else{
	tableDestination = (VALookupTable*)objectDestination;
	numEntriesDestination=tableDestination->pfTableHistogram->GetEntries();
	gotDestinationTable=true;
      }
    }

    // ***********************************************************************
    // Step 2
    // If no such table exist in the destination file than add to the destination 
    // file a copy of the one in the source file
    // ***********************************************************************
    if(!gotDestinationTable){
      ioDestination.writeAnObject(objectSource, keysSource->At(i)->GetName(),
				                                        "tables");
      cout<<"Added   : "<< keysSource->At(i)->GetName()<<endl;
    }
    else{
      // **********************************************************************
      // Step 3
      // If table in source is bigger than table in destination, replace destination
      // table
      // **********************************************************************
      if(numEntriesSource>numEntriesDestination){
	ioDestination.writeAnObject( objectSource, keysSource->At(i)->GetName(),
				     "tables");
	cout<<"Replaced: "<< keysSource->At(i)->GetName()<<endl;
      }
    }

    if ( objectSource != NULL ) {
      delete objectSource; //Need to be careful about cleaning up
      objectSource = NULL;
    }
    if ( objectDestination != NULL ) {
      delete objectDestination;    //Need to be careful about cleaning up
      objectDestination = NULL;
    }
  }


  // And done!
  ioDestination.closeTheRootFile();
  ioSource.closeTheRootFile();
  return;
}
// **********************************************************************


void combineFromList(string listOfFiles, string ltFile)
// *********************************************************************
// combine all the tables from a list of sub LT files into one  LT 
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
    //First line in List file is the first SubLT file.
    //Init ltFile by copying that to the LT combined file
    // *******************************************************
    char buffer[200];
    infile.getline(buffer, 200);
    //Lines  are the input sub LT files.
    if(infile.eof()){
      break;
    }
    string source(buffer);
    cout<<"Operating on "<<source<<endl;

    if(firstPass){
      string copyCommand="cp -v " + source + " " + ltFile;
      int rc = system(copyCommand.c_str());

      //      ifstream f1(source.c_str(), fstream::binary);
      //ofstream f2(ltFile.c_str(), fstream::trunc|fstream::binary);
      //f2 << f1.rdbuf(); 
      cout<<"Destination Lookup Table File: "<<ltFile<<endl;
      firstPass=false;
    }
    else{
      // Add in the new subLT tables. Replace table only if replacement is non-0
      combineLTK(ltFile, source);
    }
  }

  cout<<"All tables have been combined into "<<ltFile<<endl;
  cout<<"now you need to run the buildTree program in showerReconstruction2/bin"<<endl;
  return;
}
  
