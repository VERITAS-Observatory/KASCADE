
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
  // get all the lookup table keys for the source file
  // **************************************
  TList *keys = ioSource.getDirectoryKeys("tables", true);
  if(!keys){
    cout<<"Couldn't load the keys for "<<sourcefile<<endl;
    return;
  }

  VALookupTable *table;
  TObject *object = NULL;
  TObject *objtmp = NULL;
  cout << "Found " << keys->GetEntries() << " keys to check." << endl;
  int oneTenth = (int)(keys->GetEntries()/10);

  for (Int_t i=0; i<keys->GetEntries(); i++){
    if ( i%oneTenth == 0 ) {
      cout << "On key " << i << " of " << keys->GetEntries() << " ("
	   << (int)(100.*i/keys->GetEntries()) << "%)" << endl;
    }

    // *******************************************************************
    // Procedure is as follows:
    // 1:Get the next table from the source file
    // 2:If it is not empty write it
    // 3:if it is empty check to see if a table of the same name already exists
    //   in the Desrtination file.
    // 4: If not write the empty table into the destination file
    // ********************************************************************

    // Step 1
    object = ioSource.loadAnObject(keys->At(i)->GetName(), "tables", true);
    if(strcmp(object->ClassName(), "VALookupTable")==0){
      table = (VALookupTable*)object;

      // Step 2
      if(table->pfTableHistogram->GetEntries()!=0){
	ioDestination.writeAnObject(object, keys->At(i)->GetName(), "tables");
        cout<<"Added  : "<< keys->At(i)->GetName()<<endl;
      }
      else {
	// Step 3
	objtmp = NULL;
 	objtmp = ioDestination.loadAnObject(keys->At(i)->GetName(), "tables",
					    true );
 	//Step4
	if ( objtmp == NULL ) {
	  ioDestination.writeAnObject( object, keys->At(i)->GetName(),
				       "tables");
	  cout<<"Added 0: "<< keys->At(i)->GetName()<<endl;
 	}
      }

    }
    if ( objtmp != NULL ) {
      delete objtmp; //Need to be careful about cleaning up
      objtmp = NULL;
    }
    if ( object != NULL ) {
      delete object;    //Need to be careful about cleaning up
      object = NULL;
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
  
