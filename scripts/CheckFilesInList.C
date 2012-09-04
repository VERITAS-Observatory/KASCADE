int CheckFilesInList(string listOfFiles, int start=0)
{
  // Check the root files in this list. Just make sure they exist and make sure they
  // can be loaded.
  ifstream infile;
  infile.open(listOfFiles.c_str(), ifstream::in);
  if(!infile.is_open()){
    cerr<<"Couldn't open "<<listOfFiles<<endl;
    return 1;
  }
  

  int icount=0;
  int countBad=0;
  while(true){
    icount++;
    char buffer[200];
    infile.getline(buffer, 200);
    if(infile.eof()){
      cout<<"Found EOF of input file "<<listOfFiles<<endl;
      cout<<"Tested files: "<<start<< "  to "<<icount<<endl;
      cout<<"Found "<<countBad<<" bad files."<<endl;
      return countBad;
    }
    if(icount<start){
	continue;
    }

    if(icount%500==0){
      cout<<"At file: "<<icount<<endl;
    }
    string file =buffer;

    VARootIO* pfRootIO = new VARootIO(file, true);//open for read only
    if(!pfRootIO)
      {
	cout<<icount<<":Testing File: "<<file<<endl;
	cout<<"Couldn't generate a  pointer to VARootIO for "<<file<<endl;
	countBad++;
	continue;
      }

    pfRootIO->loadTheRootFile();
    if(!pfRootIO->IsOpen()){
      cout<<icount<<":Testing File: "<<file<<endl;
      cout<<"Couldn't open "<<file<<endl;
      countBad++;
      continue;
    } 
    pfRootIO->closeTheRootFile();
    if(pfRootIO!=NULL){
      delete pfRootIO;
    }    
  }
  return countBad;
}
