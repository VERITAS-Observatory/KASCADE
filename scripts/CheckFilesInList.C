void CheckFilesInList(string listOfFiles, int start=0)
{
  // Check the root files in this list. Just make sure they exist and make sure then 
  // can be loaded.
  int icount=0;
  ifstream infile;
  infile.open(listOfFiles.c_str(), ifstream::in);
  if(!infile.is_open()){
    cerr<<"Couldn't open "<<listOfFiles<<endl;
    return;
  }
  

  while(true){
    char buffer[200];
    infile.getline(buffer, 200);
    if(infile.eof()){
      cout<<"Found EOF of input file "<<listOfFiles<<endl;
      break;
    }
    icount++;
    if(icount<start){
      continue;
    }

    string file =buffer;
    cout<<icount<<":Testing File: "<<file<<endl;

    VARootIO* pfRootIO = new VARootIO(file, true);//open for read only
    if(!pfRootIO)
      {
	cout<<"Couldn't generate a  pointer to VARootIO for "<<file<<endl;
	continue;
      }

    pfRootIO->loadTheRootFile();
    if(!pfRootIO->IsOpen()){
      cout<<"Couldn't open "<<file<<endl;
      continue;
    } 
    pfRootIO->closeTheRootFile();
    if(pfRootIO!=NULL){
      delete pfRootIO;
    }    
  }
  return;
}
