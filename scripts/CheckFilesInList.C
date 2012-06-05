void CheckFilesInList(string listOfFiles, string dir,int startLine=1)
{
  // Check the root files in this list. Just make sure they exist and make sure then 
  // can be loaded.
  ifstream infile;
  infile.open(listOfFiles.c_str(), ifstream::in);
  if(!infile.is_open()){
    cerr<<"Couldn't open "<<listOfFiles<<endl;
    return;
  }
  int lineNum=0;

  while(true){
    char buffer[200];
    infile.getline(buffer, 200);
    if(infile.eof()){
      break;
    }
    lineNum++;
    if(lineNum>=startLine){
      string file =dir + '/' + buffer;
      cout<<lineNum<<":Testing File: "<<buffer<<endl;
    
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
    }
  }
  return;
}
