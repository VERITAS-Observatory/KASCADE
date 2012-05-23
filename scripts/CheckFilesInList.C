void CheckFilesInList(string listOfFiles, string dir)
{
  // Check the root files in this list. Just make sure they exist and make sure then 
  // can be loaded.
  ifstream infile;
  infile.open(listOfFiles.c_str(), ifstream::in);
  if(!infile.is_open()){
    cerr<<"Couldn't open "<<listOfFiles<<endl;
    return;
  }

  while(true){
    char buffer[200];
    infile.getline(buffer, 200);
    string file =dir + '/' + buffer;
    cout<<"Testing File: "<<file<<endl;
    
    VARootIO* pfRootIO = new VARootIO(file, true);//open for read only
    if(!pfRootIO)
      {
	cout<<"Couldn't generate a  pointer to VARootIO for "<<file<<endl;
	continue;
      }

    pfRootIO->loadTheRootFile();
    if(!pfRootIO->IsOpen()){
      cout<<"Couldn't open "<<file<<endl;
      cobntinue;
    } 
  }
  return;
}
