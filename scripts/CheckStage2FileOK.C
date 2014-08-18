void CheckStage2FileOK(string fileName)
{
  VARootIO* pfRootIO = new VARootIO(fileName.c_str(), true);//open for read only
  if(!pfRootIO) {
    int badCode=-1;
    ofstream os("CheckStage2FileOK.Result");
    os<<badCode<<endl;
    os.close();
    cout<<"BadCode"<<badCode<<endl;
    return; 
  }
  
  pfRootIO->loadTheRootFile(); 
  if(!pfRootIO->IsOpen()){
    int badCode=-2;
    ofstream os("CheckStage2FileOK.Result");
    os<<badCode<<endl;
    os.close();
    cout<<"BadCode"<<badCode<<endl;
    return; 
  } 
  
  TTree*  pfParameterisedEventTree = pfRootIO->loadTheParameterisedEventTree();
  if(!pfParameterisedEventTree)
    {
      int badCode=-3;
      ofstream os("CheckStage2FileOK.Result");
      os<<badCode<<endl;
      os.close();
      cout<<"BadCode"<<badCode<<endl;
      return; 
    }
  
  TTree* pfSimulatedEventTree = pfRootIO->loadTheSimulationEventTree();
  if(!pfSimulatedEventTree)
    {
      int badCode=-4;
      ofstream os("CheckStage2FileOK.Result");
      os<<badCode<<endl;
      os.close();
      cout<<"BadCode"<<badCode<<endl;
      return; 
    }
  
  int numEvents=pfParameterisedEventTree->GetEntries();
  if ( numEvents < 16)
    {
      int badCode=-5;
      ofstream os("CheckStage2FileOK.Result");
      os<<badCode<<endl;
      os.close();
      cout<<"BadCode"<<badCode<<endl;
      return; 
    }
  
  pfRootIO->closeTheRootFile();
  if(pfRootIO!=NULL){
    delete pfRootIO;
  }    
  
  int goodCode=numEvents;
  ofstream os("CheckStage2FileOK.Result");
  os<<goodCode<<endl;
  os.close();
  cout<<"goodCode"<<goodCode<<endl;
  return; 
}
