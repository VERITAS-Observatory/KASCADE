void CheckStage4FileOK(string fileName)
{
  ofstream os("CheckStage4FileOK.Result");
  VARootIO* pfRootIO = new VARootIO(fileName.c_str(), true);//open for read only
  int badCode=0;  
  os<<badCode<<endl;
  os.close();
  if(!pfRootIO) {
    badCode=1;
    ofstream os("CheckStage4FileOK.Result");
    os<<badCode<<endl;
    os.close();
    return; 
  }

  pfRootIO->loadTheRootFile();
  if(!pfRootIO->IsOpen()){
    badCode=2;
    ofstream os("CheckStage4FileOK.Result");
    os<<badCode<<endl;
    os.close();
    return; 
  } 
  //Probably want a ShowerData TTree check here.
  TTree*  pfShowerDataTree = pfRootIO->loadTheShowerEventTree();
  if(!pfShowerDataTree)
   {
     badCode=3;
     ofstream os("CheckStage4FileOK.Result");
     os<<badCode<<endl;
     os.close();
     return; 
   }
  
  TTree* pfSimulatedEventTree = pfRootIO->loadTheSimulationEventTree();
  if(!pfSimulatedEventTree)
    {
    badCode=4;
    ofstream os("CheckStage4FileOK.Result");
    os<<badCode<<endl;
    os.close();
    return; 
    }
  
  int numEvents=pfSimulatedEventTree->GetEntries();

  pfRootIO->closeTheRootFile();
  if(pfRootIO!=NULL){
    delete pfRootIO;
  }    
  badCode=numEvents;
  ofstream os("CheckStage4FileOK.Result");
  os<<badCode<<endl;
  os.close();
  return; 
}
