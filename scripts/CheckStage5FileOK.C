// **********************************************************************
// Checks that various parts of the stage 5 file exist.
// Same as CheckStage4FileOk.scr with additon of checking that the
// CominbinedEventTree also exists and the removal of the ShowerDataTree
// check..
// **********************************************************************

void CheckStage5FileOK(string fileName)
{
  //ofstream os("CheckStage5FileOK.Result");
  VARootIO* pfRootIO = new VARootIO(fileName.c_str(), true);//open for read only
  int badCode=0;  
  //os<<badCode<<endl;
  //os.close();
  if(!pfRootIO) {
    badCode=1;
    cout<<"Could not open the file."<<endl;
    ofstream os("CheckStage5FileOK.Result");
    os<<badCode<<endl;
    os.close();
    return; 
  }

  pfRootIO->loadTheRootFile();
  if(!pfRootIO->IsOpen()){
    cout<<" Could not load the Root file"<<endl;
    badCode=2;
    ofstream os("CheckStage5FileOK.Result");
    os<<badCode<<endl;
    os.close();
    return; 
  } 
  //No ShowerDataTree or SimulationEvent Tree in Stage5 file
  //TTree*  pfShowerDataTree = pfRootIO->loadTheShowerEventTree();
  //if(!pfShowerDataTree)
  // {
  //   cout<<"Could not load the Shower Event Tree"<<endl;
  //   badCode=3;
  //   ofstream os("CheckStage5FileOK.Result");
  //   os<<badCode<<endl;
  //   os.close();
  //   return; 
  // }
  
  //TTree* pfSimulatedEventTree = pfRootIO->loadTheSimulationEventTree();
  //if(!pfSimulatedEventTree)
  //  {
  //    cout<<" Could not load the simulated event tree"<<endl;
  //    badCode=4;
  //    ofstream os("CheckStage5FileOK.Result");
  //   os<<badCode<<endl;
  //    os.close();
  //    return; 
  //  }
  
  TTree* pfCombinedEventTree = pfRootIO->loadTheCombinedEventTree();
  if(!pfCombinedEventTree)
    {
      cout<<" Could not load the CombinedEventTree"<<endl;
      badCode=6;
      ofstream os("CheckStage5FileOK.Result");
      os<<badCode<<endl;
      os.close();
      return; 
    }
 
 VARunHeader* pfRunHeader = pfRootIO->loadTheRunHeader();
 if(!pfRunHeader)
    {
      cout<<"Could not load the Run Header"<<endl;
      badCode=5;
      ofstream os("CheckStage5FileOK.Result");
      os<<badCode<<endl;
      os.close();
      return; 
    }
 
 int numEvents=pfCombinedEventTree->GetEntries();

 pfRootIO->closeTheRootFile();
 if(pfRootIO!=NULL){
    delete pfRootIO;
 }    
  badCode=numEvents;
  ofstream os("CheckStage5FileOK.Result");
  os<<badCode<<endl;
  os.close();
  return; 
}

