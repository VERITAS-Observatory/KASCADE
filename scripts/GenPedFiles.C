vector < double > basePV;
vector < double > target;


void GenPrimaryPedFiles(string vbfDir, string stage2File, string targetPVFile)
{
  // **********************************
  // Get target pedvars
  // **********************************
  target.resize(9,0);
  ifstream tar;
  tar.open(targetPVFile.c_str());
  tar>>target.at(0)>>target.at(1)>>target.at(2)>>target.at(3)>>target.at(4)
     >>target.at(5)>>target.at(6)>>target.at(7)>>target.at(8);
  
 // ***********************************
  // Get Base Pedvars
  // ***********************************
  basePV.clear();
  basePV.resize(4,0.0);
  int numSamples=7;
  string vbfFile= vbfDir + stage2File;
  VARootIO io(vbfFile.c_str(), true);
  io.loadTheRootFile();
  VAQStatsData* q = io.loadTheQStatsData();
  basePV.at(0)=(q->getCameraAverageTraceVarTimeIndpt(0, numSamples));
  basePV.at(1)=(q->getCameraAverageTraceVarTimeIndpt(1, numSamples));
  basePV.at(2)=(q->getCameraAverageTraceVarTimeIndpt(2, numSamples));
  basePV.at(3)=(q->getCameraAverageTraceVarTimeIndpt(3, numSamples));

  // ************
  // For UA MDL12UA
  // ************
  target.at(0)=basePV.at(0);  //Biggest


  // ***************************************************
  // Get scaling values and print out
  // ***************************************************
  for (int i=0;i<9;i++) {
    for (int j=0;j<4;j++) {
      double ratio=target.at(i)/basePV.at(j);
      cout<<fixed<<setprecision(2)<<target.at(i)<<"/"<<setprecision(4)<<basePV.at(j)<<"="<<ratio<<" ";
    }
    cout<<endl;
  }

  // Or for Ped*file:
  for (int i=0;i<9;i++) {
    for (int j=0;j<4;j++) {
      double ratio=target.at(i)/basePV.at(j);
      cout<<j+1<<"/"<<fixed<<setprecision(4)<<ratio<<",";
    }
    cout<<"    #"<<fixed<<setprecision(2)<<target.at(i)<<endl;
  }
  
  for (int i=0;i<9;i++) {
    ostringstream os;
    os<<"PedUA"<<fixed<<setprecision(2)<<target.at(i);
    string PedFile=os.str();
    
    ofstream ped(PedFile.c_str());
    ped<<"IncreasePedVarOption='-PaddingApp=PaddingCustom -P_MultiPedvarScaling=";
    for (int j=0;j<4;j++) {
      double ratio=target.at(i)/basePV.at(j);
      ped<<j+1<<"/"<<fixed<<setprecision(4)<<ratio<<",";
    }
    ped<<"'    #"<<fixed<<setprecision(2)<<target.at(i)<<endl;
    ped<<"PedVarBase='PedVar"<<fixed<<setprecision(2)<<target.at(i)<<"'"<<endl;
    ped.close();
  }
  return;
}

void GenSecondaryPedFiles(string vbfDir, string stage2File, string targetPVFile)
{
  // **********************************
  // Get target pedvars
  // **********************************
  target.resize(9,0);
  ifstream tar;
  tar.open(targetPVFile.c_str());
  tar>>target.at(0)>>target.at(1)>>target.at(2)>>target.at(3)>>target.at(4)
     >>target.at(5)>>target.at(6)>>target.at(7)>>target.at(8);

  for (int i=0;i<9;i++) {
    ostringstream os;
    os<<vbfDir<<"PedVar"<<fixed<<setprecision(2)<<target.at(i)<<"_"<<stage2File;
    string simFile=os.str();
    
    // ***********************************
    // Get Adjusted Pedvars for this target
    // ***********************************
    basePV.clear();
    basePV.resize(4,0.0);
    int numSamples=7;
    VARootIO io(simFile.c_str(), true);
    io.loadTheRootFile();
    VAQStatsData *q = io.loadTheQStatsData();
    basePV.at(0)=(q->getCameraAverageTraceVarTimeIndpt(0, numSamples));
    basePV.at(1)=(q->getCameraAverageTraceVarTimeIndpt(1, numSamples));
    basePV.at(2)=(q->getCameraAverageTraceVarTimeIndpt(2, numSamples));
    basePV.at(3)=(q->getCameraAverageTraceVarTimeIndpt(3, numSamples));
    
    // ***********************************
    // Get Ped previous adjustment factors for this target
    // ***********************************
     ostringstream oss;
    oss<<"PedUA"<<fixed<<setprecision(2)<<target.at(i);
    string pedPreFile=oss.str();
    ifstream pedPre(pedPreFile.c_str());
    string line;
    std::getline(pedPre, line);

    vector < double > adj;
    adj.resize(4);
   
    std::size_t pos = line.find ("1/");     //Start of the adjustments factors
    line=line.substr (pos+2);               //get only adj factors skipping over the "1/"
    std::size_t pos1 = line.find (",");    //end of adjestment
    string adjStr=line.substr(0, pos1);
    istringstream iss(adjStr);
    iss>>adj.at(0);
    
    pos = line.find ("2/");     //Start of the adjustments factors
    line=line.substr (pos+2);               //get only adj factors skipping over the "2/"
    pos1 = line.find (",");    //end of adjestment
    adjStr=line.substr(0, pos1);
    istringstream iss(adjStr);
    iss>>adj.at(1);
    
    pos = line.find ("3/");     //Start of the adjustments factors
    line=line.substr (pos+2);               //get only adj factors skipping over the "3/"
    pos1 = line.find (",");    //end of adjestment
    adjStr=line.substr(0, pos1);
    istringstream iss(adjStr);
    iss>>adj.at(2);

    pos = line.find ("4/");     //Start of the adjustments factors
    line=line.substr (pos+2);               //get only adj factors skipping over the "4/"
    pos1 = line.find (",");    //end of adjestment
    adjStr=line.substr(0, pos1);
    istringstream iss(adjStr);
    iss>>adj.at(3);


    // ********************************************************************
    // Make the adjusted Ped files
    // ********************************************************************
    ostringstream osPost;
    osPost<<"PedUA"<<fixed<<setprecision(2)<<target.at(i);
    string PedFile=osPost.str();

    ofstream ped(PedFile.c_str());
    ped<<"IncreasePedVarOption='-PaddingApp=PaddingCustom -P_MultiPedvarScaling=";
    for (int j=0;j<4;j++) {
      double ratio=(target.at(i)/basePV.at(j))*adj.at(j);
      cout<<j+1<<"/"<<fixed<<setprecision(4)<<ratio<<",";
      //cout<<target.at(i)<<" "<<basePV.at(j)<<" "<<adj.at(j)<<" "<<ratio<<endl;
      ped<<j+1<<"/"<<fixed<<setprecision(4)<<ratio<<",";
    }
    ped<<"'    #"<<fixed<<setprecision(2)<<target.at(i)<<endl;
    ped<<"PedVarBase='PedVar"<<fixed<<setprecision(2)<<target.at(i)<<"'"<<endl;
    ped.close();
    cout<<"    #"<<fixed<<setprecision(2)<<target.at(i)<<endl;
  }
  return;
}
