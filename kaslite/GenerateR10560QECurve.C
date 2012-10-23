void GenerateR10560QECurve()
{
  // *****************************************************************
  // Construct a full 180 to 700 nm QE curve for R10560 PMT by 
  // 1: 180 to 300nm: scaled xp2970 to match PurdueR10560 at 300 nm
  // 2: 300 to 600 nm: PurdueR10560 measured average.
  // 3: 600 to 700 nm: CareR10560 scaled to match PurdueR10560 at 600 nm
  // *****************************************************************

  // Read the three curves into  TTrees
  TTree xp2970;
  TTree PurdueR10560;
  TTree CareR10560;

  xp2970.ReadFile("xp2970Qeff.txt");
  PurdueR10560.ReadFile("PurdueR10560Qeff.txt");
  CareR10560.ReadFile("CareR10560Qeff.txt");

  //Open the output file and set variqable names for TTree::ReadFile format.
  ofstream os("R10560Qeff.txt");
  os<<"lambda/I:qe/D"<<endl;

  int numxp2970=xp2970.GetEntries();
  int numPurdueR10560=PurdueR10560.GetEntries();
  int numCareR10560=CareR10560.GetEntries();

  int xp2970Lambda=0;
  int PurdueR10560Lambda=0;
  int CareR10560Lambda=0;

  double xp2970Qe=0;
  double PurdueR10560Qe=0;
  double CareR10560Qe=0;

  xp2970.SetBranchAddress("lambda",&xp2970Lambda);
  PurdueR10560.SetBranchAddress("lambda",&PurdueR10560Lambda);
  CareR10560.SetBranchAddress("lambda",&CareR10560Lambda);

  xp2970.SetBranchAddress("qe",&xp2970Qe);
  PurdueR10560.SetBranchAddress("qe",&PurdueR10560Qe);
  CareR10560.SetBranchAddress("qe",&CareR10560Qe);



  //To match at 300 nm find index for xp2970 and PurdueR10560;

  //Get scaleing factor  at 300nm

  int Index300PurdueR10560=-1;
  for (int i=0;i<numPurdueR10560;i++){
    PurdueR10560.GetEntry(i);
    if(PurdueR10560Lambda == 300){
      Index300PurdueR10560=i;
      break;
    }
  }

  int Index300xp2970=-1;
  for (int i=0;i<numxp2970;i++){
    xp2970.GetEntry(i);
    if(xp2970Lambda == 300){
      Index300xp2970=i;
      break;
    }
  }

  double ScaleAt300=PurdueR10560Qe/xp2970Qe;
 

  //Get scaleing factor  at 600nm
  int Index600PurdueR10560=-1;
  for (int i=0;i<numPurdueR10560;i++){
    PurdueR10560.GetEntry(i);
    if(PurdueR10560Lambda == 600){
      Index600PurdueR10560=i;
      break;
    }
  }

  int Index600CareR10560=-1;
  for (int i=0;i<numCareR10560;i++){
    CareR10560.GetEntry(i);
    if( CareR10560Lambda == 600){
      Index600CareR10560=i;
      break;
    }
  }

  double ScaleAt600=PurdueR10560Qe/CareR10560Qe;


  //Now we are going to write out a .txt file so we can plot this
  // We need lambda in 5 nm steps. xp2970 does this for us
  // but the rest don't. So we will have to interpolate
  //180 to 300:
  for (int i=0;i<=Index300xp2970;i++){
      xp2970.GetEntry(i);
      os<<xp2970Lambda<<" "<<xp2970Qe*ScaleAt300/100.0<<endl;
  }

  //305 to 600 and interpolate
  double qePrevious=xp2970Qe*ScaleAt300/100.;

  for (int i=Index300PurdueR10560+1;i<=Index600PurdueR10560;i++){ 
    PurdueR10560.GetEntry(i);
    //this skips a lambda interpolate

    int lambda=PurdueR10560Lambda-5;
    double qe=(PurdueR10560Qe/100.+qePrevious)/2.0;
    os<<lambda<<" "<<qe<<endl;
    os<<PurdueR10560Lambda<<" "<<PurdueR10560Qe/100.<<endl;
    qePrevious=PurdueR10560Qe/100.;
  }    

  //600 to 680

  for (int i=Index600CareR10560+1;i<numCareR10560;i++){
    CareR10560.GetEntry(i);
    if(CareR10560Lambda>680){
      break;
    }

    //this skips a lambda interpolate
    int lambda=CareR10560Lambda-5;
    double qe=(CareR10560Qe*ScaleAt600/100.+qePrevious)/2.0;
    os<<lambda<<" "<<qe<<endl;

    os<<CareR10560Lambda<<" "<<CareR10560Qe*ScaleAt600/100.<<endl;
    qePrevious=CareR10560Qe*ScaleAt600/100.;
  }    

  for(int lambda=685;lambda<=700;lambda=lambda+5){
    double qe=0.0;
    os<<lambda<<" "<<qe<<endl;
  }    

  os.close();

  // Now list the R10560 in the fortran data format.
  GenerateDatatxt("R10560Qeff","qe");


}

   

    
    
