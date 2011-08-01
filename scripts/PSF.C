

TH1D containment("ctmnt","ctmnt",500,0.0,.1);
TH1D Radial("radial","radial",500,0.0,.1);


PSF(string fileName,double ContFraction)
{
  TTree* pA= new TTree();
  pA->ReadFile(fileName.c_str());
  float WX;
  float WY;
  pA->SetBranchAddress("WX",&WX);
  pA->SetBranchAddress("WY",&WY);

  // **************************************************
  // Going to make a histogram as function of radius out from xave,yave
  // **************************************************
  int numEntries= pA->GetEntries();
  cout<<"Number of Entries: "<<numEntries<<endl;

  // *****************************************************
  // Find means of both axis
  // ****************************************************
  double xAve;
  double yAve;
  for (int i=0;i<numEntries;i++){
    pA->GetEntry(i);
    xAve=xAve+WX;
    yAve=yAve+WY;
  }
  xAve=xAve/numEntries;
  yAve=yAve/numEntries;
  double apparentOffset=sqrt(xAve*xAve+yAve*yAve);
  cout<<"PSF: Center at x,y: "<<xAve<<" deg, "<<yAve<<" deg"<<endl;
  cout<<"PSF: Apparent Offset: "<<apparentOffset<<" deg"<<endl;

  Radial.Reset();

  for (int i=0;i<numEntries;i++){
    pA->GetEntry(i);
    WX=WX-xAve;
    WY=WY-yAve;
    double R=sqrt(WX*WX+WY*WY);
    Radial.Fill(R);
  }

  // ******************************************************
  // Now convert this to a cumulative plot. print out when we hit
  // the specified contianment fraction
  // ******************************************************
  double runningSum=0;
  int ContBin=0;
  containment.Reset();

  for(int i=1;i<=500;i++){
    double rcontents=Radial.GetBinContent(i);
    runningSum= runningSum+ rcontents;
    double fraction=runningSum/numEntries;
    containment.SetBinContent(i,fraction);

    if(fraction>ContFraction){
      if(ContBin==0){
	ContBin=i;
      }
    }
  }
  double ContRadius= containment.GetBinCenter(ContBin);
  cout<<ContFraction*100<<"% containment reached at a radius of: "
      <<ContRadius<<" deg"<<endl;
  containment.Draw();
return;
}

   
