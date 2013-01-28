double noiseSlope=0;
double noiseIntercept=0;
double threshSlope=0;
double threshIntercept=0;
double TSizeSlope=0;    //Determine threshold from size
double TSizeInter=0;
vector < double > effSlope(4);
vector < double > effIntercept(4);
TGraph* Threshold;
TCanvas* pfCanvas;
float m3_5;
float m3;
float T;
float G;
int t;
float N;
float PVar;
float R;
float Eff;
float S;


MakeTGraphFromTTree(string filename)
{
  TTree A;
  A.ReadFile(filename.c_str());
  int numPoints=A.GetEntries();
  int numMDLs=numPoints/4;

  A.SetBranchAddress("Max3_50",&m3_5);
  A.SetBranchAddress("Max3",&m3);
  A.SetBranchAddress("T",&T);
  A.SetBranchAddress("G",&G);
  A.SetBranchAddress("t",&t);
  A.SetBranchAddress("N",&N);
  A.SetBranchAddress("PVar",&PVar);
  A.SetBranchAddress("R",&R);
  A.SetBranchAddress("Eff",&Eff);
  A.SetBranchAddress("S",&S);

  pfCanvas = new TCanvas("c1", "Modeling Plots", 0, 0, 1200, 700);
  pfCanvas->Divide(2,2);
  
  // ************************************************************
  //Threshold fitting and slope intercept calculation
  // ************************************************************
  pfCanvas->cd(1);

  Threshold=new TGraph(numPoints);
  Threshold->SetMarkerStyle(20);
  Threshold->SetMarkerColor(2);
  Threshold->SetLineColor(1);
  for(int i=0;i<numPoints;i++){
    A.GetEntry(i);
    float mx=T*G;
    //Threshold->SetPoint(i,mx,m3_5);
    Threshold->SetPoint(i,mx,m3);
  }
  
  //Threshold[0]->SetTitle("Threshold from 50% Max3");
  Threshold->SetTitle("Threshold from Max3 peak");
  Threshold->GetXaxis()->SetTitle("T*G");
  Threshold->GetYaxis()->SetTitle("Max3");
  Threshold->Draw("AP");
  Threshold->Fit("pol1");
  TF1 *fit =  Threshold->GetFunction("pol1");
  threshIntercept=fit->GetParameter(0);
  threshSlope=fit->GetParameter(1);


  pfCanvas->cd(2);
  TGraph* Rate[4];
  for(int k=0;k<4;k++){
    Rate[k]=new TGraph(numMDLs);
    Rate[k]->SetMarkerStyle(20);
    Rate[k]->SetMarkerColor(k+1);
    Rate[k]->SetLineColor(k+1);
    int j=0;
    for(int i=0;i<numPoints;i++){
      A.GetEntry(i);
      if(t==k+1){
	float rate=Eff/T;
	Rate[k]->SetPoint(j,rate,R);
	j++;
      }
    }
  }
  int m=1;  //biggest: Needed for nice plots only. Fit results don't care
  Rate[m]->SetTitle("Eff from Rate");
  Rate[m]->GetXaxis()->SetTitle("Eff/T");
  Rate[m]->GetYaxis()->SetTitle("Rate ");
  Rate[m]->Draw("AP");
  Rate[m]->Fit("pol1");
  TF1 *fit = Rate[m]->GetFunction("pol1");
  effIntercept.at(m)=fit->GetParameter(0);
  effSlope.at(m)=fit->GetParameter(1);
   
  for(int k=0;k<4;k++){
    if(k!=m){
      Rate[k]->Draw("P");
      Rate[k]->Fit("pol1");
      TF1 *fit = Rate[k]->GetFunction("pol1");
      effIntercept.at(k)=fit->GetParameter(0);
      effSlope.at(k)=fit->GetParameter(1);
    }
  } 



  pfCanvas->cd(3);
  TGraph* Noise;
  Noise = new TGraph(numMDLs);
  Noise->SetMarkerStyle(20);
  Noise->SetMarkerColor(4);
  Noise->SetLineColor(1);
  int j=0;
  for(int i=0;i<numPoints;i++){
    A.GetEntry(i);
    if(t==1){
      float noise=sqrt(N)*G;;
      Noise->SetPoint(j,noise,PVar);
      j++;
    }
  }
  Noise->SetTitle("Noise from PedVar");
  Noise->GetXaxis()->SetTitle("sqrt(N)*G");
  Noise->GetYaxis()->SetTitle("PVar");
  
  Noise->Draw("AP");
  Noise->Fit("pol1");
  TF1 *fit = Noise->GetFunction("pol1");
  noiseIntercept=fit->GetParameter(0);
  noiseSlope=fit->GetParameter(1);

  pfCanvas->cd(4);
  TGraph* Size[4];
  for(int k=0;k<4;k++){
    Size[k]=new TGraph(numMDLs);
    Size[k]->SetMarkerStyle(20);
    Size[k]->SetMarkerColor(k+1);
    Size[k]->SetLineColor(k+1);
    int j=0;
    for(int i=0;i<numPoints;i++){
      A.GetEntry(i);
      if(t==k+1){
	float size=T*G/sqrt(N);
	Size[k]->SetPoint(j,size,S);
	j++;
      }
    }
    if(k==0){
      Size[0]->Draw("AP");
      Size[0]->SetTitle("Threshold from Size");
      Size[0]->GetXaxis()->SetTitle("T*G/sqrt(N)");
      Size[0]->GetYaxis()->SetTitle("S");
    }
    else{
      Size[k]->Draw("P");
    }
    Size[k]->Fit("pol1");
  } 


  cout<<"noiseIntercept,noiseSlope: "<<noiseIntercept<<" "
      <<noiseSlope<<endl;
  cout<<"threshIntercept,threshSlope: "<<threshIntercept<<" "
      <<threshSlope<<endl;
  for(int k=0;k<4;k++){
    cout<<"k,effIntercept[k],effSlope[k]: "<<k<<": "<<effIntercept[k]<<"  "
	<<effSlope[k]<<endl;
  }
  return;
}
// ************************************************************************

genParams(string refParamsFileName)
{
  // *****************************************************************
  // Load up referance values, include chosen gains!!!
  // *****************************************************************

  vector <double> refPedVar;
  vector <double> refMax3Peak;
  vector <double> refRate;
  vector <double> refGains;
  refPedVar.clear();
  refMax3Peak.clear();
  refRate.clear();
  refGains.clear();

  if(refParamsFileName==" "){
    // Use params from run 54008
    
    refPedVar.push_back(5.94);
    refPedVar.push_back(5.61);
    refPedVar.push_back(6.15);
    refPedVar.push_back(5.88);

    refMax3Peak.push_back(42);
    refMax3Peak.push_back(38);
    refMax3Peak.push_back(42);
    refMax3Peak.push_back(38);

    refRate.push_back(117);
    refRate.push_back(140);
    refRate.push_back(146);
    refRate.push_back(142);

    refGains.push_back(1.32);
    refGains.push_back(1.32);
    refGains.push_back(1.32);
    refGains.push_back(1.32);
  }
  else{
    // *******************************************************
    // Open and read in referance parameters from a TTree compatable
    // txt file.
    // First line looks like:
    // Gain/F:Rate:PVar:Max3
    // *******************************************************
    TTree RefParams;
    RefParams.ReadFile(refParamsFileName.c_str());
    RefParams.SetBranchAddress("Max3",&m3);
    RefParams.SetBranchAddress("PVar",&PVar);
    RefParams.SetBranchAddress("Rate",&R);
    RefParams.SetBranchAddress("Gain",&G);

    for(int i=0;i<4;i++){
      RefParams.GetEntry(i);
      refPedVar.push_back(PVar);
      refMax3Peak.push_back(m3);
      refRate.push_back(R);
      refGains.push_back(G);
    }
  }
  // Noise:
  double noise1=( refPedVar.at(0)-noiseIntercept)/(noiseSlope*refGains.at(0));
  noise1=noise1*noise1;
    
  double noise2=( refPedVar.at(1)-noiseIntercept)/(noiseSlope*refGains.at(1));
  noise2=noise2*noise2;
    
  double noise3=( refPedVar.at(2)-noiseIntercept)/(noiseSlope*refGains.at(2));
  noise3=noise3*noise3;
    
  double noise4=( refPedVar.at(3)-noiseIntercept)/(noiseSlope*refGains.at(3));
  noise4=noise4*noise4;

  //Threshold from 50% Max3 from data
  //double thresh1= (24.-threshIntercept)/(threshSlope*refGains.at(0));
  //double thresh2= (25.-threshIntercept)/(threshSlope*refGains.at(1));
  //double thresh3= (28.-threshIntercept)/(threshSlope*refGains.at(2));
  //double thresh4= (27.-threshIntercept)/(threshSlope*refGains.at(3));
  
  //Threshold from Max3 peak from data
  double thresh1= (refMax3Peak.at(0)-threshIntercept)/
                                                (threshSlope*refGains.at(0));
  double thresh2= (refMax3Peak.at(1)-threshIntercept)/
                                                (threshSlope*refGains.at(1));
  double thresh3= (refMax3Peak.at(2)-threshIntercept)/
                                                (threshSlope*refGains.at(2));
  double thresh4= (refMax3Peak.at(3)-threshIntercept)/
                                                (threshSlope*refGains.at(3));

  //Threshold from Size
  //double thresh1= sqrt(noise1)/(TSizeSlope*refGains.at(0))*(190-TSizeInter);
  //double thresh2= sqrt(noise2)/(TSizeSlope*refGains.at(1))*(190-TSizeInter);
  //double thresh3= sqrt(noise3)/(TSizeSlope*refGains.at(2))*(230-TSizeInter);
  //double thresh4= sqrt(noise4)/(TSizeSlope*refGains.at(3))*(190-TSizeInter);

  //Efficiency from rate
  double eff1=((refRate.at(0)-effIntercept.at(0))/effSlope.at(0))*thresh1;
  double eff2=((refRate.at(1)-effIntercept.at(1))/effSlope.at(1))*thresh2;
  double eff3=((refRate.at(2)-effIntercept.at(2))/effSlope.at(2))*thresh3;
  double eff4=((refRate.at(3)-effIntercept.at(3))/effSlope.at(3))*thresh4;

  if(eff1>1.0){
    cout<<"determineParameters found T1 eff >1.0(eff1="<<eff1<<")"<<endl;
    cout<<"setting T1 eff to 1.0"<<endl;
    eff1=1.0;
  }
 
  if(eff2>1.0){
    cerr<<"determineParameters found T2 eff >1.0(eff2="<<eff2<<")"<<endl;
    cerr<<"setting T2 eff to 1.0"<<endl;
    eff2=1.0;
  }

  if(eff3>1.0){
    cerr<<"determineParameters found T3 eff >1.0(eff3="<<eff3<<")"<<endl;
    cerr<<"setting T3 eff to 1.0"<<endl;
    eff3=1.0;
  }

  if(eff4>1.0){
    cerr<<"determineParameters found T4 eff >1.0(eff4="<<eff4<<")"<<endl;
    cerr<<"setting T4 eff to 1.0"<<endl;
    eff4=1.0;
  }

  //Print out the table
  cout<<"T1      "
      <<setw(4)<<setprecision(3)<<thresh1<<"    "
      <<setw(4)<<setprecision(3)<<noise1 <<"    "
      <<setw(4)<<setprecision(2)<<eff1   <<"    "
      <<setw(4)<<setprecision(3)<<refGains.at(0)  <<"    "
      <<endl;
  
  cout<<"T2      "
      <<setw(4)<<setprecision(3)<<thresh2<<"    "
      <<setw(4)<<setprecision(3)<<noise2 <<"    "
      <<setw(4)<<setprecision(2)<<eff2   <<"    "
      <<setw(4)<<setprecision(3)<<refGains.at(1)  <<"    "
      <<endl;
  
  cout<<"T3      "
      <<setw(4)<<setprecision(3)<<thresh3<<"    "
      <<setw(4)<<setprecision(3)<<noise3 <<"    "
      <<setw(4)<<setprecision(2)<<eff3   <<"    "
      <<setw(4)<<setprecision(3)<<refGains.at(2)  <<"    "
      <<endl;
  
  cout<<"T4      "
      <<setw(4)<<setprecision(3)<<thresh4<<"    "
      <<setw(4)<<setprecision(3)<<noise4 <<"    "
      <<setw(4)<<setprecision(2)<<eff4   <<"    "
      <<setw(4)<<setprecision(3)<<refGains.at(3)  <<"    "
      <<endl;
  return;
}


determineParameters(string filename, string refParamsFileName=" ")
{
  MakeTGraphFromTTree(filename);
  genParams(refParamsFileName);
  return;
}



