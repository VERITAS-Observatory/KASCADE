double noiseSlope=0;
double noiseIntercept=0;
double threshSlope=0;
double threshIntercept=0;
double TSizeSlope=0;    //Determine threshold from size
double TSizeInter=0;
vector < double > effSlope(4);
vector < double > effIntercept(4);
TGraph* Threshold;

MakeTGraphFromTTree(string filename)
{
  TTree A;
  A.ReadFile(filename.c_str());
  int numPoints=A.GetEntries();
  int numMDLs=numPoints/4;

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

  TCanvas* pfCanvas = new TCanvas("c1", "Modeling Plots", 0, 0, 1200, 700);
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
    if(k==0){
      Rate[0]->SetTitle("Eff from Rate");
      Rate[0]->GetXaxis()->SetTitle("Eff/T");
      Rate[0]->GetYaxis()->SetTitle("Rate ");
      Rate[0]->Draw("AP");
    }
    else{
      Rate[k]->Draw("P");
    }
    Rate[k]->Fit("pol1");
    TF1 *fit = Rate[k]->GetFunction("pol1");
    effIntercept.at(k)=fit->GetParameter(0);
    effSlope.at(k)=fit->GetParameter(1);

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

}

genParams( double gain1,double gain2, double gain3,double gain4)
{

!  effSlope.resize(4,0);
!  effIntercept.resize(4,0);

 
  // Noise:
  double noise1=(5.94-noiseIntercept)/(noiseSlope*gain1);
  noise1=noise1*noise1;
    
  double noise2=(5.61-noiseIntercept)/(noiseSlope*gain2);
  noise2=noise2*noise2;
    
  double noise3=(6.15-noiseIntercept)/(noiseSlope*gain3);
  noise3=noise3*noise3;
    
  double noise4=(5.88-noiseIntercept)/(noiseSlope*gain4);
  noise4=noise4*noise4;

  //Threshold from 50% Max3 from data
  //double thresh1= (24.-threshIntercept)/(threshSlope*gain1);
  //double thresh2= (25.-threshIntercept)/(threshSlope*gain2);
  //double thresh3= (28.-threshIntercept)/(threshSlope*gain3);
  //double thresh4= (27.-threshIntercept)/(threshSlope*gain4);
  
///Threshold from Max3 peak from data
  double thresh1= (42.-threshIntercept)/(threshSlope*gain1);
  double thresh2= (38.-threshIntercept)/(threshSlope*gain2);
  double thresh3= (42.-threshIntercept)/(threshSlope*gain3);
  double thresh4= (38.-threshIntercept)/(threshSlope*gain4);

  //Threshold from Size
  //double thresh1= sqrt(noise1)/(TSizeSlope*gain1)*(190-TSizeInter);
  //double thresh2= sqrt(noise2)/(TSizeSlope*gain2)*(190-TSizeInter);
  //double thresh3= sqrt(noise3)/(TSizeSlope*gain3)*(230-TSizeInter);
  //double thresh4= sqrt(noise4)/(TSizeSlope*gain4)*(190-TSizeInter);

  //Efficiency from rate
  double eff1=((117.-effIntercept.at(0))/effSlope.at(0))*thresh1;
  double eff2=((140.-effIntercept.at(1))/effSlope.at(1))*thresh2;
  double eff3=((146.-effIntercept.at(2))/effSlope.at(2))*thresh3;
  double eff4=((142.-effIntercept.at(3))/effSlope.at(3))*thresh4;

  //Print out the table
  cout<<"T1      "
      <<setw(4)<<setprecision(3)<<thresh1<<"    "
      <<setw(4)<<setprecision(3)<<noise1 <<"    "
      <<setw(4)<<setprecision(2)<<eff1   <<"    "
      <<setw(4)<<setprecision(3)<<gain1  <<"    "
      <<endl;
  
  cout<<"T2      "
      <<setw(4)<<setprecision(3)<<thresh2<<"    "
      <<setw(4)<<setprecision(3)<<noise2 <<"    "
      <<setw(4)<<setprecision(2)<<eff2   <<"    "
      <<setw(4)<<setprecision(3)<<gain2  <<"    "
      <<endl;
  
  cout<<"T3      "
      <<setw(4)<<setprecision(3)<<thresh3<<"    "
      <<setw(4)<<setprecision(3)<<noise3 <<"    "
      <<setw(4)<<setprecision(2)<<eff3   <<"    "
      <<setw(4)<<setprecision(3)<<gain3  <<"    "
      <<endl;
  
  cout<<"T4      "
      <<setw(4)<<setprecision(3)<<thresh4<<"    "
      <<setw(4)<<setprecision(3)<<noise4 <<"    "
      <<setw(4)<<setprecision(2)<<eff4   <<"    "
      <<setw(4)<<setprecision(3)<<gain4  <<"    "
      <<endl;
  return;
}

 
determineParameters(string filename,double gain1,double gain2, double gain3,
		    double gain4)
{
  MakeTGraphFromTTree(filename);
  genParams(   gain1, gain2,  gain3, gain4);
  return;
}
