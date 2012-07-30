//You need to do ".L LTSlice.C" before loloading and running this scriopt.
string LTKW20_180S05N5_5T0_7Sample_base = 
  "_TelID_0_Azimuth_180_Zenith_20_Noise_5.5_AbsoluteOffset_0.5";
string LTKW20_180S05N5_88T0_12Sample_base = 
  "_TelID_0_Azimuth_180_Zenith_20_Noise_5.88_AbsoluteOffset_0.5";
string LTCW20_180Off05N5_77T0_7Sample_base = 
  "_TelID_0_Azimuth_180_Zenith_20_Noise_5.77_AbsoluteOffset_0.5";



string ltk7atm21;
string ltk7atm22;
string ltc7V240atm22;
string ltc7V240atm21;
string LTKW20_180S05N5_5T0;
string LTKW20_180S05N5_88T0;
string LTCW20_180Off05N5_77T0;


void makePlots(int imDist,string pltType)
{

  widthTest(ltk7atm22.c_str(),LTKW20_180S05N5_5T0.c_str(),imDist," ",1,pltType.c_str());
  widthTest(ltk7atm21.c_str(),LTKW20_180S05N5_5T0.c_str(),imDist,"same",2,pltType.c_str());

  widthTest(ltc7V240atm21.c_str(),LTCW20_180Off05N5_77T0.c_str(),imDist,"same",3,pltType.c_str());
  widthTest(ltc7V240atm22.c_str(),LTCW20_180Off05N5_77T0.c_str(),imDist,"same",4,pltType.c_str());

}


void LTKC7Plots(string pltType, string method="H")
{
  string CSKAMethod;
  if(method=="HFit25"){
    CSKAMethod="hfit25_";
  }
 
  ltk7atm21 = "ltk_20102011_atm21_7samples_"+method+"_vegas240_alloffsets.root";
  ltk7atm22 = "ltk_20102011_atm22_7samples_"+method+"_vegas240_alloffsets.root";
  ltc7V240atm22  =
    "lt_Nov2010_na_ATM22_7samples_vegasv240rc1_"+CSKAMethod+"allOffsets.root";
  ltc7V240atm21  =
    "lt_Nov2010_na_ATM21_7samples_vegasv240rc1_"+CSKAMethod+"allOffsets.root";

  LTKW20_180S05N5_5T0="LookupTable_"+pltType+LTKW20_180S05N5_5T0_7Sample_base;
  LTKW20_180S05N5_88T0="LookupTable_"+pltType+LTKW20_180S05N5_88T0_12Sample_base;
  LTCW20_180Off05N5_77T0 = "LookupTable_"+pltType+ LTCW20_180Off05N5_77T0_7Sample_base;
  cout<<"Black: "<<ltk7atm22<<endl;
  cout<<"       "<<LTKW20_180S05N5_5T0<<endl<<endl;
  cout<<"Red:   "<<ltk7atm21<<endl;
  cout<<"       "<<LTKW20_180S05N5_5T0<<endl;
  cout<<"Green: "<<ltc7V240atm21<<endl;
  cout<<"       "<<LTCW20_180Off05N5_77T0<<endl<<endl;
  cout<<"Blue: "<<ltc7V240atm21<<endl;
  cout<<"       "<<LTCW20_180Off05N5_77T0<<endl<<endl;
 
  gStyle->SetOptStat(kFALSE);
  widthTest(ltk7atm21.c_str(),LTKW20_180S05N5_5T0.c_str(),50," ",1);


  c1->Clear();
  c1->Divide(2,2);
  c1->cd(1);
  makePlots(50,pltType);

  c1->cd(2);
  makePlots(75,pltType);

  c1->cd(3);
  makePlots(100,pltType);

  c1->cd(4);
  makePlots(150,pltType);

 return;
}
