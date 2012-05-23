void LTKCPlots(string pltType)
{
  string ltk7e2_0  = "ltk_20102011_atm21_7samples_vegas240_alloffsets_E-2.0.root";
  string ltk7e2_45 = "ltk_20102011_atm21_7samples_vegas240_alloffsets.root";
  string ltc7V240  = "lt_Nov2010_na_ATM21_7samples_vegasv240rc1.root";
  string LTKW20_180S05N5_5T0_base = 
                       "_TelID_0_Azimuth_180_Zenith_20_Noise_5.5_AbsoluteOffset_0.5";
  string LTCW20_180Off05N5_77T0_base = 
                        "_TelID_0_Azimuth_180_Zenith_20_Noise_5.77";
  string LTKW20_180S05N5_5T0="LookupTable_"+pltType+LTKW20_180S05N5_5T0_base;
  string LTCW20_180Off05N5_77T0 = "LookupTable_"+pltType+LTCW20_180Off05N5_77T0_base;
  
  

  widthTest(ltk7e2_0.c_str(),LTKW20_180S05N5_5T0.c_str(),50," ",1);
  c1->Clear();
  c1->Divide(2,2);
  c1->cd(1);
  widthTest(ltk7e2_0.c_str(),LTKW20_180S05N5_5T0.c_str(),50," ",1,pltType.c_str());
  widthTest(ltc7V240.c_str(),LTCW20_180Off05N5_77T0.c_str(),50,"same",2,pltType.c_str());
  widthTest(ltk7e2_45.c_str(),LTKW20_180S05N5_5T0.c_str(),50,"same",3,pltType.c_str());

  c1->cd(2);
  widthTest(ltk7e2_0.c_str(),LTKW20_180S05N5_5T0.c_str(),75," ",1,pltType.c_str());
  widthTest(ltc7V240.c_str(),LTCW20_180Off05N5_77T0.c_str(),75,"same",2,pltType.c_str());
  widthTest(ltk7e2_45.c_str(),LTKW20_180S05N5_5T0.c_str(),75,"same",3,pltType.c_str());

  c1->cd(3);
  widthTest(ltk7e2_0.c_str(),LTKW20_180S05N5_5T0.c_str(),100," ",1,pltType.c_str());
  widthTest(ltc7V240.c_str(),LTCW20_180Off05N5_77T0.c_str(),100,"same",2,pltType.c_str());
  widthTest(ltk7e2_45.c_str(),LTKW20_180S05N5_5T0.c_str(),100,"same",3,pltType.c_str());

  c1->cd(4);
  widthTest(ltk7e2_0.c_str(),LTKW20_180S05N5_5T0.c_str(),150," ",1,pltType.c_str());
  widthTest(ltc7V240.c_str(),LTCW20_180Off05N5_77T0.c_str(),150,"same",2,pltType.c_str());
  widthTest(ltk7e2_45.c_str(),LTKW20_180S05N5_5T0.c_str(),150,"same",3,pltType.c_str());
  return;
}
