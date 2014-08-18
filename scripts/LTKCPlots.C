void LTKCPlots(string pltType, string Zn="20", string Az="180")
{
  string ltkFileName  = "ltk_Jan2013_ua_ATM21_7samples_H_vegas250_alloffsets.root";
  string ltcFileName  = "$VEGAS/../tables/lt_Oct2012_ua_ATM21_7samples_vegasv250rc5_allOffsets_LZA_noise150fix.root";

  string LTK_base = "_TelID_0_Azimuth_" + Az +"_Zenith_" + Zn + 
                                                  "_Noise_5.55_AbsoluteOffset_0.5";
  string LTC_base = "_TelID_0_Azimuth_" + Az +"_Zenith_" + Zn + 
                                                  "_Noise_5.21_AbsoluteOffset_0.5";

  string LTK_LT="LookupTable_"+pltType+LTK_base;
  string LTC_LT="LookupTable_"+pltType+LTC_base;
  
  widthTest(ltkFileName.c_str(),LTK_LT.c_str(),50," ",1);
  c1->Clear();
  c1->Divide(2,2);
  c1->cd(1);
  widthTest(ltkFileName.c_str(),LTK_LT.c_str(),50," ",1, pltType.c_str());
  widthTest(ltcFileName.c_str(),LTC_LT.c_str(),50,"same", 2, pltType.c_str());

  c1->cd(2);
  widthTest(ltkFileName.c_str(),LTK_LT.c_str(),75," ",1, pltType.c_str());
  widthTest(ltcFileName.c_str(),LTC_LT.c_str(),75,"same", 2, pltType.c_str());

  c1->cd(3);
  widthTest(ltkFileName.c_str(),LTK_LT.c_str(),100," ",1, pltType.c_str());
  widthTest(ltcFileName.c_str(),LTC_LT.c_str(),100,"same", 2, pltType.c_str());

  c1->cd(4);
  widthTest(ltkFileName.c_str(),LTK_LT.c_str(),150," ", 1, pltType.c_str());
  widthTest(ltcFileName.c_str(),LTC_LT.c_str(),150,"same", 2, pltType.c_str());
 
  cout<<"Black: "<<ltkFileName<<endl;
  cout<<"Red: "<<ltcFileName<<endl;

 return;
}
