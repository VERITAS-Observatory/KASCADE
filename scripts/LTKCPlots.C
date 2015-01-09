void LTKCPlots(string pltType, string Zn="20", string Az="180", string kNoise="5.55", string cNoise="5.21",string offSet="050")
{
  //string ltkFileName  = "ltk_Jan2013_ua_ATM21_7samples_H_vegas250_alloffsets.root";
  //string ltcFileName  = "$VEGAS/../tables/lt_Oct2012_ua_ATM21_7samples_vegasv250rc5_allOffsets_LZA_noise150fix.root";

  //  string ltkFileName  = "lt_V6_PMTUpgrade_ATM21_KASCADE_MDL10UA_vegasv250rc5_7sam_Alloff_std_d1p43_LZA.root";
  //  string ltcFileName  = "lt_V6_PMTUpgrade_ATM21_KASCADE_MDL10UA_vegasv250rc5_7sam_050off_std_d1p43_LZA.root";

  //string ltkFileName  = "$VEGAS/../tables/lt_MDL15NA_V5_T1Move_ATM21_KASCADE__vegasv250rc5_7sam_Alloff_std_d1.43_LZA.root";
  //string ltcFileName  = "$VEGAS/../tables/lt_Oct2012_na_ATM21_7samples_vegasv250rc5_allOffsets_LZA_v1.root";

  //string ltkFileName  = "lt_MDL8OA_V4_OldArray_ATM22_KASCADE_vegasv250rc5_7sam_Alloff_std_d1.43_LZA.root";
  //string ltcFileName  = "$VEGAS/../tables/lt_Oct2012_oa_ATM22_7samples_vegasv250rc5_allOffsets_LZA.root";

  //string ltkFileName  = "lt_MDL8OA_V4_OldArray_ATM21_KASCADE_vegasv250rc5_7sam_Alloff_std_d1.43_LZA.root";
  //string ltcFileName  = "$VEGAS/../tables/lt_Oct2012_oa_ATM21_7samples_vegasv250rc5_allOffsets_LZA.root";

  // WOG 050 offset
  //string ltkFileName  = "lt_MDL8OA_V4_OldArray_ATM21_KASCADE_vegasv250rc5_7sam_050off_std_d1.43_LZA.root";
  //string ltcFileName  = "$VEGAS/../tables/lt_Oct2012_oa_ATM21_7samples_vegasv250rc5_050wobb_LZA.root";

  //SNG All Offset
  //string ltkFileName  = "lt_MDL15NA_V5_T1Move_ATM22_KASCADE_vegasv250rc5_7sam_Alloff_std_d1.43_LZA.root";
  //string ltcFileName  = "$VEGAS/../tables/lt_Oct2012_na_ATM22_7samples_vegasv250rc5_allOffsets_LZA.root";

  //SUG All Offset
  //string ltkFileName  = "lt_MDL10UA_V6_PMTUpgrade_ATM22_KASCADE_vegasv250rc5_7sam_Alloff_std_d1.43_LZA.root";
  //string ltcFileName  = "$VEGAS/../tables/lt_Oct2012_ua_ATM22_7samples_vegasv250rc5_allOffsets_LZA_noise150fix.root";

 //SUG 050 Offset
  string ltkFileName  = "lt_MDL10UA_V6_PMTUpgrade_ATM22_KASCADE_vegasv250rc5_7sam_050off_std_d1.43_LZA.root";
  string ltcFileName  = "$VEGAS/../tables/lt_Oct2012_ua_ATM22_7samples_vegasv250rc5_050wobb_LZA_noise150fix.root";



  if (offSet != "050" ) {
    string LTK_base = "_TelID_0_Azimuth_" + Az +"_Zenith_" + Zn + 
      "_Noise_" + kNoise + "_AbsoluteOffset_" + offSet;
    string LTC_base = "_TelID_0_Azimuth_" + Az +"_Zenith_" + Zn + 
      "_Noise_" + cNoise + "_AbsoluteOffset_" + offSet;

  }
  else{
    string LTK_base = "_TelID_0_Azimuth_" + Az +"_Zenith_" + Zn + 
      "_Noise_" + kNoise;
    string LTC_base = "_TelID_0_Azimuth_" + Az +"_Zenith_" + Zn + 
      "_Noise_" + cNoise;
  }


  string LTK_LT="LookupTable_"+pltType+LTK_base;
  string LTC_LT="LookupTable_"+pltType+LTC_base;
  
  widthTest(ltkFileName.c_str(),LTK_LT.c_str(),50," ",1);
  c1->Clear();
  c1->Divide(2,2);
  c1->cd(1);
  string pltTitleBase =pltType+" " + Zn + "_" + Az+ " " + kNoise + " " + offSet;
  string pltTitle = pltTitleBase + " 50m";
  widthTest(ltkFileName.c_str(),LTK_LT.c_str(),50," ",1, pltTitle.c_str());
  widthTest(ltcFileName.c_str(),LTC_LT.c_str(),50,"same", 2, pltTitle.c_str());

  c1->cd(2);
  pltTitle = pltTitleBase + " 75m";
  widthTest(ltkFileName.c_str(),LTK_LT.c_str(),75," ",1, pltTitle.c_str());
  widthTest(ltcFileName.c_str(),LTC_LT.c_str(),75,"same", 2, pltTitle.c_str());

  c1->cd(3);
  pltTitle = pltTitleBase + " 100m";
  widthTest(ltkFileName.c_str(),LTK_LT.c_str(),100," ",1, pltTitle.c_str());
  widthTest(ltcFileName.c_str(),LTC_LT.c_str(),100,"same", 2, pltTitle.c_str());

  c1->cd(4);
  pltTitle = pltTitleBase + " 150m";
  widthTest(ltkFileName.c_str(),LTK_LT.c_str(),150," ", 1, pltTitle.c_str());
  widthTest(ltcFileName.c_str(),LTC_LT.c_str(),150,"same", 2, pltTitle.c_str());
 
  cout<<"Black: "<<ltkFileName<<"/"<< LTK_LT<<endl;
  cout<<"Red: "<<ltcFileName<<"/"<< LTC_LT<<endl;

 return;
}
