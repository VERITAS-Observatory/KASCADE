
void LTKCPlots(string pltType, string Az="180", string kNoise="5.55", string cNoise="5.21",string offSet="0.5")
{
  //string ltkFileName  = "ltk_Jan2013_ua_ATM21_7samples_H_vegas250_alloffsets.root";
  //string ltcFileName  = "$VEGAS/../tables/lt_Oct2012_ua_ATM21_7samples_vegasv250rc5_allOffsets_LZA_noise150fix.root";

  //WUG MDL10UA MDL12UA GrISUDet All offset
  string ltkFileName  = "lt_MDL10UA_V6_PMTUpgrade_ATM21_KASCADE_vegasv250rc5_7sam_Alloff_std_d1.43_LZA.root";
  string ltcFileName  = "$VEGAS/../tables/lt_Oct2012_ua_ATM21_7samples_vegasv250rc5_allOffsets_LZA_noise150fix.root";
  string ltk12FileName  = "lt_MDL12UA_V6_PMTUpgrade_ATM21_KASCADE_vegasv250rc5_7sam_Alloff_std_d1.43_LZA.root";
 

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
 // string ltkFileName  = "lt_MDL10UA_V6_PMTUpgrade_ATM22_KASCADE_vegasv250rc5_7sam_050off_std_d1.43_LZA.root";
  //string ltcFileName  = "$VEGAS/../tables/lt_Oct2012_ua_ATM22_7samples_vegasv250rc5_050wobb_LZA_noise150fix.root";

  //cout<<"Offset: " << offSet <<endl;
  //string Znk=Zn;
  //string Znc=Zn;
  //cout<<"Zn: "<<Zn<<endl;
  //if ( Zn == "1" || Zn == "0" ) {
  //  Znk="1";
  //  Znc="0";
  // }


  vector <string> ZenithK;
  ZenithK.push_back("1");
  ZenithK.push_back("20");
  ZenithK.push_back("30");
  ZenithK.push_back("40");
  ZenithK.push_back("50");
  ZenithK.push_back("60");
  vector <string> ZenithC;
  ZenithC.push_back("0");
  ZenithC.push_back("20");
  ZenithC.push_back("30");
  ZenithC.push_back("40");
  ZenithC.push_back("50");
  ZenithC.push_back("60");



  string Znk=ZenithK.at(0);
  string Znc=ZenithC.at(0);

  if (offSet != "050" ) { 
    string LTK_base = "_TelID_0_Azimuth_" + Az +"_Zenith_" + Znk + 
      "_Noise_" + kNoise + "_AbsoluteOffset_" + offSet;
    string LTC_base = "_TelID_0_Azimuth_" + Az +"_Zenith_" + Znc + 
      "_Noise_" + cNoise + "_AbsoluteOffset_" + offSet;

  } 
  else{
    string LTK_base = "_TelID_0_Azimuth_" + Az +"_Zenith_" + Znk + 
      "_Noise_" + kNoise;
    string LTC_base = "_TelID_0_Azimuth_" + Az +"_Zenith_" + Znc + 
      "_Noise_" + cNoise;
  }


  string LTK_LT="LookupTable_"+pltType+LTK_base;
  string LTC_LT="LookupTable_"+pltType+LTC_base;
  
  widthTest(ltkFileName.c_str(),LTK_LT.c_str(),50," ",1);
  c1->Clear();
  c1->Divide(4,6);
  

  for (int i=0;i<6;i++) {
    
    Znk=ZenithK.at(i);
    Znc=ZenithC.at(i);

    if (offSet != "050" ) { 
      LTK_base = "_TelID_0_Azimuth_" + Az +"_Zenith_" + Znk + 
       "_Noise_" + kNoise + "_AbsoluteOffset_" + offSet;
      LTC_base = "_TelID_0_Azimuth_" + Az +"_Zenith_" + Znc + 
       "_Noise_" + cNoise + "_AbsoluteOffset_" + offSet;

    } 
    else{
      LTK_base = "_TelID_0_Azimuth_" + Az +"_Zenith_" + Znk + 
      "_Noise_" + kNoise;
      LTC_base = "_TelID_0_Azimuth_" + Az +"_Zenith_" + Znc + 
	"_Noise_" + cNoise;
    }


    LTK_LT="LookupTable_"+pltType+LTK_base;
    LTC_LT="LookupTable_"+pltType+LTC_base;
  
    int j=i*4;
    
    c1->cd(j+1);
    string pltTitleBase =pltType+" " + Znk + "_" + Az+ " " + kNoise + " " + offSet;
    string pltTitle = pltTitleBase + " 50m";
    widthTest(ltkFileName.c_str(),LTK_LT.c_str(),50," ",1, pltTitle.c_str());
    widthTest(ltcFileName.c_str(),LTC_LT.c_str(),50,"same", 2, pltTitle.c_str());
    widthTest(ltk12FileName.c_str(),LTK_LT.c_str(),50,"same",3, pltTitle.c_str());
    
    c1->cd(j+2);
    pltTitle = pltTitleBase + " 75m";
    widthTest(ltkFileName.c_str(),LTK_LT.c_str(),75," ",1, pltTitle.c_str());
    widthTest(ltcFileName.c_str(),LTC_LT.c_str(),75,"same", 2, pltTitle.c_str());
    widthTest(ltk12FileName.c_str(),LTK_LT.c_str(),75,"same",3, pltTitle.c_str());
    
    c1->cd(j+3);
    pltTitle = pltTitleBase + " 100m";
    widthTest(ltkFileName.c_str(),LTK_LT.c_str(),100," ",1, pltTitle.c_str());
    widthTest(ltcFileName.c_str(),LTC_LT.c_str(),100,"same", 2, pltTitle.c_str());
    widthTest(ltk12FileName.c_str(),LTK_LT.c_str(),100,"same",3, pltTitle.c_str());
    
    c1->cd(j+4);
    pltTitle = pltTitleBase + " 150m";
    widthTest(ltkFileName.c_str(),LTK_LT.c_str(),150," ", 1, pltTitle.c_str());
    widthTest(ltcFileName.c_str(),LTC_LT.c_str(),150,"same", 2, pltTitle.c_str());
    widthTest(ltk12FileName.c_str(),LTK_LT.c_str(),150,"same",3, pltTitle.c_str());
    
  }
  
  cout<<"Black: "<<ltkFileName<<"/"<< LTK_LT<<endl;
  cout<<"Red: "<<ltcFileName<<"/"<< LTC_LT<<endl;
  cout<<"Green: "<<ltk12FileName<<"/"<< LTC_LT<<endl;

  return;
}
