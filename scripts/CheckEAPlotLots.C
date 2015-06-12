
TCanvas* pfCnvs;

//UA-050
//void CheckEAPlots(string kNoise="5.55", string cNoise="5.21", string kOff="050", string cOff="050",string zn1="20", string zn2="50", string az1="180", string az2="45")

//UA-All
void CheckEAPlotsLots(string kNoise="5.55", string cNoise="5.21", string kOff="0.5", string cOff="0.5",string zn1="20", string zn2="50", string az1="180", string az2="45")

{ 

  //  string eak1="ea_MDL15NA_V5_T1Move_ATM21_KASCADE_vegasv250rc5_7sam_050off_s200t2_std_MSW1.1_MSL1.3_MH7_ThetaSq0.03_LZA.root";
  //string eak2="ea_MDL15NA_V5_T1Move_ATM21_KASCADE_vegasv250rc5_7sam_050off_s400t2_std_MSW1.1_MSL1.3_MH7.0_ThetaSq0.01_LZA.root";
  //string eak3="ea_MDL15NA_V5_T1Move_ATM21_KASCADE_vegasv250rc5_7sam_050off_s1000t2_std_MSW1.1_MSL1.4_ThetaSq0.01_LZA.root";
  //string eak4="ea_MDL15NA_V5_T1Move_ATM21_KASCADE_vegasv250rc5_7sam_050off_s200t2_std_MSW1.1_MSL1.3_MH7_ThetaSq0.03_LZA.root";  

  //KASCADE V4 Atm21 Alloffset 
  //string eak1="ea_MDL8OA_V4_OldArray_ATM21_KASCADE_vegasv250rc5_7sam_Alloff_s200t2_std_MSW1.1_MSL1.3_MH7_ThetaSq0.03_LZA.root";
  //string eak2="ea_MDL8OA_V4_OldArray_ATM21_KASCADE_vegasv250rc5_7sam_Alloff_s400t2_std_MSW1.1_MSL1.3_MH7.0_ThetaSq0.01_LZA.root";
  //string eak3="ea_MDL8OA_V4_OldArray_ATM21_KASCADE_vegasv250rc5_7sam_Alloff_s1000t2_std_MSW1.1_MSL1.4_ThetaSq0.01_LZA.root";
  //string eak4="ea_MDL8OA_V4_OldArray_ATM21_KASCADE_vegasv250rc5_7sam_Alloff_s200t2_std_MSW1.15_MSL1.4_ThetaSq0.03_LZA.root";  

  //KASCADE V4 Atm21 050 
  //string eak1="ea_MDL8OA_V4_OldArray_ATM21_KASCADE_vegasv250rc5_7sam_050off_s200t2_std_MSW1.1_MSL1.3_MH7_ThetaSq0.03_LZA.root";
  //string eak2="ea_MDL8OA_V4_OldArray_ATM21_KASCADE_vegasv250rc5_7sam_050off_s400t2_std_MSW1.1_MSL1.3_MH7.0_ThetaSq0.01_LZA.root";
  //string eak3="ea_MDL8OA_V4_OldArray_ATM21_KASCADE_vegasv250rc5_7sam_050off_s1000t2_std_MSW1.1_MSL1.4_ThetaSq0.01_LZA.root";
  //string eak4="ea_MDL8OA_V4_OldArray_ATM21_KASCADE_vegasv250rc5_7sam_050off_s200t2_std_MSW1.15_MSL1.4_ThetaSq0.03_LZA.root";  

  //KASCADE V4 Atm22 Alloffset 
  // string eak1="ea_MDL8OA_V4_OldArray_ATM22_KASCADE_vegasv250rc5_7sam_Alloff_s200t2_std_MSW1.1_MSL1.3_MH7_ThetaSq0.03_LZA.root";
  //string eak2="ea_MDL8OA_V4_OldArray_ATM22_KASCADE_vegasv250rc5_7sam_Alloff_s400t2_std_MSW1.1_MSL1.3_MH7.0_ThetaSq0.01_LZA.root";
  //string eak3="ea_MDL8OA_V4_OldArray_ATM22_KASCADE_vegasv250rc5_7sam_Alloff_s1000t2_std_MSW1.1_MSL1.4_ThetaSq0.01_LZA.root";
  //string eak4="ea_MDL8OA_V4_OldArray_ATM22_KASCADE_vegasv250rc5_7sam_Alloff_s200t2_std_MSW1.15_MSL1.4_ThetaSq0.03_LZA.root";  

 //KASCADE V5 Atm22 All
 //string eak1="ea_MDL15NA_V5_T1Move_ATM22_KASCADE_vegasv250rc5_7sam_Alloff_s200t2_std_MSW1.1_MSL1.3_MH7_ThetaSq0.03_LZA.root";
 //string eak2="ea_MDL15NA_V5_T1Move_ATM22_KASCADE_vegasv250rc5_7sam_Alloff_s400t2_std_MSW1.1_MSL1.3_MH7.0_ThetaSq0.01_LZA.root";
 //string eak3="ea_MDL15NA_V5_T1Move_ATM22_KASCADE_vegasv250rc5_7sam_Alloff_s1000t2_std_MSW1.1_MSL1.4_ThetaSq0.01_LZA.root";
 //string eak4="ea_MDL15NA_V5_T1Move_ATM22_KASCADE_vegasv250rc5_7sam_Alloff_s200t2_std_MSW1.15_MSL1.4_ThetaSq0.03_LZA.root";


  //KASCADE V5 Atm22 050
  // string eak1="ea_MDL15NA_V5_T1Move_ATM22_KASCADE_vegasv250rc5_7sam_050off_s200t2_std_MSW1.1_MSL1.3_MH7_ThetaSq0.03_LZA.root";
  //string eak2="ea_MDL15NA_V5_T1Move_ATM22_KASCADE_vegasv250rc5_7sam_050off_s400t2_std_MSW1.1_MSL1.3_MH7.0_ThetaSq0.01_LZA.root";
  //string eak3="ea_MDL15NA_V5_T1Move_ATM22_KASCADE_vegasv250rc5_7sam_050off_s1000t2_std_MSW1.1_MSL1.4_ThetaSq0.01_LZA.root";
  //string eak4="ea_MDL15NA_V5_T1Move_ATM22_KASCADE_vegasv250rc5_7sam_050off_s200t2_std_MSW1.15_MSL1.4_ThetaSq0.03_LZA.root";


 //KASCADE V6 Atm21 All Hard
  string eak1="$VEGAS/../tables/ea_MDL12UA_V6_PMTUpgrade_ATM21_KASCADE_vegasv250rc5_7sam_Alloff_s1200t2_std_MSW1.1_MSL1.4_ThetaSq0.01_LZA.root";
  string eak2="$VEGAS/../tables/ea_MDL10UA_V6_PMTUpgrade_ATM21_KASCADE_vegasv250rc5_7sam_Alloff_s1200t2_std_MSW1.1_MSL1.4_ThetaSq0.01_LZA.root";

 //KASCADE V6 Atm21 All Medium
  //  string eak1="$VEGAS/../tables/ea_MDL12UA_V6_PMTUpgrade_ATM21_KASCADE_vegasv250rc5_7sam_Alloff_s700t2_std_MSW1.1_MSL1.3_MH7.0_ThetaSq0.01_LZA.root";
  //string eak2="$VEGAS/../tables/ea_MDL10UA_V6_PMTUpgrade_ATM21_KASCADE_vegasv250rc5_7sam_Alloff_s700t2_std_MSW1.1_MSL1.3_MH7.0_ThetaSq0.01_LZA.root";

 //KASCADE V6 Atm21 All Soft
 // string eak1="$VEGAS/../tables/ea_MDL12UA_V6_PMTUpgrade_ATM21_KASCADE_vegasv250rc5_7sam_Alloff_s400t2_std_MSW1.1_MSL1.3_MH7.0_ThetaSq0.03_LZA.root";
 // string eak2="$VEGAS/../tables/ea_MDL10UA_V6_PMTUpgrade_ATM21_KASCADE_vegasv250rc5_7sam_Alloff_s400t2_std_MSW1.1_MSL1.3_MH7.0_ThetaSq0.03_LZA.root";


 // *************************************************************************
 
//GrISUDet

  //string ea1="$VEGAS/../tables/ea_Oct2012_na_ATM21_vegasv250rc5_7sam_Alloff_s200t2_std_MSW1p1_MSL1p3_MH7_ThetaSq0p03_v1.root";
  //string ea2="$VEGAS/../tables/ea_Oct2012_na_ATM21_vegasv250rc5_7sam_Alloff_s400t2_std_MSW1p1_MSL1p3_MH7_ThetaSq0p01_v1.root";
  //string ea3="$VEGAS/../tables/ea_Oct2012_na_ATM21_vegasv250rc5_7sam_Alloff_s1000t2_std_MSW1p1_MSL1p4_ThetaSq0p01_LZA_v1.root";
  //string ea4="$VEGAS/../tables/ea_Oct2012_na_ATM21_vegasv250rc5_7sam_Alloff_s200t2_std_MSW1p1_MSL1p3_MH7_ThetaSq0p03_v1.root";

  //GrISUDet V4 Atm21 All (uses "soft" for Loose")
  //string ea1="$VEGAS/../tables/ea_Oct2012_oa_ATM21_vegasv250rc5_7sam_Alloff_s200t2_std_MSW1p1_MSL1p3_MH7_ThetaSq0p03_LZA.root";
  //string ea2="$VEGAS/../tables/ea_Oct2012_oa_ATM21_vegasv250rc5_7sam_Alloff_s400t2_std_MSW1p1_MSL1p3_MH7_ThetaSq0p01_LZA.root";
  //string ea3="$VEGAS/../tables/ea_Oct2012_oa_ATM21_vegasv250rc5_7sam_Alloff_s1000t2_std_MSW1p1_MSL1p4_ThetaSq0p01_LZA.root";
  //string ea4="$VEGAS/../tables/ea_Oct2012_oa_ATM21_vegasv250rc5_7sam_Alloff_s200t2_std_MSW1p1_MSL1p3_MH7_ThetaSq0p03_LZA.root";

  //GrISUDet V4 Atm22 050 (uses "soft" for Loose")
  //string ea1="$VEGAS/../tables/ea_Oct2012_oa_ATM22_vegasv250rc5_7sam_050off_s200t2_std_MSW1p1_MSL1p3_MH7_ThetaSq0p03.root";
  //string ea2="$VEGAS/../tables/ea_Oct2012_oa_ATM22_vegasv250rc5_7sam_050off_s400t2_std_MSW1p1_MSL1p3_MH7_ThetaSq0p01.root";
  //string ea3="$VEGAS/../tables/ea_Oct2012_oa_ATM22_vegasv250rc5_7sam_050off_s1000t2_std_MSW1p1_MSL1p4_ThetaSq0p01.root";
  //string ea4="$VEGAS/../tables/ea_Oct2012_oa_ATM22_vegasv250rc5_7sam_050off_s200t2_std_MSW1p1_MSL1p3_MH7_ThetaSq0p03.root";


  //GrISUDet V5 Atm22 All (soft for loose)
  //string ea1="$VEGAS/../tables/ea_Oct2012_na_ATM22_vegasv250rc5_7sam_Alloff_s200t2_std_MSW1p1_MSL1p3_MH7_ThetaSq0p03_LZA.root";
  //string ea2="$VEGAS/../tables/ea_Oct2012_na_ATM22_vegasv250rc5_7sam_Alloff_s400t2_std_MSW1p1_MSL1p3_MH7_ThetaSq0p01_LZA.root";
  //string ea3="$VEGAS/../tables/ea_Oct2012_na_ATM22_vegasv250rc5_7sam_Alloff_s1000t2_std_MSW1p1_MSL1p4_ThetaSq0p01_LZA.root";
  //string ea4="$VEGAS/../tables/ea_Oct2012_na_ATM22_vegasv250rc5_7sam_Alloff_s200t2_std_MSW1p1_MSL1p3_MH7_ThetaSq0p03_LZA.root";

  //GrISUDet V5 Atm22 050 (soft for loose)
  //string ea1="$VEGAS/../tables/ea_Oct2012_na_ATM22_vegasv250rc5_7sam_050off_s200t2_std_MSW1p1_MSL1p3_MH7_ThetaSq0p03_LZA.root";
  //string ea2="$VEGAS/../tables/ea_Oct2012_na_ATM22_vegasv250rc5_7sam_050off_s400t2_std_MSW1p1_MSL1p3_MH7_ThetaSq0p01_LZA.root";
  //string ea3="$VEGAS/../tables/ea_Oct2012_na_ATM22_vegasv250rc5_7sam_050off_s1000t2_std_MSW1p1_MSL1p4_ThetaSq0p01_LZA.root";
  //string ea4="$VEGAS/../tables/ea_Oct2012_na_ATM22_vegasv250rc5_7sam_050off_s200t2_std_MSW1p1_MSL1p3_MH7_ThetaSq0p03_LZA.root";

   //GrISUDet V6 Atm21 050 
 //string ea1="$VEGAS/../tables/ea_Oct2012_ua_ATM21_vegasv250rc5_7sam_050off_s400t2_std_MSW1p1_MSL1p3_MH7_ThetaSq0p03_LZA_fixed150.root";                                 
 //string ea2="$VEGAS/../tables/ea_Oct2012_ua_ATM21_vegasv250rc5_7sam_050off_s700t2_std_MSW1p1_MSL1p3_MH7_ThetaSq0p01_LZA_fixed150.root";
 //string ea3="$VEGAS/../tables/ea_Oct2012_ua_ATM21_vegasv250rc5_7sam_050off_s1200t2_std_MSW1p1_MSL1p4_ThetaSq0p01_LZA_fixed150.root";
 //string ea4="$VEGAS/../tables/ea_Oct2012_ua_ATM21_vegasv250rc5_7sam_050off_s400t2_std_MSW1p15_MSL1p4_ThetaSq0p03_LZA_fixed150.root";

   //GrISUDet V6 Atm21 All SOFT and  LOOSE
  //string ea1="$VEGAS/../tables/ea_Oct2012_ua_ATM21_vegasv250rc5_7sam_Alloff_s400t2_std_MSW1p1_MSL1p3_MH7_ThetaSq0p03_LZA_fixed150.root";

   //GrISUDet V6 Atm21 All Medium
  //string ea1="$VEGAS/../tables/ea_Oct2012_ua_ATM21_vegasv250rc5_7sam_Alloff_s700t2_std_MSW1p1_MSL1p3_MH7_ThetaSq0p01_LZA_fixed150.root";

   //GrISUDet V6 Atm21 All Hard
  string ea1="$VEGAS/../tables/ea_Oct2012_ua_ATM21_vegasv250rc5_7sam_Alloff_s1200t2_std_MSW1p1_MSL1p4_ThetaSq0p01_LZA_fixed150.root";

  //string ea2="$VEGAS/../tables/ea_Oct2012_ua_ATM21_vegasv250rc5_7sam_Alloff_s700t2_std_MSW1p1_MSL1p3_MH7_ThetaSq0p01_LZA_fixed150.root";
  //string ea3="$VEGAS/../tables/ea_Oct2012_ua_ATM21_vegasv250rc5_7sam_Alloff_s1200t2_std_MSW1p1_MSL1p4_ThetaSq0p01_LZA_fixed150.root";
  //string ea4="$VEGAS/../tables/ea_Oct2012_ua_ATM21_vegasv250rc5_7sam_Alloff_s400t2_std_MSW1p1_MSL1p3_MH7_ThetaSq0p03_LZA_fixed150.root";

   //GrISUDet V6 Atm22 050 
  //string ea1="$VEGAS/../tables/ea_Oct2012_ua_ATM22_vegasv250rc5_7sam_050off_s400t2_std_MSW1p1_MSL1p3_MH7_ThetaSq0p03_LZA.root";
  //string ea2="$VEGAS/../tables/ea_Oct2012_ua_ATM22_vegasv250rc5_7sam_050off_s700t2_std_MSW1p1_MSL1p3_MH7_ThetaSq0p01_LZA.root";
  //string ea3="$VEGAS/../tables/ea_Oct2012_ua_ATM22_vegasv250rc5_7sam_050off_s1200t2_std_MSW1p1_MSL1p4_ThetaSq0p01_LZA.root";
  //string ea4="$VEGAS/../tables/ea_Oct2012_ua_ATM22_vegasv250rc5_7sam_050off_s400t2_std_MSW1p15_MSL1p4_ThetaSq0p03_LZA.root";

   //GrISUDet V6 Atm22 All (soft for loose)
 // string ea1="$VEGAS/../tables/ea_Oct2012_ua_ATM22_vegasv250rc5_7sam_Alloff_s400t2_std_MSW1p1_MSL1p3_MH7_ThetaSq0p03_LZA.root";
 //string ea2="$VEGAS/../tables/ea_Oct2012_ua_ATM22_vegasv250rc5_7sam_Alloff_s700t2_std_MSW1p1_MSL1p3_MH7_ThetaSq0p01_LZA.root";
 // string ea3="$VEGAS/../tables/ea_Oct2012_ua_ATM22_vegasv250rc5_7sam_Alloff_s1200t2_std_MSW1p1_MSL1p4_ThetaSq0p01.root";
 //string ea4="$VEGAS/../tables/ea_Oct2012_ua_ATM22_vegasv250rc5_7sam_Alloff_s400t2_std_MSW1p1_MSL1p3_MH7_ThetaSq0p03_LZA.root";

 // ********************************************************************************
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

  //string tablk1="EffectiveArea_Azimuth_180_Zenith_20_Noise_5.55_AbsoluteOffset_0.5";
  //string tablk1="EffectiveArea_Azimuth_180_Zenith_20_Noise_5.51_AbsoluteOffset_0.5";
  //string tablk1="EffectiveArea_Azimuth_180_Zenith_20_Noise_5.51";
  string tablk1="EffectiveArea_Azimuth_" + az1 + "_Zenith_" + Znk + "_Noise_" + 
    kNoise;   //V4 KASCADE All Offset
 
  //string tablk2="EffectiveArea_Azimuth_225_Zenith_50_Noise_5.55_AbsoluteOffset_0.5";
  //string tablk2="EffectiveArea_Azimuth_225_Zenith_50_Noise_5.51_AbsoluteOffset_0.5";
  //string tablk2="EffectiveArea_Azimuth_225_Zenith_50_Noise_5.51";
  string tablk2="EffectiveArea_Azimuth_" + az2 + "_Zenith_" + Znk + "_Noise_" + 
    kNoise;                                            //V4 KASCADE All Offset

  if ( kOff != "050" ) {
     tablk1=tablk1 + "_AbsoluteOffset_" + kOff;
     tablk2=tablk2 + "_AbsoluteOffset_" + kOff;
  }

  //string tabl1="EffectiveArea_Azimuth_180_Zenith_20_Noise_5.55";
  //string tabl1="EffectiveArea_Azimuth_180_Zenith_20_Noise_5.21_AbsoluteOffset_0.5";
  //string tabl1="EffectiveArea_Azimuth_180_Zenith_20_Noise_5.28_AbsoluteOffset_0.5";
  string tabl1="EffectiveArea_Azimuth_" + az1 + "_Zenith_" + Znk + "_Noise_" + cNoise;
                                                        //V4 GrISUDet 050


  //string tabl2="EffectiveArea_Azimuth_225_Zenith_50_Noise_5.55";
  //string tabl2="EffectiveArea_Azimuth_225_Zenith_50_Noise_5.21_AbsoluteOffset_0.5";
  //string tabl2="EffectiveArea_Azimuth_225_Zenith_50_Noise_5.28_AbsoluteOffset_0.5";
  string tabl2="EffectiveArea_Azimuth_" + az2 + "_Zenith_" + Znc + "_Noise_" + cNoise;
                                                         //V4 GrISUDet 050
  
  if ( cOff != "050" ) {
    tabl1=tabl1+"_AbsoluteOffset_" + cOff;
    tabl2=tabl2+"_AbsoluteOffset_" + cOff;
  }
  cout<<eak1<<endl;
  cout<<tablk1<<endl;

  drawTables(eak1.c_str(),tablk1.c_str(),"pfEffArea_MC"," ",1);
  c1->Clear();
  c1->Divide(2,6);

  for (int i=0;i<6;i++) {
     
    Znk=ZenithK.at(i);
    Znc=ZenithC.at(i);
    
    tablk1="EffectiveArea_Azimuth_" + az1 + "_Zenith_" + Znk + "_Noise_" + kNoise;   //V4 KASCADE All Offset
    tablk2="EffectiveArea_Azimuth_" + az2 + "_Zenith_" + Znk + "_Noise_" + kNoise; 
    tabl1 ="EffectiveArea_Azimuth_" + az1 + "_Zenith_" + Znc + "_Noise_"  + cNoise;
    tabl2 ="EffectiveArea_Azimuth_" + az2 + "_Zenith_" + Znc + "_Noise_"  + cNoise;
    if ( kOff != "050" ) {
      tablk1=tablk1 + "_AbsoluteOffset_" + kOff;
      tablk2=tablk2 + "_AbsoluteOffset_" + kOff;;
    }
    if ( cOff != "050" ) {
      tabl1=tabl1+"_AbsoluteOffset_" + cOff;
      tabl2=tabl2+"_AbsoluteOffset_" + cOff;
    }

    int j=i*2;
    c1->cd(j+1);
    string tblTitle="pfEffArea_MC: "+ Znk + "_" + az1 + " S" + kOff +" N:" + kNoise;
    drawTables(eak1.c_str(),tablk1.c_str(),"pfEffArea_MC"," ",1,tblTitle);
    drawTables(eak2.c_str(),tablk1.c_str(),"pfEffArea_MC","same",2, tblTitle);
    drawTables(ea1.c_str(),tabl1.c_str(),"pfEffArea_MC","same",3, tblTitle);
  
    c1->cd(j+2);
    tblTitle="pfEffArea_MC: "+ Znk + "_" + az2 + " S" + kOff +" N:" + kNoise;

    drawTables(eak1.c_str(),tablk2.c_str(),"pfEffArea_MC"," ",1, tblTitle);
    drawTables(eak2.c_str(),tablk2.c_str(),"pfEffArea_MC","same",2, tblTitle);
    drawTables(ea1.c_str(),tabl2.c_str(),"pfEffArea_MC","same",3, tblTitle);
  }
  
}
