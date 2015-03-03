TCanvas * pfCnvs;

//UA-050
void CheckEAPlots(string kNoise="5.55", string cNoise="5.21", string kOff="050", string cOff="050",string zn1="20", string zn2="50", string az1="180", string az2="45")

//UA-All
//void CheckEAPlots(string kNoise="5.55", string cNoise="5.21", string kOff="0.5", string cOff="0.5",string zn1="20", string zn2="50", string az1="180", string az2="45")

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

 //KASCADE V6 Atm21 050
  //  string eak1="ea_MDL10UA_V6_PMTUpgrade_ATM21_KASCADE_vegasv250rc5_7sam_050off_s400t2_std_MSW1.1_MSL1.3_MH7.0_ThetaSq0.03_LZA.root";
  //string eak2="ea_MDL10UA_V6_PMTUpgrade_ATM21_KASCADE_vegasv250rc5_7sam_050off_s700t2_std_MSW1.1_MSL1.3_MH7.0_ThetaSq0.01_LZA.root";  
  //string eak3="ea_MDL10UA_V6_PMTUpgrade_ATM21_KASCADE_vegasv250rc5_7sam_050off_s1200t2_std_MSW1.1_MSL1.4_ThetaSq0.01_LZA.root";
  //string eak4="ea_MDL10UA_V6_PMTUpgrade_ATM21_KASCADE_vegasv250rc5_7sam_050off_s400t2_std_MSW1.15_MSL1.4_ThetaSq0.03_LZA.root";

 //KASCADE V6 Atm21 All
  //string eak1="ea_MDL10UA_V6_PMTUpgrade_ATM21_KASCADE_vegasv250rc5_7sam_Alloff_s400t2_std_MSW1.1_MSL1.3_MH7.0_ThetaSq0.03_LZA.root";
  //string eak2="ea_MDL10UA_V6_PMTUpgrade_ATM21_KASCADE_vegasv250rc5_7sam_Alloff_s700t2_std_MSW1.1_MSL1.3_MH7.0_ThetaSq0.01_LZA.root";  
  //string eak3="ea_MDL10UA_V6_PMTUpgrade_ATM21_KASCADE_vegasv250rc5_7sam_Alloff_s1200t2_std_MSW1.1_MSL1.4_ThetaSq0.01_LZA.root";
  //string eak4="ea_MDL10UA_V6_PMTUpgrade_ATM21_KASCADE_vegasv250rc5_7sam_Alloff_s400t2_std_MSW1.15_MSL1.4_ThetaSq0.03_LZA.root";

 //KASCADE V6 Atm22 050
 string eak1="ea_MDL10UA_V6_PMTUpgrade_ATM22_KASCADE_vegasv250rc5_7sam_050off_s400t2_std_MSW1.1_MSL1.3_MH7.0_ThetaSq0.03_LZA.root";
 string eak2="ea_MDL10UA_V6_PMTUpgrade_ATM22_KASCADE_vegasv250rc5_7sam_050off_s700t2_std_MSW1.1_MSL1.3_MH7.0_ThetaSq0.01_LZA.root";
 string eak3="ea_MDL10UA_V6_PMTUpgrade_ATM22_KASCADE_vegasv250rc5_7sam_050off_s1200t2_std_MSW1.1_MSL1.4_ThetaSq0.01_LZA.root";
 string eak4="ea_MDL10UA_V6_PMTUpgrade_ATM22_KASCADE_vegasv250rc5_7sam_050off_s400t2_std_MSW1.15_MSL1.4_ThetaSq0.03_LZA.root";


 //KASCADE V6 Atm22 All
 //string eak1="ea_MDL10UA_V6_PMTUpgrade_ATM22_KASCADE_vegasv250rc5_7sam_Alloff_s400t2_std_MSW1.1_MSL1.3_MH7.0_ThetaSq0.03_LZA.root";
 //string eak2="ea_MDL10UA_V6_PMTUpgrade_ATM22_KASCADE_vegasv250rc5_7sam_Alloff_s700t2_std_MSW1.1_MSL1.3_MH7.0_ThetaSq0.01_LZA.root";
 //string eak3="ea_MDL10UA_V6_PMTUpgrade_ATM22_KASCADE_vegasv250rc5_7sam_Alloff_s1200t2_std_MSW1.1_MSL1.4_ThetaSq0.01_LZA.root";
 //string eak4="ea_MDL10UA_V6_PMTUpgrade_ATM22_KASCADE_vegasv250rc5_7sam_Alloff_s400t2_std_MSW1.15_MSL1.4_ThetaSq0.03_LZA.root";


 // *****************************************************************************
 
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

   //GrISUDet V6 Atm21 All (soft for loose)
  //string ea1="$VEGAS/../tables/ea_Oct2012_ua_ATM21_vegasv250rc5_7sam_Alloff_s400t2_std_MSW1p1_MSL1p3_MH7_ThetaSq0p03_LZA_fixed150.root";
  //string ea2="$VEGAS/../tables/ea_Oct2012_ua_ATM21_vegasv250rc5_7sam_Alloff_s700t2_std_MSW1p1_MSL1p3_MH7_ThetaSq0p01_LZA_fixed150.root";
  //string ea3="$VEGAS/../tables/ea_Oct2012_ua_ATM21_vegasv250rc5_7sam_Alloff_s1200t2_std_MSW1p1_MSL1p4_ThetaSq0p01_LZA_fixed150.root";
  //string ea4="$VEGAS/../tables/ea_Oct2012_ua_ATM21_vegasv250rc5_7sam_Alloff_s400t2_std_MSW1p1_MSL1p3_MH7_ThetaSq0p03_LZA_fixed150.root";

   //GrISUDet V6 Atm22 050 
 string ea1="$VEGAS/../tables/ea_Oct2012_ua_ATM22_vegasv250rc5_7sam_050off_s400t2_std_MSW1p1_MSL1p3_MH7_ThetaSq0p03_LZA.root";
 string ea2="$VEGAS/../tables/ea_Oct2012_ua_ATM22_vegasv250rc5_7sam_050off_s700t2_std_MSW1p1_MSL1p3_MH7_ThetaSq0p01_LZA.root";
 string ea3="$VEGAS/../tables/ea_Oct2012_ua_ATM22_vegasv250rc5_7sam_050off_s1200t2_std_MSW1p1_MSL1p4_ThetaSq0p01_LZA.root";
 string ea4="$VEGAS/../tables/ea_Oct2012_ua_ATM22_vegasv250rc5_7sam_050off_s400t2_std_MSW1p15_MSL1p4_ThetaSq0p03_LZA.root";

   //GrISUDet V6 Atm22 All (soft for loose)
 // string ea1="$VEGAS/../tables/ea_Oct2012_ua_ATM22_vegasv250rc5_7sam_Alloff_s400t2_std_MSW1p1_MSL1p3_MH7_ThetaSq0p03_LZA.root";
 //string ea2="$VEGAS/../tables/ea_Oct2012_ua_ATM22_vegasv250rc5_7sam_Alloff_s700t2_std_MSW1p1_MSL1p3_MH7_ThetaSq0p01_LZA.root";
 // string ea3="$VEGAS/../tables/ea_Oct2012_ua_ATM22_vegasv250rc5_7sam_Alloff_s1200t2_std_MSW1p1_MSL1p4_ThetaSq0p01.root";
 //string ea4="$VEGAS/../tables/ea_Oct2012_ua_ATM22_vegasv250rc5_7sam_Alloff_s400t2_std_MSW1p1_MSL1p3_MH7_ThetaSq0p03_LZA.root";

 // ********************************************************************************

  //string tablk1="EffectiveArea_Azimuth_180_Zenith_20_Noise_5.55_AbsoluteOffset_0.5";
  //string tablk1="EffectiveArea_Azimuth_180_Zenith_20_Noise_5.51_AbsoluteOffset_0.5";
  //string tablk1="EffectiveArea_Azimuth_180_Zenith_20_Noise_5.51";
  string tablk1="EffectiveArea_Azimuth_" + az1 + "_Zenith_" + zn1 + "_Noise_" + 
    kNoise;                                              //V4 KASCADE All Offset
 
  //string tablk2="EffectiveArea_Azimuth_225_Zenith_50_Noise_5.55_AbsoluteOffset_0.5";
  //string tablk2="EffectiveArea_Azimuth_225_Zenith_50_Noise_5.51_AbsoluteOffset_0.5";
  //string tablk2="EffectiveArea_Azimuth_225_Zenith_50_Noise_5.51";
  string tablk2="EffectiveArea_Azimuth_" + az2 + "_Zenith_" + zn2 + "_Noise_" + 
    kNoise;                                            //V4 KASCADE All Offset

  if ( kOff != "050" ) {
     kOff="_AbsoluteOffset_" + kOff;
     tablk1=tablk1 + kOff;
     tablk2=tablk2 + kOff;
  }

  //string tabl1="EffectiveArea_Azimuth_180_Zenith_20_Noise_5.55";
  //string tabl1="EffectiveArea_Azimuth_180_Zenith_20_Noise_5.21_AbsoluteOffset_0.5";
  //string tabl1="EffectiveArea_Azimuth_180_Zenith_20_Noise_5.28_AbsoluteOffset_0.5";
  string tabl1="EffectiveArea_Azimuth_" + az1 + "_Zenith_" + zn1 + "_Noise_" + cNoise;
                                                        //V4 GrISUDet 050


  //string tabl2="EffectiveArea_Azimuth_225_Zenith_50_Noise_5.55";
  //string tabl2="EffectiveArea_Azimuth_225_Zenith_50_Noise_5.21_AbsoluteOffset_0.5";
  //string tabl2="EffectiveArea_Azimuth_225_Zenith_50_Noise_5.28_AbsoluteOffset_0.5";
  string tabl2="EffectiveArea_Azimuth_" + az2 + "_Zenith_" + zn2 + "_Noise_" + cNoise;
                                                         //V4 GrISUDet 050
  
  if ( cOff != "050" ) {
    cOff="_AbsoluteOffset_" + cOff;
    tabl1=tabl1+cOff;
    tabl2=tabl2+cOff;
  }
	     

  cout<<eak1<<endl;
  cout<<tablk1<<endl;
  drawTables(eak1.c_str(),tablk1.c_str(),"pfEffArea_MC"," ",1);
  pfCnvs->Clear();
  pfCnvs->Divide(2,4);

  pfCnvs->cd(1);
  drawTables(eak1.c_str(),tablk1.c_str(),"pfEffArea_MC"," ",1);
  drawTables(ea1.c_str(),tabl1.c_str(),"pfEffArea_MC","same",2);

  pfCnvs->cd(2);
  drawTables(eak1.c_str(),tablk2.c_str(),"pfEffArea_MC"," ",1);
  drawTables(ea1.c_str(),tabl2.c_str(),"pfEffArea_MC","same",2);

  pfCnvs->cd(3);
  drawTables(eak2.c_str(),tablk1.c_str(),"pfEffArea_MC"," ",1);
  drawTables(ea2.c_str(),tabl1.c_str(),"pfEffArea_MC","same",2);

  pfCnvs->cd(4);
  drawTables(eak2.c_str(),tablk2.c_str(),"pfEffArea_MC"," ",1);
  drawTables(ea2.c_str(),tabl2.c_str(),"pfEffArea_MC","same",2);

  pfCnvs->cd(5);
  drawTables(eak3.c_str(),tablk1.c_str(),"pfEffArea_MC"," ",1);
  drawTables(ea3.c_str(),tabl1.c_str(),"pfEffArea_MC","same",2);

  pfCnvs->cd(6);
  drawTables(eak3.c_str(),tablk2.c_str(),"pfEffArea_MC"," ",1);
  drawTables(ea3.c_str(),tabl2.c_str(),"pfEffArea_MC","same",2);

  pfCnvs->cd(7);
  drawTables(eak4.c_str(),tablk1.c_str(),"pfEffArea_MC"," ",1);
  drawTables(ea4.c_str(),tabl1.c_str(),"pfEffArea_MC","same",2);

  pfCnvs->cd(8);
  drawTables(eak4.c_str(),tablk2.c_str(),"pfEffArea_MC"," ",1);
  drawTables(ea4.c_str(),tabl2.c_str(),"pfEffArea_MC","same",2);

}
