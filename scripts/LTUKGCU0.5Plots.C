void LTUKGCPlots(string pltType)
{
  string ltkFile050  = "$VEGAS/../tables/ltk_Jan2013_ua_ATM21_7samples_H_vegas250rc5_050offset.root";
  string ltkFile  = "$VEGAS/../tables/ltk_Jan2013_ua_ATM21_7samples_H_vegas250_alloffsets.root";
  //  string ltgFile  = "$VEGAS/../tables/lt_Oct2012_ua_ATM21_7samples_vegasv250rc4_allOffsets.root";
  string ltgFile  = "$VEGAS/../tables/lt_Oct2012_ua_ATM21_7samples_vegasv250rc5_050wobb_LZA.root";
  string ltcFile  = "$VEGAS/../tables/Care_00deg_65deg_50MHz_200MHz_UpgradedArray.root";

  string ltkTableBase  = "_TelID_2_Azimuth_180_Zenith_20_Noise_6.51";
  string ltcTableBase  = "_TelID_2_Azimuth_180_Zenith_20_Noise_6.59";
//  string ltgTableBase  = "_TelID_2_Azimuth_180_Zenith_20_Noise_6.68_AbsoluteOffset_0.5";
  string ltgTableBase  = "_TelID_2_Azimuth_180_Zenith_20_Noise_6.68";

  string ltkTable = "LookupTable_"+pltType+ltkTableBase+"_AbsoluteOffset_0.5";
  string ltkTable050 = "LookupTable_"+pltType+ltkTableBase;
  string ltcTable = "LookupTable_"+pltType+ltcTableBase;
  string ltgTable = "LookupTable_"+pltType+ltgTableBase;

  widthTest(ltkFile.c_str(),ltkTable.c_str(),50," ",1);
  c1->Clear();
  c1->Divide(2,2);
  c1->cd(1);
  widthTest(ltkFile.c_str(),ltkTable.c_str(),50," ",1);
  widthTest(ltkFile050.c_str(),ltkTable050.c_str(),50,"same",2);
  widthTest(ltgFile.c_str(),ltgTable.c_str(),50,"same",3);
  widthTest(ltcFile.c_str(),ltcTable.c_str(),50,"same",4);
  c1->cd(2);
  widthTest(ltkFile.c_str(),ltkTable.c_str(),75," ",1);
  widthTest(ltkFile050.c_str(),ltkTable050.c_str(),75,"same",2);
  widthTest(ltgFile.c_str(),ltgTable.c_str(),75,"same",3);
  widthTest(ltcFile.c_str(),ltcTable.c_str(),75,"same",4);
  c1->cd(3);
  widthTest(ltkFile.c_str(),ltkTable.c_str(),100," ",1);
  widthTest(ltkFile050.c_str(),ltkTable050.c_str(),100,"same",2);
  widthTest(ltgFile.c_str(),ltgTable.c_str(),100,"same",3);
  widthTest(ltcFile.c_str(),ltcTable.c_str(),100,"same",4);
  c1->cd(4);
  widthTest(ltkFile.c_str(),ltkTable.c_str(),150," ",1);
  widthTest(ltkFile050.c_str(),ltkTable050.c_str(),150,"same",2);
  widthTest(ltgFile.c_str(),ltgTable.c_str(),150,"same",3);
  widthTest(ltcFile.c_str(),ltcTable.c_str(),150,"same",4);
 
  cout<<"Black: "<<ltkFile<<endl;
  cout<<"Red: "<<ltkFile050<<endl;
  cout<<"Green: "<<ltgFile<<endl;
  cout<<"Blue: "<<ltcFile<<endl;

 return;
}
