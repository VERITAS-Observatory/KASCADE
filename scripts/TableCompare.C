void TableCompare()
{
   string ltkFile  = "$VEGAS/../tables/ltk_Jan2013_ua_ATM21_7samples_H_vegas250rc5_alloffsets.root";
  string ltgFile  = "$VEGAS/../tables/lt_Oct2012_ua_ATM21_7samples_vegasv250rc5_050wobb_LZA.root";
  string ltcFile  = "$VEGAS/../tables/Care_00deg_65deg_50MHz_200MHz_UpgradedArray.root";
  
  //examineTables(ltkFile.c_str());
  examineTables(ltkFile.c_str(),"LookupTable_Width_TelID_2_Azimuth_180_Zenith_20_Noise_6.51_AbsoluteOffset_0.5", "Width");
  c1->Clear();
  c1->Divide(3,2);
  c1->cd(1);
  examineTables(ltkFile.c_str(),"LookupTable_Width_TelID_2_Azimuth_180_Zenith_20_Noise_6.51_AbsoluteOffset_0.5", "KASKADE Width");
  c1->cd(4);
  examineTables(ltkFile.c_str(),"LookupTable_Length_TelID_2_Azimuth_180_Zenith_20_Noise_6.51_AbsoluteOffset_0.5", "KASKADE Length");
  c1->cd(2);
  examineTables(ltgFile.c_str(),"LookupTable_Width_TelID_2_Azimuth_180_Zenith_20_Noise_6.68", "GrISUDet Width");
  c1->cd(5);
  examineTables(ltgFile.c_str(),"LookupTable_Length_TelID_2_Azimuth_180_Zenith_20_Noise_6.68", "GrISUDet Length");
  c1->cd(3);
  examineTables(ltcFile.c_str(),"LookupTable_Width_TelID_2_Azimuth_180_Zenith_20_Noise_6.59", "CARE Width");
  c1->cd(6);
  examineTables(ltcFile.c_str(),"LookupTable_Length_TelID_2_Azimuth_180_Zenith_20_Noise_6.59", "CARE Length");
  return;
}
