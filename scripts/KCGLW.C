void KCGWL()
{
  examineTables("$VEGAS/../tables/Care_00deg_65deg_50MHz_200MHz_UpgradedArray.root","LookupTable_Width_TelID_2_Azimuth_180_Zenith_20_Noise_6.59");
  c1->Clear();
  c1->Divide(3,2);
  c1->cd(1);
  examineTables("$VEGAS/../tables/Care_00deg_65deg_50MHz_200MHz_UpgradedArray.root","LookupTable_Width_TelID_2_Azimuth_180_Zenith_20_Noise_6.59");
  c1->cd(2);
  examineTables("$VEGAS/../tables/lt_Oct2012_ua_ATM21_7samples_vegasv250rc4_allOffsets.root","LookupTable_Width_TelID_2_Azimuth_180_Zenith_20_Noise_6.68_AbsoluteOffset_0.5");
  c1->cd(3);
  examineTables("$VEGAS/../tables/ltk_Jan2013_ua_ATM21_7samples_H_vegas250_alloffsets.root","LookupTable_Width_TelID_2_Azimuth_180_Zenith_20_Noise_6.51_AbsoluteOffset_0.5");
  c1->cd(4);
  examineTables("$VEGAS/../tables/Care_00deg_65deg_50MHz_200MHz_UpgradedArray.root","LookupTable_Length_TelID_2_Azimuth_180_Zenith_20_Noise_6.59");
  c1->cd(5);
  examineTables("$VEGAS/../tables/lt_Oct2012_ua_ATM21_7samples_vegasv250rc4_allOffsets.root","LookupTable_Length_TelID_2_Azimuth_180_Zenith_20_Noise_6.68_AbsoluteOffset_0.5");
  c1->cd(6);
  examineTables("$VEGAS/../tables/ltk_Jan2013_ua_ATM21_7samples_H_vegas250_alloffsets.root","LookupTable_Length_TelID_2_Azimuth_180_Zenith_20_Noise_6.51_AbsoluteOffset_0.5");
}
