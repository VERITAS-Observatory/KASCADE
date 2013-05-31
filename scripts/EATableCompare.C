void EATableCompare()
{
  string EAKFile="$VEGAS/../tables/eak_Jan2013_ua_ATM21_7samples_H_vegas250rc5_alloffsets_UpgradeMediumCuts.root";
  string EAGFile="$VEGAS/../tables/ea_Oct2012_ua_ATM21_vegasv250rc5_7sam_050off_std_med-0_LZA.root";

  string EAKTable="EffectiveArea_Azimuth_180_Zenith_20_Noise_7.64_AbsoluteOffset_0.5";
  string EAGTable="EffectiveArea_Azimuth_180_Zenith_20_Noise_7.82";

  drawTables(EAKFile.c_str(),EAKTable.c_str(),"pfRelativeEnergyBias","same");
  c1.Clear();
  c1.Divide(2,1);
  c1.cd(1);
  drawTables(EAKFile.c_str(),EAKTable.c_str(),"pfRelativeEnergyBias","same",1,"KASCADE Upgrade Medium Cuts: 20_180deg");
  c1.cd(2);
  drawTables(EAGFile.c_str(),EAGTable.c_str(),"pfRelativeEnergyBias","same",1,"GrISUDet Upgrade Medium Cuts: 20_180deg");
}

