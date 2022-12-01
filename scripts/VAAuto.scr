#!/bin/bash
# This script is used to automate the generation and processing of the veritas 
# KASCADE vbf files in 1:Noise levals, LT and EA tables.
# The script has the followinf argument
#$1 Command. Do "VAAuto.scr ListCommands" for a list!
#$2 ksProduction season spec: Ex: W or S
#$3 Array designation: W or OA or NA  or UA (Whipple, OldArray,NewArray, 
#   UpgradeArray) 
#   Note the UA.NA.OA extention also appears in argument $5
#$4 ksProduction Type spec: Ex: G or  P or He4_ or CR or E. Also CR may have 
#                simset specification CR1-25 (and others like it)
#$5 MDL designation for VBF files: Ex MDL12UA or MDL15NA
#$6 ZN values comma seperated list: Ex 1,10,20,30,40,50,60,70 or "All
#$7 AZ values comma seperated list: Ex 0,45,90,135,180,225,270,315 or "All"
#$8 wbl (offset) values comma seperated list:Ex: 0.0,0.25,0.5,0.75,1.0  or "All" 
#            (CR ignores wbl, except in eSpec stage5 processing where it is the 
#            aperture radius (1 value only)) 
#$9 Threshold: Ex 45mv or 50mv
#${10} PedVar comma seperated list: 5.18, 5.55 6.51 or "All" or "Base"
#${11} Specifes Type of LT or DT table to use or make for LT,DT (Ex All or 050)
#      for S4 and EA gen add particle type src of LT Ex. AllG or AllE or 050G 
#      or 050E. 
#      O50 and All default to Gamma type LT. (eSpec uses AllE)
#${12} Root File name extention (ie.  Cuts type):  Ex. UpgradeMedium or 
#      NewArrayHard. 
#  Default for LTCutsFile is LookUpTableStdCuts
#  For Quality cuts (Stage4), file would be $VERITASAPPS/tables/Quality${12}Cuts
#  For Stage4, filename would extention be:  ${12}Cuts ending
#  For Stage5, ZAlpha: {12}  would be something like: UpgradeMediumElectron or
#      UpgradeMediumHadron or UpgradeMediumRecon
#      Then ShowerCuts, file would be $VERITASAPPS/tables/Shower7Sample${12}Cuts 
#
#${13} (Optional)  Single telescope to cut for Stage4 Gen and EA gen.Ex: T1 or 
#                  T2 or T3 or T4
# ************************************************************************

#FindNoiseLevels=enabled
LimitSubmissions=enabled # Enable if you want to submit more that 9 HTAR
MaxQsubsDefault=450      # jobs but only want 9 at a time to run (limit for 
                         # HTAR) Or you want to generate S2 or S4 files and 
                         # only make 450 submission in queue at one time

JobArray=enabled         #For cori jobs arrays
###############################################################################

function commandEnable()
{
  if [ "$1" = "ListCommands" ]; then
    usage
    echo '***VAAuto Valid Commands:'
    echo '    #------------------------        #------------------------'
    echo '    #  Stage 2 PedVar files          # Lookup Tables'
    echo '    #  Uses arguments $1-${10}       # Uses Argument $1-${11}'
    echo '    #------------------------        #------------------------'
    echo '    GetVBFFileFromArchive            GenerateSubLTListFiles'
    echo '    GenerateSimLaserFile             GenerateSubLTConfigFiles'
    echo '    GenerateStage2FromVBF            ProduceSubLookupTables'  
    echo '    CheckStage2Files                 CombineSubLT'     
    echo '    HTARPedVarZnOffsetS2ToArchive    BuildLTTree'
    echo '    HTARPedVarZnOffsetS2FromArchive  CheckLT'
    echo
    echo '    #------------------------        #------------------------'
    echo '    # Disp Lookup Tables             # Stage4'
    echo '    # Uses arguments $1-${11}        # Uses Arguments $1-${12};${13}Optional'
    echo '    #------------------------        #------------------------'
    echo '    GenerateDispSubLTListFiles       GenerateStage4FromStage2'
    echo '    GenerateDispSubLTConfigFiles     CheckStage4Files'
    echo '    ProduceDispSubLookupTables       HTARPedVarZnOffsetS4ToArchive'
    echo '    CombineDispSubLT                 HTARPedVarZnOffsetS4FromArchive'
    echo '    BuildDispLTTree'
    echo '    CheckDispLT'
    echo
    echo '    #------------------------           #------------------------'
    echo '    # EA Tables                         # Stage5'
    echo '    # Uses Argument $1-${12}            # Uses Arguments $1-${12}'
    echo '    #------------------------           #------------------------'
    echo '    GenerateEASubLTListAndConfigFiles  GenerateStage5Combined' 
    echo '    ProduceEASubLookupTables           GenerateStage5CombinedRCE'
    echo '    CombineBuildCheckEALT              (RCE is RemoveCutEvents)'
    echo '                                       CheckStage5Files'
    exit
  else
    if [ "$1" = "GetVBFFileFromArchive" ]; then eval $1=enable; return 0; fi;
    if [ "$1" = "GenerateSimLaserFile" ]; then eval $1=enable; return 0; fi;
    if [ "$1" = "GenerateStage2FromVBF" ]; then eval $1=enable; return 0; fi;  
    if [ "$1" = "CheckStage2Files" ]; then eval $1=enable; return 0; fi;     
    if [ "$1" = "HTARPedVarZnOffsetS2ToArchive" ]; then eval $1=enable; 
        return 0; fi;
    if [ "$1" = "HTARPedVarZnOffsetS2FromArchive" ]; then eval $1=enable; 
        return 0; fi;

    if [ "$1" = "GenerateSubLTListFiles" ]; then eval $1=enable; return 0; fi;         
    if [ "$1" = "GenerateSubLTConfigFiles" ]; then eval $1=enable; return 0; fi;       
    if [ "$1" = "ProduceSubLookupTables" ]; then eval $1=enable; return 0; fi;         
    if [ "$1" = "CombineSubLT" ]; then eval $1=enable; return 0; fi;
    if [ "$1" = "BuildLTTree" ]; then eval $1=enable; return 0; fi;
    if [ "$1" = "CheckLT" ]; then eval $1=enable; return 0; fi;

    if [ "$1" = "GenerateDispSubLTListFiles" ]; then eval $1=enable; 
        return 0; fi;
    if [ "$1" = "GenerateDispSubLTConfigFiles" ]; then eval $1=enable; 
        return 0; fi;
    if [ "$1" = "ProduceDispSubLookupTables" ]; then eval $1=enable;return 0;fi;
    if [ "$1" = "CombineDispSubLT" ]; then eval $1=enable; return 0; fi;
    if [ "$1" = "BuildDispLTTree" ]; then eval $1=enable; return 0; fi;
    if [ "$1" = "CheckDispLT" ]; then eval $1=enable; return 0; fi;

    if [ "$1" = "GenerateStage4FromStage2" ]; then eval $1=enable; return 0; fi; 
    if [ "$1" = "CheckStage4Files" ]; then eval $1=enable; return 0; fi;
    if [ "$1" = "HTARPedVarZnOffsetS4ToArchive" ]; then eval $1=enable; 
        return 0; fi;  
    if [ "$1" = "HTARPedVarZnOffsetS4FromArchive" ]; then eval $1=enable; 
        return 0; fi;

    if [ "$1" = "GenerateEASubLTListAndConfigFiles" ]; then eval $1=enable; 
        return 0; fi;
    if [ "$1" = "ProduceEASubLookupTables" ]; then eval $1=enable; return 0; fi;
    if [ "$1" = "CombineBuildCheckEALT" ]; then eval $1=enable; return 0; fi;

    if [ "$1" = "GenerateStage5Combined" ]; then eval $1=enable; return 0; fi;
    if [ "$1" = "GenerateStage5CombinedRCE" ]; then eval $1=enable; return 0;fi;
    if [ "$1" = "CheckStage5Files" ]; then eval $1=enable; return 0; fi;
    echo ' VAAuto: Invalid Command: '$1
    echo ' VAAuto: Do "./VAAuto ListCommands" for a list of valid commands.'
    exit
  fi
}
################################################################################

function usage()
{
  echo '***VAAuto.scr usage:'
  echo ' $1: Command. Do "VAAuto.scr ListCommands" for a list!'
  echo ' $2: Season specicifaction Winter is ATM21 Summer is ATM22: Ex: W or S'
  echo ' $3: Array Cofig: Whipple,OldArray,NewArray,UpgradeArray:   Ex: W or OA or'\
       'NA or UA'
  echo ' $4: Particle Type spec: Ex: G or P or He4_ or E or CR (which may have'\
       'simset specification ie. CR1-25)'
  echo ' $5: Kascade Detector Model for VBF files: Ex MDL12UA or MDL15NA or MDL8OA'
  echo ' $6: List of ZN values:   Ex: 1,10,20,30,40,50,60,70 or "All"'
  echo ' $7: List of AZ values:   Ex: 0,45,90,135,180,225,270,315 or "All"'
  echo ' $8: List of Wbl values:  Ex: 0.0,0.25,0.5,0.75,1.0  or "All" (CR ignores'\
       'wbl or for eSpec Stage5 Wbl[0] = aperture).'
  echo ' $9: Threshold: Ex 45mv (for UA) or 50mv (for OA and NA)'
  echo ' ${10}: List of PedVar values: 5.18, 5.55 6.51 or "All" or "Base"'
  echo ' ${11}: Specifes Type of LT table to use. Ex. All or 050 or ALLE or'\
       'AllG(same as All)'  
  echo ' ${12}: Cuts Ex: UpgradeLoose or UpgradeMedium or NewArrayHard or OldArraySoft'
  echo '        For zAlpha: UpgradeMediumElectron or UpgradeMediumHadron or '\
       'UpgradeMediumRecon'
  echo ' ${13}: (Optional) Tel to cut for Stage4 and EA gen. Ex: T1 or T2 or T3 or T4'
  echo
  echo ' EX: ./VAAuto.scr GenerateStage4FromStage2 W UA E MDL12UA 1,10,20 45,90,135'\
       '0.5 45mv All AllE UpgradeSoft'
  echo ' EX: ./VAAuto.scr GenerateStage2FromVBF W UA CR1-25 MDL12UA 1,10,20'\
       '45,90,135 0.5 45mv All'
  echo
}
#############################################################################

##############################################################################

function GetOptionValue()
#$1 Cuts file name
#$2 Option whose value we need to find
{
  OptionString=$2
  #echo 'Cuts File Name: '$1' Opt: '$OptionString
  {
    while read Opt OptionValue;
     do
      if  [ "$OptionString" = "$Opt" ]; then
         return
      fi
     done
   }<$1
}
##############################################################################      
function GenerateEAFileName()
#$1 Cuts Ex: UpgradeSoft  or OldArrayMedium
#$2 Season EX: W (Winter)  or S (Summer)
#$3 Offset id: Ex; All or 050
#$4 Method: Ex: std or hfit or disp
#$5 TelConfig: Ex: 1234(ignored) 123X or X234 or 1X34 or 1X3X or etc.
#$6 Particle type: Blank if gamma otherwise: Ex: Electrons or CosmicRays
{
  Cuts=$1
  SimModel=Jan2013
  a=${Cuts:0:1}
  if [ "$a" = "U" ]; then
     Epoch=V6_PMTUpgrade
     SimModel=MDL12UA
     GPSYear=2014
  fi
  if [ "$a" = "N" ]; then
     Epoch=V5_T1Move
     SimModel=MDL15NA
     GPSYear=2010
  fi
  if [ "$a" = "O" ]; then
     Epoch=V4_OldArray
     SimModel=MDL8OA
     GPSYear=2006
  fi

  if [ "$2" = "W" ]; then
    SeasonID='21'
  fi
  if [ "$2" = "S" ]; then
    SeasonID='22'
  fi
  echo SeasonID: $SeasonID 'for ' $2
  offsetID=$3

  #############################
  # Make up quality factor file name
  #############################
  tbl=$VERITASAPPS'/tables/'
  GetOptionValue $tbl'Quality'$Cuts'Cuts'  SizeLower  #sets OptionValue
  sizeCut=${OptionValue:2}                       #Drop "0/"

  telescopeMultiplicity=t2                #Multiplicity (2 tels trigger, OA ?)
  method=$4
  
  GetOptionValue  $tbl'Shower7Sample'$Cuts'Cuts'  MeanScaledWidthUpper  #sets OptionValue
  
  MSWUpperCut=$OptionValue            #Tricky way to replace a "." with a "p"

  GetOptionValue  $tbl'Shower7Sample'$Cuts'Cuts'  MeanScaledLengthUpper  #sets OptionValue
  MSLUpperCut=$OptionValue
  
  GetOptionValue  $tbl'Shower7Sample'$Cuts'Cuts'  MaxHeightLower  #sets OptionValue
  if [ "$OptionValue" != "-100" ]; then           #check if not used
      MaxHeightLowerCut=$OptionValue     #will remain undefined if not used
  fi

  GetOptionValue  $tbl'Shower7Sample'$Cuts'Cuts'  ThetaSquareUpper
  ThetaSquareUpperCut=$OptionValue

  TelConfiguration=$5
  if [ "$5" != "1234" ]; then
     TELCONFIGURATION=$5
  fi   

  ###################################################
  # Ready to build file name (from OAWG 2.5 wiki page)
  ###################################################
# ea_[SimModel]_[Epoch]_ATM[SeasonID]_[SimulationSource]_[PrimaryType]_
# vegasv[VEGASMODULEVERSION]_7sam_[offsetID]off_s[sizeCut]t[telescopeMultiplicity]_
# [method]_MSW[MSW upper cut]_MSL[MSL upper cut]_MH[Max height lower cut]_
# ThetaSq[Theta square upper cut]_LZA.root

# ea_[array]_ATM[SeasonID]_[SimulationTool]_[SimulationToolVersion]_
# [DetectorModelID]_[Primary]_vegasv[VEGASMODULEVERSION]_7sam_[offsetID]off_
# s[sizeCut][telescopeCut]_[method]_MSW[MSW upper cut]_MSL[MSL upper cut]_
# MH[Max height lower cut]_ThetaSq[Theta square upper cut]_LZA.root


  EAFILENAME='ea_'$SimModel'_'$Epoch'_ATM'$SeasonID'_KASCADE_'
  if  [ -n "$6" ]; then                             #PrimaryType
     EAFILENAME=$EAFILENAME$6'_'
  fi

  EAFILENAME=$EAFILENAME'vegasv'$VEGASMODULEVERSION'_7sam_'$offsetID'off_'
  EAFILENAME=$EAFILENAME's'$sizeCut$telescopeMultiplicity'_'$method'_MSW'
  EAFILENAME=$EAFILENAME$MSWUpperCut'_MSL'$MSLUpperCut'_'

  if [ -n "$MaxHeightLowerCut" ]; then
    EAFILENAME=$EAFILENAME'MH'$MaxHeightLowerCut'_'
  fi
 
  EAFILENAME=$EAFILENAME'ThetaSq'$ThetaSquareUpperCut'_'

  if [ -n "$TELCONFIGURATION" ]; then
    EAFILENAME=$EAFILENAME'T'$TELCONFIGURATION'_'
  fi
  EAFILENAME=$EAFILENAME'LZA.root'
  return
}
##################################################################################
function GenerateLTFileName()
#$1 Array  EX: UA or NA or OA
#$2 Season EX: W (Winter)  or S (Summer)
#$3 Offset id: Ex; All or 050
#$4 Method: Ex: std or hfit or disp
#$5 Particle type: Blank if gamma otherwise: Ex: Electrons or CosmicRays
{
  a=$1
  if [ "$a" = "UA" ]; then
     Epoch=V6_PMTUpgrade
     SimModel=MDL12UA
  fi
  if [ "$a" = "NA" ]; then
     Epoch=V5_T1Move
         SimModel=MDL15NA
  fi
  if [ "$a" = "OA" ]; then
     Epoch=V4_OldArray
     SimModel=MDL8OA
  fi

  if [ "$2" = "W" ]; then
    SeasonID='21'
  fi
  if [ "$2" = "S" ]; then
    SeasonID='22'
  fi
  echo SeasonID: $SeasonID 'for ' $2
  offsetID=$3
  echo offsetID: $offsetID


  #############################
  # LookuptableCuts file name
  #############################
  tbl=$VERITASAPPS'/tables/'
  GetOptionValue $tbl'LookupTableStdCuts'  DistanceUpper   #sets OptionValue
  distCut=${OptionValue:2}                       #Drop "0/"
  #leave in decimal points.   #distCut=${distCut/./p}#Convert "." to "p". Not sure why

  method=$4
  

  ###################################################
  # Ready to build file names (From OAWG 2.5 wiki page)
  ###################################################


  # lt_[SimModel]_[Epoch]_ATM[SeasonID]_[SimulationSource]_[PrimaryType]_
  # vegasv[VEGASMODULEVERSION]_7sam_[offsetID]off_[method]_d[distCut]_LZA.root

  LTFILENAME='lt_'$SimModel'_'$Epoch'_ATM'$SeasonID'_KASCADE_'
  if  [ -n "$5" ]; then                                        #PrimaryType
     LTFILENAME=$LTFILENAME$5'_'
  fi

  LTFILENAME=$LTFILENAME'vegasv'$VEGASMODULEVERSION'_7sam_'$offsetID'off_'$method'_d'$distCut'_'
  #eSpec Doesn't use LZA
  #LTFILENAME=$LTFILENAME'LZA.root'
  LTFILENAME=$LTFILENAME'.root'
  echo LTFILENAME: $LTFILENAME
  return
}
##################################################################################

function CheckQsubSubmissions()
#$1  Max HTAR Qsubs active at one time
#$2  Running qsub List File name
{
  #############################################################
  # File RunningHtarQsubLogs.txt is a list of the names of the qsub.log files that
  # will be generated when the various running HTAR jobs complete.
  # Until then the jobs complete they don't exist.  We count the not existing files. 
  # If less than $1 files dont exist  whioch means the jobs are still active, we 
  # sleep for 60 seconds and try again. When a job is found to exist it is 
  # removed from the List file and this function returns so a new submission can be 
  # made. Other wise the code just sits here.
  ################################################################
  RunningQsubListFile=$2   
  RunningQsubListTmp=$2'.tmp'
  if [ ! -e "$RunningQsubListFile" ]; then  #empty (just starting up)
     #echo no $RunningQsubListFile
     return
  fi
  if [ -e "$RunningQsubListTmp" ]; then
      rm $RunningQsubListTmp
  fi

  let count=$1
  while test $count -ge  $1 
   do
    let count=0
    {
      while  read QsublogFile;  
       do
         if [ !  -e "$QsublogFile" ]; then
           let count=count+1
           echo $QsublogFile >>$RunningQsubListTmp
         fi
       done
    } < $RunningQsubListFile

    if [ -e "$RunningQsubListTmp" ]; then
        cp $RunningQsubListTmp  $RunningQsubListFile
        rm $RunningQsubListTmp
    else
        rm  $RunningQsubListFile
    fi

    if [ $count -ge $1 ]; then             #We are full, wait a bit and try again
        sleep 60
    fi
   done
}
####################################################################

function GetPedVars()
{
  RFile2=$1
  PVFile=$2
  #################################
  # We now need to run root in batch mode and get list of pedvars
  # and the max one
  #################################
  echo "{"                                                    >VAAuto.C
  echo 'VARootIO io("'$RFile'", true);'                      >>VAAuto.C
  echo 'io.loadTheRootFile();'                               >>VAAuto.C
  echo 'VAQStatsData *q = io.loadTheQStatsData();'           >>VAAuto.C
  echo 'std::ofstream ofs("PedVarBaseRatios.dat");'          >>VAAuto.C
  echo 'std::vector< double > ped;'                          >>VAAuto.C
  echo 'ped.Resize(4,0.0);'                                  >>VAAuto.C
  echo 'ped.at(0)=q->getCameraAverageTraceVarTimeIndpt(0,7)' >>VAAuto.C
  echo 'ped.at(1)=q->getCameraAverageTraceVarTimeIndpt(1,7)' >>VAAuto.C
  echo 'ped.at(2)=q->getCameraAverageTraceVarTimeIndpt(2,7)' >>VAAuto.C
  echo 'ped.at(3)=q->getCameraAverageTraceVarTimeIndpt(3,7)' >>VAAuto.C
  echo 'double max= *max_element(ped.begin(),ped.end());'    >>VAAuto.C
  echo 'ofs<<ped.at(0)<<" "<<ped.at(1)<<" ";'                >>VAAuto.C
  echo '   <<ped.at(2)<<" "<<ped.at(3)<<" ";'                >>VAAuto.C
  echo '   <<max<<std::endl;'                                >>VAAuto.C
  echo 'io.closeTheRootFile();'                              >>VAAuto.C
  echo '}'                                                   >>VAAuto.C

 # ~/Switch.rootrcTo.rootrc_glenn.scr
  root -q -b VAAuto.C  >VAAuto.C.log
}
######################################################################
function LoadPVOpt()
{
  BaseRat=$1
  PVBase=$2
  PVTar=$3
  NewRatioFile=$4
  NewOptFile=$5
  ############################################
  #We have to do some foalingpoint arithmatic and some semi-fancy
  #formatting, so lets do it in root
  ############################################
#  echo "{"                                                      >VAAuto.C
#  echo '  std::string line;'                                   >>VAAuto.C
#  echo '  std::ifstream iBR(\"'$BaseRat'\");'                  >>VAAuto.C
#  echo '  std::getline(iBR,line);'                             >>VAAuto.C
#  echo '  std::istringstream issBR(line);'                     >>VAAuto.C
#  echo '  std::vector< double > oldRat;'                       >>VAAuto.C
#  echo '  double Value;'                                       >>VAAuto.C
#  echo '  for(int i=0;i<4;i++){'                                >>VAAuto.C
#  echo '    issBR>>Value'                                      >>VAAuto.C
#  echo '    oldRat.push_back(Value);'                          >>VAAuto.C
#  echo '  }'                                                   >>VAAuto.C
#  echo '  std::ifstream iPVB(\"'$PVBase'\");'                  >>VAAuto.C
#  echo '  std::getline(iPVB,line);'                            >>VAAuto.C
#  echo '  std::istringstream issPVB(line);'                    >>VAAuto.C
#  echo '  std::vector< double > PVB;'                          >>VAAuto.C
#  echo '  for(int i=0;i<4;i++){'                                >>VAAuto.C
  #echo '    issPVB>>Value'                                     >>VAAuto.C
  #echo '    PVB.push_back(Value);'                             >>VAAuto.C
  #echo '  }'                                                   >>VAAuto.C
  #echo '  std::ofstream oRF(\"'$NewRatioFile'\");'             >>VAAuto.C
  #echo '  std::ofstream oOF(\"'$NewOptFile'\");'               >>VAAuto.C
  #echo '  oOF<<\"IncreasePedVarOption='                        >>VAAuto.C
  #echo '     <<\"\'-PaddingApp=PaddingCustom '                 >>VAAuto.C
  #echo '     << -P_MultiPedvarScaling='                        >>VAAuto.C
  #echo '  for(int i=0;i<4;i++){'                               >>VAAuto.C
  #echo '    Value=oldRat.at(i)*'$PVTar'/PVB.at(i);'            >>VAAuto.C
  #echo '    oOF<<i+1<<\"/\"<<Value;'                           >>VAAuto.C
  #echo '    oRF<<Value<<\" \";'                                >>VAAuto.C
  #echo '  }'                                                   >>VAAuto.C
  #echo '  oOF<<'\"\'\"<<std::endl;                             >>VAAuto.C 
  #echo '  oOF<<\"PedVarBase=\'PedVar'$PVTar'\"<<std::endl;'    >>VAAuto.C 
  #echo '  oRF<<std::endl;'                                     >>VAAuto.C
  #echo '}'                                                     >>VAAuto.C

  #~/Switch.rootrcTo.rootrc_glenn.scr
  root -q -b VAAuto.C  >VAAuto.C.log
}
###########################################################################
#########################################################################

function SubmitHtarToArchive()
{
  #$1 HTAR File Name (Includes Path)
  #$2 Disk Base Directory that has files we want to archive
  #$3 Filespec (with wildcards) of files we want to archive
  Destination=$1    #Archive htar file
  SourceDir=$2         #Disk Base Directory
  FileSpec=$3          #Files to archive
  

  echo Destination: $Destination
  echo SourceDir: $SourceDir
  echo FileSpec: "$FileSpec"
  HTARFileName=${Destination##*/}
  HTARFileName=${HTARFileName%%.tar}

  lcl=$PWD
  if [ -n "$LimitSubmissions" ]; then
    HTARDoneLog=$HTARFileName'HTARDone.log'     
    if [ -e "$HTARDoneLog" ]; then              #MaxHTARQsubs jobs are active 
         rm $HTARDoneLog
    fi
  fi


  #build a submission .pbs file
  sgeFile=$lcl'/'$HTARFileName'To.pbs'
  echo "#"PBS -q $QUEUE                                            >$sgeFile
  echo "#"PBS $WALLTIME                                           >>$sgeFile
  if [ -n "$PURDUE" ]; then  
    echo source /etc/profile                                      >>$sgeFile
    echo module load gcc/5.2.0                                    >>$sgeFile
  fi

  echo cd $SourceDir                                              >>$sgeFile
  echo htar -cvf  $Destination'  '"$FileSpec" \\                  >>$sgeFile
  echo '>'$lcl'/'$HTARFileName'.log'                              >>$sgeFile
  if [ -n "$LimitSubmissions" ]; then
     echo 'echo Done >'$lcl'/'$HTARDoneLog                        >>$sgeFile
  fi
  chmod 700 $sgeFile

  if [ -n "$LimitSubmissions" ]; then
     CheckQsubSubmissions  $MaxHTARQsubs  $QsubLogs     #This will wait 
     echo $HTARDoneLog >>$QsubLogs                      #add the next one
  fi

  HTARpbsLog=$HTARFileName'HTARTo.pbs.log'
  HTARLOG=$lcl'/VAHTARTo.log'
  $QSUB$QSUBEXT -e $HTARFileName'To.pbs.err' -o HTARpbsLog $sgeFile >$HTARLOG

  #####################################################################
  # We are now going to wait for the htar to finish.
  #####################################################################

  if [ -n "$BELL" ]; then
   echo "VAAuto: htar to archive "$HTARFileName" job submission to Bell "$QUEUE" queue complete."
  fi

  if [ -n "$CORI" ]; then
   echo "VAAuto: htar to archive "$HTARFileName" job submission to Cori cluster complete."
  fi

  date
  cd $lcl
}
#########################################################################
function SubmitHtarFromArchive()
{
  #$1 Archive directory wqhere our tar file exists
  #$2 Destination directory where we want to unTar into.
  #$3 name of the Tar file we want to untar(without .tar extention).

  local=$PWD

  ArchiveDir=$1          #Archive direxctory
  DestinationDir=$2      #Base Disk directory
  Source=$3              #Base Tar file name
  SourceTarFile=$3'.tar'

  echo 'Src: '$ArchiveDir'/'$SourceTarFile 
  if [ -n "$LimitSubmissions" ]; then
    HTARDoneLog=$Source'HTARDone.log'     
    if [ -e "$HTARDoneLog" ]; then              #MaxHTARQsubs jobs are active 
         rm $HTARDoneLog
    fi
  fi

  #build a submission .pbs file
  sgeFile=$lcl'/'$Source'From.pbs'
  echo "#"PBS -q $QUEUE                               >$sgeFile
  echo "#"PBS $WALLTIME                              >>$sgeFile
  if [ -n "$PURDUE" ]; then
    echo source /etc/profile                         >>$sgeFile
    echo module load gcc/5.2.0                       >>$sgeFile
  fi

  echo cd $DestinationDir                            >>$sgeFile
  echo htar -xvf  $ArchiveDir'/'$SourceTarFile    \\ >>$sgeFile
  echo '>'$local'/'$Source'.log'     >>$sgeFile
  if [ -n "$LimitSubmissions" ]; then
     echo 'echo Done >'$lcl'/'$HTARDoneLog           >>$sgeFile
  fi

  if [ -n "$LimitSubmissions" ]; then
     CheckQsubSubmissions  $MaxHTARQsubs $QsubLogs  #This will wait 
     echo $HTARDoneLog >>$QsubLogs #add the next one
  fi

  chmod 700 $sgeFile
  HTARpbsLog=$Source'HTARFrom.pbs.log'
  HTARLOG=$local'/KSHTAR'$ZnAz'From.log'
  $QSUB$QSUBEXT -e $Source'HTARFrom.pbs.err' -o $HTARpbsLog $sgeFile >$HTARLOG

  # And now cleanup
  cd $lcl
}
##########################################################################

function SubmitMultipleSerialJobs()
#########################################################################
#$1 Name of JobList file
#$2 MSJ id.
########################################################################
{
  local MSJList=$1
  local MSJId=${MSJList##*List}
  ###########################
  #  Finish off the Job command List file
  ###########################
  echo 'wait' >> $MSJList

  ##############################
  # Build the PBS file
  ##############################
  local sgeFile=$lcl'/MSJ'$MSJId'.pbs'
  echo "#"PBS -l mppwidth=$NUMCORESNODE            >$sgeFile
  echo "#"PBS $WALLTIME                           >>$sgeFile
  if [ -n "$PURDUE" ]; then
    echo source /etc/profile                      >>$sgeFile
    echo module load gcc/5.2.0                    >>$sgeFile
  fi

  echo cd $lcl                                    >>$sgeFile
  echo ccmrun $MSJobList                          >>$sgeFile
  chmod 700 $sgeFile
  $QSUB$QSUBEXT -q $QUEUE -V -e MSJ$$MSJId'pbs.err' -o MSJ$$MSJId'pbs..log' $sgeFile 
}  
#####################################################################

function SubmitVegasSimProduction()
#######################################################################
#$1 PedVar command file. Ex: PedUA4.04  or PedNA10.27 or  PedOA7.53
#$2 FileBase: VBF Base name of the file to be processed
#$3 Vegas script file to run. Ex:  VegasSimProductionS4.269295369.scr
#$4 VBF file name 
#######################################################################
# Submits VegasSimProduction jobs to batch queue.
#######################################################################
# This keeps track of how many jobs are submitted and limits them.
# This routine is called once for each serial job to be submitted.
#######################################################################
{
  local FilePedVar=$1
  local FileBase=$2
  local VEGASScript=$3
  local FileVBF=$4

  ##########################################
  #  Make sure we have the PedVar command file
  ##########################################
  if [ "$FilePedVar" = "NONE" ]; then
      FilePedVar=""
  else
    if [ ! -e "$lcl"/"$FilePedVar" ]; then
      cp $KASCADEBASE/inputs/$FilePedVar $lcl/
      if [ ! -e "$lcl"/"$FilePedVar" ]; then
        echo 'VAAUTO: SubmitVegasSimProduction requires file' $FilePedVar
        echo 'VAAuto: FFatal!--Attemp to copy ' $KASCADEBASE'/inputs/'$FilePedVar' to '$lcl'/ Failed!'
        exit
      fi 
    fi
  fi

  ################################################
  # Set up unique idientifier for this job
  ################################################
  GetUniqueNumber
  local IDUNIQUE=$UNIQUE

  ############################################
  # Generate name of file that will hold this jobs "Done" flag
  # Unique to this job.
  ############################################
  if [ -n "$LimitSubmissions" ]; then
    local VEGASDoneLog=$FilePedVar'_'$FileBase$IDUNIQUE'Done.log'     
    if [ -e "$VEGASDoneLog" ]; then              #Max queue jobs are active 
         rm $VEGASDoneLog
    fi
  fi
  
  ##############################################
  # Generate the veagsSimProduction command and wait until we have room to 
  # submit another job
  ##############################################
  local VSPCommand=$lcl'/'$VEGASScript' '$FileVBF' '$FilePedVar' >'$lcl'/'$FilePedVar'_'$FileBase$IDUNIQUE'VBF.log'

  if [ -n "$LimitSubmissions" ]; then
     CheckQsubSubmissions  $MaxQsubs $QsubLogs      #This will wait 
     echo $VEGASDoneLog >>$QsubLogs                 #add the next one
  fi


   #############################################
   # Generate pbs file for this job. (Normal method)  
   #############################################
   local sgeFile=$lcl'/'$FilePedVar$FileBase$IDUNIQUE'.pbs'



   echo "#"PBS $WALLTIME                  >$sgeFile
   echo "#PBS "$MEMREQUEST                        >>$sgeFile
   if [ -n "$PURDUE" ]; then
     echo source /etc/profile                        >>$sgeFile
     echo module load gcc/5.2.0                      >>$sgeFile
   fi
   echo cd $lcl                                      >>$sgeFile
   echo $VSPCommand                                  >>$sgeFile

   if [ -n "$LimitSubmissions" ]; then
      echo 'echo Done >'$lcl'/'$VEGASDoneLog         >>$sgeFile
   fi

   chmod 700 $sgeFile
   $QSUB$QSUBEXT -q $QUEUE  -V -e $FilePedVar$FileBase$IDUNIQUE'.err' -o $FilePedVar$FileBase$IDUNIQUE'pbs.log' $sgeFile 
}
##############################################################################

function BuildVegasSimProduction()
#######################################################################
#$1 PedVar command file. Ex: PedUA4.04  or PedNA10.27 or  PedOA7.53
#$2 FileBase: VBF Base name of the file to be processed
#$3 Vegas script file to run. Ex:  VegasSimProductionS4.269295369.scr
#$4 VBF file name 
#######################################################################
# Builds a  VegasSimProduction .pbs job file (but does not submit it. Puts
# it in a qsubFileNameList instead for later submission(usually as a job 
# ray).
#######################################################################
# No track is kept on the number of jobs in the list. This is ok on CORI
# where the limit for the shared queue is 10,000
# This routine is called once for each serial job to be submitted.
# This is a sligtly modified version of the original SubmitVegasSimProduction
# function
#######################################################################
{
  local FilePedVar=$1
  local FileBase=$2
  local VEGASScript=$3
  local FileVBF=$4

  #echo BuilsVegasSimProduction Args: $1 $2 $3 $4

  ##########################################
  #  Make sure we have the PedVar command file
  ##########################################
  if [ "$FilePedVar" = "NONE" ]; then
      FilePedVar=""
  else
    if [ ! -e "$lcl"/"$FilePedVar" ]; then
      cp $KASCADEBASE/inputs/$FilePedVar $lcl/
      if [ ! -e "$lcl"/"$FilePedVar" ]; then
        echo 'VAAuto--BuildVegasSimProduction requires file' $FilePedVar
        echo 'VAAuto--Fatal!--The attempt to copy ' $KASCADEBASE'/inputs/'$FilePedVar' to '$lcl'/ Failed!'
        exit
      fi 
    fi
  fi

  ################################################
  # Make sure the VegasSimProduciton file exists
  ################################################
  if [ ! -e "$lcl"/"$VEGASScript" ]; then
        echo 'VAAuto--BuildVegasSimProduction requires file' $VEGASScript
        exit
  fi      

  ################################################
  # Set up unique idientifier for this job
  ################################################
  GetUniqueNumber
  local IDUNIQUE=$UNIQUE

  #############################################
  # Check to see if we are on Cori. If so make the .pbs job, then add this 
  # job  to the qsub list file.
  #############################################
  if [ -n "$CORI" ] && [ -n "$JobArray" ]; then
    ##############################################
    # Generate the veagsSimProduction command 
    ##############################################
    local VSPCommand=$lcl'/'$VEGASScript' '$FileVBF' '$FilePedVar' >'$lcl'/'$FilePedVar'_'$FileBase$OUTEXT'.'$IDUNIQUE'.log'
    sgeFile=$lcl'/VA'$FilePedVar'_'$FileBase$IDUNIQUE'.pbs'
    SgeFilePBSCmds   $sgeFile
    SgeFileBashCmds  $sgeFile

    echo $VSPCommand                             >>$sgeFile

    SgeFileCompleteAndSave $sgeFile
  else
    echo 'VAAuto--Fatal:BuildVegasSimProduction: Not setup for this cluster'
    exit
  fi
}
##############################################################################

function BuildProduceLTOrEALookupTables()
#$1 SCRIPTNAME: 'produce_lookuptables' or 'makeEA'
#$2 SUBLTBASE: Base name for input and output sublt, root and log files
#$3 ConfigName: Configuration file name
#$4 LTCUTSFILE: Quality cuts file name(LT) or ShowerCuts (EA)
#$5 ListNames: File with list of input Stage2 files
#$6 SubLTFileName: Output SubLT file name
################################################################################
# Make a .pbs comand file fot this SubLt or SubEAlt file generation. DO NOT 
# submit but do add its name to the QsubFileNameList. Later we willsubmit 
# this list as a job array (after a little screwing around and cleanup)
################################################################################
{
  local SCRIPTNAME=$1
  local BASESUBLT=$2
  local CNFGNAME=$3
  local QLTYNAME=$4
  local LISTNAME=$5
  local SUBLTNAME=$6
  
  ################################################
  # Set up unique idientifier for this job
  ################################################
  GetUniqueNumber
  local IDUNIQUE=$UNIQUE

  #############################################
  # Check to see if we are on Cori. If so make the .pbs job, then add this 
  # job  to the qsub list file.
  #############################################
  if [ -n "$CORI" ] && [ -n "$JobArray" ]; then
    sgeFile=$lcl'/PRLT'$IDUNIQUE$BASESUBLT'.pbs'
    SgeFilePBSCmds   $sgeFile                       #In UtilityFunctions
    SgeFileBashCmds  $sgeFile                       #In UtilityFunctions
            
    echo /usr/bin/time  \\                      >>$sgeFile
    echo $VEGAS/bin/$SCRIPTNAME \\     >>$sgeFile
    echo -config $lcl'/'$CNFGNAME \\            >>$sgeFile
    echo -cuts $QLTYNAME  \\                    >>$sgeFile
    echo  $lcl'/'$LISTNAME   \\                 >>$sgeFile
    echo  $SUBLTNAME  \\                        >>$sgeFile
    echo '>'$lcl'/'$BASESUBLT'.log'             >>$sgeFile


    #Save this file name into the QsubFileNameList
    SgeFileCompleteAndSave $sgeFile              #In UtilityFunctions
  else
     echo 'Fatal-- BuildProduceLTOrEALookupTables: Not setup for this cluster'
     exit
  fi
}
#############################################################################

function GenerateQsubNames()
{
  ####################################################################
  # See note below in GeneraterStage2FromVBF section on CORI use of Job 
  # arrays. Requires a QsubFileNameList and a QsubLogs file. Setup names 
  # here.
  # Eventual use on all clusters
  ####################################################################
  unique=$1
  QsubFileNameList='VA'$unique'AutoqsubList'
  echo QsubFileNameList: $QsubFileNameList
  if [ -e "$QsubFileNameList" ]; then
    rm $QsubFileNameList
  fi
  
  QsubLogs='VARunning'$unique'QsubLogs.txt'
  if [ -e "$QsubLogs" ]; then
    rm $QsubLogs
  fi
}
# ****************************************************************************
# ****************************************************************************




##############
#Main Program:
##############
echo 'Starting VAAuto at ' $(date)

#################################
# Make sure KASCADE has been loaded
#################################
if [ ! -n "$KASCADE" ]; then
    # Following needed for $KASCADE definition. Script files kept in 
    # $KASCADE/scripts
    echo "VAUTO:  Loading KASCADE with 'module load KASCADE'"
    #Following needed to restore correct root version and set $VEGAS definitions
    echo "VAUTO:  Loading VEGAS   with 'module load vegas'"
    module unload KASCADE
    module load KASCADE
    module unload vegas
    module load vegas
    if [ ! -n "$KASCADE" ]; then
        echo 'VAAuto: Fatal--Failed to load KASCADE'
        exit
    fi
    module li
fi


##################
# Bring in SetupForHost, GetUniqueNumber and  GenerateVBFName
#  functions
##################
if [ ! -e UtilityFunctions.scr ]; then
    cp $KASCADEBASE/scripts/UtilityFunctions.scr ./
fi
source UtilityFunctions.scr

###################
#Usage:
###################
if [ ! -n "$1" ]; then
  commandEnable "ListCommands"   #List all commands and exits 
fi

commandEnable $1   #This will enable the specified command or exit if its not a 
                   #known command.

lcl=$PWD           #Note that the local directory is assumed to be the source 
                   #and destination of input and output files

# ################################
# Get all hte argumenbtsa here (and nowhere else). Makes it easier to insert 
# new arguments if we need them.
# ################################
SPECSEA=$2
SPECCFG=$3
SPECPART=$4
SPECMDL=$5
SPECZN=$6
SPECAZ=$7
SPECWBL=$8
SPECTHR=$9
SPECPV=${10}
LTWBL=${11}      #Type of LT table
CUTTYPE=${12}
TELCUT=${13}

##########################
# Check VEGASBASE and VEGASMODULEVERSION
##########################
if [ ! -n "$VEGASBASE" ]; then
  echo VAAUTO***This version of VEGAS module file does not set VEGASBASE
  exit
fi
if [ ! -n "$VEGASMODULEVERSION" ]; then
    echo VAAuto: Fatal--This version of vegas dones not set VEGASMODULEVERSION
    exit
fi


###########################
#Winter/Summer
###########################
if [ "$SPECSEA" = "W" ]; then
    LTATM="ATM21"
fi
if [ "$SPECSEA" = "S" ]; then
    LTATM="ATM22"
fi

############################
# Array seaon
############################
if [ "$SPECCFG" = "UA" ]; then
    PMT="U"
    LTSEASON="ua"
fi
if [ "$SPECCFG" = "OA" ]; then
    PMT="O"
    LTSEASON="oa"
fi
if [ "$SPECCFG" = "NA" ]; then
    PMT="N"
    LTSEASON="na"
fi
if [ "$SPECCFG" = "W" ]; then
    PMT=$SPECCFG
fi

SetupForHost        #From UtilityFumctions.

if [ -n "$CORI" ]; then
  JobIDHost='cori'
  ARCHIVE=/home/u/u15013/$SPECMDL'/'
  VBFDir=$ARCHIVE$SPECSEA$PMT$SPECPART$SPECMDL'VBF'     
  MaxQsubsDefault=9000
fi

if [ -n "$BELL" ]; then
    JobIDHost='bell-adm'
    ARCHIVE=/archive/fortress/group/veritas/simulations/
    VBFDir=$ARCHIVE'/gammas/'$SPECSEA$PMT$SPECPART$SPECMDL'VBF'
fi

########################################################################
# To find the Zn,AZ,Offset and Pedvar combinations first put $SPECZN,$SPECAZ,
# $SPECWBL and $SPECPV into arrays. This is tricky. We save away the present 
# Internal Filed Seperator and then set IFS to a comma. We then use the -a 
# option with the read command to read in our stings into arrays. Then we 
# restore the IFS.
#########################################################################
SAVEIFS=$IFS
if [ $SPECZN = "All" ]; then
    Zenith=([0]=1 10 20 30 40 50 60 70)
else  
    IFS=, read -a Zenith <<< "$SPECZN"
fi

if [ $SPECAZ = "All" ]; then
    Azimuth=([0]=0 45 90 135 180 225 270 315)
else  
    IFS=, read -a Azimuth <<< "$SPECAZ"
fi

if [ $SPECWBL = "All" ]; then
    WblOffset=([0]=0.0 0.25 0.5 0.75 1.0 1.25 1.5 1.75 2.0)
else  
    IFS=, read -a WblOffset <<< "$SPECWBL"
    let iwblEnd=${#WblOffset[@]}
fi

if [[ $SPECPV = "All" ]]; then
    if [ $SPECCFG = "UA" ]; then
      if [ $SPECMDL = "MDL12UA" ]; then
          PedVar=([0]=5.18 5.55 6.51 7.64 8.97 10.52 12.35 14.49 17.00)
      else
          PedVar=([0]=4.73 5.55 6.51 7.64 8.97 10.52 12.35 14.49 17.00)
      fi
    fi
    if [ $SPECCFG = "NA" ] || [ $SPECCFG = "OA" ]; then
      PedVar=([0]=4.04 4.72 5.51 6.44 7.53 8.79 10.27 12.00)
    fi
else
    if [[ $SPECPV = "Base" ]]; then
        PedVar=([0]=0 )
    else
        IFS=, read -a PedVar <<< "$SPECPV"
    fi
fi
IFS=$SAVEIFS

# ##########################################################################
# For cosmicRays check if the use of simsets was specified
# For example, if we want sims sets 1-25 we would have SPECPART="CR1-25"
# #########################################################################
if [ ${SPECPART:0:2} = "CR" ]; then
   SIMSETS=${SPECPART##CR}
   if [ -n "$SIMSETS" ]; then
     SimSetStart=${SIMSETS%%-*}
     SimSetEnd=${SIMSETS##*-}
     #Now do a check that they indeed were set.
     if [ ! -n "$SimSetStart" ] || [ ! -n "$SimSetEnd" ]; then
        echo VAAuto:Fatal--Simsets improperly defined: #SPECPART
        echo VAAuto: Should be of the form CR1-25 oe CR4-4
        exit 
     fi
     echo 'VAAuto: SimSets Enabled:'$SimSetStart' to '$SimSetEnd
    fi
    SPECPART="CR"
fi

# ##########################################################################
# Lookup table setups
# ##########################################################################
HMode=H

# ###########################
# See if we are to use the electron tables. LTWBL is type of LT table
# possible values: "All"or "050" or "ALLE" or "AllG"
# Note that the bash construct ${#Variable} is length in characters of $vraiable
# ###########################
len=${#LTWBL}
if [ "$len" != "3" ]; then  #AllE or AllG
   PART=${LTWBL:3:1}        #"E" or "G"     
   LTWBL=${LTWBL:0:3}       # "All" or "050"
   if [ "$PART" = "E" ]; then
     LTPARTTYPE=Electrons
     echo LTPARTTYPE for LT: $LTPARTTYPE
   fi
fi

######################################
#SubLT bin ave mode for length, width and EaxisEnergy tables:Mean or MEDIAN
######################################
CFG_L=MEDIAN
CFG_W=MEAN
CFG_E=MEAN

if [ $HMode = "HFit" ]; then
    LTCUTSFILE=$VERITASAPPS'/tables/LookupTableHFit'
else
    LTCUTSFILE=$VERITASAPPS'/tables/LookupTable'
fi

if [ -n "$CUTTYPE" ]; then 
    LTCUTSFILE=$LTCUTSFILE$CUTTYPE'Cuts'
else
    LTCUTSFILE=$LTCUTSFILE'StdCuts'
fi

if [ -n "$ProduceSubLookupTables" ]; then
  if [ ! -e $LTCUTSFILE ]; then
      echo 'VAAuto: Fatal- Can not find file: '$LTCUTSFILE
      exit
  else
      echo 'VAAuto: Using Quality Cuts file: '$LTCUTSFILE
  fi
fi

# #################################
# LookupTableDispCuts file has:
#   DistanceUpper 0/1.38
#   NTubesMin 0/5
# #################################
DISPLTCUTSFILE=$VERITASAPPS'/tables/LookupTableDispCuts'
if [ -n "$ProduceDispSubLookupTables" ]; then
    if [ ! -e $DISPLTCUTSFILE ]; then
        echo 'VAAuto: Fatal- Can not find file: '$DISPLTCUTSFILE
        exit
    fi
fi

DTMWidthStr=0.04,0.06,0.08,0.1,0.12,0.14,0.16,0.2,0.25,0.35
DTMLengthStr=0.05,0.09,0.13,0.17,0.21,0.25,0.29,0.33,0.37,0.41,0.45,0.5,0.6,0.7,0.8


#################
#Some defaults
Samples=7

EACUTSFILE=$VERITASAPPS'/tables/Shower'$Samples'Sample'$CUTTYPE'Cuts'
if [ -n "$ProduceEASubLookupTables" ]; then
  if [ ! -e $EACUTSFILE ]; then
     echo 'VAAuto: Fatal- Can not find file: '$EACUTSFILE
     exit
  fi
fi

#########################################
#Default strings for the config files.
#########################################
TelIDStr=0,1,2,3
ZNStr=1,10,20,30,40,50,60,70
ABOStr=0.0,0.25,0.5,0.75,1.0,1.25,1.5,1.75,2.0  #Absolute Offset, ie. wobble
AzStr=0,45,90,135,180,225,270,315

if [ $SPECCFG = "UA" ]; then
    if [ $SPECMDL = "MDL12UA" ]; then
        NoiseStr=5.18,5.55,6.51,7.64,8.97,10.52,12.35,14.49,17.00
    else
        NoiseStr=4.73,5.55,6.51,7.64,8.97,10.52,12.35,14.49,17.00
    fi
fi
if [ $SPECCFG = "NA" ] || [ $SPECCFG = "OA" ]; then
   NoiseStr=4.04,4.72,5.51,6.44,7.53,8.79,10.27,12.00
fi

#ZNStr: Convert to an array
let iznEnd=${#Zenith[@]}
if test $iznEnd -ne "8"
 then
  let izn=0
  ZNStr=${Zenith[$izn]}
  izn=$((izn+1))
  while test $izn -lt $iznEnd
   do
     ZNStr=$ZNStr','${Zenith[$izn]}
     izn=$((izn+1))
   done
fi

#ABOStr:Convert to an array (Absolute Offset ie. wobble)
let iwblEnd=${#WblOffset[@]}
if [ "$LTWBL" = "050" ]; then
    ABOStr=" "
else
  if test $iwblEnd -ne "9"
   then
    let iwbl=0
    ABOStr=${WblOffset[$iwbl]}
    iwbl=$((iwbl+1))
    while test $iwbl -lt $iwblEnd
      do
       ABOStr=$ABOStr','${WblOffset[$iwbl]}
       iwbl=$((iwbl+1))
      done
   fi
fi

#AzStr; Convert to an array
let kazEnd=${#Azimuth[@]}
if test $kazEnd -ne "8"
 then
  let kaz=0
  AzStr=${Azimuth[$kaz]}
  kaz=$((kaz+1))
  while test $kaz -lt $kazEnd
   do
     AzStr=$AzStr','${Azimuth[$kaz]}
     kaz=$((kaz+1))
   done
fi

#NoiseStr: Convert to an array
let kpvarEnd=${#PedVar[@]}
if test $kpvarEnd -ne "9"
 then
  let kpvar=0
  NoiseStr=${PedVar[$kpvar]}
  kpvar=$((kpvar+1))
  while test $kpvar -lt $kpvarEnd
   do
    NoiseStr=$NoiseStr','${PedVar[$kpvar]}
    kpvar=$((kpvar+1))
   done
fi
###############################################################################

#See if optional CutTelelescope in use for GenerateStage4 or GenerateEA
TelConfig=1234

if [ -n "$TELCUT" ]; then
   T=$TELCUT
   TT=${T:0:1}
   if [ "$TT" != "T" ]; then
     echo 'VAuto: Bad CutTelscope option: '$T' Allowed values:T1 or T2 or T3 or T4'
     exit
   fi
   let CutTel=${T:1:1}
   if [ "$CutTel" -lt "1" ] ||  [ "$CutTel" -gt "4" ]; then
     echo 'VAuto: Bad CutTlescope option: '$T' Allowed values:T1 or T2 or T3 or T4'
     exit
   fi
   #Clear TelConfig and fill it correctly with out cut tel.
   TelConfig=
   if [ "$CutTel" != "1" ] ; then
      TelConfig=1
   else
      TelConfig='-'
   fi
   if [ "$CutTel" != "2" ] ; then
      TelConfig=$TelConfig'2'
   else
      TelConfig=$TelConfig'-'
   fi
   if [ "$CutTel" != "3" ] ; then
      TelConfig=$TelConfig'3'
   else
      TelConfig=$TelConfig'-'
   fi
   if [ "$CutTel" != "4" ] ; then
      TelConfig=$TelConfig'4'
   else
      TelConfig=$TelConfig'-'
   fi
   echo 'VAAuto: Array configuration: '$TelConfig'  No '$TELCUT
fi
###############################################################################


if [ -n "$LimitSubmissions" ]; then
  if [ -n "$HTARPedVarZnOffsetS2ToArchive" ]   || \
     [ -n "$HTARPedVarZnOffsetS2FromArchive" ] || \
     [ -n "$HTARPedVarZnOffsetS4ToArchive" ]   || \
     [ -n "$HTARPedVarZnOffsetS4FromArchive" ]; then
      GetUniqueNumber
      QsubLogs='RunningHtarQsubLogs'$UNIQUE'.txt'
      let MaxHTARQsubs=9
      echo "VAAuto: HTAR qsub active submissions being limited to " $MaxHTARQsubs 
  fi
  if [ -n "$GenerateSimLaserFile" ]     || \
     [ -n "$GenerateStage2FromVBF" ]    || \
     [ -n "$GenerateStage4FromStage2" ]|| \
     [ -n "$GenerateStage5Combined" ]   || \
     [ -n "$GenerateStage5CombinedRCE" ]; then
      GetUniqueNumber
      QsubLogs='RunningVegasQsubLogs'$UNIQUE'.txt'
      let MaxQsubs=$MaxQsubsDefault
      echo "VAAuto: Overall qsub active submissions being limited to " $MaxQsubs 
  fi

  if [ -e "$QsubLogs" ]; then
      rm $QsubLogs                     #Just some cleanup if previous run died.
  fi
fi

############################################################################
# Sim Laser generation. Note this is run on login node. 
# Not submitted to cluster.
############################################################################

if [ -n "$GenerateSimLaserFile" ]; then
    echo '##########################################################'
    echo '# GenerateSimLaserFile '
    echo '##########################################################'
    cd $lcl
    date
    ####################################################################
    #Generate a sim laser file 
    ####################################################################
    #Make sure VegasSimProduction.scr is set up correctly to get simLaser  
    cp $KASCADEBASE/scripts/VegasSimProduction.scr VegasSimProductionLaser.scr

    #####################################################################
    sed '/#Stage1Laser=enable/s/#Stage1Laser/Stage1Laser/g' \
                                        < VegasSimProductionLaser.scr >tmp1
    sed '/Stage1Data=enable/s/Stage1Data/#Stage1Data/g'      <tmp1 >tmp2
    sed '/Stage2=enable/s/Stage2/#Stage2/g'                  <tmp2 >tmp1
    sed '/Stage4=enable/s/Stage4/#Stage4/g'                  <tmp1 >tmp2
    sed '/Stage5=enable/s/Stage5/#Stage5/g' \
                                         <tmp2 >VegasSimProductionLaser.scr
    rm tmp1
    rm tmp2
    ####################################################################
       
    echo "VAAuto: Running VegasSimProductionLaser for simLaser generation"
    echo "VAAuto: Running on this login node. Not on cluster"
    echo "VAAuto: This takes a couple of minutes"
    ./VegasSimProductionLaser.scr  DummyFileName >$lcl'/SimLaserFile.log'
    echo "VAAuto: simLaser.root generated in local directory"
fi
#############################################################################


if [ -n "$GenerateStage2FromVBF" ]; then
  echo '##########################################################'
  echo '# GenerateStage2FromVBF (Cori ready)'
  echo '##########################################################'
  ####################################################################
  #1: Setup the VegasSimProduction.scr we will use for these jobs.
  #2: Setup the QsubFileNameList and QsubsLogs file
  #3: For Bell shorten the walltime for these Stage2 jobs.
  ####################################################################
  cd $lcl
  date

  # ##################################################################
  # Make sure VegasSimProduction.scr is set up correctly to get  
  # Stage1 and Stage2
  # ##################################################################
  GetUniqueNumber
  VSProdFile=VegasSimProductionS1S2.$UNIQUE'.scr'
  echo VSProdFile:   $VSProdFile

  GenerateQsubNames $UNIQUE

  cp $KASCADE/scripts/VegasSimProduction.scr $VSProdFile

  #######################################################################
  #The standard $KASCADE/scripts/VegasSimProduction.scr should have all 
  #options turned off.  We just have to make sure only stages 1 and 2 
  #get run
  #??????Sample size:We will need to set integration window size if 
  #different from 7?????? Not implimented yet.
  ##################################################################
  # I've added a check and fix of the year in the VBF file. GPSYear depends on 
  # the season: OA = 2006, NA = 2010, UA= 2014. Set above.
  # Before 2019-11-21 all UA(upgrade array) KASCADE sim files had GPSYear=2006
  # In vaStag2(beta/2.6.0+) the LowGain template correcting code uses GPSYear 
  # to determine which PMTs we were using OA/NA=Photonis, UA=Hamamatsu
  #
  sed '/FixGPSYearInVBF=enable/s/#FixGPSYearInVBF/FixGPSYearInVBF/g' $GPSYear \
                                         <$VSProdFile >tmp2
  sed '/Stage1Laser=enable/s/Stage1Laser/#Stage1Laser/g'      <tmp2 >tmp1
  sed '/#Stage1Data=enable/s/#Stage1Data/Stage1Data/g'        <tmp1 >tmp2
  sed '/#Stage2=enable/s/#Stage2/Stage2/g'                    <tmp2 >tmp1
  sed '/Stage4=enable/s/Stage4/#Stage4/g'                     <tmp1 >tmp2
  sed '/Stage5=enable/s/Stage5/#Stage5/g'                     <tmp2 >tmp1
  mv tmp1 $VSProdFile

  ####################################################################
  # Assume from now on that we are using a LowGain (LGD) capable vegas. 
  # We will need to make sure out simulations .vbf file has the year 
  # appropriate for ther season. All sims (by my screwup) were made with 
  # year 2006 (V4/V5).
  # This is all DEPRECIATED as of use of vegas beta/2.6.0
  ####################################################################
  #runningLGD="$($VEGAS/bin/vaStage2 -h=all | grep TE_LowGainTransferFunctionFilePath)"
  #if [ -n "$runningLGD" ]; then
  #  echo VAAuto: vaStage2 is LGD capable.
  #  #########################################
  #  #Define the file names needed in the options.
  #  #########################################
  #  if [ -n "$VEGASBASE" ]; then
  #    if [ -n "$CORI" ] ; then
  #      lgdf=$VEGASBASE/common/lowGainDataFiles/   #on cori
  #    else
  #      if [ -n "$BELL" ]; then
  #        lgdf=$VEGASBASE/common/lowGainDataFiles/   #on Bell
  #      fi
  #    fi
  #  else
  #    echo "VAAUTO--Fatal: This version of vegas does not define VEGASBASE"
  #    echo "VAAUTO--Needed for LowGain analysis"
  #    exit
  #  fi

  #   #Note:  Difference (cap/nocap) between PMT_Mfr and PMT_mfr 
  #  echo PMT_Mfr: $PMT_Mfr
  #  echo PMT_mfr: $PMT_mfr
  #  echo ldgf: $lgdf

  #  VTMPLT=$lgdf'/lowGainTemplateLookup'$PMT_Mfr'WithTimeSpread.root'
  #  VTF=$lgdf'/'$PMT_mfr'_low_gain_to_high_gain_transfer.root'


  #  sed "/#LGDAlg/s/#LGDAlgorithumOption=/LGDAlgorithumOption='-TE_Algorithm=TraceEvaluatorLowGainDiagnostics'/g"  <$VSProdFile >tmp12
  #  sed "/#LGDTempl/s,#LGDTemplateOption=,LGDTemplateOption='-TE_LowGainTemplateFilePath=$VTMPLT',g"  <tmp12 >tmp2
  #  sed "/#LGDTranf/s,#LGDTranferFuntionOption=,LGDTranferFuntionOption='-TE_LowGainTransferFunctionFilePath=$VTF',g"  <tmp2 >$VSProdFile
  #fi
  # ########################################################################


  chmod 755 $VSProdFile
  rm tmp2
        
  ###########################################################################
  # Stage 2 only needs longer time.
  ###########################################################################
  WALLTIME=' -t 30:00:00'
  MEMREQUEST='--mem=3GB'

fi

############################################################################

if [ -n "$GenerateStage4FromStage2" ]; then
  echo '##########################################################'
  echo '# GenerateStage4FromStage2'
  echo '##########################################################'
  cd $lcl
  date
  GetUniqueNumber
  VSProdFile=VegasSimProductionS1S2.$UNIQUE'.scr'
  echo VSProdFile:   $VSProdFile

  GenerateQsubNames $UNIQUE

  #Make sure VegasSimProduction.scr is set up correctly to get Stage4

  GetUniqueNumber
  VSProdFile=VegasSimProductionS4.$UNIQUE'.scr'

  cp $KASCADEBASE/scripts/VegasSimProduction.scr $VSProdFile

  ###################################################################
  #The standard VegasSimProduction.scr should have all options 
  #turned off.  We just have to make sure only stage 4 gets run
  #Sample size:We will need to set integration window size if 
  #different from 7.
  ##################################################################
  sed '/Stage1Laser=enable/s/Stage1Laser/#Stage1Laser/g' \
                                          < $VSProdFile  >tmp1
  sed '/Stage1Data=enable/s/Stage1Data/#Stage1Data/g'        <tmp1 >tmp2
  sed '/Stage2=enable/s/Stage2/#Stage2/g'                    <tmp2 >tmp1
  sed '/#Stage4=enable/s/#Stage4/Stage4/g'                   <tmp1 >tmp2
  sed '/Stage5=enable/s/Stage5/#Stage5/g'                    <tmp2 >tmp1

  ######################################################################
  #  Check to see if we are to exclude a telescope from the stage4 analysis
  ######################################################################
  if [ -n "$CutTel" ]; then
    sed '/CutTelescopeOption=/s/#CutTel/CutTel/g'              <tmp1 >tmp2
    sed '/CutTelescopeOption=/s/=0/='$CutTel'/g'               <tmp2 >tmp1
  fi

  ######################################################################
  # If this is an Old Array run, enable the TelCombosToDeny=T1T4
  ######################################################################
  if  [ $SPECCFG = "OA" ]; then
    sed '/TelCombosToDeny=T1T4/s/#Deny/Deny/g'  <tmp1 >tmp2 
    cp tmp2 tmp1
  fi

  ######################################################################
  # We also need to set the quality cuts file and the file extention and the 
  # LT name
  ####################################################################
  QUALITYCUTSFILE=Quality$CUTTYPE'Cuts'
  QUALCUTSSOURCE=$VERITASAPPS'/tables/'$QUALITYCUTSFILE
  if [ ! -e "$QUALCUTSSOURCE" ]; then
    echo 'VAAuto: Fatal- Can not find QualityCuts file: '$QUALCUTSSOURCE
    exit
  fi

  GenerateLTFileName $SPECCFG $SPECSEA $LTWBL  'std' $LTPARTTYPE
  LTFILESOURCE=$VERITASAPPS'/tables/'$LTFILENAME
  if [ ! -e "$LTFILESOURCE" ]; then
    echo 'VAAuto: Fatal- Can not find LT file: '$LTFILESOURCE
    exit
  fi

  FILEBASE=$CUTTYPE'Cuts'

  sed '/FileBase/s/StdCuts/'$FILEBASE'/g'                          <tmp1 >tmp2
  sed '/QualityCuts/s/QualityStdCuts/'$QUALITYCUTSFILE'/g'         <tmp2 >tmp1
  sed '/Table=LookupTable/s/LookupTable/'$LTFILENAME'/g'           <tmp1 >tmp2
  mv tmp2 $VSProdFile
  chmod 755  $VSProdFile

  #############################################################################
  # Stage 4 only needs short time
  #############################################################################
  WALLTIME='-t 04:00:00'
#  MEMREQUEST='--mem=1GB'  #This gets us chagred for  2 core on cori
  MEMREQUEST='--mem=3GB'  #This gets us chagred for  4 core on cori
  if [ -n "$BELL" ]; then
    WALLTIME='-l walltime=04:00:00'
    MEMREQUEST='-l mem=7GB'
  fi
fi
############################################################################

if [ -n "$GenerateStage5Combined" ] || [ -n "$GenerateStage5CombinedRCE" ]; then
  if [ -n "$GenerateStage5Combined" ]; then
    echo '##########################################################'
    echo '# GenerateStage5Combined (Does not  RemoveCutEvents)'
    echo '##########################################################'
  fi
  if [ -n "$GenerateStage5CombinedRCE" ]; then
    echo '##############################################WUCRMDL12eSpecHiPedVarS2############'
    echo '# GenerateStage5CombinedRCE (Has RemoveCutEvents)'
    echo '##########################################################'
  fi

  cd $lcl
  date
  #Make sure VegasSimProduction.scr is set up correctly to get  
  #Stage5 combined tree

  GetUniqueNumber
  VSProdFile=VegasSimProductionS5.$UNIQUE'.scr'
  echo VSProdFile: $VSProdFile

  GenerateQsubNames $UNIQUE

  ###################################################################
  # The standard VegasSimProduction.scr should have all options 
  # turned off.  We just have to make sure only stage 5 gets run
  # Sample size:We will need to set integration window size if 
  # different from 7.
  ##################################################################
  cp $KASCADEBASE/scripts/VegasSimProduction.scr $VSProdFile
  sed '/Stage1Laser=enable/s/Stage1Laser/#Stage1Laser/g'     < $VSProdFile  >tmp1
  sed '/Stage1Data=enable/s/Stage1Data/#Stage1Data/g'        <tmp1 >tmp2
  sed '/Stage2=enable/s/Stage2/#Stage2/g'                    <tmp2 >tmp1
  sed '/Stage4=enable/s/Stage4/#Stage4/g'                    <tmp1 >tmp2
  sed '/#Stage5=enable/s/#Stage5/Stage5/g'                   <tmp2 >tmp1

  # Stage5 needs to have the Shower Cuts file name in VSProdFile set
  SHOWERCUTSFILE=Shower7Sample$CUTTYPE'Cuts'
  cp $VERITASAPPS/tables/$SHOWERCUTSFILE  .
  echo SHOWERCUTSFILE: $SHOWERCUTSFILE
  sed '/ShowerCutsFile=/s/ShowerStdCuts/'$SHOWERCUTSFILE'/g' <tmp1 >tmp2

  ###############################################################################    
  # If this is a Stage5 run for ZAlpha the input CUTTYPE will be somethin like:
  # UpgradeMediumHadron or UpgradeMediumElectron or UpgradeMediumRecon.
  # if so we have to modify the filebase to get rid of the Electron or Hadron 
  # or Recon so that the input cuts filename from Stage4 looks correct in 
  #  VagasSimProduciton
  ###############################################################################    
  if [ ! "${CUTTYPE%%Electron}" = "$CUTTYPE" ];  then
    CUTBASE=${CUTTYPE%%Electron}
    OUTEXT=Electron
  else
    if [ ! "${CUTTYPE%%Hadron}" = "$CUTTYPE" ];  then
      CUTBASE=${CUTTYPE%%Hadron}   #Used later to check Stage4 file existance
      OUTEXT=Hadron
    else
      if [ ! "${CUTTYPE%%Recon}" = "$CUTTYPE" ];  then
        CUTBASE=${CUTTYPE%%Recon}
        OUTEXT=Recon
      else
        CUTBASE=$CUTTYPE
        # Note: no OUTEXT extention being defined. => no output file name generated
        # in VegasSimProduction
      fi
    fi
  fi
  FILEBASE=$CUTBASE'Cuts'
  echo FILEBASE: $FILEBASE
  sed '/FileBase/s/StdCuts/'$FILEBASE'/g'                          <tmp2 >tmp1

  ###############################################################################
  # Now for eSpec we need to also enable the -outputfile option in the vaStage5 
  # call within VegasSimProduction. We do this by replacing the 
  # '#OutputFileNameExt=S7' with something like: 'OutputFileNameExt=Electron'
  # For eSpec we also need to set the aperature. For this the offset shoiuld 
  # have been set to the required aperature.
  # #############################################################################
  if [ -n "$OUTEXT" ]; then
    if [ ${#WblOffset[@]} -eq "1" ]; then
      sed '/#OutputFileNameExt/s/#OutputFileNameExt=S7/OutputFileNameExt='$OUTEXT'/g' \
                                                                       <tmp1 >tmp2
      off=${WblOffset[0]}
      OFFSQR=$(awk 'BEGIN{print '$off' * '$off'}')
      sed '/#ThetaSquareUpper=/s/#ThetaSquareUpper=.01/ThetaSquareUpper='$OFFSQR'/g' \
                                                                       <tmp2 >tmp1
      sed '/#Stage5ThetaSquareOption/s/#Stage5/Stage5/g'               <tmp1 >tmp2 
      echo 'VAAuto: eSpec radius^2 aperature set to '$OFFSQR 'deg^2 '
    else
      echo 'VAAuto: Fatal-For eSpec more than one aperture (offset) specified'
      exit
    fi
    mv tmp2 tmp1
  fi  
  ###########################################################################
  # And set Method to combined for stage5
  # All OutputMethodOptions start out disabled (#). 
  # For no RemoveCutEvents, Disable a second time (#->##) the  Combined 
  # output methods with RemoveCutEvents and then  remove the single # from 
  # the combined one without a RemoveCutEvents.
  ###########################################################################
  if [ -n "$GenerateStage5Combined" ]; then
    sed '/RemoveCutEvents/s/#OutputMethod/##OutputMethod/g'       <tmp1 >tmp2
    sed '/Method=combined/s/#OutputMethod/OutputMethod/g'         <tmp2 >tmp1
  fi
   
  ##########################################################################
  # For a combined option with RemoveCutEvents only remove # from that option
  # Have to double disable stereo option first
  ##########################################################################
  if [ -n "$GenerateStage5CombinedRCE" ]; then
    sed '/Method=stereo/s/#OutputMethod/##OutputMethod/g'       <tmp1 >tmp2
    sed '/RemoveCutEvents/s/#OutputMethod/OutputMethod/g'       <tmp2 >tmp1
  fi

  mv tmp1 $VSProdFile
  chmod 755 $VSProdFile
  rm tmp1
  rm tmp2
fi
############################################################################

if [ -n "$CheckStage2Files" ]; then
  echo '##########################################################'
  echo '# CheckStage2Files'
  echo '##########################################################'
  if [ ! -e "CheckStage2FileOK.C" ]; then
     cp $KASCADEBASE/scripts/CheckStage2FileOK.C .
  fi
fi
############################################################################

if [ -n "$CheckStage4Files" ]; then
  echo '##########################################################'
  echo '# CheckStage4Files'
  echo '##########################################################'
  date 
  if [ ! -e "CheckStage4FileOK.C" ]; then
    cp $KASCADEBASE/scripts/CheckStage4FileOK.C .
  fi
  ~/Switch.rootrcTo.rootrc_glenn.scr
fi
############################################################################
############################################################################

if [ -n "$CheckStage5Files" ]; then
  echo '##########################################################'
  echo '# CheckStage5Files'
  echo '##########################################################'
  date 
  if [ ! -e "CheckStage5FileOK.C" ]; then
    cp $KASCADEBASE/scripts/CheckStage5FileOK.C .
  fi
  ~/Switch.rootrcTo.rootrc_glenn.scr
fi
############################################################################

if [ -n "$GenerateDispSubLTListFiles" ]; then
   echo '##########################################################'
   echo '# GenerateDispSubLTListFiles'
   echo '##########################################################'
   cd $lcl
   date
   DispListNames='DispSubLT'$SPECSEA$Samples'Sample*'$HMode'List'
   rm $DispListNames
fi
#############################################################################
if [ -n "$GenerateDispSubLTConfigFiles" ]; then
    echo '##########################################################'
    echo '# GenerateDispSubLTConfigFiles'
    echo '##########################################################'
    cd $lcl
    date
    ConfigNames='DispSubLT'$SPECSEA$Samples'Sample*Deg*noise'$HMode'.config'
    rm $ConfigNames
    # *************************************************************************
    # Generate template config file for the Disp tables
    # Change for KASCADE and Number of samples
    # Note: No DTM_TelID(All tel tables look identicle)  or 
    #       DTM_AbsoluteOffset (doesn't make sense for DTM) differences used
    # *************************************************************************
    $VEGAS/bin/produceDispTables -save_config_and_exit tmp1
    sed '/DTC_SimulationType/s/GrISU/KASCADE/g'                  <tmp1 >tmp2
    sed '/DTM_FillType "MEDIAN"/s/MEDIAN/MEDIAN/g'               <tmp2 >tmp1
    sed '/DTM_Azimuth      /s/Azimuth/Azimuth '$AzStr'/g'        <tmp1 >tmp2
    sed '/DTM_Zenith         /s/Zenith/Zenith '$ZNStr'/g'        <tmp2 >tmp1
    sed '/DTM_Noise         /s/Noise/Noise '$NoiseStr'/g'        <tmp1 >tmp2
    sed '/DTM_Width         /s/Width/Width '$DTMWidthStr'/g'     <tmp2 >tmp1
    sed '/DTM_Length         /s/Length/Length '$DTMLengthStr'/g' <tmp1 >tmp2
    sed '/Log10SizePerBin/s/0.04/0.25/g'                         <tmp2 >tmp1
    sed '/Log10SizeUpperLimit/s/5.5/6.0/g'                       <tmp1 >tmp2
    sed '/RatioPerBin/s/0.05/1.0/g'                              <tmp2 >tmp1
   #Set the number of samples for noise
    sed '/DTM_WindowSizeForNoise/s/7/'$Samples'/g' <tmp1 >tmp2
   #Replace HillasBranchName argument
    sed '/HillasBranchName "H"/s/"H"/"'$HMode'"/g'    <tmp2 >config1.tmp
    
   ###########################################################################
   #Config settings not used:
   #sed '/DTM_FillType "MEDIAN"/s/MEDIAN/MEAN/g'                 <tmp2 >tmp1
   #sed '/GC_CorePositionFractionalErrorCut/s/100/.25/g'         <tmp1 >tmp2
   #sed '/GC_CorePositionAbsoluteErrorCut/s/1000/20/g'           <tmp2 >tmp1
   ###########################################################################
   # Disp cut file DispLookupTableStdCuts should only have:
   # -SizeLower=0/0
   # -DistanceUpper=0/1.38
   #-NTubesMin=0/5
   ###########################################################################
fi
########################################################################

if [ -n "$ProduceDispSubLookupTables" ]; then
   echo '##########################################################'
   echo '# ProduceDispSubLookupTables'
   echo '##########################################################'
   cd $lcl
   date
fi  
##############################################################################
if   [ -n "$CombineDispSubLT" ]; then
   echo '##########################################################'
   echo '# CombineDispSubLT'
   echo '##########################################################'
   cd $lcl
   date
   GenerateLTFileName $SPECCFG $SPECSEA $LTWBL  'std' $LTPARTTYPE
   LTFILENAME=dt${LTFILENAME#lt}           #Convert to a dt filename
   SubLTList='SubLTList'
   if [ -e $lcl'/'$SubLTList ]; then
     rm $lcl'/'$SubLTList
   fi
fi

########################################################################
# Specail test for SubLTables (LT and Disp): We need at least 2 specs for Az and 
# PedVar. This is a produce_lookuptable and produceDispTable "feature"
########################################################################
let kpvarEnd=${#PedVar[@]}
let kazEnd=${#Azimuth[@]}
let iznEnd=${#Zenith[@]}
let iwblEnd=${#WblOffset[@]}
if [ -n "$GenerateSubLTConfigFiles" ]     || [ -n "$GenerateSubLTListFiles" ] || \
   [ -n "$ProduceSubLookupTables" ]       || [ -n "$ProduceDispSubLookupTables" ] || \
   [ -n "$GenerateDispSubLTConfigFiles" ] || [ -n "$GenerateDispSubLTListFiles" ] || \
   [ -n "$ProduceEASubLookupTables" ]     || \
   [ -n "$GenerateEASubLTListAndConfigFiles" ]; then
   if test  $kazEnd -lt "1" 
   then
     echo 'VAAuto: Fatal! Need at least 2 Az values specified'
     #Note we could also just remove the AZ line in the config file!
     exit
   fi
   if test $kpvarEnd -lt "1"
    then
      echo 'VAAuto: Fatal! Need at least 2 PedVar values specified'
     #Note we could also just remove the Noise line in the config file!
      exit
   fi
   if test  $iznEnd -lt "1" 
    then
     echo 'VAAuto: Fatal! Need at least 2 Zn values specified'
     #Note we could also just remove the Zn line in the config file!
     exit
   fi
   #Allow one Offset(ie any number at all) (Comment out the following if then)
   #if test $iwblEnd -lt "1"
   # then
   #   echo 'VAAuto: Fatal! Need at least 2 Offset(Wbl) values specified'
   #  #Note we could also just remove the Noise line in the config file!
   #   exit
   #fi
fi
##########################################################################

MissingListName='CheckPedvarMissingList'
 


# ############################################################################
# ############################################################################
# now we loop over Zn,AZ WblOffset and PedVar
# ############################################################################
# ############################################################################


if [ -n "$GetVBFFileFromArchive" ]      || [ -n "$GenerateStage2FromVBF" ] || \
   [ -n "$CheckStage2Files" ]           || [ -n "$GenerateDispSubLTConfigFiles" ] || \
   [ -n "$GenerateDispSubLTListFiles" ] || [ -n "$ProduceDispSubLookupTables" ]   || \
   [ -n "$CombineDispSubLT" ]; then
   
  #Zenith
  let izn=0
  let iznEnd=${#Zenith[@]}
  let iFilesNotFound=0
  while test $izn -lt $iznEnd
   do
    #Azimuth
    let kaz=0
    let kazEnd=${#Azimuth[@]}
    while test $kaz -lt $kazEnd
     do
      #Make   Zn_Az string
      AZ=${Azimuth[$kaz]}
      ZN=${Zenith[$izn]}
      if test $AZ = "0"
       then
        ZnAz=$ZN'Deg'
      else
        ZnAz=$ZN'_'$AZ'Deg'
      fi
     
      # ###################################################################
      # Construct the Disp Sub File Names
      # ###################################################################
      DISPSUBLTBASE='DispSubLT'$SPECSEA$Samples'Sample'$ZnAz
      DispConfigName=$DISPSUBLTBASE$HMode'.config'
      DispListNames=$DISPSUBLTBASE$HMode'List'
      DispSubLTFileName=$lcl'/'$DISPSUBLTBASE'.root'
      #*******************************************************************

      if [ -n "$GenerateDispSubLTConfigFiles" ]; then
        #############################################################
        #Now we need to "edit the new config file
        #############################################################
        #Figure out what the Zenith argument should look like
        if test $izn -eq "0" 
         then
          Zn=${Zenith[izn]}','${Zenith[1]}
        else
          Zn=${Zenith[0]}','${Zenith[izn]}
        fi
        #now replace Zenith argument and put in a tmp file
        sed '/DTM_Zenith/s/'$ZNStr'/'$Zn'/g' <config1.tmp >config2.tmp 

        ############################
        #Now do the same for the Azimuth but put in final file
        ############################
        if test $kaz -eq "0" 
         then
           Az=${Azimuth[$kaz]},${Azimuth[1]}
        else
           Az=${Azimuth[0]},${Azimuth[$kaz]}
        fi
        #now replace Offset argument and put in a final file
        sed '/DTM_Azimuth/s/'$AzStr'/'$Az'/g'  <config2.tmp >$DispConfigName
        rm config2.tmp
      fi
      #********************************************************************

      if [ -n "$ProduceDispSubLookupTables" ]; then
        ####################################################################
        # Make up the job script file that will be submitted below
        ####################################################################
        sgeFile=$lcl'/'$DISPSUBLTBASE'.pbs'
        echo "#"PBS $WALLTIME                >$sgeFile
        echo "#PBS "$MEMREQUEST                         >>$sgeFile

        if [ -n "$PURDUE" ]; then
          echo source /etc/profile                      >>$sgeFile
          echo module load gcc/5.2.0                    >>$sgeFile
        fi
        echo cd $lcl                                    >>$sgeFile
        echo $VEGAS/bin/produceDispTables \\            >>$sgeFile
        echo -config $lcl'/'$DispConfigName \\          >>$sgeFile
        echo -cuts $DISPLTCUTSFILE  \\                  >>$sgeFile
        echo  $lcl'/'$DispListNames   \\                 >>$sgeFile
        echo  $DispSubLTFileName  \\                    >>$sgeFile
        echo '>'$lcl'/'$DISPSUBLTBASE'.log'             >>$sgeFile
        chmod 700 $sgeFile

        $QSUB$QSUBEXT -q $QUEUE -V -e $DISPSUBLTBASE.qsub.err -o $DISPSUBLTBASE.qsub.log $sgeFile
      fi
      #*******************************************************************
    
      if [ -n "$CombineDispSubLT" ]; then
        #*******************************************************************
        # Create the List of Disp SubLT files to be combined
        #******************************************************************
        # But first check that each file does exixt. We may want to test deeper
        # if this isn't enough later.
        ###################################################################
        if  [ ! -e "$DispSubLTFileName" ]; then
          echo 'VAAuto: Fatal--Disp SubLT file '$DispSubLTFileName ' does not exist'
          # exit
        fi
        echo $DispSubLTFileName >>$lcl'/'$SubLTList
      fi
      #******************************************************************

      ####################################################################
      # Iterate over WBLOffsets and PedVars
      ####################################################################
      #WblOffset
      let iwbl=0
      let iwblEnd=${#WblOffset[@]}
      while test $iwbl -lt $iwblEnd
       do
        WBL=${WblOffset[$iwbl]}
        if [ "$WBL" !=  '0.0' ]; then
         WBLSPEC=S$WBL'Wbl'
        else
         WBLSPEC=$WBL'Wbl'
        fi
 
        if [ "$SPECPART" = "CR" ]; then
          GenerateVBFName $SPECPART $SPECCFG $SPECSEA $ZN $AZ "-1" $SPECMDL #no wbl for CR
        else
          GenerateVBFName $SPECPART $SPECCFG $SPECSEA $ZN $AZ $WBL $SPECMDL
        fi

        BaseFileS2=${VBFFILENAME%%.vbf}
        ################################################################
             
        if [ -n "$GetVBFFileFromArchive" ]; then
          echo '##########################################################'
          echo '# GetVBFFileFromArchive'
          echo '##########################################################'
          cd $lcl
          date
          echo VBFFILENAME: $VBFFILENAME
          echo VBFDIR: $ARCHIVE$SPECSEA$PMT$SPECPART$SPECMDL'VBF'
          if [ ! -e  "$VBFFILENAME" ]; then
             hsi 'cd '$ARCHIVE$SPECSEA$PMT$SPECPART$SPECMDL'VBF; get '$VBFFILENAME';'
          fi
        fi
        ###############################################################

        #Iterate through PedVars
        let kpvar=0
        let kpvarEnd=${#PedVar[@]}
        while test $kpvar -lt $kpvarEnd
         do
          #Test for defaults
          PV=${PedVar[$kpvar]} 
          if test "$PV" = "0"
           then
            PedVarFile="NONE"
            PedVarName=""
          else
            PedVarFile=Ped$SPECCFG$PV
            PedVarName=PedVar$PV
          fi  
   
          # ******************************************************************
          #  Now all the things we can do for a particular Zn,AZ,WblOffset and 
          #  PedVar
          # ******************************************************************
          #Generate  file names
 
          FileName=$PedVarName'_'$BaseFileS2'.root'

          # ******************************************************************
          #  GenerateStage2FromVBF and CheckStage2Files also need SimSet 
          #  interation if applicable
          # ******************************************************************
          if [ -n "$GenerateStage2FromVBF" ] || [ -n "$CheckStage2Files" ]; then
            # ###############################################################
            # Due to size and walltime limits we have divided the each CR vbf 
            # file into a number of seperate files. This is implimented by adding 
            # a SimSet number at the end of the VBF file name ( just before the 
            # .vbf extention). This program does this by using the SimSetStar and 
            # SimSetEnd symbols. If they are both defined the we are using SimSets
            # ################################################################
            if [ -n "$SimSetStart" ] && [ -n "$SimSetEnd" ]; then
              let sSimSetNum=$SimSetStart
              let sSimSetEnd=$SimSetEnd
            else
              let sSimSetNum=0 
              let sSimSetEnd=0
            fi

            while test $sSimSetNum -le $sSimSetEnd
             do
	       
              if [ -n "$GenerateStage2FromVBF" ]; then
                #echo '##########################################################'
                #echo '# GenerateStage2FromVBF '
                #echo '##########################################################' 

                ################################################################
                # I made a change for the Cori version . Enable the Job array 
                # algorithum as used in the KAAuto.scr file for Cori.  This 
                # requires that we make all the pbs files and place their names 
                # in a QSubListFile instead of running them directly. Then after
                # that is  complete, a single routine, SubmitQsubJobs, (now in 
                # UtilityFuntions)is called to submit a job array submission task.
                #(Note:
                # there is no QsubLimit procedure when using the job arrays (for 
                # now).
                # Eventually we will switch over to using the QSubListFile for  
                # all clusters. Only Cori for now.
                #################################################################

                cd $lcl
                if [ "$sSimSetNum" = "0" ]; then
                  VBFNM=$VBFFILENAME
                  BFNMS2=$BaseFileS2   
                else
                  VBFNM=${VBFFILENAME%%.vbf}$sSimSetNum'.vbf'
                  BFNMS2=$BaseFileS2$sSimSetNum
                fi
                if [ -n "$CORI" ] && [ -n "$JobArray" ]; then
                  #######################################################
                  # BuildVegasSimProduction will create the .pbs file and add
                  # that file's name to the QsubFileNameList  file
                  # The actual submision of the job array job to the cluster and 
                  # moidification of the .pbs job to be a job array job is done 
                  # later in this script.
                  #########################################################
                  # Check that the vbf file exists in the lcl directory
                  if [ ! -e "$VBFNM" ]; then
                    echo VAAuto--Fatal: VBF input file $VBFNM does not exist in lcl.
                    echo VAAuto--Job Array submission aborted.
                    exit
                  fi

                  BuildVegasSimProduction $PedVarFile $BFNMS2 $VSProdFile $VBFNM 
                else
                  SubmitVegasSimProduction $PedVarFile $BFNMS2 $VSProdFile $VBFNM
                fi
                echo VAAuto--File:  $VBFNM 
              fi
              ###############################################################


              if [ -n "$CheckStage2Files" ]; then
                #echo '##########################################################'
                #echo '# CheckStage2Files (a)'
                #echo '##########################################################'
                cd $lcl
             
                #Check VARootIO can open and load file 
                #Setip for running root. 
                ~/Switch.rootrcTo.rootrc_glenn.scr
           
                # Previously we made sure we have the root script file 
                # CheckStage2FileOK.C

                if [ "$sSimSetNum" = "0" ]; then
                  RootFileName=$PedVarName'_'$BaseFileS2'.root'
                else
                  RootFileName=$PedVarName'_'$BaseFileS2$sSimSetNum'.root'
                fi

                echo VAAuto--Checking Stage2 file: $RootFileName
                Arg=$lcl'/CheckStage2FileOK.C("'$RootFileName'")'
                if [ -e  "CheckStage2FileOK.Result" ]; then
                  rm "CheckStage2FileOK.Result"
                fi 
                root -q -b -q $Arg  >CheckStage2OK.log

                if [ !  -e  "CheckStage2FileOK.Result" ]; then
                  echo 'VAAuto: root CheckStage2FileOK.C failed to produce ' \
                       'CheckStage2FileOK.Result'
                  echo 'VAAuto: You shouold check to see if another VAAuto is ' \
                       'running: $> ps aux'
                  echo 'Bad: "$RootFileName'
                  cat CheckStage2OK.log
                else 
                 {
                  read BAD
                  let suzBAD=$BAD
                  if [ $suzBAD -lt "0" ]; then
                    echo "Bad: "$RootFileName
                  
                    if [ $suzBAD -eq "-1" ]; then
                      echo "VAAuto: VARootIO failed to open. " 
                    fi
                    if [ $suzBAD -eq "-2" ]; then
                      echo "VAAuto: VARootIO failed to load file. May not exist"
                    fi
                    if [ $suzBAD -eq "-3" ]; then
                      echo 'VAAuto: VARootIO failed to find ' \
                           'ParameterisedEventTree'
                    fi
                    if [ $suzBAD -eq "-4" ]; then
                      echo "VAAuto: VARootIO failed to find SimulatedEventTree"
                    fi
                    if [ $suzBAD -eq "-5" ]; then
                      echo "VAAuto: Too few events ( <20 )  in " \
                           "ParameterisedEventTree"
                    fi
                  fi 
                  rm CheckStage2FileOK.Result
                 } <CheckStage2FileOK.Result
                fi 
              fi
              # ##############################################################
         
              sSimSetNum=$((sSimSetNum + 1))
             done        #End of SimSet loop
          fi           
          # ##################################################################

          if [ -n "$GenerateDispSubLTListFiles" ]; then
            # echo '##########################################################'
            # echo '# GenerateDispSubLTListFiles'
            # echo '##########################################################'
            cd $lcl
            echo $FileName >>$DispListNames
          fi
          #******************************************************************
 

          ################################################
          #should all be done now. Go on to next combo.
          ##############################################
          let kpvar=$kpvar+1
         done                    #End of PedVar while loop.
        let iwbl=$iwbl+1
       done                      #End of wbl (offset) while loop
      kaz=$((kaz+1))
     done                        #End of Azimuth while oop
    if [ -n "$CheckStage2Files" ]; then
      if [ -n "$SimSetStart" ] && [ -n "$SimSetEnd" ]; then
        echo 'VAAuto: PedVar files: '$SPECSEA$PMT$SPECPART' Zn:  '${Zenith[$izn]} \
             'AZ: '$SPECAZ', pedv: '$SPECPV', Sim Sets: '$SimSetStart' to '$SimSetEnd \
             ' : Check complete.' 
      else
        echo 'VAAuto: PedVar files: '$SPECSEA$PMT$SPECPART' Zn:  '${Zenith[$izn]} \
             ' AZ: '$SPECAZ', offset: '$SPECWBL', pedv: '$SPECPV' Check complete.'
      fi
    fi
    izn=$((izn+1))
   done
fi

###########################################################################

if [ -n "$GenerateStage2FromVBF" ]; then
        
  #echo '##########################################################'
  #echo '# GenerateStage2FromVBF '
  #echo '##########################################################'
        
  ######################################################################
  # For NERSC (Cori ) JobArray only: generate Jobarry .pbs file for 
  # all the .pbs jobs in the qsubFileNameList.
  # SubmitQsubJobs is in UtilityFuncitons.scr
  ######################################################################
  if [[ -n "$CORI" ] && [ -n "$JobArray" ]; then
    SubmitQsubJobs  $QsubFileNameList
  fi
fi
##########################################################################

if [ -n "$GenerateSubLTListFiles" ]; then
  echo '##########################################################'
  echo '# GenerateSubLTListFiles'
  echo '##########################################################'
  cd $lcl
  date
  ListNames='SubLT'$SPECSEA$Samples'Sample*Deg*List'
  rm $ListNames
fi
#############################################################################

if [ -n "$GenerateSubLTConfigFiles" ]; then
  echo '##########################################################'
  echo '# GenerateSubLTConfigFiles'
  echo '##########################################################'
  cd $lcl
  date
  # *************************************************************************
  # Generate template config file
  # Change for KASCADE and Number of samples
  # *************************************************************************
  $VEGAS/bin/produce_lookuptables -save_config_and_exit tmp1
  sed '/CorePositionFractionalErrorCut/s/100/.25/g' <tmp1 >tmp2
  sed '/CorePositionAbsoluteErrorCut/s/1000/20/g'   <tmp2 >tmp1
  sed '/LTC_SimulationType/s/GrISU/KASCADE/g'       <tmp1 >tmp2
  sed '/LTM_FillType "MEDIAN"/s/MEDIAN/MEAN/g'           <tmp2 >tmp1
  sed '/LTM_WidthFillType " "/s/" "/"'$CFG_W'"/g'       <tmp1 >tmp2
  sed '/LTM_LengthFillType " "/s/" "/"'$CFG_L'"/g'      <tmp2 >tmp1
  sed '/LTM_EaxisEnergyFillType " "/s/" "/"'$CFG_E'"/g' <tmp1 >tmp2
  
  sed '/LTM_EnergyFillType " "/s/" "/"MEAN"/g'        <tmp2 >tmp1
  sed '/TelID        /s/TelID/TelID  '$TelIDStr'/g'   <tmp1 >tmp2
  sed '/Azimuth      /s/Azimuth/Azimuth '$AzStr'/g'   <tmp2 >tmp1
  sed '/Zenith         /s/Zenith/Zenith '$ZNStr'/g'   <tmp1 >tmp2
  sed '/Noise         /s/Noise/Noise '$NoiseStr'/g'   <tmp2 >tmp1
  if [ "$LTWBL" != "050" ]; then
    sed '/luteOffset    /s/Offset/Offset '$ABOStr'/g'   <tmp1 >tmp2
  else
    cp tmp1 tmp2
  fi
  sed '/Log10SizePerBin/s/0.04/0.07/g'              <tmp2 >tmp1
  sed '/ImpDistUpperLimit/s/400/800/g'                     <tmp1 >tmp2
  sed '/Log10EaxisEnergyUpperLimit/s/6/5/g'         <tmp2 >tmp1
  #Set the number of samples for noise
  sed '/LTM_WindowSizeForNoise/s/7/'$Samples'/g' <tmp1 >tmp2
  #Replace HillasBranchName argument
  sed '/HillasBranchName "H"/s/"H"/"'$HMode'"/g'    <tmp2 >config1.tmp
fi
########################################################################

if [ -n "$GenerateEASubLTListAndConfigFiles" ]; then
  echo '##########################################################'
  echo '# GenerateEASubLTListAndConfigFiles'
  echo '##########################################################'
  cd $lcl
  date
  ListNames='EASubLT'$SPECSEA$Samples'Sample*Deg*'$CUTTYPE'List'
  rm $ListNames
  EASPEC=EA  

  # *************************************************************************
  # Generate template config file
  # Change for KASCADE and Number of samples
  # *************************************************************************
  $VEGAS/bin/makeEA  -save_config_and_exit tmp1
  sed '/EA_SimulationType/s/E_GrISU/E_KASCADE/g'       <tmp1 >tmp2
  sed '/EA_WindowSizeForNoise/s/7/'$Samples'/g'        <tmp2 >tmp1
  sed '/Azimuth      /s/Azimuth/Azimuth '$AzStr'/g'    <tmp1 >tmp2
  sed '/Zenith         /s/Zenith/Zenith '$ZNStr'/g'    <tmp2 >tmp1
  sed '/Noise         /s/Noise/Noise '$NoiseStr'/g'    <tmp1 >tmp2
  if [ "$LTWBL" != "050" ]; then
    sed '/luteOffset    /s/Offset/Offset '$ABOStr'/g' <tmp2 >config1.tmp
  else
    cp tmp2 config1.tmp
  fi
fi
########################################################################

if [ -n "$HTARPedVarZnOffsetS4ToArchive" ]; then
  echo '##########################################################'
  echo '# HTARPedVarZnOffsetS4ToArchive'
  echo '##########################################################'
  cd $lcl
  date
fi  
##############################################################################
if [ -n "$HTARPedVarZnOffsetS4FromArchive" ]; then
  echo '##########################################################'
  echo '# HTARPedVarZnOffsetS4FromArchive'
  echo '##########################################################'
  cd $lcl
  date
fi  
##############################################################################


if [ -n "$ProduceSubLookupTables" ]; then
  echo '##########################################################'
  echo '# ProduceSubLookupTables'
  echo '##########################################################'
  ########################################
  # produceLookupTables needs lots of memory
  ########################################
  MEMREQUEST=" -l mem=24GB"       #This is for pbs (Purdue)
  if [ -n "$CORI" ]; then
    MEMREQUEST=" --mem=24GB"
  fi
  date
fi  
########################################################################

if [ -n "$ProduceEASubLookupTables" ]; then
  echo '##########################################################'
  echo '# ProduceEASubLookupTables'
  echo '##########################################################'
  cd $lcl
  EASPEC=EA
  WALLTIME='-t 08:00:00'
  if [ -n "$BELL" ]; then
    WALLTIME='-l walltime=04:00:00'
  fi
  date
fi  
##############################################################################

if [ -n "$ProduceSubLookupTables" ] || [ -n "$ProduceEASubLookupTables" ]; then
  cd $lcl
  GetUniqueNumber
  ####################################################################
  # See note below in GenerateStage2FromVBF section on CORI use of Job 
  # arrays. Requires a QsubFileNameList. Setup its name here.
  # Eventual use on all clusters
  ####################################################################
  QsubFileNameList='PLT'$UNIQUE'AutoqsubList'
  echo QsubFileNameList: $QsubFileNameList
fi
#############################################################################

if [ -n "$HTARPedVarZnOffsetS2ToArchive" ]; then
  echo '##########################################################'
  echo '# HTARPedVarZnOffsetS2ToArchive'
  echo '##########################################################'
  fi
  cd $lcl
  date
fi  
##############################################################################
if [ -n "$HTARPedVarZnOffsetS2FromArchive" ]; then
  echo '##########################################################'
  echo '# HTARPedVarZnOffsetS2FromArchive'
  echo '##########################################################'
  cd $lcl
  date
fi  
##############################################################################
if [ -n "$CombineSubLT" ]; then
  echo '##########################################################'
  echo '# CombineSubLT'
  echo '##########################################################'
  cd $lcl
  date
  GenerateLTFileName $SPECCFG $SPECSEA $LTWBL  'std' $LTPARTTYPE

  SubLTList='SubLTList'
  if [ -e $lcl'/'$SubLTList ]; then
    rm $lcl'/'$SubLTList
  fi
fi
##############################################################################

if   [ -n "$CombineBuildCheckEALT" ]; then
  echo '##########################################################'
  echo '# CombineBuildCheckEALT'
  echo '##########################################################'
  cd $lcl
  date
  EASPEC=EA
  GenerateEAFileName $CUTTYPE $SPECSEA $LTWBL  'std' $TelConfig  $LTPARTTYPE

  SubLTList='EASubLTList'
  if [ -e $lcl'/'$SubLTList ]; then
    rm $lcl'/'$SubLTList                #Remove any existing EA SUBLT List file
                                        #Make a new one later.
  fi
fi
#################################################################################



##################################################################################
if [ -n "$GenerateSubLTConfigFiles" ] || [ -n "$GenerateSubLTListFiles" ]          || \
   [ -n "$$ProduceEASubLookupTables[ -n "$ProduceEASubLookupTables" ]ProduceSubLookupTables" ]   || [ -n "$HTARPedVarZnOffsetS2ToArchive" ]   || \
   [ -n "$CombineSubLT" ]             || [ -n "$HTARPedVarZnOffsetS2FromArchive" ] || \
   [ -n "$GenerateStage4FromStage2" ] || [ -n "$HTARPedVarZnOffsetS4ToArchive" ]   || \
   [ -n "$CheckStage4Files" ]         || [ -n "$HTARPedVarZnOffsetS4FromArchive" ] || \
   [ -n "$GenerateStage5Combined" ]   || [ -n "$GenerateStage5CombinedRCE" ]       || \
   [ -n "$CombineBuildCheckEALT" ]    || [ -n "$ProduceEASubLookupTables" ]        || \
   [ -n "$GenerateEASubLTListAndConfigFiles" ] || [ -n "$CheckStage5Files" ] ; then

  #****************************************************************************
  # For SubLT gen we make a seperate Sub LT config file for each of ZN Offset 
  # combination
  # we loop over Zn and Offset 
  #############################################################################

  #######################################################################
  #Zenith
  let izn=0
  let iznEnd=${#Zenith[@]}
  while test $izn -lt $iznEnd
   do
    ZN=${Zenith[$izn]}
    #echo izn: $izn' ZN: '$ZN

    ####################################################################
    # Iterate over WBL Offsets
    ####################################################################
    #Wbl Offset
    let iwbl=0
    let iwblEnd=${#WblOffset[@]}
    while test $iwbl -lt $iwblEnd
     do
      WBL=${WblOffset[$iwbl]}
      if [ "$WBL" !=  '0.0' ]; then
        WBLSPEC=S$WBL'Wbl'
      else
        WBLSPEC=$WBL'Wbl'
      fi
      if [ -n "$CheckStage4Files" ] ||  [ -n "$CheckStage5Files" ]; then
         echo $ZN':' $WBLSPEC >&2   #Print to stderr during operation to 
      fi                            #let user know things are running

      #Construct the Sub Lt or  EASubLT Config File Name. 
      #EASPEC is only defined for EA gens.
      SUBLTBASE=$EASPEC'SubLT'$SPECSEA$Samples'Sample'$ZN'Deg'$WBLSPEC$CUTTYPE

      ConfigName=$SUBLTBASE$HMode'.config'
      ListNames=$SUBLTBASE'List'
      SubLTFileName=$lcl'/'$SUBLTBASE'.root'

      #**********************************************************
      if [ -n "$GenerateSubLTConfigFiles" ] || \
         [ -n "$GenerateEASubLTListAndConfigFiles" ] ; then
        #############################################################
        #Now we need to "edit the new config file
        #############################################################
        #Figure out what the Zenith argument should look like
        if test $izn -eq "0" 
         then
          Zn=${Zenith[izn]}','${Zenith[1]}
        else
          Zn=${Zenith[0]}','${Zenith[izn]}
        fi
        #now replace Zenith argument and put in a tmp file
        sed '/Zenith/s/'$ZNStr'/'$Zn'/g' <config1.tmp >config2.tmp

        ##############################################################
        #Now do the same for the Offset but put in final file
        #If we only are doing one offset leavce AbsoluteOffset blank
        if test $iwblEnd -ne "1"   #Allows for only one WBl
         then   
          if test $iwbl -eq "0" 
           then
            W=${WblOffset[$iwbl]},${WblOffset[1]}
          else
            W=${WblOffset[0]},${WblOffset[$iwbl]}
          fi
          sed '/luteOffset/s/'$ABOStr'/'$W'/g'  <config2.tmp >$ConfigName
        else
          cp config2.tmp $ConfigName
          #   sed '/luteOffset/s/'$ABOStr'/   /g'  <config2.tmp >$ConfigName
        fi   
        #now replace Offset argument and put in a final file
        rm config2.tmp
      fi
      #********************************************************************


      if [ -n "$HTARPedVarZnOffsetS2ToArchive" ]; then
        # *****************************************************************
        # For each of ZN Az, and  Offset combination we make a tar file of all
        # the files with that Zn Az and Offset and all  PedVar (9 files)
        # #################################################################
        # Make up Pedvar .tar file name
        PedVarHTARFile='S2Zn'$ZN'Deg2D'$WBLSPEC'AllPedVarAllAz'$SPECSEA$PMT$SPECPART$SPECMDL'.tar'
        if [ "$SPECPART" = "CR" ]; then
          GenerateVBFName $SPECPART $SPECCFG $SPECSEA $ZN $AZ "-1" $SPECMDL#No wbl for CR
        else
          GenerateVBFName $SPECPART $SPECCFG $SPECSEA $ZN $AZ $WBL $SPECMDL
        fi
        BaseFileS2=${VBFFILENAME%%_az*}
        PVFILESPEC='PedVar*'$BaseFileS2'*'$WBL'wobb.root'
        SubmitHtarToArchive $ARCHIVE$SPECSEA$PMT$SPECPART$SPECMDL'PedVarS2/'$PedVarHTARFile $lcl "$PVFILESPEC"
      fi
      #*********************************************************************

      if [ -n "$HTARPedVarZnOffsetS2FromArchive" ]; then
        #*****************************************************************
        #For each of ZN Az, and  Offset combination we recover a tar file of 
        # all
        #the files with that Zn Az and Offset and all  PedVar (9 files)
        ##################################################################
        # Make up Pedvar .tar file name
        PedVarHTARFile='S2Zn'$ZN'Deg2D'$WBLSPEC'AllPedVarAllAz'$SPECSEA$PMT$SPECPART$SPECMDL

        SubmitHtarFromArchive $ARCHIVE$SPECSEA$PMT$SPECPART$SPECMDL'PedVarS2/' $lcl $PedVarHTARFile 
      fi
      #*********************************************************************

      if [ -n "$HTARPedVarZnOffsetS4ToArchive" ]; then
        #*****************************************************************
        # For each of ZN Az, and  Offset combination we make a tar file of all
        # the files with that Zn Az and Offset and all  PedVar (9 files)
        ##################################################################
        # Make up Pedvar .tar file name
        if [ ! -d "$ARCHIVE$SPECSEA$PMT$SPECPART$SPECMDL'PedVarS4'" ]; then
          #Check Archive Directory exists 
          hsi 'cd '$ARCHIVE'; mkdir '$SPECSEA$PMT$SPECPART$SPECMDL'PedVarS4;'
          echo $ARCHIVE$SPECSEA$PMT$SPECPART$SPECMDL'PedVarS4' ;
        fi
        PedVarHTARFile='S4Zn'$ZN'Deg2D'$WBLSPEC'AllPedVarAllAz'$SPECSEA$PMT$SPECPART$SPECMDL$CUTTYPE'Cuts'
        if [ -n "$TELCUT" ]; then
          PedVarHTARFile=$PedVarHTARFile$TelConfig
        fi

        PedVarHTARFile=$PedVarHTARFile'.tar'

        PVFILESPEC='PedVar*'$SPECSEA$PMT$SPECPART$SPECMDL$ZN{D,_45D,_90D,_135D,_180D,_225D,_270D,_315D}'eg2D'$WBLSPEC$SPECTHR$TelConfig'M2'$CUTTYPE'Cuts.root'
              
        SubmitHtarToArchive $ARCHIVE$SPECSEA$PMT$SPECPART$SPECMDL'PedVarS4/'$PedVarHTARFile $lcl "$PVFILESPEC"
      fi
      #*********************************************************************

      if [ -n "$HTARPedVarZnOffsetS4FromArchive" ]; then
        #*****************************************************************
        # For each of ZN Az, and  Offset combination we recover a tar file of all
        # the files with that Zn Az and Offset and all  PedVar (9 files)
        ##################################################################
        # Make up Pedvar .tar file name
        PedVarHTARFile='S4Zn'$ZN'Deg2D'$WBLSPEC'AllPedVarAllAz'$SPECSEA$PMT$SPECPART$SPECMDL$CUTTYPE'Cuts'
        if [ -n "$TELCUT" }; then
          PedVarHTARFile=$PedVarHTARFile$TelConfig
        fi
           
        SubmitHtarFromArchive $ARCHIVE$SPECSEA$PMT$SPECPART$SPECMDL'PedVarS4/' $lcl $PedVarHTARFile 
      fi
      #*********************************************************************
       
      if [ -n "$ProduceSubLookupTables" ]; then
        ####################################################################
        # Make up the job script file that will be submitted below
        ###################################################################
        BuildProduceLTOrEALookupTables 'produce_lookuptables' $SUBLTBASE $ConfigName $LTCUTSFILE $ListNames $SubLTFileName
      fi
      #*******************************************************************

      if [ -n "$ProduceEASubLookupTables" ]; then
        ####################################################################
        # Make up the job script file that will be submitted below
        ###################################################################
        BuildProduceLTOrEALookupTables 'makeEA' $SUBLTBASE $ConfigName $EACUTSFILE $ListNames $SubLTFileName
      fi
      #*******************************************************************


      if [ -n "$CombineSubLT" ] || [ -n "$CombineBuildCheckEALT" ] ; then
        #*******************************************************************
        # Create the List of all SubLT files to be combined
        #******************************************************************
        # But first check that each file does exixt. We may want to test deeper
        # if this isn't enough later.
        ###################################################################
        if  [ ! -e "$SubLTFileName" ]; then
          echo 'VAAuto: Fatal--SubLT file '$SubLTFileName ' dose not exist'
          exit
        fi
        echo $SubLTFileName >>$lcl'/'$SubLTList
      fi
      #*******************************************************************

      #Azimuth
      let kaz=0
      let kazEnd=${#Azimuth[@]}
      while test $kaz -lt $kazEnd
       do
        #Make   Zn_Az string
        AZ=${Azimuth[$kaz]}
        if test $AZ = "0"
         then
          ZnAz=$ZN'Deg'
        else
          ZnAz=$ZN'_'$AZ'Deg'
        fi
        # #########################################################################
        # Generate the VBF file name(no path yet)
        # #########################################################################
        if [ "$SPECPART" = "CR" ]; then
          GenerateVBFName $SPECPART $SPECCFG $SPECSEA $ZN $AZ "-1" $SPECMDL #No wbl for CR
        else
          GenerateVBFName $SPECPART $SPECCFG $SPECSEA $ZN $AZ $WBL $SPECMDL
        fi
        BaseFileS2=${VBFFILENAME%%.vbf}
        BaseFileS4S5=$BaseFileS2$CUTTYPE'Cuts'
        #Iterate through PedVars
        let kpvar=0
        while test $kpvar -lt $kpvarEnd
         do   
          PV=${PedVar[$kpvar]} 
          PedVarFile=Ped$SPECCFG$PV
          PedVarName=PedVar$PV
          PedVarS2FileName=$PedVarName'_'$BaseFileS2'.root'
          PedVarS4FileName=$PedVarName'_'$BaseFileS4S5'.root'
          #********************************************************************
           
          if [ -n "$GenerateSubLTListFiles" ]; then
            # echo '##########################################################'
            # echo '# GenerateSubLTListFiles'
            # echo '##########################################################'
            cd $lcl
            echo $PedVarS2FileName >>$ListNames
          fi
          #******************************************************************

          if [ -n "$GenerateEASubLTListAndConfigFiles" ]; then
            cd $lcl
            echo $PedVarS4FileName >>$ListNames
          fi
          #******************************************************************

          # ***********************************************************************
          #  GenerateStage4FromStage2 and CheckStage4Files also need SimSet interation
          #  is applicable
          # ***********************************************************************
            #echo At 1
          if [ -n "$GenerateStage4FromStage2" ] ||  [ -n "$CheckStage4Files" ] || \
             [ -n "$GenerateStage5Combined" ] || [ -n "$GenerateStage5CombinedRCE" ]  || \
             [ -n "$CheckStage5Files" ]; 
           then
            # ###############################################################
            # Due to size and walltime limits we have divided the each CR vbf 
            # file into a number of seperate files. This is implimented by adding 
            # a SimSet number at the end of the VBF file name ( just before the 
            # .vbf extention). This program does this by using the SimSetStary\t and 
            # SimSetEnd symbols. If they are both defined the we are using SimSets
            # ################################################################
	    if [ -n "$SimSetStart" ] && [ -n "$SimSetEnd" ]; then
              let sSimSetNum=$SimSetStart
              let sSimSetEnd=$SimSetEnd
            else
              let sSimSetNum=0 
              let sSimSetEnd=0
            fi
            echo sSimSetEnd: $sSimSetEnd
	    while test $sSimSetNum -le $sSimSetEnd  #Loop only once for not simsets.
             do
              if [ "$sSimSetNum" = "0" ]; then
                VBFNM=$VBFFILENAME
                BFNMS2=$BaseFileS2
              else
                VBFNM=${VBFFILENAME%%.vbf}$sSimSetNum'.vbf'
                BFNMS2=$BaseFileS2$sSimSetNum
             fi

              #echo PedVarFile:  $PedVarFile 
              #echo BaseFileS2: $BFNMS2 
              #echo VBFFILENAME: $VBFNM
              

              if [ -n "$GenerateStage4FromStage2" ]; then
                #echo '##########################################################'
                #echo '# GenerateStage4FromStage2 '             
                #echo '##########################################################'
                cd $lcl

                ###################################################
                # Note here that VeagsSimProductionS4.scr will generate the PedVar 
                # Stage4 filename with the correct TelConfig and cuts
                # the VSProdFile has that info added to it with sed commands.
                ###################################################

                if [ -n "$CORI" ]  && [ -n "$JobArray" ]; then
                  #######################################################
                  # BuildVegasSimProduction will create the .pbs file and add
                  # that file's name to the QsubFileNameList  file
                  # The actual submision of the job array job to the cluster and 
                  # moidification of the .pbs job to be a job array job is done 
                  # later in this script.
                  #########################################################
                  # Check that the stage2 root file exists in the lcl directory
                  #Generate the root file name
		  ROOTFILE=PedVar$PV'_'$BFNMS2'.root'
		  if [ ! -e "$ROOTFILE" ]; then
                    echo 'VAAuto--Fatal: Stage2 root file '$ROOTFILE' does not exist'\
                         'in lcl.'
                    echo VAAuto--Job Array submission aborted.
                    exit
                  fi

                  BuildVegasSimProduction $PedVarFile $BFNMS2 $VSProdFile $VBFNM 
                else
                  SubmitVegasSimProduction $PedVarFile $BFNMS2 $VSProdFile $VBFNM
                fi
              fi
              ###############################################################
                
              if [ -n "$CheckStage4Files" ]; then
                # echo '##########################################################'
                # echo '# CheckStage4Files'
                # echo '##########################################################'
                cd $lcl
                BaseFileS4=$BFNMS2$CUTTYPE'Cuts'
                PedVarS4FileName=$PedVarName'_'$BaseFileS4'.root'
                #echo BaseFileS4:     $BaseFileS4
                echo PedVarS4FileName: $PedVarS4FileName

                #Check VARootIO can open and load S4 file 
                Arg=$lcl'/CheckStage4FileOK.C("'$PedVarS4FileName'")'
               
                root -q -b -q $Arg >CheckStage4OK.log
                 {
                  read BAD
                  let suzBAD="$BAD"
                  if [ "$suzBAD" -lt "15" ]; then
                    echo "Bad: "$PedVarS4FileName
                    echo "Reason: "$suzBAD
                    if [ $suzBAD -eq "1" ]; then
                      echo "VAAuto: VARootIO failed to open" 
                    fi
                    #if [ $suzBAD -eq "2" ]; then
                    #  echo "VAAuto: VARootIO failed to load"
                    #fi
                    if [ $suzBAD -eq "3" ]; then
                      echo "VAAuto: VARootIO failed to find ShowerDataTree"
                    fi
                  fi 
                   
                 } <CheckStage4FileOK.Result
                 #rm CheckStage4FileOK.Resultq
              fi
              
              ##########################################################
               
              if [ -n "$GenerateStage5Combined" ] || \
                 [ -n "$GenerateStage5CombinedRCE" ]; then

	        cd $lcl
                date
            
                # OK! We are ready to run.  Need to submit since this will take more 
                # than 1 hours or we have many to run in parallel 
                #Cori  values
                WALLTIME='-t 20:00:00'
                MEMREQUEST=" --mem=2GB"
	
		#Bell values
                if [ -n "$BELL" ]; then
                  WALLTIME='-l walltime= 20:00:00'
                  MEMREQUEST=" -l mem=5GB"
                fi

                echo 'VAAuto: Submitting '$VSProdFile ' to '$QUEUE' queue for '\
                     $lcl'/'$VBFNM 
               
                ###################################################
                # Note here that VeagsSimProductionS5.scr will generate the PedVar 
                # Stage4 filename with the correct TelConfig and Cuts. 
                # the VSProdFile has that info added to it with sed commands.
                # Also the VSProdFile will generate an Output file using  correct 
                # OutputFileNameExt
                ###################################################
                if [ -n "$CORI" ] && [ -n "$JobArray" ]; then
                  #######################################################
                  # BuildVegasSimProduction will create the .pbs file and add
                  # that file's name to the QsubFileNameList  file
                  # The actual submision of the job array job to the cluster and 
                  # modification of the .pbs job to be a job array job is done 
                  # later in this script.
                  #########################################################
                  # Check that the stage4 root file exists in the lcl directory
                  #Generate the root file name
		  ROOTFILE=PedVar$PV'_'$BFNMS2$CUTBASE'Cuts.root'
		  if [ ! -e "$ROOTFILE" ]; then
                    echo 'VAAuto--Fatal: Stage4 root file '$ROOTFILE' does not exist'\
                         'in lcl.'
                    echo VAAuto--Job Array submission aborted.
                    exit
                  fi
                  BuildVegasSimProduction $PedVarFile $BFNMS2 $VSProdFile $VBFNM 
                else
                  SubmitVegasSimProduction $PedVarFile $BFNMS2 $VSProdFile $VBFNM
                fi
             
              fi  #End of GenerateStage5Combined and GenerateStage5CombinedRCE
              # ###############################################################
              
              if [ -n "$CheckStage5Files" ]; then
                # echo '##########################################################'
                # echo '# CheckStage5Files'
                # echo '##########################################################'
                cd $lcl
                BaseFileS5=$BFNMS2$CUTTYPE'Cuts'
                PedVarS5FileName=$PedVarName'_'$BaseFileS5'.root'
                #echo BaseFileS5:     $BaseFileS5
                echo PedVarS5FileName: $PedVarS5FileName

                #Check VARootIO can open and load S5 file 
                Arg=$lcl'/CheckStage5FileOK.C("'$PedVarS5FileName'")'
               
                root -q -b -q $Arg >CheckStage5OK.log
                 {
                  read BAD
                  let suzBAD="$BAD"
                  if [ "$suzBAD" -lt "15" ]; then
                    echo "Bad: "$PedVarS5FileName
                    echo "Reason: "$suzBAD
                    if [ $suzBAD -eq "1" ]; then
                      echo "VAAuto: VARootIO failed to open" 
                    fi
                    #if [ $suzBAD -eq "2" ]; then
                    #  echo "VAAuto: VARootIO failed to load"
                    #fi
                    if [ $suzBAD -eq "3" ]; then
                      echo "VAAuto: VARootIO failed to find ShowerDataTree"
                    fi
                    if [ $suzBAD -eq "6" ]; then
                      echo "VAAuto: VARootIO failed to find CombinedEventsTree"
                    fi
                  fi 
                   
                 } <CheckStage5FileOK.Result
                 #rm CheckStage5FileOK.Resultq
              fi
              
              ##########################################################
                      
              sSimSetNum=$((sSimSetNum + 1))
	      #echo sSimSetNumA: $sSimSetNum
             done        #End of SimSet while loop
          fi           

          ###############################################################
          let kpvar=$kpvar+1
         done                 #End of Pedvar while loop
        kaz=$((kaz+1))
        echo kaz: $kaz
       done                   #End of Axzimuth while loop
      let iwbl=$iwbl+1
      echo iwbl: $iwbl
     done                     #End of wbl (offset) while loop
    izn=$((izn+1))
    echo izn: $izn
   done                       #End of zenith while loop
fi
###########################################################################

if [ -n "$GenerateStage4FromStage2" ] || [ -n "$GenerateStage5Combined" ] || \
   [ -n "$GenerateStage5CombinedRCE" ]; then
        
  ######################################################################
  # For NERSC (Cori ) JobArray only: generate Jobarry .pbs file for 
  # all the .pbs jobs in the qsubFileNameList.
  # SubmitQsubJobs is in UtilityFuncitons.scr
  ######################################################################
  if [[ -n "$CORI" ] && [ -n "$JobArray" ]; then
    SubmitQsubJobs  $QsubFileNameList
  fi
fi
##########################################################################

########################################################################

if [ -n "$ProduceSubLookupTables" ] || [ -n "$ProduceEASubLookupTables" ]; then
  #echo '##########################################################'
  #echo '#  ProduceSubLookupTables or ProduceEASubLookupTables
  #echo '##########################################################'
        
  ######################################################################
  # For Cori JobArray only: generate Jobarry .pbs file for all the .pbs jobs
  # in the qsubFileNameList. SubmitQsubJobs is in UtilityFuncitons.scr
  ######################################################################
  if [ -n "$CORI" ] && [ -n "$JobArray" ]; then
    SubmitQsubJobs  $QsubFileNameList
  else
    echo 'VAAuto:: Failed to submit job arry. Bad cluster chosen?'
    exit
  fi
fi
##########################################################################

if [ -n "$CombineSubLT" ] ||  [ -n "$CombineDispSubLT" ]; then
  cd $VEGASBASE/showerReconstruction2/macros
  if [ ! -e "combineKASCADE_LT.C" ]; then
    cp $KASCADEBASE'/scripts/combineKASCADE_LT.C' .
  fi
   
  if [ -e "$LTFILENAME" ]; then
    rm -v $LTFILENAME
  fi

   ###########################################################################
   #This file is loaded by the rootlogon.C in showerReconstruction2/macros

   ~/Switch.rootrcTo.rootrc_showerReconstruction2 
   #############################################################################
   # Make up a script to run on root batch mode
   #############################################################################
   Script='CombineSubLT.C'
   echo '{'                                                          >$Script
   echo '  combineFromList("'$lcl'/'$SubLTList'","'$LTFILENAME'");' >>$Script
   echo '}'                                                         >>$Script
    
   echo "VAAuto: Running root batch command to Create LT file:"
   echo "VAAuto: "$LTFILENAME
   echo 'VAAuto: Do: less CombineSubLT.log,  then shift-F to see when it finishes!'
    
   if [ -n "$CombineSubLT" ]; then
     root -b -q $Script '>'$lcl'/CombineSubLT.log'
   fi

   if  [ -n "$CombineDispSubLT" ]; then
     #The combine Disp takes too long to run interactivly, so submit it.
     #build a submission .pbs file
     sgeFile=$lcl'/CmbDisp.pbs'
     echo "#"PBS -q $QUEUE                                           >$sgeFile
     echo "#"PBS $WALLTIME                                         >>$sgeFile
     echo "#PBS "$MEMREQUEST                                     >>$sgeFile
     if [ -n "$PURDUE" ]; then  
       echo source /etc/profile                                   >>$sgeFile
       echo module load gcc/5.2.0                                 >>$sgeFile
     fi
         
     echo cd $VEGASBASE/showerReconstruction2/macros                    >>$sgeFile
     echo root -b -q $Script '>'$lcl'/CombineSubLT.log'             >>$sgeFile
         
     chmod 700 $sgeFile
     $QSUB$QSUBEXT -e $lcl'/CmbDisp.pbs.err' -o $lcl'/CmfDisp.pbs.log' $sgeFile >CmbDisp.log
  fi
fi
#**********************************************************************

if [ -n "$CombineBuildCheckEALT" ]; then
  cd $VEGASBASE/resultsExtractor/macros
  if [ ! -e "combineEAKascade.C" ]; then
    cp $KASCADEBASE'/scripts/combineEAKascade.C' .
  fi
  ###########################################################################
  # This file is loaded by the rootlogon.C in showerReconstruction2/macros
  ###########################################################################    
  ~/Switch.rootrcTo.rootrc_resultsExtractor.scr
    
  #############################################################################
  # Make up a script to run on root batch mode
  #############################################################################
  Script='CombineEASubLT.C'
  echo '{'                                                             >$Script
  echo ' gROOT->LoadMacro("combineEAKascade.C");'                       >>$Script
  #echo '  combineEAKFromList("'$lcl'/'$SubLTList'","'$EAFILENAME'");' >>$Script
  echo '  combineEAKFromList("'$lcl'/'$SubLTList'","'$lcl'/'$EAFILENAME'");' >>$Script
  echo '}'                                                            >>$Script
    
  echo "VAAuto: Running root batch command to Create EA file:"
  #echo "VAAuto: "$EAFILENAME
  echo "VAAuto: "$lcl'/'$EAFILENAME
  echo "VAAuto: Do: less CombineEASubLT.log;  then shift-F to see when it  finishes!"
    
  root -b -q $Script '>'$lcl'/CombineEASubLT.log'  
fi
#############################################################################

if [ -n "$BuildLTTree" ]; then
  echo '##########################################################'
  echo '# BuildLTTree'
  echo '##########################################################'
  GenerateLTFileName $SPECCFG $SPECSEA $LTWBL  'std' $LTPARTTYPE
    
  cd $VEGASBASE/showerReconstruction2/macros
  date
  BUILDCONFIG='BuildTree'$Samples'Samples.config' 
  $VEGAS/bin/buildLTTree -save_config_and_exit tmp1
    
  #**********************************************************************
  #Modify for this LT: except for TelID we need at least 2 to change things
  #**********************************************************************
  sed '/TelID        /s/TelID/TelID  '$TelIDStr'/g' <tmp1 >tmp2
  let iznEnd=${#Zenith[@]}
  let kazEnd=${#Azimuth[@]}
  let kpvarEnd=${#PedVar[@]}
                
  INFILE=tmp2
  OUTFILE=tmp1
                
  if [ ${#Azimuth[@]} -gt "1" ]; then
    sed '/Azimuth      /s/Azimuth/Azimuth '$AzStr'/g' <$INFILE >$OUTFILE
    TMPFILE=$INFILE
    INFILE=$OUTFILE
    OUTFILE=$TMPFILE
  fi
   
  if [ ${#Zenith[@]} -gt "1" ]; then
    echo ZNStr: $ZNStr
    sed '/Zenith         /s/Zenith/Zenith '$ZNStr'/g' <$INFILE >$OUTFILE
    TMPFILE=$INFILE
    INFILE=$OUTFILE
    OUTFILE=$TMPFILE
  fi
  if [ ${#PedVar[@]} -gt "1" ]; then
    sed '/Noise         /s/Noise/Noise '$NoiseStr'/g' <$INFILE >$OUTFILE
    TMPFILE=$INFILE
    INFILE=$OUTFILE
    OUTFILE=$TMPFILE
  fi

  #This should allow for single offset in which case the AbsoluteOFffset is left 
  #blank in the buildTree config file.
   if [ ${#WblOffset[@]} -gt "1" ]; then
     sed '/luteOffset    /s/Offset/Offset '$ABOStr'/g' <$INFILE >$OUTFILE
     TMPFILE=$INFILE
     INFILE=$OUTFILE
     OUTFILE=$TMPFILE
   fi
   mv $INFILE $BUILDCONFIG

   $VEGAS/bin/buildLTTree '-config='$BUILDCONFIG $LTFILENAME 
fi
##############################################################################

if [ -n "$BuildDispLTTree" ]; then
  echo '##########################################################'
  echo '# BuildDispLTTree'
  echo '##########################################################'
  cd $VEGASBASE/showerReconstruction2/macros
  date
  GenerateLTFileName $SPECCFG $SPECSEA $LTWBL  'std' $LTPARTTYPE
  LTFILENAME='dt'${LTFILENAME#lt}                 #Convert to a dt filename

  BUILDCONFIG='BuildDispTree'$Samples'Samples.config' 
  $VEGAS/bin/buildDispTree -save_config_and_exit tmp1
  #**********************************************************************
  # Modify for this Disp LT: we need at least 2 to change things
  #**********************************************************************
  let iznEnd=${#Zenith[@]}
  let kazEnd=${#Azimuth[@]}
  let kpvarEnd=${#PedVar[@]}

  INFILE=tmp1
  OUTFILE=tmp2

  if [ ${#Azimuth[@]} -gt "1" ]; then
    sed '/DTM_Azimuth      /s/Azimuth/Azimuth '$AzStr'/g' <$INFILE >$OUTFILE
    TMPFILE=$INFILE
    INFILE=$OUTFILE
    OUTFILE=$TMPFILE
  fi
   
  if [ ${#Zenith[@]} -gt "1" ]; then
    sed '/DTM_Zenith         /s/Zenith/Zenith '$ZNStr'/g' <$INFILE >$OUTFILE
    TMPFILE=$INFILE
    INFILE=$OUTFILE
    OUTFILE=$TMPFILE
  fi
  if [ ${#PedVar[@]} -gt "1" ]; then
    sed '/DTM_Noise         /s/Noise/Noise '$NoiseStr'/g' <$INFILE >$OUTFILE
    TMPFILE=$INFILE
    INFILE=$OUTFILE
    OUTFILE=$TMPFILE
  fi
  sed '/DTM_Width         /s/Width/Width '$DTMWidthStr'/g'    <$INFILE >$OUTFILE
  sed '/DTM_Length        /s/Length/Length '$DTMLengthStr'/g' <$OUTFILE >$BUILDCONFIG

  $VEGAS/bin/buildDispTree '-config='$BUILDCONFIG $LTFILENAME 
fi
#################################################################################

if [ -n "$CombineBuildCheckEALT" ]; then
   echo '##########################################################'
   echo '# CombineBuildCheckEALT '
   echo '##########################################################'
   EASPEC=EA

   date
   #**********************************************************************
   # Modify for this EA: except for TelID we need at least 2 to change things
   #**********************************************************************

   if [ ${#Azimuth[@]} -gt "1" ]; then
      StrAz='-Azimuth='$AzStr
   fi
   if [ ${#Zenith[@]} -gt "1" ]; then
      StrZn='-Zenith='$ZNStr
   fi
   if [ ${#PedVar[@]} -gt "1" ]; then
      StrNoise='-Noise='$NoiseStr
   fi
   if [ ${#WblOffset[@]} -gt "1" ]; then
      StrOffset='-AbsoluteOffset='$ABOStr
   fi

   cd $VEGASBASE/resultsExtractor/macros

   #if [ ! -e "$EAFILENAME" ]; then
   #   echo 'VAAuto:  Fatal--EA file '$VEGASBASE'/resultsExtractor/macros/'$EAFILENAME 'does not exist!'
   #   exit
   #fi
   if [ ! -e "$lcl"'/'"$EAFILENAME" ]; then
      echo 'VAAuto:  Fatal--EA file '$lcl'/'$EAFILENAME 'does not exist!'
      exit
   fi
#   $VEGAS/bin/buildEATree \
#       $StrAz \
#       $StrZn \
#       $StrNoise \
#       $StrOffset \
#       $EAFILENAME
   $VEGAS/bin/buildEATree \
       $StrAz \
       $StrZn \
       $StrNoise \
       $StrOffset \
       $lcl'/'$EAFILENAME

   echo '##########################################################'
   echo '# CheckEA  '
   echo '##########################################################'
   LCL=$PWD

   EADIAGFILENAME=${EAFILENAME/root/diag}
   date
   ###########################################################################
   # Make up a script to run on root batch mode
   ###########################################################################
   cd $VEGASBASE'/resultsExtractor/macros'
   Script='CheckEA.C'
   echo '{'                                                           >$Script
   #echo '  eaValidator2("'$VEGASBASE'/resultsExtractor/macros/'$EAFILENAME'");' >>$Script
   echo '  eaValidator2("'$lcl'/'$EAFILENAME'");'                    >>$Script
   echo '}'                                                          >>$Script

   #if [ ! -e "$VEGASBASE"/resultsExtractor/macros/"$EAFILENAME" ]; then
      #echo 'VAAuto:  Fatal--EA file '$VEGASBASE'/resultsExtractor/macros/'$EAFILENAME 'does not exist!'
      #exit
   #fi
   if [ ! -e "$lcl"'/'"$EAFILENAME" ]; then
      echo 'VAAuto:  Fatal--EA file '$lcl'/'$EAFILENAME 'does not exist!'
      exit
   fi
   echo "VAAuto: Running root batch command to Check EA file:"
   echo "VAAuto: "$lcl'/'$EAFILENAME

   ~/Switch.rootrcTo.rootrc_resultsExtractor.scr
   rm $lcl'/'$EADIAGFILENAME
   
   root -b -q $Script '>'$lcl'/CheckEA.log'  
   
   if [ -e  "$lcl"'/'"$EADIAGFILENAME" ]; then
       cat  $lcl'/'$EADIAGFILENAME >>$lcl'/CheckEA.log'
   fi

   ~/Switch.rootrcTo.rootrc_glenn.scr
   cd $LCL
fi
# ***********************************************************************

if [ -n "$CheckLT" ]; then
   echo '##########################################################'
   echo '# CheckLT'
   echo '##########################################################'
   LCL=$PWD
   cd $VEGASBASE/showerReconstruction2/macros
   GenerateLTFileName $SPECCFG $SPECSEA $LTWBL  'std' $LTPARTTYPE
   LTDiagFileName=${LTFILENAME%.root}'.diag'

   date
   #############################################################################
   # Make up a script to run on root batch mode
   #############################################################################
   Script='CheckLT.C'
   echo '{'                                                          >$Script
   echo '  ltValidator2("'$LTFILENAME'");'                          >>$Script
   echo '}'                                                         >>$Script

   echo "VAAuto: Running root batch command to Check  LT file:"
   echo "VAAuto: "$LTFILENAME

   ~/Switch.rootrcTo.rootrc_showerReconstruction2
   rm $LTDiagFileName
   
   root -b -q $Script '>'$lcl'/CheckLT.log'  
   
   if [ -e  "$LTDiagFileName" ]; then
       cat  $LTDiagFileName >>$lcl'/CheckLT.log'
   fi

   ~/Switch.rootrcTo.rootrc_glenn.scr
   cd $LCL
fi
##################################################################################

if [ -n "$CheckDispLT" ]; then
   echo '##########################################################'
   echo '# CheckDispLT'
   echo '##########################################################'
   LCL=$PWD
   cd $VEGASBASE/showerReconstruction2/macros
   GenerateLTFileName $SPECCFG $SPECSEA $LTWBL  'std' $LTPARTTYPE
  
   DTFILENAME=dt${LTFILENAME#lt}           #Convert to a dt filename
   DTDiagFileName=${DTFILENAME%.root}'.diag'
   date
   #############################################################################
   # Make up a script to run on root batch mode
   #############################################################################
   Script='CheckDispLT.C'
   echo '{'                                                          >$Script
   echo '  dtValidator2("'$DTFILENAME'");'                          >>$Script
   echo '}'                                                         >>$Script

   echo "VAAuto: Running root batch command to Check DT file:"
   echo "VAAuto: "$DTFILENAME

   ~/Switch.rootrcTo.rootrc_showerReconstruction2
   rm $DTDiagFileName
   
   root -b -q $Script '>'$lcl'/CheckDispLT.log'  
   
   if [ -e  "$DTDiagFileName" ]; then
       cat  $DTDiagFileName >>$lcl'/CheckDispLT.log'
   fi

   ~/Switch.rootrcTo.rootrc_glenn.scr
   cd $LCL
fi
# ***********************************************************************

#if [ -n "$CombineBuildCheckEALT" ]; then
#   echo '##########################################################'
#   echo '# CheckEA'
#   echo '##########################################################'
#   LCL=$PWD
#
#   GenerateEAFileName $CUTTYPE $SPECSEA $LTWBL  'std' $TelConfig  $LTPARTTYPE
#  
#   EADIAGFILENAME=${EAFILENAME/root/diag}
#   date
#   #############################################################################
#   # Make up a script to run on root batch mode
#   #############################################################################
#   cd $VEGASBASE'/resultsExtractor/macros'
#   Script='CheckEA.C'
#   echo '{'                                                          >$Script
#   echo '  eaValidator2("'$VEGASBASE'/resultsExtractor/macros/'$EAFILENAME'");'  >>$Script
#   echo '}'                                                         >>$Script###
##
#   if [ ! -e "$VEGASBASE"/resultsExtractor/macros/"$EAFILENAME" ]; then
#      echo 'VAAuto:  Fatal--EA file '$VEGASBASE'/resultsExtractor/macros/'$EAFILENAME# 'does not exist!'
#      exit
#   fi
#   echo "VAAuto: Running root batch command to Check EA file:"
#   echo "VAAuto: "$EAFILENAME#
#
#   ~/Switch.rootrcTo.rootrc_resultsExtractor.scr
#   rm $EADIAGFILENAME
#   
#   root -b -q $Script '>'$lcl'/CheckEA.log'  
#   
#   if [ -e  "$EADIAGFILENAME" ]; then
#       cat  $EADIAGFILENAME >>$lcl'/CheckEA.log'
#   fi
#
#   ~/Switch.rootrcTo.rootrc_glenn.scr
#   cd $LCL
#fi

   
#################################################################################
echo 'VAAuto: All done at' $(date)'!'

