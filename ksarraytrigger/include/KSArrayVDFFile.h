/**
 * Original Author: Glenn H. Sembroski
 * $Author$
 * $Date$
 * $Revision$
 * $Tag$
 *
 **/
#ifndef KSArrayVDFFile_H
#define KSArrayVDFFile_H


#include <stdlib.h>
#include <iostream>
#include <vector>
#include <string>
#include <cmath>

#include "VAVDF.h"
#include "VAArrayInfo.h"
#include "VARootIO.h"
#include "VAPixelStatusData.h"
#include "VAQStatsData.h"
#include "VARelGainData.h"
#include "VATime.h"

#include "KSCommon.h"
#include "KSTelescope.h"
#include "KSArrayTriggerDataIn.h"


class KSArrayVDFFile
// *******************************************************
// ** File class for creating and filling records for VERITAS
// ** Array VDF files for multi telescope runs
// *******************************************************
{
 public:
  KSArrayVDFFile(std::vector< KSTelescope* >& pArray,
		   VATime& startTime);
  virtual ~KSArrayVDFFile();
  void CreateVDFFile(string fFileName);
  VAVDF* getVDFFilePtr(){return pfOut;};
  VACalibratedArrayEvent* getCalibratedArrayEventPtr()
                                {return pfOut->getCalibratedArrayEventPtr();};
  void writeCalibratedArrayEvent(int fNumTrigTels)
                    {pfOut->writeCalibratedArrayEvent(fNumTrigTels); return;};

  VAKascadeSimulationData* getKascadeSimulationDataPtr()
                                                    {return pfVDFKSimEvent;};
  void writeSimulationData(){pfOut->writeSimulationData(); return;};

  void FillRunHeader(int& runNumber);
  void FillAndWriteQStatsData();
  void FillAndWriteRelGainsData();
  void FillAndWritePixelStatusData();
  void FillAndWriteSimulationHeader();
  void CreateKascadeSimulationDataEventTree();

  //VAArrayInfo* GetArrayInfoPtr(){return pfArrayInfo;};

 private:
  std::vector< KSTelescope* > pfArray;
  VAVDF* pfOut;
  int fNumTels;

  VAArrayInfo* pfArrayInfo;
  VAQStatsData* pfQStats;
  VARelGainData* pfRelGainData;
  VAPixelStatusData* pfPixelStatus;
  VAKascadeSimulationData* pfVDFKSimEvent;
  TTree* pfVDFSimulationEventTree;


  int fNumPixels;
  VATime fStartTime;
  VATime fEndTime;
  int fTelID;
  int fNumWindowSamples;
  KSCameraTypes fCameraType;

};
// ***************************************************************************


#endif
