#include <vector>
#include <math.h>
void paramGen(string PSF, double gain1,double gain2, double gain3,double gain4,
	      double rise=3.2, double fall=8.5)
{
  double noise1=0;
  double noise2=0;
  double noise3=0;
  double noise4=0;

  double thresh1=0;
  double thresh2=0;
  double thresh3=0;
  double thresh4=0;

  double eff1=0;
  double eff2=0;
  double eff3=0;
  double eff4=0;

  double noiseSlope=0;
  double noiseIntercept=0;
  double threshSlope=0;
  double threshIntercept=0;
  double TSizeSlope=0;    //Determine threshold from size
  double TSizeInter=0;

  vector < double > effSlope;
  vector < double > effIntercept;
  effSlope.resize(4,0);
  effIntercept.resize(4,0);

  if(PSF=="V230"){
    cout<<"V230:3.2/8.5"<<endl;
    noiseSlope=1.565;
    noiseIntercept=.484;
    TSizeSlope=164.3;
    TSizeInter=-134.6.;

    effSlope.at(0)=866.7;
    effSlope.at(1)=1064.5;
    effSlope.at(2)=1200;
    effSlope.at(3)=979.7;

    effIntercept.at(0)=-73.7;
    effIntercept.at(1)=-93.4;
    effIntercept.at(2)=-117;
    effIntercept.at(3)=-89.2;
  }
  else if(PSF=="V290"){
    cout<<"V290:3.2/8.5"<<endl;
    noiseSlope=.638;
    noiseIntercept=-.307;
    threshSlope=5.673;
    threshIntercept=5.105;
    TSizeSlope=122.;
    TSizeInter=-71.;

    effSlope.at(0)=871.4;
    effSlope.at(1)=1033.3;
    effSlope.at(2)=910.;
    effSlope.at(3)=950.;

    effIntercept.at(0)=72.4;
    effIntercept.at(1)=73.0;
    effIntercept.at(2)=40.2;
    effIntercept.at(3)=80.;
  }

  else if(PSF=="V250"){
    cout<<"V250:3.2/8.5"<<endl;
    noiseSlope=.64;
    noiseIntercept=-.33;
    threshSlope=5.556;
    threshIntercept=2.336;
    TSizeSlope=842.1;
    TSizeInter=-1190.;

    effSlope.at(0)=1051.6;
    effSlope.at(1)=1170.9;
    effSlope.at(2)=1100.;
    effSlope.at(3)=1103.2;

    effIntercept.at(0)=61.7;
    effIntercept.at(1)=63.7;
    effIntercept.at(2)=52.5;
    effIntercept.at(3)=68.4;
  }

  else if(PSF=="V330"){
    cout<<"V330:3.2/8.5"<<endl;
    noiseSlope=.631;
    noiseIntercept=-.277;
    threshSlope=5.2;
    threshIntercept=1.8;
    TSizeSlope=130.;
    TSizeInter=-76.;

    effSlope.at(0)=942.9;
    effSlope.at(1)=1350.0;
    effSlope.at(2)=1216.7;
    effSlope.at(3)=1000.0;

    effIntercept.at(0)=94.3;
    effIntercept.at(1)=145.5;
    effIntercept.at(2)=112.7;
    effIntercept.at(3)=95.;
  }   
  else{
    if(rise==3.6 && fall==9.0){
    
      cout<<"V190:3.6/9.0:"<<endl;
   
      noiseSlope=.595;
      noiseIntercept=-.218;
    
      threshSlope=6.19;
      threshIntercept=6.54;

      effSlope.at(0)=1097.;
      effSlope.at(1)=1450.;
      effSlope.at(2)=1270.;
      effSlope.at(3)=1217.;
      
      effIntercept.at(0)=94.;
      effIntercept.at(1)=121.;
      effIntercept.at(2)=91.;
      effIntercept.at(3)=116.;
    }
    else{
      //First noise from pedvars from data.3.2/8.5
    
      cout<<"V190:3.2/8.5:"<<endl;
      noiseSlope=.63;
      noiseIntercept=-.26;
  
      threshSlope=5.2;
      threshIntercept=0.0;
    
      effSlope.at(0)=1000.;
      effSlope.at(1)=1200.;
      effSlope.at(2)=1240.;
      effSlope.at(3)=1062.;
      
      effIntercept.at(0)=75.;
      effIntercept.at(1)=90.;
      effIntercept.at(2)=82.;
      effIntercept.at(3)=79.;
    }
  } 
  // Noise:
  noise1=(5.94-noiseIntercept)/(noiseSlope*gain1);
  noise1=noise1*noise1;
    
  noise2=(5.61-noiseIntercept)/(noiseSlope*gain2);
  noise2=noise2*noise2;
    
  noise3=(6.15-noiseIntercept)/(noiseSlope*gain3);
  noise3=noise3*noise3;
    
  noise4=(5.88-noiseIntercept)/(noiseSlope*gain4);
  noise4=noise4*noise4;

  //Threshold from 50% Max3 from data
  //thresh1= (25.+threshIntercept)/(threshSlope*gain1);
  //thresh2= (25.+threshIntercept)/(threshSlope*gain2);
  //thresh3= (27.+threshIntercept)/(threshSlope*gain3);
  //thresh4= (26.+threshIntercept)/(threshSlope*gain4);

  //Threshold from Size
  thresh1= sqrt(noise1)/(TSizeSlope*gain1)*(195-TSizeInter);
  thresh2= sqrt(noise2)/(TSizeSlope*gain2)*(175-TSizeInter);
  thresh3= sqrt(noise3)/(TSizeSlope*gain3)*(210-TSizeInter);
  thresh4= sqrt(noise4)/(TSizeSlope*gain4)*(185-TSizeInter);

  //Efficiency from rate
  eff1=((116.-effIntercept.at(0))/effSlope.at(0))*thresh1;
  eff2=((138.-effIntercept.at(1))/effSlope.at(1))*thresh2;
  eff3=((146.-effIntercept.at(2))/effSlope.at(2))*thresh3;
  eff4=((138.-effIntercept.at(3))/effSlope.at(3))*thresh4;

  //Print out the table
  cout<<"T1      "
      <<setw(4)<<setprecision(3)<<thresh1<<"    "
      <<setw(4)<<setprecision(3)<<noise1 <<"    "
      <<setw(4)<<setprecision(2)<<eff1   <<"    "
      <<setw(4)<<setprecision(3)<<gain1  <<"    "
      <<endl;
  
  cout<<"T2      "
      <<setw(4)<<setprecision(3)<<thresh2<<"    "
      <<setw(4)<<setprecision(3)<<noise2 <<"    "
      <<setw(4)<<setprecision(2)<<eff2   <<"    "
      <<setw(4)<<setprecision(3)<<gain2  <<"    "
      <<endl;
  
  cout<<"T3      "
      <<setw(4)<<setprecision(3)<<thresh3<<"    "
      <<setw(4)<<setprecision(3)<<noise3 <<"    "
      <<setw(4)<<setprecision(2)<<eff3   <<"    "
      <<setw(4)<<setprecision(3)<<gain3  <<"    "
      <<endl;
  
  cout<<"T4      "
      <<setw(4)<<setprecision(3)<<thresh4<<"    "
      <<setw(4)<<setprecision(3)<<noise4 <<"    "
      <<setw(4)<<setprecision(2)<<eff4   <<"    "
      <<setw(4)<<setprecision(3)<<gain4  <<"    "
      <<endl;
  return;
}

