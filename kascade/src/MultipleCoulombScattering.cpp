//This is the file for the methods for the MultipleCoulombScattering class. 
//This class generates a scattering angle due to multiple scattering using a 
//method devised by Vladimir Vassiliev (VVV to those who know him). This 
//method is lifted (better word then stolen) from VVV's CHESS program. 
//  GHS The Chess code is for electrons/positrons only!!!!
// ************************************************************************
//Converted to a C++ class by Glenn Sembroski 12/31/04.
//           Use only doubles
//           Retaining variable names from original code (ie. don't blame me!)
// ************************************************************************
//Modified:

//  14/01/05 GHS Allow multiple scatters when path<.00245 (30 scatters)
//              Result is inaccurate but do it anyway.
//  17/01/05 GHS Don't allow scatters when path<.001 gm/cm**2 (12 scatters)
//                Program blows up if you do.
//  19/01/05 GHS Add code to handle things heavier than e+/e-

#include "MultipleCoulombScattering.h"
#include "Random.h"
#include <algorithm>                 //Has max function
#include <cmath>                      //Has pow,exp,log,asin,sqrt functions
#include <iomanip>

using namespace std;

extern "C" float pran(float* dummy);
//RandomNumbers RanP("kasMultScatt.ran"); //VVV random number generator.
//                                    //This definition should happen only once
//                                    //in a program.
//                                    //All other classes(in other files) get 
//                                    //ranp with extern declarations of 
//                                    //ranp(looks like):
//                                    //  extern RandomNumbers ranp;

MultipleCoulombScattering::MultipleCoulombScattering()
// ************************************************************************
// Constructor for MultipleCoulombScattering class
// Original code from VVV's chess.for,subroutine Coulomb_Scattering_ini
// ************************************************************************

 //--- VVV 5/11/96
//Calculates cubic spline for fast calculation of
//Multiple Coulomb Scattering.
//Routine calculates integral from first order correction function f(1) 
//H.A. Bethe (1953), Phys. Rev. V.89, N.6, p. 1256
//This function is responsible for Rutherford tail  
//in angular distribution
{
  //First some constants:
                                   // effective charge for screening
  double Z_sc=(56*log(7.)*P_n) + (72*log(8.)*P_o) +((342/2.)*log(18.)*P_a); 
  Z_sc=exp(Z_sc/(ZZ_av+Z_av));        // Exp(<Z(Z+1)ln(Z)>/<Z(Z+1)>)    
 
  X_cb=4.*pi*(Avg/A_av)*re*re*(ZZ_av+Z_av)*pow(Z_sc,-2./3.);
  X_cb=alpha*alpha/X_cb;        //Coulomb scattering length [g/cm^2]
  E_sc=alpha*pow(Z_sc,1./3)*(e_m/2.);// Screening characteristic energy


  // f(1) calculations
  double x=50.;
  double v=1./x/x+4./x/x/x; 
  double f1=1./x+2./x/x;
  f_cb[50]=f1;

  double h;
  for(int j=4999;j>-1;j--)
    {
      x=.01*j;
      int n=(int)(max(M,x*M));  
      h=1./n;
      double q=exp(-h*x);
      double ex=exp(-x);
      double e=1.;
      double y=0.;
      double s=ex*(log(h)+x*h+x*x*h*h/4.);
      for(int i=1;i<n;i++)
	{
	  y=y+h;
	  double z=h/(1.-y);
	  z=log(1.+z)+x*h*z/2.;
	  e=e*q;
	  s=s+e*z;
        }
      
      s=(c*(x-1.)+2.)*ex+(x-1.)*s-1.;
      f1=f1+.01*(s+v)/2.;
      v=s;
      int l=(int) x;
      if(x-l==0.) 
	{
	  f_cb[l]=f1; 
	}
    }
  f_cb[0]=0.;

  x=50.;


  //GHS Note: fl is not the same as f1  !!!
  double fl=-(1.-c)*.87;  // VVV: correction for derivative (.87) is adjusted
                   // VVV:to do better fit with function in [0.,1.] interval
  double fr=-(1./x/x+4./x/x/x); 
  Spline(f_cb,g_cb,h_cb,n_cb,fl,fr);

  // VVV:f(0) calculations (simple exp); 
  // VVV:   it's done just to save calculation time
  for(int j=0;j<51;j++)
    {
      f_ce[j]=exp(-h_cb*j);
    }

  fl=-1.;
  fr=exp(-50.); 
  Spline(f_ce,g_ce,h_cb,n_cb,fl,fr);
 

  //debug
  // for(int j=0;j<51;j++)
  // {
  //   cout<<j<<","<<std::setprecision(8)<<f_ce[j]<<","<<g_ce[j]<<","<<f_cb[j]<<","<<g_cb[j]<<endl;
  //  }
  //enddebug

  // **************************************************************************


  return;
}
// ****************************************************************************

void MultipleCoulombScattering::Spline(double* f, double* g, double h, int n,
				      double fl, double fr)
{
// ****************************************************************************
// VVV 2/15/96
// Calculates cubic spline with known first derivatives at boundaries
// Original code from VVV's chess.for,subroutine Spline.
// ****************************************************************************

// f(i) - function (in), coefficients at linear terms (out)
// g(i) - second derivative of the function times h*h/6.
//        or coefficients at cubic terms (out)
// h    - step  (in/out)
// n    - length of the arrays f and g  (in/out)
// fl   - first derivative at left boundary  (in/out)
// fr   - first derivative at right boundary (in/out)

  double A[101];
  double B[101];
  
  A[0]=-1./2.;                      //VVV: left boundary condition
  B[0]=3.*((f[1]-f[0])/h-fl)/h;
  
  for(int i=1;i<n;i++)
    {
      A[i]=-1./(4.+A[i-1]);
      B[i]=A[i]*(B[i-1]-6.*(f[i+1]-2.*f[i]+f[i-1])/h/h);
    }
  
  g[n]=h*h/(2.+A[n-1])/6.;          //VVV: right boundary condition
  g[n]=g[n]*(-B[n-1]+6.*(fr-(f[n]-f[n-1])/h)/h);
  
  for(int k=1;k<n+1;k++)
    {
      int i=n-k;
      g[i]=A[i]*g[i+1]+B[i]*h*h/6.;
    }
  
  for(int i=0;i<n+1;i++)
    {
      f[i]=f[i]-g[i];
    }
  return;
}
// ************************************************************************


double MultipleCoulombScattering::getScatteringAngle(double E, double path_s,
						     int charge, double mass)
// ************************************************************************
//  Calculate the angle this particle multiple scatters in the length
//     path_s and return the scatter angle (in radians)
// ************************************************************************
// Original code (for e+/e-) from VVV's chess.for,subroutine propagate. 
// Code for other particles (muons, protons, heavies) from particle data book.
// See notes in MultipleCoulombScattering.h
{
  //First see if we are doing non e+/e-. If so, find the KE of this particle
  // and use that to find the total energy of an electron with this KE.
  // Then find the electron scattering angle.
  // Finally modify this angle as needed for the heavier particle 
  if(mass>2.*(e_m))
    {
      E=E-mass+e_m;           //Total energy of electron.
    }

  double sth2=0; //This is sin(theta/2)**2 where theta is scattering angle.
             // VVV: No scattering for particles with E>1. Tev
             // Maximal deflection angle in this case
             // will be less than 1% of the Cherenkov angle.
             // This effect is due to the finite bin of
             // random generator 2**(-24).(may not be true in general:GHS)
  if((E-e_m)<1.)
    {
      double path_n=path_s/X_cb;       // number of collisions

// ****************************************************************************
// GHS-This e+/e- calculation becomes inaccurate for path_n<30. 
//     ie.path_s<.00243 
// ****************************************************************************
      if(path_n<12) 
	{
	  sth2=0.;//no deflection if less  than 12 collisions(path_s~<.001 
                      // gm/cn**2
	              // Normally this may happen only at the ends of 
                      // propagation intervals
// ****************************************************************************
// GHS-I think following code bombs when we try to calculate angle 
//     when path_n<1 or 2. 
// ****************************************************************************
	}

      else                             // particle is scattered
	
	{
	  double x=log(path_n)-.366724;// .366724 replaced 1.15443133
	  double Qx=(x-2.)/18.;
	  double Qy=1.-Qx;
	               // fit to the solution of B=b+Alog(B);B_prmtr=B;x=b
	  double B_prmtr=-2.142962*pow(Qy,3) + 0.4802084*pow(Qx,3) + 
	    5.289103*Qy + 22.66143*Qx;
         
	  // **************************************************************
	  //GHS Replace S_Vec(4) in orginal code with E. I think thats ok.
	  //My best guess is that E is the total energy of the scattering 
          //particle at mid segment. I think (??) that originally (in 
	  // chesss.for) E was starting energy of particle at beginning of 
	  //segment and S_Vec(4) was the energy after dE/dx losses (at the 
	  // end of the segment). So maybe E*S_Vec(4) was a way to get a crude 
	  // average????.
	  //y=path_n*B_prmtr*E_sc*E_sc/E/S_Vec(4);
	  // **************************************************************
	  
	  double y=path_n*B_prmtr*E_sc*E_sc/E/E;


	  x=1./y;                      // x_max
	      
	  double Qz;
	  //double rand=(double)RanP.Uniform();
	  double rand=pran(&fDummy);
	  double Z=CoulombScattering(x,B_prmtr,Qz,0.);
	  Z=rand+(1.-rand)*Z;
	      
	  x=0.;
	  double DQXY;
	  for(;;)
	    {
	      DQXY=CoulombScattering(x,B_prmtr,Qz,Z);
	      Qz=-DQXY/Qz;
	      x=x+Qz;
	      DQXY=Qz/x;
	      if(DQXY<=5.e-3)
		{
		  break;
		}
	    }

	  sth2=x*y;
//Debug
//	  cout<<"sth2: "<<setprecision(10)<<sth2<<endl;
//enddebug
	}
// *****************************************************************
//GHS: At this point we now have sth2=sin(theta/2)**2 where theta
//     is the Multiple scattering angle (may be 0)
// *****************************************************************
    }
  double tix=0;

  //debug
  //  if(sth2>2.e-8)//VVV:no deflection if angle is less than 1% of Cherenkov 
  //enddebug
                  //angle. It saves time
  {
    double Sin_t=2.*sqrt(sth2*(1.-sth2));  //Get scattering angle
    tix=abs(asin(Sin_t));   //radians
  }

  // ******************************************************************
  // Multiple scattering for non e+/e-: muons,protons, heavies etc.
  // ******************************************************************
  if(mass>2.*(e_m))   //The 2 here is just for insurance.(your e_m may 
	                  //not be quite the same as my e_m)
    { //at this point tix is for an electron of KE=E-e_m
      //tix for particle is relative to Z/(P*beta)
      double epbeta=(pow(E,2)-pow(e_m,2))/E;    // P*beta(Tev)for elec.
      E=E-e_m+mass;                             // Original particle Etotal
      double pbeta=(pow(E,2)-pow(mass,2))/E;    // P*beta For orig part.
      tix=(charge*epbeta/pbeta)*tix;            // New tix
    }
  
  return tix;
}

// **************************************************************************


double MultipleCoulombScattering::CoulombScattering(double x, double B,
						    double& dq, double Z)   
{
// ************************************************************************
//         Fast calculation of Multiple Coulomb Scattering.
// ************************************************************************
// Original code from VVV's chess.for,function Coulomb_Scattering
//--- VVV 5/11/96
  double e;
  double q;
  double de;

  if(x<50.)
    {           // spline treatment
      int i=(int)(x/h_cb);
      double xl=x/h_cb-1.*i;
      double xr=1.-xl;
      double xl2=xl*xl;
      double xr2=xr*xr;
      double xl3=xl2*xl;
      double xr3=xr2*xr;

//-- Function
      q=g_cb[i]*xr3+g_cb[i+1]*xl3+f_cb[i]*xr+f_cb[i+1]*xl;
      e=g_ce[i]*xr3+g_ce[i+1]*xl3+f_ce[i]*xr+f_ce[i+1]*xl;
      q=e-Z+q/B;

//-- Derivative
      dq=3.*(g_cb[i+1]*xl2-g_cb[i]*xr2)+f_cb[i+1]-f_cb[i];
      de=3.*(g_ce[i+1]*xl2-g_ce[i]*xr2)+f_ce[i+1]-f_ce[i];
      dq=(de+dq/B)/h_cb;
    }
  else                           // functional treatment (asymptotic)
    {
      e=1./x;
      de=1./(1.-4.*e);
      q=e*sqrt(de)/B;               // leading order difference is +4/x**4
      dq=-q*e*de*(1.-2.*e);         // at x=50. it is 6.4e-7
      q=q-Z;
    }
 
  return q;
}
