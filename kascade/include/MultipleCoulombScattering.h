//This is the header file for the MultipleCoulombScattering class. This class
//generates a scattering angle due to multiple scattering using a method
//devised by Vladimir Vassiliev (VVV to those who know him). This method is
//lifted (better word then stolen) from VVV's CHESS program. 
//Converted to a C++ class by Glenn Sembroski 12/31/04 for use in KASCADE
//
// GHS: The code is for electrons/positron only
//
// Code for non e+/e-: muons,protons,heavies etc from Particle Data book
// See notes below.


// **************************************************************************
//Notes from original code (VVV's chess as of 10/01/05) follow:
//--MULTIPLE COULOMB SCATTERING
// To produce code following articles were used,
// V.L. Highland, (1975) Nuclear Instruments and Methods, 129, 497
// G.R. Lynch and O.I. Dahl, (1991) Nuc. Instr. and Meth. in Physics B58 ,6  
// H.A.Bethe (1953) Phys. Rev. ,V89, 1256
// S.A. Goudsmit, J.L. Saunderson, (1940) Phys. Rev., V57,24 and V58,36
// along with monte carlo simulation of this process.
// There is one main problem in the treatment of multiple 
// Coulomb scattering. This problem is correct calculation of the 
// singular scattering of particles on large angles.
// The contribution from such scattering becomes small for the large
// travel distances, but the trajectory of the particle becomes discontinuous,
// in this case. To continuously follow trajectory of the particle
// one needs to decrease step size (typically to 3.e-2 - 3. g/cm^2) and
// as a result account for singular scattering on large angles.
// The widely used Dahl formula has in fact very good agreement with 
// theory when singular deflection on large angles can be neglected.
// It still describes well enough (few percent) the multiple scattering
// part of the theoretically obtained distributions even for a hundred
// collisions. However, the long Rutherford tail of the distribution is
// not included in consideration at all. To calculate the correct distributions
// we rely upon two theoretical results by Goudsmit, Saunderson and then
// H.A.Bethe. The latter perfect article is especially important because of
// the practical asymptotic formulas obtained in it. The algorithm in CHESS
// is heavily based on them. It precalculates look-up tables of zero and
// first order terms of expansion series. And then it uses the tables 
// to generate scattering angle. There is one addition to the H.A. Bethe
// article. Namely, we found that if we use 2Sin(theta/2) instead of theta
// in Bethe's formulas we match asymptotic behavior for large theta 
// and small number of collisions exactly, and we match it reasonably
// well for the number of scatterings of our interest (1.e+2 - 1.e+4).
// This was proved with MC simulations. In the limit of small theta it's
// identical to Bethe's formulas, of course. This replacement of the
// argument leads to its natural upper limit of variable correspondent to
// scattering on Pi.
// We based our algorithm of Multiple Coulomb scattering mainly on 
// theoretical results because no good experimental data on the scattering
// of electrons are available (see G.R. Lynch and O.I. Dahl).  
// **************************************************************************
// 
// Notes on non e+/e- (ie. muons,protons, heavies etc) code origins:
// This code was derived from a formula taken from the Particle Properties 
// Data Booklet of April 1984, pg. 112-113 (Which is reproduced as a Review of 
// Modern Physics volume.April 1984).  The book claims that this formulation 
// is good to 5% for our regime and implies that particle type is unimportant.
// Particle data book says that the main dependence of the scattering angle 
// for various particles is Z/P*Beta. So Our scheme is:
// 1:Find KE of particles to be scattered.
// 2:Find total E of electron with this KE
// 3.Generate a scattering angle for an electron of this E
// 4.Modify that angle by: Z(heavy)*PBeta(electron)/PBeta(heavy particle)
//  This seems to replicate the results gotten from useing the formula's in 
//  the particle data book very well. (My only concern was what energy to use 
//  for the Electron). Nice thing is this guarantees nice long tails which the
//  particle data book didn't.
// **************************************************************************
// CONCLUSION: This good is very good(the best?) for e+/e-.
//             I think its pretty good for muon,proton...Fe56 etc)
//***************************************************************************


//Modified:

                  //Parameters for air
const double  P_n=.78;                         // nitrogen concentration 
const double  P_o=.21;                         // oxygen   concentration 
const double  P_a=.01;                         // argon    concentration 
const double  A_av =14*P_n+16*P_o+ 40/2.*P_a;  // average atomic mass
const double  Z_av = 7*P_n+ 8*P_o+ 18/2.*P_a;  // average charge Z
const double  ZZ_av=49*P_n+64*P_o+324/2.*P_a;  // average Z*Z

const double Avg=6.022137e+23;             // Avogadro's number   [1/g]
const double re=2.8179409E-13;             // classical electron radius [cm]
const double alpha=1./137.036;             // alpha
const double pi=3.14159265;                // pi
const double e_m=.51099906e-6;             // electron mass        [Tev]  
const double rad_len=36.6;                 // Radiation length in air.gm/cm**2 
                                           // Tsai 1974.

const double c=.577215665;
const double M=100.;
const int n_cb=50;               // number of points in spline
const double h_cb=1.;            // step size for spline [1]

class MultipleCoulombScattering
{
 private:
  double g_cb[51];
  double f_cb[51];
  double g_ce[51];
  double f_ce[51];
  double X_cb,E_sc; 

  void Spline(double* f, double* g, double h, int n, double fl, double fr);
  double CoulombScattering(double x, double B, double& dq, double Z);
  
 public:
  MultipleCoulombScattering();
  ~MultipleCoulombScattering(){};
  double  getScatteringAngle(double E, double path_s, int charge, 
			     double xmass);
};

