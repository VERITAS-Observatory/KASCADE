/**
  * :FILENAME:     RandomNumbers.cpp
  *
  * :PURPOSE:      C++ implementation of the random number
  *                generators as a class based on a long period
  *                (>2e+18) uniform random number generator of
  *                L'Ecuyer with Bays-Durham shuffle.
  *
  * :REFERENCES:   Numerical Recipes
  *
  * :AUTHOR:       Name            Vladimir V. Vassiliev
  *                Institution     U of U
  *                E-mail          vvv@physics.utah.edu
  *
  * :DATE:         Saturday, March 11, 2001
  *
  * :NOTES:
  *       05/12/01 GHS
  *       Use the symbol USE_RANLUX to determine if we generate the random 
  *       numbers
  *       locally or if we use the F77 RANLUX package. We get random numbers
  *       from RANLUX by calling 'pran(&xdummy)'.
  *      
  */

/*! \example example/random.cpp
    This is an example of how to use this class
 */


#include <stdio.h>
#include <stdlib.h>
#include <float.h>
#include <math.h>
#include <string.h>

#ifndef   RandomNumbersH           // Header File Guard
#define   RandomNumbersH
#include "Random.h"
#endif                             // RandomNumbersH

static const double PI=3.14159265359;

static const long RAN2_A1=40014;
static const long RAN2_A2=40692;
static const long RAN2_Q1=53668;
static const long RAN2_Q2=52774;
static const long RAN2_R1=12211;
static const long RAN2_R2=3791;
static const long RAN2_M1=2147483563;
static const long RAN2_M2=2147483399;

static const double RNMX=(1.0-FLT_EPSILON);

#ifdef USE_RANLUX                       //We get random numbers from RANLUX
extern "C" float pran(float*);
#endif

//-------------------------------------------------------------------
// constructor //////////////////////////////////////////////////////
//-------------------------------------------------------------------
RandomNumbers::RandomNumbers( char* pSeeds)
{

#ifndef USE_RANLUX                       //We generate random numbers locally

  pFileName = new char [strlen(pSeeds)+1];// make local copy of the name
  strcpy(pFileName,pSeeds);               // of random seeds file
  //cout << pFileName <<strlen(pSeeds)+1<<endl;

  ran2_read();                            // initialize seeds

  ran2();                                 // change seeds in seeds file to
  ran2_write();                           // prevent repetitive crashes

#endif
}

//-------------------------------------------------------------------
// destructor ///////////////////////////////////////////////////////
//-------------------------------------------------------------------
RandomNumbers::~RandomNumbers()
{
#ifndef USE_RANLUX                       //We generate random numbers locally
  ran2_write();              // save seeds
  delete [] pFileName;       // delete file name string
#endif
}

//-------------------------------------------------------------------
// accessor functions ///////////////////////////////////////////////
//-------------------------------------------------------------------

//-------------------------------------------------------------------
// public methods ///////////////////////////////////////////////////
//-------------------------------------------------------------------
//-------------------- RandomNumbers::Uniform -----------------------
double RandomNumbers::Uniform(void)
{
  return ran2();
}
//-------------------- RandomNumbers::Exponential -------------------
double RandomNumbers::Exponential(void)
/** Returns an exponentially distributed, positive, random deviate
  * of unit mean, using ran2() as the source of uniform deviates.
  * Waiting times between independent Poisson-random events is
  * exponentially distributed, for example.
  */
{
  double dum;

  do
    dum=ran2();
  while (dum == 0.0);
  return -log(dum);
}
//-------------------- RandomNumbers::Normal ------------------------
double RandomNumbers::Normal(void)
/** Returns a normally distributed deviate with zero mean and unit variance,
  * using ran2() as the source of uniform deviates. Algorithm is based on
  * the Box-Muller transformation to get two normal deviates.
  */
{
  static int iset=0;
  static double gset;
  double fac,rsq,v1,v2;

  if(iset == 0) {
    do {
      v1=2.0*ran2()-1.0;
      v2=2.0*ran2()-1.0;
      rsq=v1*v1+v2*v2;
    } while (rsq >= 1.0 || rsq == 0.0);
    fac=sqrt(-2.0*log(rsq)/rsq);
    gset=v1*fac;
    iset=1;
    return v2*fac;
  }
  else {
    iset=0;
    return gset;
  }
}
//-------------------- RandomNumbers::Gamma -------------------------
double RandomNumbers::Gamma(int ia)
/** Returns a deviate distributed as a gamma distribution of integer order ia,
  * i.e., a waiting time to the iath event in a Poisson process of unit mean,
  * using ran2() as the source of uniform deviates.
  * pdf=x^(ia-1)*exp(-x)/Gamma(ia)
  */
{
  int j;
  double am,e,s,v1,v2,x,y;
  if (ia < 1) {
    fprintf(stderr," Error in Gamma: integer order %d is not > 0\n",ia);
    exit (1);
  }
  if(ia < 6) {
    x=1.0;
    for (j=1;j<=ia;j++) x *= ran2();
    x = -log(x);
  }
  else {
    do {
      do {
        do {
          v1=ran2();
          v2=2.0*ran2()-1.0;
        } while (v1*v1+v2*v2 > 1.0);
        y=v2/v1;
        am=ia-1;
        s=sqrt(2.0*am+1.0);
        x=s*y+am;
      } while (x <= 0.0);
      e=(1.0+y*y)*exp(am*log(x/am)-s*y);
    } while (ran2() > e);
  }
  return x;
}
//-------------------- RandomNumbers::Poisson -----------------------
int RandomNumbers::Poisson(double xm)
/** Returns random deviate drawn from a Poisson distribution of mean xm,
  * using ran2() as a source of uniform random deviates.
  */
{
  static double sq,alxm,g,oldm=(-1.0);
  double em,t,y;

  if (xm < 12.0) {           /* Use direct method */
    if (xm != oldm) {
      oldm=xm;
      g=exp(-xm);
    }
    em = -1;
    t=1.0;
    do {
      ++em;
      t *= ran2();
    } while (t > g);
  }
  else {                     /* Use rejection method */
    if (xm != oldm) {
      oldm=xm;
      sq=sqrt(2.0*xm);
      alxm=log(xm);
      g=xm*alxm-gammln(xm+1.0);
    }
    do {
      do {
        y=tan(PI*ran2());
        em=sq*y+xm;
      } while (em < 0.0);
      em=floor(em);
      t=0.9*(1.0+y*y)*exp(em*alxm-gammln(em+1.0)-g);
    } while (ran2() > t);
  }
  return (int)em;
}
//-------------------- RandomNumbers::Binomial ----------------------
int RandomNumbers::Binomial(double pp, int n)
/** Returns an integer value that is a random deviate drawn from a
  * binomial distribution of n trials each of probability pp, using
  * ran2() as a source of uniform random deviates.
  */
{
  int j;
  static int nold=(-1);
  double am,em,g,angle,p,bnl,sq,t,y;
  static double pold=(-1.0),pc,plog,pclog,en,oldg;

  p=(pp <= 0.5 ? pp : 1.0-pp);
  am=n*p;
  if (n < 25) {              /* Use direct method */
    bnl=0.0;
    for (j=1;j<=n;j++) if (ran2() < p) ++bnl;
  }
  else if (am < 1.0) {
    g=exp(-am);
    t=1.0;
    for (j=0;j<=n;j++) {
      t *= ran2();
      if (t < g) break;
    }
    bnl=(j <= n ? j : n);
  }
  else {                     /* Use rejection method */
    if (n != nold) {
      en=n;
      oldg=gammln(en+1.0);
      nold=n;
    }
    if (p != pold) {
      pc=1.0-p;
      plog=log(p);
      pclog=log(pc);
      pold=p;
    }
    sq=sqrt(2.0*am*pc);
    do {
      do {
        angle=PI*ran2();
        y=tan(angle);
        em=sq*y+am;
      } while (em < 0.0 || em >= (en+1.0));
      em=floor(em);
      t=1.2*sq*(1.0+y*y)*exp(oldg-gammln(em+1.0)
        -gammln(en-em+1.0)+em*plog+(en-em)*pclog);
    } while (ran2() > t);
    bnl=em;
  }
  if (p != pp) bnl=n-bnl;
  return (int)bnl;
}


//-------------------------------------------------------------------
// private methods //////////////////////////////////////////////////
//-------------------------------------------------------------------
//-------------------- RandomNumbers::copy constructor --------------
RandomNumbers::RandomNumbers( const RandomNumbers & rhs )
{
  // stub copy constructor
  // this prevents copeing of RandomNumber class
  // allowing only its new instaniation
}
//-------------------- RandomNumbers::copy constructor --------------
RandomNumbers RandomNumbers::operator=( const RandomNumbers & rhs )
{
  // stub operator=
  // this prevents copeing of RandomNumber class
  // allowing only its new instaniation
}
//-------------------- RandomNumbers::ran2 --------------------------

#ifndef USE_RANLUX                       //We generate random numbers locally


double RandomNumbers::ran2(void)
/**  Long period (>2e+18) random number generator of L'Ecuyer with
  *  Bays-Durham shuffle. Returns a uniform random deviate between 0.0 and 1.0
  *  (exclusive of the endpoint values). To initialize call ran2_init(&idum)
  *  with any idum (sign of idum makes no difference, idum equal zero is
  *  the same as idum equal 1). Call to ran2_write() will write state
  *  of generator into RAN2_FILE, and call to ran2_read() will load it
  *  back to continue pseudo-random sequence. RNMX approximates the largest
  *  floating value that is less than 1 which is defined in <float.h>.
  */
{
  int    j;
  long   k;
  double am=(1.0/RAN2_M1);
  long   imm1=(RAN2_M1-1);
  long   ndiv=(1+(RAN2_M1-1)/NTAB);
  double tmp;

  k=ran2_idum1/RAN2_Q1;
  ran2_idum1=RAN2_A1*(ran2_idum1-k*RAN2_Q1)-RAN2_R1*k;
  if (ran2_idum1<0) ran2_idum1+=RAN2_M1;
  k=ran2_idum2/RAN2_Q2;
  ran2_idum2=RAN2_A2*(ran2_idum2-k*RAN2_Q2)-RAN2_R2*k;
  if (ran2_idum2<0) ran2_idum2+=RAN2_M2;
  j=ran2_iy/ndiv;
  ran2_iy=ran2_iv[j]-ran2_idum2;
  ran2_iv[j]=ran2_idum1;
  if(ran2_iy<1) ran2_iy+=imm1;
  tmp=(double)(am*ran2_iy);
  if(tmp>RNMX) return RNMX;
  else return tmp;
}
#else
/* 05/12/01 GHS
 * Use Ranlux(a F77 Cern Package) to generate random numbers. Assume all
 * initalizatrion (opening, reading seed files etc) is handled somewhere
 * else.
 */

double RandomNumbers::ran2(void)
{
  float xdummy;
  float r;
  r=pran(&xdummy);
  return r;
}
#endif

//-------------------- RandomNumbers::ran2_init ---------------------
void RandomNumbers::ran2_init(long *idum)
{
  int  j;
  long k;

  if(*idum <0) *idum=-(*idum);
  if(*idum==0) *idum=1;
  ran2_idum2=*idum;
  for (j=NTAB+7;j>=0;j--) {
    k=(*idum)/RAN2_Q1;
    *idum=RAN2_A1*(*idum-k*RAN2_Q1)-RAN2_R1*k;
    if (*idum<0) *idum+=RAN2_M1;
    if (j<NTAB) ran2_iv[j]= *idum;
  }
  ran2_iy=ran2_iv[0];
  ran2_idum1=*idum;
  return;
}
//-------------------- RandomNumbers::ran2_read ---------------------
void RandomNumbers::ran2_read(void)
{
  int    j;
  FILE *fp;

  fp=fopen(pFileName,"r");
  if( fp == 0 ) {             // no seeds file
    {
      long dummy2=-1962;      // initialize seeds
      ran2_init(&dummy2);
      ran2_write();           // write seeds file
    }
  } else {                    // seeds file is opened
    fscanf(fp,"%d\n",&ran2_idum1);
    fscanf(fp,"%d\n",&ran2_idum2);
    fscanf(fp,"%d\n",&ran2_iy);
    for (j=0;j<NTAB;j++) {
      fscanf(fp,"%d\n",&ran2_iv[j]);
    }
    fclose(fp);
  }
  return;
}
//-------------------- RandomNumbers::ran2_write --------------------
void RandomNumbers::ran2_write(void)
{
  int    j;
  FILE *fp;

  fp=fopen(pFileName,"w");
  fprintf(fp,"%d\n",ran2_idum1);
  fprintf(fp,"%d\n",ran2_idum2);
  fprintf(fp,"%d\n",ran2_iy);
  for (j=0;j<NTAB;j++) {
    fprintf(fp,"%d\n",ran2_iv[j]);
  }
  fclose(fp);
  return;
}


//-------------------- RandomNumbers::gammln ------------------------
double RandomNumbers::gammln(double xx)
/* Returns the value ln[Gamma(xx)] for xx > 0. */
{
  double x,y,tmp,ser;
  static double cof[6]={76.18009172947146, -86.50532032941677,
                        24.01409824083091, -1.231739572450155,
                    0.1208650973866179e-2, -0.5395239384953e-5};
  int j;

  if (xx <= 0) {
    fprintf(stderr," Error in gammln: argument %e is not > 0\n",xx);
    exit (1);
  }

  y=x=xx;
  tmp=x+5.5;
  tmp-=(x+0.5)*log(tmp);
  ser=1.000000000190015;
  for (j=0;j<=5;j++) ser+=cof[j]/++y;
  return -tmp+log(2.5066282746310005*ser/x);
}
