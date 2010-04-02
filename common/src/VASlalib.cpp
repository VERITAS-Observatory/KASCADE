/********************************************************************
 * \file VASlalib.cpp
 * \ingroup common
 * \brief Astronomical tools used by the sp24 analysis package
 * 
 * Original Author: Pascal Fortin
 * $Author$
 * $Date$
 * $Revision$
 * $Tag$
 *******************************************************************/

// These deinitions tell the makefile which library the cpp file
// should be included in
// VA_LIBRARY_TAG: libSP24common.a
// VA_LIBRARY_TAG: libSP24commonLite.a

#include "VASlalib.h"
#include "VASlamac.h"

#include <iostream>
#include <sys/time.h>
#include <cstring>

void slaAmpqk ( double ra, double da, double amprms[21],
                double *rm, double *dm )
/*
**  - - - - - - - - -
**   s l a A m p q k
**  - - - - - - - - -
**
**  Convert star RA,Dec from geocentric apparent to mean place.
**
**  The mean coordinate system is the post IAU 1976 system,
**  loosely called FK5.
**
**  Use of this routine is appropriate when efficiency is important
**  and where many star positions are all to be transformed for
**  one epoch and equinox.  The star-independent parameters can be
**  obtained by calling the slaMappa routine.
**
**  Given:
**     ra       double      apparent RA (radians)
**     da       double      apparent Dec (radians)
**
**     amprms   double[21]  star-independent mean-to-apparent parameters:
**
**       (0)      time interval for proper motion (Julian years)
**       (1-3)    barycentric position of the Earth (AU)
**       (4-6)    heliocentric direction of the Earth (unit vector)
**       (7)      (grav rad Sun)*2/(Sun-Earth distance)
**       (8-10)   abv: barycentric Earth velocity in units of c
**       (11)     sqrt(1-v*v) where v=modulus(abv)
**       (12-20)  precession/nutation (3,3) matrix
**
**  Returned:
**     *rm      double      mean RA (radians)
**     *dm      double      mean Dec (radians)
**
**  References:
**     1984 Astronomical Almanac, pp B39-B41.
**     (also Lederle & Schwan, Astron. Astrophys. 134, 1-6, 1984)
**
**  Note:
**
**     Iterative techniques are used for the aberration and
**     light deflection corrections so that the routines
**     slaAmp (or slaAmpqk) and slaMap (or slaMapqk) are
**     accurate inverses;  even at the edge of the Sun's disc
**     the discrepancy is only about 1 nanoarcsecond.
**
**  Called:  slaDcs2c, slaDimxv, slaDvdv, slaDvn, slaDcc2s,
**           slaDranrm
**
**  Last revision:   7 May 2000
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
   double gr2e;    /* (grav rad Sun)*2/(Sun-Earth distance) */
   double ab1;     /* sqrt(1-v*v) where v=modulus of Earth vel */
   double ehn[3];  /* Earth position wrt Sun (unit vector, FK5) */
   double abv[3];  /* Earth velocity wrt SSB (c, FK5) */
   double p[3], p1[3], p2[3], p3[3];  /* work vectors */
   double ab1p1, p1dv, p1dvp1, w, pde, pdep1;
   int i, j;

/* Unpack some of the parameters */
   gr2e = amprms[7];
   ab1  = amprms[11];
   for ( i = 0; i < 3; i++ ) {
      ehn[i] = amprms[i + 4];
      abv[i] = amprms[i + 8];
   }

/* Apparent RA,Dec to Cartesian */
   slaDcs2c ( ra, da, p3 );

/* Precession and nutation */
   slaDimxv ( (double(*)[3]) &amprms[12], p3, p2 );

/* Aberration */
   ab1p1 = ab1 + 1.0;
   for ( i = 0; i < 3; i++ ) {
      p1[i] = p2[i];
   }
   for ( j = 0; j < 2; j++ ) {
      p1dv = slaDvdv ( p1, abv );
      p1dvp1 = 1.0 + p1dv;
      w = 1.0 + p1dv / ab1p1;
      for ( i = 0; i < 3; i++ ) {
         p1[i] = ( p1dvp1 * p2[i] - w * abv[i] ) / ab1;
      }
      slaDvn ( p1, p3, &w );
      for ( i = 0; i < 3; i++ ) {
         p1[i] = p3[i];
      }
   }

/* Light deflection */
   for ( i = 0; i < 3; i++ ) {
      p[i] = p1[i];
   }
   for ( j = 0; j < 5; j++ ) {
      pde = slaDvdv ( p, ehn );
      pdep1 = 1.0 + pde;
      w = pdep1 - gr2e * pde;
      for ( i = 0; i < 3; i++ ) {
         p[i] = ( pdep1 * p1[i] - gr2e * ehn[i] ) / w;
      }
      slaDvn ( p, p2, &w );
      for ( i = 0; i < 3; i++ ) {
         p[i] = p2[i];
      }
   }
/* Mean RA,Dec */
   slaDcc2s ( p, rm, dm );
   *rm = slaDranrm ( *rm );
}

void slaAmp ( double ra, double da, double date, double eq,
              double *rm, double *dm )
/*
**  - - - - - - -
**   s l a A m p
**  - - - - - - -
**
**  Convert star RA,Dec from geocentric apparent to mean place.
**
**  The mean coordinate system is the post IAU 1976 system,
**  loosely called FK5.
**
**  Given:
**     ra       double      apparent RA (radians)
**     da       double      apparent Dec (radians)
**     date     double      TDB for apparent place (JD-2400000.5)
**     eq       double      equinox:  Julian epoch of mean place
**
**  Returned:
**     *rm      double      mean RA (radians)
**     *dm      double      mean Dec (radians)
**
**  References:
**     1984 Astronomical Almanac, pp B39-B41.
**     (also Lederle & Schwan, Astron. Astrophys. 134, 1-6, 1984)
**
**  Notes:
**
**  1)  The distinction between the required TDB and TT is always
**      negligible.  Moreover, for all but the most critical
**      applications UTC is adequate.
**
**  2)  Iterative techniques are used for the aberration and light
**      deflection corrections so that the routines slaAmp (or
**      slaAmpqk) and slaMap (or slaMapqk) are accurate inverses;
**      even at the edge of the Sun's disc the discrepancy is only
**      about 1 nanoarcsecond.
**
**  3)  Where multiple apparent places are to be converted to mean
**      places, for a fixed date and equinox, it is more efficient to
**      use the slaMappa routine to compute the required parameters
**      once, followed by one call to slaAmpqk per star.
**
**  4)  The accuracy is sub-milliarcsecond, limited by the
**      precession-nutation model (IAU 1976 precession, Shirai &
**      Fukushima 2001 forced nutation and precession corrections).
**
**  5)  The accuracy is further limited by the routine slaEvp, called
**      by slaMappa, which computes the Earth position and velocity
**      using the methods of Stumpff.  The maximum error is about
**      0.3 mas.
**
**  Called:  slaMappa, slaAmpqk
**
**  Last revision:   17 September 2001
**
**  Copyright P.T.Wallace.  All rights reserved.
**
*/
{
   double amprms[21];    /* Mean-to-apparent parameters */

   slaMappa ( eq, date, amprms );
   slaAmpqk ( ra, da, amprms, rm, dm );
}

void slaCldj ( int iy, int im, int id, double *djm, int *j )
  /*
  **  - - - - - - - -
  **   s l a C l d j
  **  - - - - - - - -
  **
  **  Gregorian calendar to Modified Julian Date.
  **
  **  Given:
  **     iy,im,id     int    year, month, day in Gregorian calendar
  **
  **  Returned:
  **     *djm         double Modified Julian Date (JD-2400000.5) for 0 hrs
  **     *j           int    status:
  **                           0 = OK
  **                           1 = bad year   (MJD not computed)
  **                           2 = bad month  (MJD not computed)
  **                           3 = bad day    (MJD computed)
  **
  **  The year must be -4699 (i.e. 4700BC) or later.
  **
  **  The algorithm is derived from that of Hatcher 1984 (QJRAS 25, 53-55).
  **
  **  Last revision:   29 August 1994
  **
  **  Copyright P.T.Wallace.  All rights reserved.
  */
{
  long iyL, imL;
  
  /* Month lengths in days */
  static int mtab[12] = { 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 };
  
  /* Validate year */
  if ( iy < -4699 ) { *j = 1; return; }
  
  /* Validate month */
  if ( ( im < 1 ) || ( im > 12 ) ) { *j = 2; return; }
  
  /* Allow for leap year */
  mtab[1] = ( ( ( iy % 4 ) == 0 ) &&
	      ( ( ( iy % 100 ) != 0 ) || ( ( iy % 400 ) == 0 ) ) ) ?
    29 : 28;
  
  /* Validate day */
  *j = ( id < 1 || id > mtab[im-1] ) ? 3 : 0;
  
  /* Lengthen year and month numbers to avoid overflow */
  iyL = (long) iy;
  imL = (long) im;
  
  /* Perform the conversion */
  *djm = (double)
    ( ( 1461L * ( iyL - ( 12L - imL ) / 10L + 4712L ) ) / 4L
      + ( 306L * ( ( imL + 9L ) % 12L ) + 5L ) / 10L
      - ( 3L * ( ( iyL - ( 12L - imL ) / 10L + 4900L ) / 100L ) ) / 4L
      + (long) id - 2399904L );
  return;
}

void slaClyd ( int iy, int im, int id, int *ny, int *nd, int *jstat )
/*
 **  - - - - - - - -
 **   s l a C l y d
 **  - - - - - - - -
 **
 **  Gregorian calendar to year and day in year (in a Julian calendar
												 **  aligned to the 20th/21st century Gregorian calendar).
 **
 **  Given:
 **     iy,im,id     int    year, month, day in Gregorian calendar
 **
 **  Returned:
 **     ny          int    year (re-aligned Julian calendar)
 **     nd          int    day in year (1 = January 1st)
 **     jstat       int    status:
 **                          0 = OK
 **                          1 = bad year (before -4711)
 **                          2 = bad month
 **                          3 = bad day (but conversion performed)
 **
 **  Notes:
 **
 **  1  This routine exists to support the low-precision routines
 **     slaEarth, slaMoon and slaEcor.
 **
 **  2  Between 1900 March 1 and 2100 February 28 it returns answers
 **     which are consistent with the ordinary Gregorian calendar.
 **     Outside this range there will be a discrepancy which increases
 **     by one day for every non-leap century year.
 **
 **  3  The essence of the algorithm is first to express the Gregorian
 **     date as a Julian Day Number and then to convert this back to
 **     a Julian calendar date, with day-in-year instead of month and
 **     day.  See 12.92-1 and 12.95-1 in the reference.
 **
 **  Reference:  Explanatory Supplement to the Astronomical Almanac,
 **              ed P.K.Seidelmann, University Science Books (1992),
 **              p604-606.
 **
 **  Last revision:   26 November 1994
 **
 **  Copyright P.T.Wallace.  All rights reserved.
 */
{
	long i, j, k, l, n, iyL, imL;
	
	/* Month lengths in days */
	static int mtab[12] = { 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 };
	
	
	
	/* Validate year */
	if ( iy < -4711 ) { *jstat = 1; return; }
	
	/* Validate month */
	if ( ( im < 1 ) || ( im > 12 ) ) { *jstat = 2; return; }
	
	/* Allow for (Gregorian) leap year */
	mtab[1] = ( ( ( iy % 4 ) == 0 ) &&
				( ( ( iy % 100 ) != 0 ) || ( ( iy % 400 ) == 0 ) ) ) ?
29 : 28;
	
	/* Validate day */
	*jstat = ( id < 1 || id > mtab[im-1] ) ? 3 : 0;
	
	/* Perform the conversion */
	iyL = (long) iy;
	imL = (long) im;
	i = ( 14 - imL ) /12L;
	k = iyL - i;
	j = ( 1461L * ( k + 4800L ) ) / 4L
		+ ( 367L * ( imL - 2L + 12L * i ) ) / 12L
		- ( 3L * ( ( k + 4900L ) / 100L ) ) / 4L + (long) id - 30660L;
	k = ( j - 1L ) / 1461L;
	l = j - 1461L * k;
	n = ( l - 1L ) / 365L - l / 1461L;
	j = ( ( 80L * ( l - 365L * n + 30L ) ) / 2447L ) / 11L;
	i = n + j;
	*nd = 59 + (int) ( l -365L * i + ( ( 4L - n ) / 4L ) * ( 1L - j ) );
	*ny = (int) ( 4L * k + i ) - 4716;
}

void slaCalyd ( int iy, int im, int id, int *ny, int *nd, int *j )
/*
 **  - - - - - - - - -
 **   s l a C a l y d
 **  - - - - - - - - -
 **
 **  Gregorian calendar date to year and day in year (in a Julian
													  **  calendar aligned to the 20th/21st century Gregorian calendar).
 **
 **  (Includes century default feature:  use slaClyd for years
	  **   before 100AD.)
 **
 **  Given:
 **     iy,im,id   int    year, month, day in Gregorian calendar
 **                       (year may optionally omit the century)
 **  Returned:
 **     *ny        int    year (re-aligned Julian calendar)
 **     *nd        int    day in year (1 = January 1st)
 **     *j         int    status:
 **                         0 = OK
 **                         1 = bad year (before -4711)
 **                         2 = bad month
 **                         3 = bad day (but conversion performed)
 **
 **  Notes:
 **
 **  1  This routine exists to support the low-precision routines
 **     slaEarth, slaMoon and slaEcor.
 **
 **  2  Between 1900 March 1 and 2100 February 28 it returns answers
 **     which are consistent with the ordinary Gregorian calendar.
 **     Outside this range there will be a discrepancy which increases
 **     by one day for every non-leap century year.
 **
 **  3  Years in the range 50-99 are interpreted as 1950-1999, and
 **     years in the range 00-49 are interpreted as 2000-2049.
 **
 **  Called:  slaClyd
 **
 **  Last revision:   22 September 1995
 **
 **  Copyright P.T.Wallace.  All rights reserved.
 */
{
	int i;
	
	/* Default century if appropriate */
	if ( ( iy >= 0 ) && ( iy <= 49 ) )
		i = iy + 2000;
	else if ( ( iy >= 50 ) && ( iy <= 99 ) )
		i = iy + 1900;
	else
		i = iy;
	
	/* Perform the conversion */
	slaClyd ( i, im, id, ny, nd, j );
}

void slaCtf2r ( int ihour, int imin, float sec, float *rad, int *j )
/*
**  - - - - - - - - -
**   s l a C t f 2 r
**  - - - - - - - - -
**
**  Convert hours, minutes, seconds to radians.
**
**  (single precision)
**
**  Given:
**     ihour       int       hours
**     imin        int       minutes
**     sec         float     seconds
**
**  Returned:
**     *rad        float     angle in radians
**     *j          int       status:  0 = OK
**                                    1 = ihour outside range 0-23
**                                    2 = imin outside range 0-59
**                                    3 = sec outside range 0-59.999...
**
**  Called:
**     slaDtf2d
**
**  Notes:
**
**     1)  The result is computed even if any of the range checks fail.
**
**     2)  The sign must be dealt with outside this routine.
**
**  Defined in slamac.h:  D2PI
**
**  Last revision:   30 October 1993
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
   double turns = 0;

/* Convert to turns */
   slaDtf2d ( ihour, imin, (double) sec, &turns, j );

/* To radians */
   *rad = (float) ( turns * D2PI );
}

void slaDaf2r ( int ideg, int iamin, double asec, double *rad, int *j )
/*
**  - - - - - - - - -
**   s l a D a f 2 r
**  - - - - - - - - -
**
**  Convert degrees, arcminutes, arcseconds to radians.
**
**  (double precision)
**
**  Given:
**     ideg        int       degrees
**     iamin       int       arcminutes
**     asec        double    arcseconds
**
**  Returned:
**     *rad        double    angle in radians
**     *j          int       status:  0 = OK
**                                    1 = ideg outside range 0-359
**                                    2 = iamin outside range 0-59
**                                    3 = asec outside range 0-59.999...
**
**  Notes:
**     1)  The result is computed even if any of the range checks fail.
**
**     2)  The sign must be dealt with outside this routine.
**
**  Defined in slamac.h:  DAS2R
**
**  Last revision:   31 October 1993
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
/* Preset status */
   *j = 0;

/* Validate arcsec, arcmin, deg */
   if ( ( asec < 0.0 ) || ( asec >= 60.0 ) ) {
      *j = 3;
      return;
   }
   if ( ( iamin < 0 ) || ( iamin > 59 ) ) {
      *j = 2;
      return;
   }
   if ( ( ideg < 0 ) || ( ideg > 359 ) ) {
      *j = 1;
      return;
   }

/* Compute angle */
   *rad = DAS2R * ( 60.0 * ( 60.0 * (double) ideg
                                  + (double) iamin )
                                           + asec );
}

void slaDd2tf ( int ndp, double days, char *sign, int ihmsf[4] )
/*
 **  - - - - - - - - -
 **   s l a D d 2 t f
 **  - - - - - - - - -
 **
 **  Convert an interval in days into hours, minutes, seconds.
 **
 **  (double precision)
 **
 **  Given:
 **     ndp       int      number of decimal places of seconds
 **     days      double   interval in days
 **
 **  Returned:
 **     *sign     char     '+' or '-'
 **     ihmsf     int[4]   hours, minutes, seconds, fraction
 **
 **  Last revision:   31 August 1995
 **
 **  Copyright P.T.Wallace.  All rights reserved.
 */

#define D2S 86400.0    /* Days to seconds */

{
	double rs, rm, rh, a, ah, am, as, af;
	
	/* Handle sign */
	*sign = (char) ( ( days < 0.0 ) ?  '-' : '+' );
	
	/* Field units in terms of least significant figure */
	rs = pow ( 10.0, (double) gmax ( ndp, 0 ) );
	rs = dint ( rs );
	rm = rs * 60.0;
	rh = rm * 60.0;
	
	/* Round interval and express in smallest units required */
	a = rs * D2S * fabs ( days );
	a = dnint ( a );
	
	/* Separate into fields */
	ah = a / rh;
	ah = dint ( ah );
	a  = a - ah * rh;
	am = a / rm;
	am = dint ( am );
	a  = a - am * rm;
	as = a / rs;
	as = dint ( as );
	af = a - as * rs;
	
	/* Return results */
	ihmsf[0] = (int) ah;
	ihmsf[1] = (int) am;
	ihmsf[2] = (int) as;
	ihmsf[3] = (int) af;
}

void slaDimxv ( double dm[3][3], double va[3], double vb[3] )
/*
**  - - - - - - - - -
**   s l a D i m x v
**  - - - - - - - - -
**
**  Performs the 3-d backward unitary transformation:
**
**     vector vb = (inverse of matrix dm) * vector va
**
**  (double precision)
**
**  (n.b.  The matrix must be unitary, as this routine assumes that
**   the inverse and transpose are identical)
**
**
**  Given:
**     dm       double[3][3]   matrix
**     va       double[3]      vector
**
**  Returned:
**     vb       double[3]      result vector
**
**  Note:  va and vb may be the same array.
**
**  Last revision:   6 November 1999
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
  int i, j;
  double w, vw[3];

/* Inverse of matrix dm * vector va -> vector vw */
   for ( j = 0; j < 3; j++ ) {
      w = 0.0;
      for ( i = 0; i < 3; i++ ) {
         w += dm[i][j] * va[i];
      }
      vw[j] = w;
   }

/* Vector vw -> vector vb */
   for ( j = 0; j < 3; j++ ) {
     vb[j] = vw[j];
   }
}

void slaDjcl ( double djm, int *iy, int *im, int *id, double *fd, int *j)
/*
 **  - - - - - - - -
 **   s l a D j c l
 **  - - - - - - - -
 **
 **  Modified Julian Date to Gregorian year, month, day,
 **  and fraction of a day.
 **
 **  Given:
 **     djm      double     Modified Julian Date (JD-2400000.5)
 **
 **  Returned:
 **     *iy      int        year
 **     *im      int        month
 **     *id      int        day
 **     *fd      double     fraction of day
 **     *j       int        status:
 **                      -1 = unacceptable date (before 4701BC March 1)
 **
 **  The algorithm is derived from that of Hatcher 1984 (QJRAS 25, 53-55).
 **
 **  Defined in slamac.h:  dmod
 **
 **  Last revision:   12 March 1998
 **
 **  Copyright P.T.Wallace.  All rights reserved.
 */
{
	double f, d;
	long jd, n4, nd10;
	
	/* Check if date is acceptable */
	if ( ( djm <= -2395520.0 ) || ( djm >= 1e9 ) ) {
		*j = -1;
		return;
	} else {
		*j = 0;
		
		/* Separate day and fraction */
		f = dmod ( djm, 1.0 );
		if ( f < 0.0 ) f += 1.0;
		d = djm - f;
		d = dnint ( d );
		
		/* Express day in Gregorian calendar */
		jd = (long) dnint ( d ) + 2400001;
		n4 = 4L*(jd+((6L*((4L*jd-17918L)/146097L))/4L+1L)/2L-37L);
		nd10 = 10L*(((n4-237L)%1461L)/4L)+5L;
		*iy = (int) (n4/1461L-4712L);
		*im = (int) (((nd10/306L+2L)%12L)+1L);
		*id = (int) ((nd10%306L)/10L+1L);
		*fd = f;
		*j = 0;
	}
}

void slaDmxm ( double a[3][3], double b[3][3], double c[3][3] )
/*
**  - - - - - - - -
**   s l a D m x m
**  - - - - - - - -
**
**  Product of two 3x3 matrices:
**    matrix c  =  matrix a  x  matrix b
**
**  (double precision)
**
**  Given:
**     a      double[3][3]        matrix
**     b      double[3][3]        matrix
**
**  Returned:
**     c      double[3][3]        matrix result
**
**  Note:  the same array may be nominated more than once.
**
**  Last revision:   6 November 1999
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
   int i, j, k;
   double w, wm[3][3];

/* Multiply into scratch matrix */
   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 3; j++ ) {
         w = 0.0;
         for ( k = 0; k < 3; k++ ) {
            w += a[i][k] * b[k][j];
         }
         wm[i][j] = w;
      }
   }

/* Return the result */
   for ( j = 0; j < 3; j++ ) {
      for ( i = 0; i < 3; i++ ) {
         c[i][j] = wm[i][j];
      }
   }
}

void slaDmxv ( double dm[3][3], double va[3], double vb[3] )
/*
**  - - - - - - - -
**   s l a D m x v
**  - - - - - - - -
**
**  Performs the 3-d forward unitary transformation:
**     vector vb = matrix dm * vector va
**
**  (double precision)
**
**  Given:
**     dm       double[3][3]    matrix
**     va       double[3]       vector
**
**  Returned:
**     vb       double[3]       result vector
**
**  Note:  va and vb may be the same array.
**
**  Last revision:   6 November 1999
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
   int i, j;
   double w, vw[3];

/* Matrix dm * vector va -> vector vw */
   for ( j = 0; j < 3; j++ ) {
      w = 0.0;
      for ( i = 0; i < 3; i++ ) {
         w += dm[j][i] * va[i];
      }
      vw[j] = w;
   }

/* Vector vw -> vector vb */
   for ( j = 0; j < 3; j++ ) {
      vb[j] = vw[j];
   }
}

void slaDr2af ( int ndp, double angle, char *sign, int idmsf[4] )
/*
 **  - - - - - - - - -
 **   s l a D r 2 a f
 **  - - - - - - - - -
 **
 **  Convert an angle in radians to degrees, arcminutes, arcseconds.
 **
 **  (double precision)
 **
 **  Given:
 **     ndp       int          number of decimal places of arcseconds
 **     angle     double       angle in radians
 **
 **  Returned:
 **     sign      char*        '+' or '-'
 **     idmsf     int[4]       degrees, arcminutes, arcseconds, fraction
 **
 **  Called:
 **     slaDd2tf
 **
 **  Defined in slamac.h:  D15B2P
 **
 **  Last revision:   19 November 1995
 **
 **  Copyright P.T.Wallace.  All rights reserved.
 */
{
	/* Scale then use days to h,m,s routine */
	slaDd2tf ( ndp, angle * D15B2P, sign, idmsf );
}

void slaDr2tf ( int ndp, double angle, char *sign, int ihmsf[4] )
/*
 **  - - - - - - - - -
 **   s l a D r 2 t f
 **  - - - - - - - - -
 **
 **  Convert an angle in radians to hours, minutes, seconds.
 **
 **  (double precision)
 **
 **  Given:
 **     ndp       int          number of decimal places of seconds
 **     angle     double       angle in radians
 **
 **  Returned:
 **     sign      char*        '+' or '-'
 **     ihmsf     int[4]       hours, minutes, seconds, fraction
 **
 **  Called:
 **     slaDd2tf
 **
 **  Defined in slamac.h:  D2PI
 **
 **  Last revision:   18 November 1993
 **
 **  Copyright P.T.Wallace.  All rights reserved.
 */
{
	/* Scale then use days to h,m,s routine */
	slaDd2tf ( ndp, angle / D2PI, sign, ihmsf );
}

void slaDs2tp ( double ra, double dec, double raz, double decz,
                double *xi, double *eta, int *j )
    /*
    **  - - - - - - - - -
    **   s l a D s 2 t p
    **  - - - - - - - - -
    **
    **  Projection of spherical coordinates onto tangent plane
    **  ('gnomonic' projection - 'standard coordinates').
    **
    **  (double precision)
    **
    **  Given:
    **     ra,dec      double   spherical coordinates of point to be projected
    **     raz,decz    double   spherical coordinates of tangent point
    **
    **  Returned:
    **     *xi,*eta    double   rectangular coordinates on tangent plane
    **     *j          int      status:   0 = OK, star on tangent plane
    **                                    1 = error, star too far from axis
    **                                    2 = error, antistar on tangent plane
    **                                    3 = error, antistar too far from axis
    **
    **  Last revision:   18 July 1996
    **
    **  Copyright P.T.Wallace.  All rights reserved.
    */
#define TINYBIT 1e-6
{
   double sdecz, sdec, cdecz, cdec, radif, sradif, cradif, denom;


   /* Trig functions */
   sdecz = sin ( decz );
   sdec = sin ( dec );
   cdecz = cos ( decz );
   cdec = cos ( dec );
   radif = ra - raz;
   sradif = sin ( radif );
   cradif = cos ( radif );

   /* Reciprocal of star vector length to tangent plane */
   denom = sdec * sdecz + cdec * cdecz * cradif;

   /* Handle vectors too far from axis */
   if ( denom > TINYBIT ) {
      *j = 0;
   } else if ( denom >= 0.0 ) {
      *j = 1;
      denom = TINYBIT;
   } else if ( denom > -TINYBIT ) {
      *j = 2;
      denom = -TINYBIT;
   } else {
      *j = 3;
   }

   /* Compute tangent plane coordinates (even in dubious cases) */
   *xi = cdec * sradif / denom;
   *eta = ( sdec * cdecz - cdec * sdecz * cradif ) / denom;
}

double slaGmst ( double ut1 )
  /*
  **  - - - - - - - -
  **   s l a G m s t
  **  - - - - - - - -
  **
  **  Conversion from Universal Time to Sidereal Time.
  **
  **  (double precision)
  **
  **  Given:
  **    ut1    double     Universal Time (strictly UT1) expressed as
  **                      Modified Julian Date (JD-2400000.5)
  **
  **  The result is the Greenwich Mean Sidereal Time (double
  **  precision, radians).
  **
  **  The IAU 1982 expression (see page S15 of the 1984 Astronomical
  **  Almanac) is used, but rearranged to reduce rounding errors.
  **  This expression is always described as giving the GMST at
  **  0 hours UT.  In fact, it gives the difference between the
  **  GMST and the UT, which happens to equal the GMST (modulo
  **  24 hours) at 0 hours UT each day.  In this routine, the
  **  entire UT is used directly as the argument for the
  **  standard formula, and the fractional part of the UT is
  **  added separately;  note that the factor 1.0027379... does
  **  not appear.
  **
  **  See also the routine slaGmsta, which delivers better numerical
  **  precision by accepting the UT date and time as separate arguments.
  **
  **  Called:  slaDranrm
  **
  **  Defined in slamac.h:  D2PI, DS2R, dmod
  **
  **  Last revision:   19 March 1996
  **
  **  Copyright P.T.Wallace.  All rights reserved.
  */
{
  double tu;

  /* Julian centuries from fundamental epoch J2000 to this UT */
  tu = ( ut1 - 51544.5 ) / 36525.0;
  
  /* GMST at this UT */
  return slaDranrm ( dmod ( ut1, 1.0 ) * D2PI +
		     ( 24110.54841 +
                       ( 8640184.812866 +
			 ( 0.093104 - 6.2e-6 * tu ) * tu ) * tu ) * DS2R );
}

double slaGmsta ( double date, double ut )
  /*
  **  - - - - - - - - -
  **   s l a G m s t a
  **  - - - - - - - - -
  **
  **  Conversion from Universal Time to Greenwich mean sidereal time,
  **  with rounding errors minimized.
  **
  **  (double precision)
  **
  **  Given:
  *     date   double     UT1 date (MJD: integer part of JD-2400000.5))
  **    ut     double     UT1 time (fraction of a day)
  **
  **  The result is the Greenwich Mean Sidereal Time (double precision,
  **  radians, in the range 0 to 2pi).
  **
  **  There is no restriction on how the UT is apportioned between the
  **  date and ut1 arguments.  Either of the two arguments could, for
  **  example, be zero and the entire date+time supplied in the other.
  **  However, the routine is designed to deliver maximum accuracy when
  **  the date argument is a whole number and the ut argument lies in
  **  the range 0 to 1, or vice versa.
  **
  **  The algorithm is based on the IAU 1982 expression (see page S15 of
  **  the 1984 Astronomical Almanac).  This is always described as giving
  **  the GMST at 0 hours UT1.  In fact, it gives the difference between
  **  the GMST and the UT, the steady 4-minutes-per-day drawing-ahead of
  **  ST with respect to UT.  When whole days are ignored, the expression
  **  happens to equal the GMST at 0 hours UT1 each day.  Note that the
  **  factor 1.0027379... does not appear explicitly but in the form of
  **  the coefficient 8640184.812866, which is 86400x36525x0.0027379...
  **
  **  In this routine, the entire UT1 (the sum of the two arguments date
  **  and ut) is used directly as the argument for the standard formula.
  **  The UT1 is then added, but omitting whole days to conserve accuracy.
  **
  **  See also the routine slaGmst, which accepts the UT1 as a single
  **  argument.  Compared with slaGmst, the extra numerical precision
  **  delivered by the present routine is unlikely to be important in
  **  an absolute sense, but may be useful when critically comparing
  **  algorithms and in applications where two sidereal times close
  **  together are differenced.
  **
  **  Called:  slaDranrm
  **
  **  Defined in slamac.h:  DS2R, dmod
  **
  **  Last revision:   14 October 2001
  **
  **  Copyright P.T.Wallace.  All rights reserved.
  */
{
  double d1, d2, t;
  
  /* Julian centuries since J2000. */
  if ( date < ut ) {
    d1 = date;
    d2 = ut;
  } else {
    d1 = ut;
    d2 = date;
  }
  t = ( d1 + ( d2 - 51544.5 ) ) / 36525.0;
  
  /* GMST at this UT1. */
  return slaDranrm ( DS2R * ( 24110.54841
			      + ( 8640184.812866
				  + ( 0.093104
				      - 6.2e-6 * t ) * t ) * t
			      + 86400.0 * ( dmod ( d1, 1.0 ) +
					    dmod ( d2, 1.0 ) ) ) );
}

double slaDrange ( double angle )
/*
**  - - - - - - - - - -
**   s l a D r a n g e
**  - - - - - - - - - -
**
**  Normalize angle into range +/- pi.
**
**  (double precision)
**
**  Given:
**     angle     double      the angle in radians
**
**  The result is angle expressed in the +/- pi (double precision).
**
**  Defined in slamac.h:  DPI, D2PI, dmod
**
**  Last revision:   19 March 1996
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
  double w;

  w = dmod ( angle, D2PI );
  return ( fabs ( w ) < DPI ) ? w : w - dsign ( D2PI, angle );
}

double slaDranrm ( double angle )
  /*
  **  - - - - - - - - - -
  **   s l a D r a n r m
  **  - - - - - - - - - -
  **
  **  Normalize angle into range 0-2 pi.
  **
  **  (double precision)
  **
  **  Given:
  **     angle     double      the angle in radians
  **
  **  The result is angle expressed in the range 0-2 pi (double).
  **
  **  Defined in slamac.h:  D2PI, dmod
  **
  **  Last revision:   19 March 1996
  **
  **  Copyright P.T.Wallace.  All rights reserved.
  */
{
  double w;
  
  w = dmod ( angle, D2PI );
  return ( w >= 0.0 ) ? w : w + D2PI;
}

void slaAltaz(double ha, double dec, double phi,
	      double *az, double *azd, double *azdd,
	      double *el, double *eld, double *eldd,
	      double *pa, double *pad, double *padd )
  /*
  **  - - - - - - - - -
  **   s l a A l t a z
  **  - - - - - - - - -
  **
  **  Positions, velocities and accelerations for an altazimuth
  **  telescope mount.
  **
  **  (double precision)
  **
  **  Given:
  **     ha          double      hour angle
  **     dec         double      declination
  **     phi         double      latitude
  **
  **  Returned:
  **     *az         double      azimuth
  **     *azd        double         "    velocity
  **     *azdd       double         "    acceleration
  **     *el         double      elevation
  **     *eld        double          "     velocity
  **     *eldd       double          "     acceleration
  **     *pa         double      parallactic angle
  **     *pad        double          "      "   velocity
  **     *padd       double          "      "   acceleration
  **
  **  Notes:
  **
  **  1)  Natural units are used throughout.  HA, DEC, PHI, AZ, EL
  **      and ZD are in radians.  The velocities and accelerations
  **      assume constant declination and constant rate of change of
  **      hour angle (as for tracking a star);  the units of AZD, ELD
  **      and PAD are radians per radian of HA, while the units of AZDD,
  **      ELDD and PADD are radians per radian of HA squared.  To
  **      convert into practical degree- and second-based units:
  **
  **        angles * 360/2pi -> degrees
  **        velocities * (2pi/86400)*(360/2pi) -> degree/sec
  **        accelerations * ((2pi/86400)**2)*(360/2pi) -> degree/sec/sec
  **
  **      Note that the seconds here are sidereal rather than SI.  One
  **      sidereal second is about 0.99727 SI seconds.
  **
  **      The velocity and acceleration factors assume the sidereal
  **      tracking case.  Their respective numerical values are (exactly)
  **      1/240 and (approximately) 1/3300236.9.
  **
  q**  2)  Azimuth is returned in the range 0-2pi;  north is zero,
  **      and east is +pi/2.  Elevation and parallactic angle are
  **      returned in the range +/-pi.  Position angle is +ve
  **      for a star west of the meridian and is the angle NP-star-zenith.
  **
  **  3)  The latitude is geodetic as opposed to geocentric.  The
  **      hour angle and declination are topocentric.  Refraction and
  **      deficiencies in the telescope mounting are ignored.  The
  **      purpose of the routine is to give the general form of the
  **      quantities.  The details of a real telescope could profoundly
  **      change the results, especially close to the zenith.
  **
  **  4)  No range checking of arguments is carried out.
  **
  **  5)  In applications which involve many such calculations, rather
  **      than calling the present routine it will be more efficient to
  **      use inline code, having previously computed fixed terms such
  **      as sine and cosine of latitude, and (for tracking a star)
  **      sine and cosine of declination.
  **
  **  Defined in slamac.h:  DPI, D2PI
  **
  **  Last revision:   30 November 2000
  **
  **  Copyright P.T.Wallace.  All rights reserved.
  */
  
#define TINY 1e-30   /* Zone of avoidance around zenith/nadir */
  
{
  double sh, ch, sd, cd, sp, cp, chcd, sdcp, x, y, z, rsq, r, a, e,
    c, s, q, qd, ad, ed, edr, add, edd, qdd;
  
  /* Useful functions */
  sh = sin ( ha );
  ch = cos ( ha );
  sd = sin ( dec );
  cd = cos ( dec );
  sp = sin ( phi );
  cp = cos ( phi );
  chcd = ch * cd;
  sdcp = sd * cp;
  x = - chcd * sp + sdcp;
  y = - sh * cd;
  z = chcd * cp + sd * sp;
  rsq = x * x + y * y;
  r = sqrt ( rsq );
  
  /* Azimuth and elevation */
  a = rsq != 0.0 ? atan2 ( y, x ) : 0.0;
  if ( a < 0.0 ) a += D2PI;
  e = atan2 ( z, r );
  
  /* Parallactic angle */
  c = cd * sp - ch * sdcp;
  s = sh * cp;
  q = ( s != 0.0 || c != 0.0 ) ? atan2 ( s, c ) : DPI - ha;
  
  /* Velocities and accelerations (clamped at zenith/nadir) */
  if ( rsq < TINY ) {
    rsq = TINY;
    r = sqrt ( rsq );
  }
  qd = - x * cp / rsq;
  ad = sp + z * qd;
  ed = cp * y / r;
  edr = ed / r;
  add = edr * ( z * sp + ( 2.0 - rsq ) * qd );
  edd = - r * qd * ad;
  qdd = edr * ( sp + 2.0 * z * qd );
  
  /* Results */
  *az = a;
  *azd = ad;
  *azdd = add;
  *el = e;
  *eld = ed;
  *eldd = edd;
  *pa = q;
  *pad = qd;
  *padd = qdd;
}

void slaDe2h ( double ha, double dec, double phi, double *az, double *el )
  /*
  **  - - - - - - - -
  **   s l a D e 2 h
  **  - - - - - - - -
  **
  **  Equatorial to horizon coordinates:  HA,Dec to Az,El
  **
  **  (double precision)
  **
  **  Given:
  **     ha          double       hour angle
  **     dec         double       declination
  **     phi         double       observatory latitude
  **
  **  Returned:
  **     *az         double       azimuth
  **     *el         double       elevation
  **
  **  Notes:
  **
  **  1)  All the arguments are angles in radians.
  **
  **  2)  Azimuth is returned in the range 0-2pi;  north is zero,
  **      and east is +pi/2.  Elevation is returned in the range
  **      +/-pi/2.
  **
  **  3)  The latitude must be geodetic.  In critical applications,
  **      corrections for polar motion should be applied.
  **
  **  4)  In some applications it will be important to specify the
  **      correct type of hour angle and declination in order to
  **      produce the required type of azimuth and elevation.  In
  **      particular, it may be important to distinguish between
  **      elevation as affected by refraction, which would
  **      require the "observed" HA,Dec, and the elevation
  **      in vacuo, which would require the "topocentric" HA,Dec.
  **      If the effects of diurnal aberration can be neglected, the
  **      "apparent" HA,Dec may be used instead of the topocentric
  **      HA,Dec.
  **
  **  5)  No range checking of arguments is carried out.
  **
  **  6)  In applications which involve many such calculations, rather
  **      than calling the present routine it will be more efficient to
  **      use inline code, having previously computed fixed terms such
  **      as sine and cosine of latitude, and (for tracking a star)
  **      sine and cosine of declination.
  **
  **  Defined in slamac.h:  D2PI
  **
  **  Last revision:   30 November 2000
  **
  **  Copyright P.T.Wallace.  All rights reserved.
  */
{
  double sh, ch, sd, cd, sp, cp, x, y, z, r, a;
  
  /* Useful trig functions */
  sh = sin ( ha );
  ch = cos ( ha );
  sd = sin ( dec );
  cd = cos ( dec );
  sp = sin ( phi );
  cp = cos ( phi );
  
  /* Az,El as x,y,z */
  x = - ch * cd * sp + sd * cp;
  y = - sh * cd;
  z = ch * cd * cp + sd * sp;
  
  /* To spherical */
  r = sqrt ( x * x + y * y );
  a = ( r != 0.0 ) ? atan2 ( y, x ) : 0.0;
  *az = ( a < 0.0 ) ? a + D2PI : a;
  *el = atan2 ( z, r );
}

void slaDh2e ( double az, double el, double phi, double *ha, double *dec )
  /*
  **  - - - - - - - -
  **   s l a D h 2 e
  **  - - - - - - - -
  **
  **  Horizon to equatorial coordinates:  Az,El to HA,Dec
  **
  **  (double precision)
  **
  **  Given:
  **     az          double       azimuth
  **     el          double       elevation
  **     phi         double       observatory latitude
  **
  **  Returned:
  **     *ha         double       hour angle
  **     *dec        double       declination
  **
  **  Notes:
  **
  **  1)  All the arguments are angles in radians.
  **
  **  2)  The sign convention for azimuth is north zero, east +pi/2.
  **
  **  3)  HA is returned in the range +/-pi.  Declination is returned
  **      in the range +/-pi/2.
  **
  **  4)  The is latitude is (in principle) geodetic.  In critical
  **      applications, corrections for polar motion should be applied.
  **
  **  5)  In some applications it will be important to specify the
  **      correct type of elevation in order to produce the required
  **      type of HA,Dec.  In particular, it may be important to
  **      distinguish between the elevation as affected by refraction,
  **      which will yield the "observed" HA,Dec, and the elevation
  **      in vacuo, which will yield the "topocentric" HA,Dec.  If the
  **      effects of diurnal aberration can be neglected, the
  **      topocentric HA,Dec may be used as an approximation to the
  **      "apparent" HA,Dec.
  **
  **  6)  No range checking of arguments is done.
  **
  **  7)  In applications which involve many such calculations, rather
  **      than calling the present routine it will be more efficient to
  **      use inline code, having previously computed fixed terms such
  **      as sine and cosine of latitude.
  **
  **  Last revision:   30 November 2000
  **
  **  Copyright P.T.Wallace.  All rights reserved.
  */
{
  double sa, ca, se, ce, sp, cp, x, y, z, r;
  
  /* Useful trig functions */
  sa = sin ( az );
  ca = cos ( az );
  se = sin ( el );
  ce = cos ( el );
  sp = sin ( phi );
  cp = cos ( phi );
  
  /* HA,Dec as x,y,z */
  x = - ca * ce * sp + se * cp;
  y = - sa * ce;
  z = ca * ce * cp + se * sp;
  
  /* To spherical */
  r = sqrt ( x * x + y * y );
  *ha = ( r != 0.0 ) ? atan2 ( y, x ) : 0.0;
  *dec = atan2 ( z, r );
  return;
}

void slaDtf2d ( int ihour, int imin, double sec, double *days, int *j )
/*
**  - - - - - - - - -
**   s l a D t f 2 d
**  - - - - - - - - -
**
**  Convert hours, minutes, seconds to days.
**
**  (double precision)
**
**  Given:
**     ihour       int           hours
**     imin        int           minutes
**     sec         double        seconds
**
**  Returned:
**     *days       double        interval in days
**     *j          int           status:  0 = OK
**                                        1 = ihour outside range 0-23
**                                        2 = imin outside range 0-59
**                                        3 = sec outside range 0-59.999...
**
**  Notes:
**
**     1)  The result is computed even if any of the range checks fail.
**
**     2)  The sign must be dealt with outside this routine.
**
**  Last revision:   31 January 1997
**
**  Copyright P.T.Wallace.  All rights reserved.
*/

/* Seconds per day */
#define D2S 86400.0

{
/* Preset status */
   *j = 0;

/* Validate sec, min, hour */
   if ( ( sec < 0.0 ) || ( sec >= 60.0 ) ) {
      *j = 3;
      return;
   }
   if ( ( imin < 0 ) || ( imin > 59 ) ) {
      *j = 2;
      return;
   }
   if ( ( ihour < 0 ) || ( ihour > 23 ) ) {
      *j = 1;
      return;
   }

/* Compute interval */
   *days = ( 60.0 * ( 60.0 * (double) ihour + (double) imin ) + sec ) / D2S;
}

void slaDtp2s ( double xi, double eta, double raz, double decz,
                double *ra, double *dec )
/*
**  - - - - - - - - -
**   s l a D t p 2 s
**  - - - - - - - - -
**
**  Transform tangent plane coordinates into spherical.
**
**  (double precision)
**
**  Given:
**     xi,eta      double   tangent plane rectangular coordinates
**     raz,decz    double   spherical coordinates of tangent point
**
**  Returned:
**     *ra,*dec    double   spherical coordinates (0-2pi,+/-pi/2)
**
**  Called:  slaDranrm
**
**  Last revision:   3 June 1995
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
  double sdecz, cdecz, denom;

  sdecz = sin ( decz );
  cdecz = cos ( decz );
  denom = cdecz - eta * sdecz;
  *ra = slaDranrm ( atan2 ( xi, denom ) + raz );
  *dec = atan2 ( sdecz + eta * cdecz, sqrt ( xi * xi + denom * denom ) );
}

double slaEpj ( double date )
/*
**  - - - - - - -
**   s l a E p j
**  - - - - - - -
**
**  Conversion of Modified Julian Date to Julian epoch.
**
**  (double precision)
**
**  Given:
**     date     double      Modified Julian Date (JD - 2400000.5)
**
**  The result is the Julian epoch.
**
**  Reference:
**     Lieske,J.H., 1979. Astron. Astrophys.,73,282.
**
**  Last revision:   31 October 1993
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
  return 2000.0 + ( date - 51544.5 ) / 365.25;
}

double slaEqeqx ( double date )
/*
**  - - - - - - - - -
**   s l a E q e q x
**  - - - - - - - - -
**
**  Equation of the equinoxes (IAU 1994, double precision).
**
**  Given:
**     date    double      TDB (loosely ET) as Modified Julian Date
**                                          (JD-2400000.5)
**
**  The result is the equation of the equinoxes (double precision)
**  in radians:
**
**  Greenwich apparent ST = Greenwich mean ST + equation of the equinoxes
**
**  References:  IAU Resolution C7, Recommendation 3 (1994)
**               Capitaine, N. & Gontier, A.-M., Astron. Astrophys.,
**               275, 645-650 (1993)
**
**  Called:  slaNutc
**
**  Last revision:   21 November 1994
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
#define T2AS 1296000.0                 /* Turns to arc seconds */
#define AS2R 0.4848136811095359949E-5  /* Arc seconds to radians */
{
   double t, om, dpsi, deps, eps0;

/* Interval between basic epoch J2000.0 and current epoch (JC) */
   t = ( date - 51544.5 ) / 36525.0;

/* Longitude of the mean ascending node of the lunar orbit on the
   ecliptic, measured from the mean equinox of date */
   om = AS2R * ( 450160.280 + ( -5.0 * T2AS - 482890.539
                               + ( 7.455 + 0.008 * t ) * t ) * t );

/* Nutation */
   slaNutc ( date, &dpsi, &deps, &eps0 );

/* Equation of the equinoxes */
   return dpsi * cos ( eps0 ) + AS2R * ( 0.00264 * sin ( om ) +
                                         0.000063 * sin ( om + om ) );
}

void slaEqgal ( double dr, double dd, double *dl, double *db )
/*
**  - - - - - - - - -
**   s l a E q g a l
**  - - - - - - - - -
**
**  Transformation from J2000.0 equatorial coordinates to
**  IAU 1958 Galactic coordinates.
**
**  (double precision)
**
**  Given:
**     dr,dd       double       J2000.0 RA,Dec
**
**  Returned:
**     *dl,*db     double       Galactic longitude and latitude l2,b2
**
**  (all arguments are radians)
**
**  Called:
**     slaDcs2c, slaDmxv, slaDcc2s, slaDranrm, slaDrange
**
**  Note:
**     The equatorial coordinates are J2000.0.  Use the routine
**     slaEg50 if conversion from B1950.0 'FK4' coordinates is
**     required.
**
**  Reference:
**     Blaauw et al, Mon.Not.R.astron.Soc.,121,123 (1960)
**
**  Last revision:   21 September 1998
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
   double v1[3], v2[3];

/*
**  l2,b2 system of Galactic coordinates
**
**  p = 192.25       RA of Galactic north pole (mean B1950.0)
**  q =  62.6        inclination of Galactic to mean B1950.0 equator
**  r =  33          longitude of ascending node
**
**  p,q,r are degrees
**
**  Equatorial to Galactic rotation matrix (J2000.0), obtained by
**  applying the standard FK4 to FK5 transformation, for zero proper
**  motion in FK5, to the columns of the B1950 equatorial to
**  Galactic rotation matrix:
*/
   static double rmat[3][3];

   rmat[0][0] = -0.054875539726;
   rmat[0][1] = -0.873437108010;
   rmat[0][2] = -0.483834985808;
   rmat[1][0] =  0.494109453312;
   rmat[1][1] = -0.444829589425;
   rmat[1][2] =  0.746982251810;
   rmat[2][0] = -0.867666135858;
   rmat[2][1] = -0.198076386122;
   rmat[2][2] =  0.455983795705;

/* Spherical to Cartesian */
   slaDcs2c ( dr, dd, v1 );

/* Equatorial to Galactic */
   slaDmxv ( rmat, v1, v2 );

/* Cartesian to spherical */
   slaDcc2s ( v2, dl, db );

/* Express in conventional ranges */
   *dl = slaDranrm ( *dl );
   *db = slaDrange ( *db );
}

void slaEvp ( double date, double deqx, double dvb[3], double dpb[3],
              double dvh[3], double dph[3] )
/*
**  - - - - - - -
**   s l a E v p
**  - - - - - - -
**
**  Barycentric and heliocentric velocity and position of the Earth.
**
**  Given:
**
**     date    double     TDB (loosely ET) as a Modified Julian Date
**                                         (JD-2400000.5)
**
**     deqx    double     Julian epoch (e.g. 2000.0) of mean equator and
**                        equinox of the vectors returned.  If deqx <= 0.0,
**                        all vectors are referred to the mean equator and
**                        equinox (FK5) of epoch date.
**
**  Returned (all 3D Cartesian vectors):
**
**     dvb,dpb double[3]  barycentric velocity, position
**
**     dvh,dph double[3]  heliocentric velocity, position
**
**  (Units are AU/s for velocity and AU for position)
**
**  Called:  slaEpj, slaPrec
**
**  Accuracy:
**
**     The maximum deviations from the JPL DE96 ephemeris are as
**     follows:
**
**     barycentric velocity                  42  cm/s
**     barycentric position                6900  km
**
**     heliocentric velocity                 42  cm/s
**     heliocentric position               1600  km
**
**  This routine is adapted from the BARVEL and BARCOR Fortran
**  subroutines of P.Stumpff, which are described in
**  Astron. Astrophys. Suppl. Ser. 41, 1-8 (1980).  The present
**  routine uses double precision throughout;  most of the other
**  changes are essentially cosmetic and do not affect the
**  results.  However, some adjustments have been made so as to
**  give results that refer to the new (IAU 1976 "FK5") equinox
**  and precession, although the differences these changes make
**  relative to the results from Stumpff's original "FK4" version
**  are smaller than the inherent accuracy of the algorithm.  One
**  minor shortcoming in the original routines that has not been
**  corrected is that better numerical accuracy could be achieved
**  if the various polynomial evaluations were nested.  Note also
**  that one of Stumpff's precession constants differs by 0.001 arcsec
**  from the value given in the Explanatory Supplement to the A.E.
**
**  Defined in slamac.h:  D2PI, DS2R, dmod
**
**  Last revision:   21 March 1999
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
   int ideq, i, j, k;

   double a, pertl,
          pertld, pertr, pertrd, cosa, sina, e, twoe, esq, g, twog,
          phi, f, sf, cf, phid, psid, pertp, pertpd, tl, sinlm, coslm,
          sigma, b, plon, pomg, pecc, flatm, flat;

   double dt, dlocal, dml = 0,
          deps, dparam, dpsi, d1pdro, drd, drld, dtl, dsinls,
          dcosls, dxhd, dyhd, dzhd, dxbd, dybd, dzbd, dcosep,
          dsinep, dyahd, dzahd, dyabd, dzabd, dr,
          dxh, dyh, dzh, dxb, dyb, dzb, dyah, dzah, dyab,
          dzab, depj, deqcor;

   double sn[4], forbel[7], sorbel[17], sinlp[4], coslp[4];

   double dprema[3][3], w, vw[3];

/* Sidereal rate dcsld in longitude, rate ccsgd in mean anomaly */
   static double dcsld = 1.990987e-7;
   static double ccsgd = 1.990969e-7;

/* Some constants used in the calculation of the lunar contribution */
   static double cckm  = 3.122140e-5;
   static double ccmld = 2.661699e-6;
   static double ccfdi = 2.399485e-7;

/* Besselian epoch 1950.0 expressed as a Julian epoch */
   static double b1950 = 1949.9997904423;

/*
** ccpamv(k)=a*m*dl/dt (planets), dc1mme=1-mass(Earth+Moon)
*/
   static double ccpamv[4] = {
      8.326827e-11,
      1.843484e-11,
      1.988712e-12,
      1.881276e-12
   };
   static double dc1mme = 0.99999696;

/*
** ccpam(k)=a*m(planets)
** ccim=inclination(Moon)
*/
   static double ccpam[4] = {
      4.960906e-3,
      2.727436e-3,
      8.392311e-4,
      1.556861e-3
   };
   static double ccim = 8.978749e-2;

/*
** Constants dcfel(i,k) of fast changing elements
*/
   static double dcfel[3][8] = {
      {  1.7400353,                /* dcfel[0][0] */
         6.2565836,                /* dcfel[0][1] */
         4.7199666,                /* dcfel[0][2] */
         1.9636505e-1,             /* dcfel[0][3] */
         4.1547339,                /* dcfel[0][4] */
         4.6524223,                /* dcfel[0][5] */
         4.2620486,                /* dcfel[0][6] */
         1.4740694 },              /* dcfel[0][7] */
      {  6.2833195099091e+2,       /* dcfel[1][0] */
         6.2830194572674e+2,       /* dcfel[1][1] */
         8.3997091449254e+3,       /* dcfel[1][2] */
         8.4334662911720e+3,       /* dcfel[1][3] */
         5.2993466764997e+1,       /* dcfel[1][4] */
         2.1354275911213e+1,       /* dcfel[1][5] */
         7.5025342197656,          /* dcfel[1][6] */
         3.8377331909193 },        /* dcfel[1][7] */
      {  5.2796e-6,                /* dcfel[2][0] */
        -2.6180e-6,                /* dcfel[2][1] */
        -1.9780e-5,                /* dcfel[2][2] */
        -5.6044e-5,                /* dcfel[2][3] */
         5.8845e-6,                /* dcfel[2][4] */
         5.6797e-6,                /* dcfel[2][5] */
         5.5317e-6,                /* dcfel[2][6] */
         5.6093e-6 }               /* dcfel[2][7] */
   };

/*
** Constants dceps and ccsel(i,k) of slowly changing elements
*/
   static double dceps[3] = {
      4.093198e-1,
     -2.271110e-4,
     -2.860401e-8
   };
   static double ccsel[3][17] = {
      {  1.675104e-2,              /* ccsel[0][0]  */
         2.220221e-1,              /* ccsel[0][1]  */
         1.589963,                 /* ccsel[0][2]  */
         2.994089,                 /* ccsel[0][3]  */
         8.155457e-1,              /* ccsel[0][4]  */
         1.735614,                 /* ccsel[0][5]  */
         1.968564,                 /* ccsel[0][6]  */
         1.282417,                 /* ccsel[0][7]  */
         2.280820,                 /* ccsel[0][8]  */
         4.833473e-2,              /* ccsel[0][9]  */
         5.589232e-2,              /* ccsel[0][10] */
         4.634443e-2,              /* ccsel[0][11] */
         8.997041e-3,              /* ccsel[0][12] */
         2.284178e-2,              /* ccsel[0][13] */
         4.350267e-2,              /* ccsel[0][14] */
         1.348204e-2,              /* ccsel[0][15] */
         3.106570e-2 },            /* ccsel[0][16] */
      { -4.179579e-5,              /* ccsel[1][0]  */
         2.809917e-2,              /* ccsel[1][1]  */
         3.418075e-2,              /* ccsel[1][2]  */
         2.590824e-2,              /* ccsel[1][3]  */
         2.486352e-2,              /* ccsel[1][4]  */
         1.763719e-2,              /* ccsel[1][5]  */
         1.524020e-2,              /* ccsel[1][6]  */
         8.703393e-3,              /* ccsel[1][7]  */
         1.918010e-2,              /* ccsel[1][8]  */
         1.641773e-4,              /* ccsel[1][9]  */
        -3.455092e-4,              /* ccsel[1][10] */
        -2.658234e-5,              /* ccsel[1][11] */
         6.329728e-6,              /* ccsel[1][12] */
        -9.941590e-5,              /* ccsel[1][13] */
        -6.839749e-5,              /* ccsel[1][14] */
         1.091504e-5,              /* ccsel[1][15] */
        -1.665665e-4 },            /* ccsel[1][16] */
      { -1.260516e-7,              /* ccsel[2][0]  */
         1.852532e-5,              /* ccsel[2][1]  */
         1.430200e-5,              /* ccsel[2][2]  */
         4.155840e-6,              /* ccsel[2][3]  */
         6.836840e-6,              /* ccsel[2][4]  */
         6.370440e-6,              /* ccsel[2][5]  */
        -2.517152e-6,              /* ccsel[2][6]  */
         2.289292e-5,              /* ccsel[2][7]  */
         4.484520e-6,              /* ccsel[2][8]  */
        -4.654200e-7,              /* ccsel[2][9]  */
        -7.388560e-7,              /* ccsel[2][10] */
         7.757000e-8,              /* ccsel[2][11] */
        -1.939256e-9,              /* ccsel[2][12] */
         6.787400e-8,              /* ccsel[2][13] */
        -2.714956e-7,              /* ccsel[2][14] */
         6.903760e-7,              /* ccsel[2][15] */
        -1.590188e-7 }             /* ccsel[2][16] */
   };

/*
** Constants of the arguments of the short-period perturbations
** by the planets:   dcargs(i,k)
*/
   static double dcargs[2][15] = {
      {  5.0974222,                /* dcargs[0][0]  */
         3.9584962,                /* dcargs[0][1]  */
         1.6338070,                /* dcargs[0][2]  */
         2.5487111,                /* dcargs[0][3]  */
         4.9255514,                /* dcargs[0][4]  */
         1.3363463,                /* dcargs[0][5]  */
         1.6072053,                /* dcargs[0][6]  */
         1.3629480,                /* dcargs[0][7]  */
         5.5657014,                /* dcargs[0][8]  */
         5.0708205,                /* dcargs[0][9]  */
         3.9318944,                /* dcargs[0][10] */
         4.8989497,                /* dcargs[0][11] */
         1.3097446,                /* dcargs[0][12] */
         3.5147141,                /* dcargs[0][13] */
         3.5413158 },              /* dcargs[0][14] */
      { -7.8604195454652e+2,       /* dcargs[1][0]  */
        -5.7533848094674e+2,       /* dcargs[1][1]  */
        -1.1506769618935e+3,       /* dcargs[1][2]  */
        -3.9302097727326e+2,       /* dcargs[1][3]  */
        -5.8849265665348e+2,       /* dcargs[1][4]  */
        -5.5076098609303e+2,       /* dcargs[1][5]  */
        -5.2237501616674e+2,       /* dcargs[1][6]  */
        -1.1790629318198e+3,       /* dcargs[1][7]  */
        -1.0977134971135e+3,       /* dcargs[1][8]  */
        -1.5774000881978e+2,       /* dcargs[1][9]  */
         5.2963464780000e+1,       /* dcargs[1][10] */
         3.9809289073258e+1,       /* dcargs[1][11] */
         7.7540959633708e+1,       /* dcargs[1][12] */
         7.9618578146517e+1,       /* dcargs[1][13] */
        -5.4868336758022e+2 }      /* dcargs[1][14] */
   };

/*
** Amplitudes ccamps(n,k) of the short-period perturbations
*/
   static double ccamps[5][15] = {
      { -2.279594e-5,              /* ccamps[0][0]  */
        -3.494537e-5,              /* ccamps[0][1]  */
         6.593466e-7,              /* ccamps[0][2]  */
         1.140767e-5,              /* ccamps[0][3]  */
         9.516893e-6,              /* ccamps[0][4]  */
         7.310990e-6,              /* ccamps[0][5]  */
        -2.603449e-6,              /* ccamps[0][6]  */
        -3.228859e-6,              /* ccamps[0][7]  */
         3.442177e-7,              /* ccamps[0][8]  */
         8.702406e-6,              /* ccamps[0][9]  */
        -1.488378e-6,              /* ccamps[0][10] */
        -8.043059e-6,              /* ccamps[0][11] */
         3.699128e-6,              /* ccamps[0][12] */
         2.550120e-6,              /* ccamps[0][13] */
        -6.351059e-7 },            /* ccamps[0][14] */
      {  1.407414e-5,              /* ccamps[1][0]  */
         2.860401e-7,              /* ccamps[1][1]  */
         1.322572e-5,              /* ccamps[1][2]  */
        -2.049792e-5,              /* ccamps[1][3]  */
        -2.748894e-6,              /* ccamps[1][4]  */
        -1.924710e-6,              /* ccamps[1][5]  */
         7.359472e-6,              /* ccamps[1][6]  */
         1.308997e-7,              /* ccamps[1][7]  */
         2.671323e-6,              /* ccamps[1][8]  */
        -8.421214e-6,              /* ccamps[1][9]  */
        -1.251789e-5,              /* ccamps[1][10] */
        -2.991300e-6,              /* ccamps[1][11] */
        -3.316126e-6,              /* ccamps[1][12] */
        -1.241123e-6,              /* ccamps[1][13] */
         2.341650e-6 },            /* ccamps[1][14] */
      {  8.273188e-6,              /* ccamps[2][0]  */
         1.289448e-7,              /* ccamps[2][1]  */
         9.258695e-6,              /* ccamps[2][2]  */
        -4.747930e-6,              /* ccamps[2][3]  */
        -1.319381e-6,              /* ccamps[2][4]  */
        -8.772849e-7,              /* ccamps[2][5]  */
         3.168357e-6,              /* ccamps[2][6]  */
         1.013137e-7,              /* ccamps[2][7]  */
         1.832858e-6,              /* ccamps[2][8]  */
        -1.372341e-6,              /* ccamps[2][9]  */
         5.226868e-7,              /* ccamps[2][10] */
         1.473654e-7,              /* ccamps[2][11] */
         2.901257e-7,              /* ccamps[2][12] */
         9.901116e-8,              /* ccamps[2][13] */
         1.061492e-6 },            /* ccamps[2][14] */
      {  1.340565e-5,              /* ccamps[3][0]  */
         1.627237e-5,              /* ccamps[3][1]  */
        -4.674248e-7,              /* ccamps[3][2]  */
        -2.638763e-6,              /* ccamps[3][3]  */
        -4.549908e-6,              /* ccamps[3][4]  */
        -3.334143e-6,              /* ccamps[3][5]  */
         1.119056e-6,              /* ccamps[3][6]  */
         2.403899e-6,              /* ccamps[3][7]  */
        -2.394688e-7,              /* ccamps[3][8]  */
        -1.455234e-6,              /* ccamps[3][9]  */
        -2.049301e-7,              /* ccamps[3][10] */
        -3.154542e-7,              /* ccamps[3][11] */
         3.407826e-7,              /* ccamps[3][12] */
         2.210482e-7,              /* ccamps[3][13] */
         2.878231e-7 },            /* ccamps[3][14] */
      { -2.490817e-7,              /* ccamps[4][0]  */
        -1.823138e-7,              /* ccamps[4][1]  */
        -3.646275e-7,              /* ccamps[4][2]  */
        -1.245408e-7,              /* ccamps[4][3]  */
        -1.864821e-7,              /* ccamps[4][4]  */
        -1.745256e-7,              /* ccamps[4][5]  */
        -1.655307e-7,              /* ccamps[4][6]  */
        -3.736225e-7,              /* ccamps[4][7]  */
        -3.478444e-7,              /* ccamps[4][8]  */
        -4.998479e-8,              /* ccamps[4][9]  */
         0.0,                      /* ccamps[4][10] */
         0.0,                      /* ccamps[4][11] */
         0.0,                      /* ccamps[4][12] */
         0.0,                      /* ccamps[4][13] */
         0.0 }                     /* ccamps[4][14] */
    };

/*
** Constants of the secular perturbations in longitude
** ccsec3 and ccsec(n,k)
*/
   static double ccsec3 = -7.757020e-8;
   static double ccsec[3][4] = {
      {  1.289600e-6,              /* ccsec[0][0] */
         3.102810e-5,              /* ccsec[0][1] */
         9.124190e-6,              /* ccsec[0][2] */
         9.793240e-7 },            /* ccsec[0][3] */
      {  5.550147e-1,              /* ccsec[1][0] */
         4.035027,                 /* ccsec[1][1] */
         9.990265e-1,              /* ccsec[1][2] */
         5.508259 },               /* ccsec[1][3] */
      {  2.076942,                 /* ccsec[2][0] */
         3.525565e-1,              /* ccsec[2][1] */
         2.622706,                 /* ccsec[2][2] */
         1.559103e+1 }             /* ccsec[2][3] */
   };

/*
** Constants dcargm(i,k) of the arguments of the perturbations
** of the motion of the Moon
*/
   static double dcargm[2][3] = {
      {  5.167983,                 /* dcargm[0][0] */
         5.491315,                 /* dcargm[0][1] */
         5.959853 },               /* dcargm[0][2] */
      {  8.3286911095275e+3,       /* dcargm[1][0] */
        -7.2140632838100e+3,       /* dcargm[1][1] */
         1.5542754389685e+4 }      /* dcargm[1][2] */
   };

/*
** Amplitudes ccampm(n,k) of the perturbations of the Moon
*/
   static double ccampm[4][3] = {
      {  1.097594e-1,              /* ccampm[0][0] */
        -2.223581e-2,              /* ccampm[0][1] */
         1.148966e-2 },            /* ccampm[0][2] */
      {  2.896773e-7,              /* ccampm[1][0] */
         5.083103e-8,              /* ccampm[1][1] */
         5.658888e-8 },            /* ccampm[1][2] */
      {  5.450474e-2,              /* ccampm[2][0] */
         1.002548e-2,              /* ccampm[2][1] */
         8.249439e-3 },            /* ccampm[2][2] */
      {  1.438491e-7,              /* ccampm[3][0] */
        -2.291823e-8,              /* ccampm[3][1] */
         4.063015e-8 }             /* ccampm[3][2] */
   };

/*
**
** Execution
** ---------
**
** Control parameter ideq, and time arguments
*/
   ideq = ( deqx <= 0.0 ) ? 0 : 1;
   dt = ( date - 15019.5 ) / 36525.0;

/* Values of all elements for the instant date */
   for ( k = 0; k < 8; k++ ) {
      dlocal = dmod ( dcfel[0][k]
             + dt * ( dcfel[1][k]
               + dt * dcfel[2][k] ), D2PI );
      if ( k == 0 ) {
         dml = dlocal;
      } else {
         forbel[k-1] = dlocal;
      }
   }
   deps = dmod ( dceps[0]
        + dt * ( dceps[1]
          + dt * dceps[2] ) , D2PI );
   for ( k = 0; k < 17; k++ ) {
      sorbel[k] = dmod ( ccsel[0][k]
                + dt * ( ccsel[1][k]
                  + dt * ccsel[2][k] ), D2PI );
   }

/* Secular perturbations in longitude */
   for ( k = 0; k < 4; k++ ) {
      a = dmod ( ccsec[1][k] + dt * ccsec[2][k] , D2PI );
      sn[k] = sin ( a );
   }

/* Periodic perturbations of the EMB (Earth-Moon barycentre) */
   pertl = ccsec[0][0] * sn[0]
         + ccsec[0][1] * sn[1]
       + ( ccsec[0][2] + dt * ccsec3 ) * sn[2]
         + ccsec[0][3] * sn[3];
   pertld = 0.0;
   pertr = 0.0;
   pertrd = 0.0;
   for ( k = 0; k < 15; k++ ) {
      a = dmod ( dcargs[0][k] + dt * dcargs[1][k] , D2PI );
      cosa = cos ( a );
      sina = sin ( a );
      pertl += ccamps[0][k] * cosa + ccamps[1][k] * sina;
      pertr += ccamps[2][k] * cosa + ccamps[3][k] * sina;
      if ( k < 10 ) {
         pertld += ( ccamps[1][k] * cosa
                   - ccamps[0][k] * sina ) * ccamps[4][k];
         pertrd += ( ccamps[3][k] * cosa
                   - ccamps[2][k] * sina ) * ccamps[4][k];
      }
   }

/* Elliptic part of the motion of the EMB */
   e = sorbel[0];
   twoe = e + e;
   esq = e * e;
   dparam = 1.0 - esq;
   g = forbel[0];
   twog = g + g;
   phi = twoe * ( ( 1.0 - esq / 8.0 ) * sin ( g )
                + 5.0 * e * sin ( twog ) / 8.0
                + 13.0 * esq * sin ( g + twog ) / 24.0 );
   f = forbel[0] + phi;
   sf = sin ( f );
   cf = cos ( f );
   dpsi = dparam / ( 1.0 + e * cf );
   phid = twoe * ccsgd * ( ( 1.0 + esq * 1.5 ) * cf
                         + e * ( 1.25 - sf * sf / 2.0 ) );
   psid = ccsgd * e * sf / sqrt ( dparam );

/* Perturbed heliocentric motion of the EMB */
   d1pdro = 1.0 + pertr;
   drd = d1pdro * ( psid + dpsi * pertrd );
   drld = d1pdro * dpsi * ( dcsld + phid + pertld );
   dtl = dmod ( dml + phi + pertl , D2PI );
   dsinls = sin ( dtl );
   dcosls = cos ( dtl );
   dxhd = drd * dcosls - drld * dsinls;
   dyhd = drd * dsinls + drld * dcosls;

/* Influence of eccentricity, evection and variation on the
** geocentric motion of the Moon */
   pertl = 0.0;
   pertld = 0.0;
   pertp = 0.0;
   pertpd = 0.0;
   for ( k = 0; k < 3; k++ ) {
      a = dmod ( dcargm[0][k] + dt * dcargm[1][k] , D2PI );
      sina = sin ( a );
      cosa = cos ( a );
      pertl += ccampm[0][k] * sina;
      pertld += ccampm[1][k] * cosa;
      pertp += ccampm[2][k] * cosa;
      pertpd += - ccampm[3][k] * sina;
   }

/* Heliocentric motion of the Earth */
   tl = forbel[1] + pertl;
   sinlm = sin ( tl );
   coslm = cos ( tl );
   sigma = cckm / ( 1.0 + pertp );
   a = sigma * ( ccmld + pertld );
   b = sigma * pertpd;
   dxhd  += a * sinlm + b * coslm;
   dyhd  += - a * coslm + b * sinlm;
   dzhd  = - sigma * ccfdi * cos ( forbel[2] );

/* Barycentric motion of the Earth */
   dxbd = dxhd * dc1mme;
   dybd = dyhd * dc1mme;
   dzbd = dzhd * dc1mme;
   for ( k = 0; k < 4; k++ ) {
      plon = forbel[k+3];
      pomg = sorbel[k+1];
      pecc = sorbel[k+9];
      tl = dmod( plon + 2.0 * pecc * sin ( plon - pomg ) , D2PI );
      sinlp[k] = sin ( tl );
      coslp[k] = cos ( tl );
      dxbd += ccpamv[k] * ( sinlp[k] + pecc * sin ( pomg ) );
      dybd += - ccpamv[k] * ( coslp[k] + pecc * cos ( pomg ) );
      dzbd += - ccpamv[k] * sorbel[k+13] * cos ( plon - sorbel[k+5] );
   }

/* Transition to mean equator of date */
   dcosep = cos ( deps );
   dsinep = sin ( deps );
   dyahd  = dcosep * dyhd - dsinep * dzhd;
   dzahd  = dsinep * dyhd + dcosep * dzhd;
   dyabd  = dcosep * dybd - dsinep * dzbd;
   dzabd  = dsinep * dybd + dcosep * dzbd;

/* Heliocentric coordinates of the Earth */
   dr = dpsi * d1pdro;
   flatm = ccim * sin ( forbel[2] );
   a = sigma * cos ( flatm );
   dxh = dr * dcosls - a * coslm;
   dyh = dr * dsinls - a * sinlm;
   dzh = - sigma * sin ( flatm );

/* Barycentric coordinates of the Earth */
   dxb = dxh * dc1mme;
   dyb = dyh * dc1mme;
   dzb = dzh * dc1mme;
   for ( k = 0; k < 4; k++ ) {
      flat = sorbel[k+13] * sin ( forbel[k+3] - sorbel[k+5] );
      a = ccpam[k] * (1.0 - sorbel[k+9] * cos ( forbel[k+3] - sorbel[k+1]));
      b = a * cos(flat);
      dxb -= b * coslp[k];
      dyb -= b * sinlp[k];
      dzb -= a * sin ( flat );
   }

/* Transition to mean equator of date */
   dyah = dcosep * dyh - dsinep * dzh;
   dzah = dsinep * dyh + dcosep * dzh;
   dyab = dcosep * dyb - dsinep * dzb;
   dzab = dsinep * dyb + dcosep * dzb;

/* Copy result components into vectors, correcting for FK4 equinox */
   depj = slaEpj ( date );
   deqcor = DS2R * ( 0.035 + ( 0.00085 * ( depj - b1950 ) ) );
   dvh[0] = dxhd - deqcor * dyahd;
   dvh[1] = dyahd + deqcor * dxhd;
   dvh[2] = dzahd;
   dvb[0] = dxbd - deqcor * dyabd;
   dvb[1] = dyabd + deqcor * dxbd;
   dvb[2] = dzabd;
   dph[0] = dxh - deqcor * dyah;
   dph[1] = dyah + deqcor * dxh;
   dph[2] = dzah;
   dpb[0] = dxb - deqcor * dyab;
   dpb[1] = dyab + deqcor * dxb;
   dpb[2] = dzab;

/* Was precession to another equinox requested? */
   if ( ideq != 0 ) {

   /* Yes: compute precession matrix from MJD date to Julian Epoch deqx */
      slaPrec ( depj, deqx, dprema );

   /* Rotate dvh */
      for ( j = 0; j < 3; j++ ) {
         w = 0.0;
         for ( i = 0; i < 3; i++ ) {
            w += dprema[j][i] * dvh[i];
         }
         vw[j] = w;
      }
      for ( j = 0; j < 3; j++ ) {
         dvh[j] = vw[j];
      }

   /* Rotate dvb */
      for ( j = 0; j < 3; j++ ) {
         w = 0.0;
         for ( i = 0; i < 3; i++ ) {
            w += dprema[j][i] * dvb[i];
         }
         vw[j] = w;
      }
      for ( j = 0; j < 3; j++ ) {
         dvb[j] = vw[j];
      }

   /* Rotate dph */
      for ( j = 0; j < 3; j++ ) {
         w = 0.0;
         for ( i = 0; i < 3; i++ ) {
            w += dprema[j][i] * dph[i];
         }
         vw[j] = w;
      }
      for ( j = 0; j < 3; j++ ) {
         dph[j] = vw[j];
      }

   /* Rotate dpb */
      for ( j = 0; j < 3; j++ ) {
         w = 0.0;
         for ( i = 0; i < 3; i++ ) {
            w += dprema[j][i] * dpb[i];
         }
         vw[j] = w;
      }
      for ( j = 0; j < 3; j++ ) {
         dpb[j] = vw[j];
      }
   }
}

void slaMap ( double rm, double dm, double pr, double pd,
              double px, double rv, double eq, double date,
              double *ra, double *da )
/*
**  - - - - - - -
**   s l a M a p
**  - - - - - - -
**
**  Transform star RA,Dec from mean place to geocentric apparent.
**
**  The reference frames and timescales used are post IAU 1976.
**
**  References:
**     1984 Astronomical Almanac, pp B39-B41.
**     (also Lederle & Schwan, Astron. Astrophys. 134, 1-6, 1984)
**
**  Given:
**     rm,dm    double     mean RA,Dec (rad)
**     pr,pd    double     proper motions:  RA,Dec changes per Julian year
**     px       double     parallax (arcsec)
**     rv       double     radial velocity (km/sec, +ve if receding)
**     eq       double     epoch and equinox of star data (Julian)
**     date     double     TDB for apparent place (JD-2400000.5)
**
**  Returned:
**     *ra,*da  double     apparent RA,Dec (rad)
**
**  Called:
**     slaMappa       star-independent parameters
**     slaMapqk       quick mean to apparent
**
**  Notes:
**
**  1)  eq is the Julian epoch specifying both the reference frame and
**      the epoch of the position - usually 2000.  For positions where
**      the epoch and equinox are different, use the routine slaPm to
**      apply proper motion corrections before using this routine.
**
**  2)  The distinction between the required TDB and TT is always
**      negligible.  Moreover, for all but the most critical
**      applications UTC is adequate.
**
**  3)  The proper motions in RA are dRA/dt rather than cos(Dec)*dRA/dt.
**
**  4)  This routine may be wasteful for some applications because it
**      recomputes the Earth position/velocity and the precession-
**      nutation matrix each time, and because it allows for parallax
**      and proper motion.  Where multiple transformations are to be
**      carried out for one epoch, a faster method is to call the
**      slaMappa routine once and then either the slaMapqk routine
**      (which includes parallax and proper motion) or slaMapqkz (which
**      assumes zero parallax and proper motion).
**
**  5)  The accuracy is sub-milliarcsecond, limited by the
**      precession-nutation model (IAU 1976 precession, Shirai &
**      Fukushima 2001 forced nutation and precession corrections).
**
**  6)  The accuracy is further limited by the routine slaEvp, called
**      by slaMappa, which computes the Earth position and velocity
**      using the methods of Stumpff.  The maximum error is about
**      0.3 mas.
**
**  Last revision:   17 September 2001
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
   double amprms[21];

/* Star-independent parameters */
   slaMappa ( eq, date, amprms );

/* Mean to apparent */
   slaMapqk ( rm, dm, pr, pd, px, rv, amprms, ra, da );
}

void slaMappa ( double eq, double date, double amprms[21] )
/*
**  - - - - - - - - -
**   s l a M a p p a
**  - - - - - - - - -
**
**  Compute star-independent parameters in preparation for
**  conversions between mean place and geocentric apparent place.
**
**  The parameters produced by this routine are required in the
**  parallax, light deflection, aberration, and precession/nutation
**  parts of the mean/apparent transformations.
**
**  The reference frames and timescales used are post IAU 1976.
**
**  Given:
**     eq       double      epoch of mean equinox to be used (Julian)
**     date     double      TDB (JD-2400000.5)
**
**  Returned:
**     amprms   double[21]  star-independent mean-to-apparent parameters:
**
**       (0)      time interval for proper motion (Julian years)
**       (1-3)    barycentric position of the Earth (AU)
**       (4-6)    heliocentric direction of the Earth (unit vector)
**       (7)      (grav rad Sun)*2/(Sun-Earth distance)
**       (8-10)   abv: barycentric Earth velocity in units of c
**       (11)     sqrt(1-v**2) where v=modulus(abv)
**       (12-20)  precession/nutation (3,3) matrix
**
**  References:
**     1984 Astronomical Almanac, pp B39-B41.
**     (also Lederle & Schwan, Astron. Astrophys. 134, 1-6, 1984)
**
**  Notes:
**
**  1)  For date, the distinction between the required TDB and TT
**      is always negligible.  Moreover, for all but the most
**      critical applications UTC is adequate.
**
**  2)  The vectors amprms(1-3) and amprms(4-6) are referred to the
**      mean equinox and equator of epoch eq.
**
**  3)  The parameters AMPRMS produced by this routine are used by
**      slaAmpqk, slaMapqk and slaMapqkz.
**
**  4)  The accuracy is sub-milliarcsecond, limited by the
**      precession-nutation model (IAU 1976 precession, Shirai &
**      Fukushima 2001 forced nutation and precession corrections).
**
**  5)  A further limit to the accuracy of routines using the parameter
**      array AMPRMS is imposed by the routine slaEvp, used here to
**      compute the Earth position and velocity by the methods of
**      Stumpff.  The maximum error in the resulting aberration
**      corrections is about 0.3 milliarcsecond.
**
**  Called:
**     slaEpj, slaEvp, slaDvn, slaPrenut
**
**  Last revision:   17 September 2001
**
**  Copyright P.T.Wallace.  All rights reserved.
*/

#define CR 499.004782     /* Light time for 1 AU (sec) */
#define GR2 1.974126e-8   /* Gravitational radius of the Sun x 2:
                                                  (2*mu/c**2, au) */
{
   int i;

   double ebd[3], ehd[3], eh[3], e, vn[3], vm;

/* Time interval for proper motion correction */
   amprms[0] = slaEpj ( date ) - eq;

/* Get Earth barycentric and heliocentric position and velocity */
   slaEvp ( date, eq, ebd, &amprms[1], ehd, eh );

/* Heliocentric direction of Earth (normalized) and modulus */
   slaDvn ( eh, &amprms[4], &e );

/* Light deflection parameter */
   amprms[7] = GR2 / e;

/* Aberration parameters */
   for ( i = 0; i < 3; i++ ) {
      amprms[i+8] = ebd[i] * CR;
   }
   slaDvn ( &amprms[8], vn, &vm );
   amprms[11] = sqrt ( 1.0 - vm * vm );

/* Precession/nutation matrix */
   slaPrenut ( eq, date, (double(*)[3]) &amprms[12] );
}

void slaMapqk ( double rm, double dm, double pr, double pd,
                double px, double rv, double amprms[21],
                double *ra, double *da )
/*
**  - - - - - - - - -
**   s l a M a p q k
**  - - - - - - - - -
**
**  Quick mean to apparent place:  transform a star RA,Dec from
**  mean place to geocentric apparent place, given the
**  star-independent parameters.
**
**  Use of this routine is appropriate when efficiency is important
**  and where many star positions, all referred to the same equator
**  and equinox, are to be transformed for one epoch.  The
**  star-independent parameters can be obtained by calling the
**  slaMappa routine.
**
**  If the parallax and proper motions are zero the slaMapqkz
**  routine can be used instead.
**
**  The reference frames and timescales used are post IAU 1976.
**
**  Given:
**     rm,dm    double      mean RA,Dec (rad)
**     pr,pd    double      proper motions:  RA,Dec changes per Julian year
**     px       double      parallax (arcsec)
**     rv       double      radial velocity (km/sec, +ve if receding)
**
**     amprms   double[21]  star-independent mean-to-apparent parameters:
**
**       (0)      time interval for proper motion (Julian years)
**       (1-3)    barycentric position of the Earth (AU)
**       (4-6)    heliocentric direction of the Earth (unit vector)
**       (7)      (grav rad Sun)*2/(Sun-Earth distance)
**       (8-10)   barycentric Earth velocity in units of c
**       (11)     sqrt(1-v**2) where v=modulus(abv)
**       (12-20)  precession/nutation (3,3) matrix
**
**  Returned:
**     *ra,*da  double      apparent RA,Dec (rad)
**
**  References:
**     1984 Astronomical Almanac, pp B39-B41.
**     (also Lederle & Schwan, Astron. Astrophys. 134, 1-6, 1984)
**
**  Notes:
**
**    1)  The vectors amprms(1-3) and amprms(4-6) are referred to
**        the mean equinox and equator of epoch eq.
**
**    2)  Strictly speaking, the routine is not valid for solar-system
**        sources, though the error will usually be extremely small.
**        However, to prevent gross errors in the case where the
**        position of the Sun is specified, the gravitational
**        deflection term is restrained within about 920 arcsec of the
**        centre of the Sun's disc.  The term has a maximum value of
**        about 1.85 arcsec at this radius, and decreases to zero as
**        the centre of the disc is approached.
**
**  Called:
**     slaDcs2c       spherical to Cartesian
**     slaDvdv        dot product
**     slaDmxv        matrix x vector
**     slaDcc2s       Cartesian to spherical
**     slaDranrm      normalize angle 0-2pi
**
**  Defined in slamac.h:  DAS2R
**
**  Last revision:   15 January 2000
**
**  Copyright P.T.Wallace.  All rights reserved.
*/

#define VF 0.21094502     /* Km/s to AU/year */

{
   int i;
   double pmt, gr2e, ab1, eb[3], ehn[3], abv[3],
          q[3], pxr, w, em[3], p[3], pn[3], pde, pdep1,
          p1[3], p1dv, p2[3], p3[3];

/* Unpack scalar and vector parameters */
   pmt = amprms[0];
   gr2e = amprms[7];
   ab1 = amprms[11];
   for ( i = 0; i < 3; i++ )
   {
      eb[i] = amprms[i+1];
      ehn[i] = amprms[i+4];
      abv[i] = amprms[i+8];
   }

/* Spherical to x,y,z */
   slaDcs2c ( rm, dm, q );

/* Space motion (radians per year) */
   pxr = px * DAS2R;
   w = VF * rv * pxr;
   em[0] = (-pr * q[1]) - ( pd * cos ( rm ) * sin ( dm ) ) + ( w * q[0] );
   em[1] = ( pr * q[0]) - ( pd * sin ( rm ) * sin ( dm ) ) + ( w * q[1] );
   em[2] =                ( pd * cos ( dm )              ) + ( w * q[2] );

/* Geocentric direction of star (normalized) */
   for ( i = 0; i < 3; i++ ) {
      p[i] = q[i] + ( pmt * em[i] ) - ( pxr * eb[i] );
   }
   slaDvn ( p, pn, &w );

/* Light deflection (restrained within the Sun's disc) */
   pde = slaDvdv ( pn, ehn );
   pdep1 = 1.0 + pde;
   w = gr2e / gmax ( pdep1, 1.0e-5 );
   for ( i = 0; i < 3; i++ ) {
      p1[i] = pn[i] + ( w * ( ehn[i] - pde * pn[i] ) );
   }

/* Aberration (normalization omitted) */
   p1dv = slaDvdv ( p1, abv );
   w = 1.0 + p1dv / ( ab1 + 1.0 );
   for ( i = 0; i < 3; i++ ) {
      p2[i] = ab1 * p1[i] + w * abv[i];
   }

/* Precession and nutation */
   slaDmxv ( (double(*)[3]) &amprms[12], p2, p3 );

/* Geocentric apparent RA,dec */
   slaDcc2s ( p3, ra, da );

   *ra = slaDranrm ( *ra );
}

void slaNut ( double date, double rmatn[3][3] )
/*
**  - - - - - - -
**   s l a N u t
**  - - - - - - -
**
**  Form the matrix of nutation for a given date - Shirai & Fukushima
**  2001 theory
**
**  (double precision)
**
**  Reference:
**     Shirai, T. & Fukushima, T., Astron.J. 121, 3270-3283 (2001).
**
**  Given:
**     date   double        TDB (loosely ET) as Modified Julian Date
**                                           (=JD-2400000.5)
**
**  Returned:
**     rmatn  double[3][3]  nutation matrix
**
**  The matrix is in the sense   v(true)  =  rmatn * v(mean) .
**
**  Called:   slaNutc, slaDeuler
**
**  Last revision:   17 September 2001
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
   double dpsi, deps, eps0;

/* Nutation components and mean obliquity */
   slaNutc ( date, &dpsi, &deps, &eps0 );

/* Rotation matrix */
   slaDeuler ( (char*)"xzx", eps0, -dpsi, - ( eps0 + deps ), rmatn );
}

void slaNutc ( double date, double *dpsi, double *deps, double *eps0 )
/*
**  - - - - - - - -
**   s l a N u t c
**  - - - - - - - -
**
**  Nutation:  longitude & obliquity components and mean obliquity,
**  using the Shirai & Fukushima (2001) theory.
**
**  Given:
**     date        double    TDB (loosely ET) as Modified Julian Date
**                                                 (JD-2400000.5)
**  Returned:
**     dpsi,deps   double    nutation in longitude,obliquity
**     eps0        double    mean obliquity
**
**  Notes:
**
**  1  The routine predicts forced nutation (but not free core nutation)
**     plus corrections to the IAU 1976 precession model.
**
**  2  Earth attitude predictions made by combining the present nutation
**     model with IAU 1976 precession are accurate to 1 mas (with respect
**     to the ICRF) for a few decades around 2000.
**
**  3  The slaNutc80 routine is the equivalent of the present routine
**     but using the IAU 1980 nutation theory.  The older theory is less
**     accurate, leading to errors as large as 350 mas over the interval
**     1900-2100, mainly because of the error in the IAU 1976 precession.
**
**  References:
**
**     Shirai, T. & Fukushima, T., Astron.J. 121, 3270-3283 (2001).
**
**     Fukushima, T., 1991, Astron.Astrophys. 244, L11 (1991).
**
**     Simon, J. L., Bretagnon, P., Chapront, J., Chapront-Touze, M.,
**     Francou, G. & Laskar, J., Astron.Astrophys. 282, 663 (1994).
**
**  Called:  slaDeuler, slaPrec, slaEpj, slaDmxm
**
**  Last revision:   7 October 2001
**
**  Copyright P.T.Wallace.  All rights reserved.
*/

#define TURNAS 1296000.0          /* Arc seconds in a full circle */
#define DJM0 51544.5              /* Reference epoch (J2000), MJD */
#define DJC 36525.0               /* Days per Julian century */

{

/*
** --------------------------------
** The SF2001 forced nutation model
** --------------------------------
*/

/* Coefficients of fundamental angles */
   static int na[][9] = {
      {  0,    0,    0,    0,   -1,    0,    0,    0,    0 },
      {  0,    0,    2,   -2,    2,    0,    0,    0,    0 },
      {  0,    0,    2,    0,    2,    0,    0,    0,    0 },
      {  0,    0,    0,    0,   -2,    0,    0,    0,    0 },
      {  0,    1,    0,    0,    0,    0,    0,    0,    0 },
      {  0,    1,    2,   -2,    2,    0,    0,    0,    0 },
      {  1,    0,    0,    0,    0,    0,    0,    0,    0 },
      {  0,    0,    2,    0,    1,    0,    0,    0,    0 },
      {  1,    0,    2,    0,    2,    0,    0,    0,    0 },
      {  0,   -1,    2,   -2,    2,    0,    0,    0,    0 },
      {  0,    0,    2,   -2,    1,    0,    0,    0,    0 },
      { -1,    0,    2,    0,    2,    0,    0,    0,    0 },
      { -1,    0,    0,    2,    0,    0,    0,    0,    0 },
      {  1,    0,    0,    0,    1,    0,    0,    0,    0 },
      {  1,    0,    0,    0,   -1,    0,    0,    0,    0 },
      { -1,    0,    2,    2,    2,    0,    0,    0,    0 },
      {  1,    0,    2,    0,    1,    0,    0,    0,    0 },
      { -2,    0,    2,    0,    1,    0,    0,    0,    0 },
      {  0,    0,    0,    2,    0,    0,    0,    0,    0 },
      {  0,    0,    2,    2,    2,    0,    0,    0,    0 },
      {  2,    0,    0,   -2,    0,    0,    0,    0,    0 },
      {  2,    0,    2,    0,    2,    0,    0,    0,    0 },
      {  1,    0,    2,   -2,    2,    0,    0,    0,    0 },
      { -1,    0,    2,    0,    1,    0,    0,    0,    0 },
      {  2,    0,    0,    0,    0,    0,    0,    0,    0 },
      {  0,    0,    2,    0,    0,    0,    0,    0,    0 },
      {  0,    1,    0,    0,    1,    0,    0,    0,    0 },
      { -1,    0,    0,    2,    1,    0,    0,    0,    0 },
      {  0,    2,    2,   -2,    2,    0,    0,    0,    0 },
      {  0,    0,    2,   -2,    0,    0,    0,    0,    0 },
      { -1,    0,    0,    2,   -1,    0,    0,    0,    0 },
      {  0,    1,    0,    0,   -1,    0,    0,    0,    0 },
      {  0,    2,    0,    0,    0,    0,    0,    0,    0 },
      { -1,    0,    2,    2,    1,    0,    0,    0,    0 },
      {  1,    0,    2,    2,    2,    0,    0,    0,    0 },
      {  0,    1,    2,    0,    2,    0,    0,    0,    0 },
      { -2,    0,    2,    0,    0,    0,    0,    0,    0 },
      {  0,    0,    2,    2,    1,    0,    0,    0,    0 },
      {  0,   -1,    2,    0,    2,    0,    0,    0,    0 },
      {  0,    0,    0,    2,    1,    0,    0,    0,    0 },
      {  1,    0,    2,   -2,    1,    0,    0,    0,    0 },
      {  2,    0,    0,   -2,   -1,    0,    0,    0,    0 },
      {  2,    0,    2,   -2,    2,    0,    0,    0,    0 },
      {  2,    0,    2,    0,    1,    0,    0,    0,    0 },
      {  0,    0,    0,    2,   -1,    0,    0,    0,    0 },
      {  0,   -1,    2,   -2,    1,    0,    0,    0,    0 },
      { -1,   -1,    0,    2,    0,    0,    0,    0,    0 },
      {  2,    0,    0,   -2,    1,    0,    0,    0,    0 },
      {  1,    0,    0,    2,    0,    0,    0,    0,    0 },
      {  0,    1,    2,   -2,    1,    0,    0,    0,    0 },
      {  1,   -1,    0,    0,    0,    0,    0,    0,    0 },
      { -2,    0,    2,    0,    2,    0,    0,    0,    0 },
      {  0,   -1,    0,    2,    0,    0,    0,    0,    0 },
      {  3,    0,    2,    0,    2,    0,    0,    0,    0 },
      {  0,    0,    0,    1,    0,    0,    0,    0,    0 },
      {  1,   -1,    2,    0,    2,    0,    0,    0,    0 },
      {  1,    0,    0,   -1,    0,    0,    0,    0,    0 },
      { -1,   -1,    2,    2,    2,    0,    0,    0,    0 },
      { -1,    0,    2,    0,    0,    0,    0,    0,    0 },
      {  2,    0,    0,    0,   -1,    0,    0,    0,    0 },
      {  0,   -1,    2,    2,    2,    0,    0,    0,    0 },
      {  1,    1,    2,    0,    2,    0,    0,    0,    0 },
      {  2,    0,    0,    0,    1,    0,    0,    0,    0 },
      {  1,    1,    0,    0,    0,    0,    0,    0,    0 },
      {  1,    0,   -2,    2,   -1,    0,    0,    0,    0 },
      {  1,    0,    2,    0,    0,    0,    0,    0,    0 },
      { -1,    1,    0,    1,    0,    0,    0,    0,    0 },
      {  1,    0,    0,    0,    2,    0,    0,    0,    0 },
      { -1,    0,    1,    0,    1,    0,    0,    0,    0 },
      {  0,    0,    2,    1,    2,    0,    0,    0,    0 },
      { -1,    1,    0,    1,    1,    0,    0,    0,    0 },
      { -1,    0,    2,    4,    2,    0,    0,    0,    0 },
      {  0,   -2,    2,   -2,    1,    0,    0,    0,    0 },
      {  1,    0,    2,    2,    1,    0,    0,    0,    0 },
      {  1,    0,    0,    0,   -2,    0,    0,    0,    0 },
      { -2,    0,    2,    2,    2,    0,    0,    0,    0 },
      {  1,    1,    2,   -2,    2,    0,    0,    0,    0 },
      { -2,    0,    2,    4,    2,    0,    0,    0,    0 },
      { -1,    0,    4,    0,    2,    0,    0,    0,    0 },
      {  2,    0,    2,   -2,    1,    0,    0,    0,    0 },
      {  1,    0,    0,   -1,   -1,    0,    0,    0,    0 },
      {  2,    0,    2,    2,    2,    0,    0,    0,    0 },
      {  1,    0,    0,    2,    1,    0,    0,    0,    0 },
      {  3,    0,    0,    0,    0,    0,    0,    0,    0 },
      {  0,    0,    2,   -2,   -1,    0,    0,    0,    0 },
      {  3,    0,    2,   -2,    2,    0,    0,    0,    0 },
      {  0,    0,    4,   -2,    2,    0,    0,    0,    0 },
      { -1,    0,    0,    4,    0,    0,    0,    0,    0 },
      {  0,    1,    2,    0,    1,    0,    0,    0,    0 },
      {  0,    0,    2,   -2,    3,    0,    0,    0,    0 },
      { -2,    0,    0,    4,    0,    0,    0,    0,    0 },
      { -1,   -1,    0,    2,    1,    0,    0,    0,    0 },
      { -2,    0,    2,    0,   -1,    0,    0,    0,    0 },
      {  0,    0,    2,    0,   -1,    0,    0,    0,    0 },
      {  0,   -1,    2,    0,    1,    0,    0,    0,    0 },
      {  0,    1,    0,    0,    2,    0,    0,    0,    0 },
      {  0,    0,    2,   -1,    2,    0,    0,    0,    0 },
      {  2,    1,    0,   -2,    0,    0,    0,    0,    0 },
      {  0,    0,    2,    4,    2,    0,    0,    0,    0 },
      { -1,   -1,    0,    2,   -1,    0,    0,    0,    0 },
      { -1,    1,    0,    2,    0,    0,    0,    0,    0 },
      {  1,   -1,    0,    0,    1,    0,    0,    0,    0 },
      {  0,   -1,    2,   -2,    0,    0,    0,    0,    0 },
      {  0,    1,    0,    0,   -2,    0,    0,    0,    0 },
      {  1,   -1,    2,    2,    2,    0,    0,    0,    0 },
      {  1,    0,    0,    2,   -1,    0,    0,    0,    0 },
      { -1,    1,    2,    2,    2,    0,    0,    0,    0 },
      {  3,    0,    2,    0,    1,    0,    0,    0,    0 },
      {  0,    1,    2,    2,    2,    0,    0,    0,    0 },
      {  1,    0,    2,   -2,    0,    0,    0,    0,    0 },
      { -1,    0,   -2,    4,   -1,    0,    0,    0,    0 },
      { -1,   -1,    2,    2,    1,    0,    0,    0,    0 },
      {  0,   -1,    2,    2,    1,    0,    0,    0,    0 },
      {  2,   -1,    2,    0,    2,    0,    0,    0,    0 },
      {  0,    0,    0,    2,    2,    0,    0,    0,    0 },
      {  1,   -1,    2,    0,    1,    0,    0,    0,    0 },
      { -1,    1,    2,    0,    2,    0,    0,    0,    0 },
      {  0,    1,    0,    2,    0,    0,    0,    0,    0 },
      {  0,    1,    2,   -2,    0,    0,    0,    0,    0 },
      {  0,    3,    2,   -2,    2,    0,    0,    0,    0 },
      {  0,    0,    0,    1,    1,    0,    0,    0,    0 },
      { -1,    0,    2,    2,    0,    0,    0,    0,    0 },
      {  2,    1,    2,    0,    2,    0,    0,    0,    0 },
      {  1,    1,    0,    0,    1,    0,    0,    0,    0 },
      {  2,    0,    0,    2,    0,    0,    0,    0,    0 },
      {  1,    1,    2,    0,    1,    0,    0,    0,    0 },
      { -1,    0,    0,    2,    2,    0,    0,    0,    0 },
      {  1,    0,   -2,    2,    0,    0,    0,    0,    0 },
      {  0,   -1,    0,    2,   -1,    0,    0,    0,    0 },
      { -1,    0,    1,    0,    2,    0,    0,    0,    0 },
      {  0,    1,    0,    1,    0,    0,    0,    0,    0 },
      {  1,    0,   -2,    2,   -2,    0,    0,    0,    0 },
      {  0,    0,    0,    1,   -1,    0,    0,    0,    0 },
      {  1,   -1,    0,    0,   -1,    0,    0,    0,    0 },
      {  0,    0,    0,    4,    0,    0,    0,    0,    0 },
      {  1,   -1,    0,    2,    0,    0,    0,    0,    0 },
      {  1,    0,    2,    1,    2,    0,    0,    0,    0 },
      {  1,    0,    2,   -1,    2,    0,    0,    0,    0 },
      { -1,    0,    0,    2,   -2,    0,    0,    0,    0 },
      {  0,    0,    2,    1,    1,    0,    0,    0,    0 },
      { -1,    0,    2,    0,   -1,    0,    0,    0,    0 },
      { -1,    0,    2,    4,    1,    0,    0,    0,    0 },
      {  0,    0,    2,    2,    0,    0,    0,    0,    0 },
      {  1,    1,    2,   -2,    1,    0,    0,    0,    0 },
      {  0,    0,    1,    0,    1,    0,    0,    0,    0 },
      { -1,    0,    2,   -1,    1,    0,    0,    0,    0 },
      { -2,    0,    2,    2,    1,    0,    0,    0,    0 },
      {  2,   -1,    0,    0,    0,    0,    0,    0,    0 },
      {  4,    0,    2,    0,    2,    0,    0,    0,    0 },
      {  2,    1,    2,   -2,    2,    0,    0,    0,    0 },
      {  0,    1,    2,    1,    2,    0,    0,    0,    0 },
      {  1,    0,    4,   -2,    2,    0,    0,    0,    0 },
      {  1,    1,    0,    0,   -1,    0,    0,    0,    0 },
      { -2,    0,    2,    4,    1,    0,    0,    0,    0 },
      {  2,    0,    2,    0,    0,    0,    0,    0,    0 },
      { -1,    0,    1,    0,    0,    0,    0,    0,    0 },
      {  1,    0,    0,    1,    0,    0,    0,    0,    0 },
      {  0,    1,    0,    2,    1,    0,    0,    0,    0 },
      { -1,    0,    4,    0,    1,    0,    0,    0,    0 },
      { -1,    0,    0,    4,    1,    0,    0,    0,    0 },
      {  2,    0,    2,    2,    1,    0,    0,    0,    0 },
      {  2,    1,    0,    0,    0,    0,    0,    0,    0 },
      {  0,    0,    5,   -5,    5,   -3,    0,    0,    0 },
      {  0,    0,    0,    0,    0,    0,    0,    2,    0 },
      {  0,    0,    1,   -1,    1,    0,    0,   -1,    0 },
      {  0,    0,   -1,    1,   -1,    1,    0,    0,    0 },
      {  0,    0,   -1,    1,    0,    0,    2,    0,    0 },
      {  0,    0,    3,   -3,    3,    0,    0,   -1,    0 },
      {  0,    0,   -8,    8,   -7,    5,    0,    0,    0 },
      {  0,    0,   -1,    1,   -1,    0,    2,    0,    0 },
      {  0,    0,   -2,    2,   -2,    2,    0,    0,    0 },
      {  0,    0,   -6,    6,   -6,    4,    0,    0,    0 },
      {  0,    0,   -2,    2,   -2,    0,    8,   -3,    0 },
      {  0,    0,    6,   -6,    6,    0,   -8,    3,    0 },
      {  0,    0,    4,   -4,    4,   -2,    0,    0,    0 },
      {  0,    0,   -3,    3,   -3,    2,    0,    0,    0 },
      {  0,    0,    4,   -4,    3,    0,   -8,    3,    0 },
      {  0,    0,   -4,    4,   -5,    0,    8,   -3,    0 },
      {  0,    0,    0,    0,    0,    2,    0,    0,    0 },
      {  0,    0,   -4,    4,   -4,    3,    0,    0,    0 },
      {  0,    1,   -1,    1,   -1,    0,    0,    1,    0 },
      {  0,    0,    0,    0,    0,    0,    0,    1,    0 },
      {  0,    0,    1,   -1,    1,    1,    0,    0,    0 },
      {  0,    0,    2,   -2,    2,    0,   -2,    0,    0 },
      {  0,   -1,   -7,    7,   -7,    5,    0,    0,    0 },
      { -2,    0,    2,    0,    2,    0,    0,   -2,    0 },
      { -2,    0,    2,    0,    1,    0,    0,   -3,    0 },
      {  0,    0,    2,   -2,    2,    0,    0,   -2,    0 },
      {  0,    0,    1,   -1,    1,    0,    0,    1,    0 },
      {  0,    0,    0,    0,    0,    0,    0,    0,    2 },
      {  0,    0,    0,    0,    0,    0,    0,    0,    1 },
      {  2,    0,   -2,    0,   -2,    0,    0,    3,    0 },
      {  0,    0,    1,   -1,    1,    0,    0,   -2,    0 },
      {  0,    0,   -7,    7,   -7,    5,    0,    0,    0 }
   };

/* Nutation series:  longitude. */
   static double psi[][4] = {
      {  3341.5,  17206241.8,    3.1,  17409.5 },
      { -1716.8,  -1317185.3,    1.4,   -156.8 },
      {   285.7,   -227667.0,    0.3,    -23.5 },
      {   -68.6,   -207448.0,    0.0,    -21.4 },
      {   950.3,    147607.9,   -2.3,   -355.0 },
      {   -66.7,    -51689.1,    0.2,    122.6 },
      {  -108.6,     71117.6,    0.0,      7.0 },
      {    35.6,    -38740.2,    0.1,    -36.2 },
      {    85.4,    -30127.6,    0.0,     -3.1 },
      {     9.0,     21583.0,    0.1,    -50.3 },
      {    22.1,     12822.8,    0.0,     13.3 },
      {     3.4,     12350.8,    0.0,      1.3 },
      {   -21.1,     15699.4,    0.0,      1.6 },
      {     4.2,      6313.8,    0.0,      6.2 },
      {   -22.8,      5796.9,    0.0,      6.1 },
      {    15.7,     -5961.1,    0.0,     -0.6 },
      {    13.1,     -5159.1,    0.0,     -4.6 },
      {     1.8,      4592.7,    0.0,      4.5 },
      {   -17.5,      6336.0,    0.0,      0.7 },
      {    16.3,     -3851.1,    0.0,     -0.4 },
      {    -2.8,      4771.7,    0.0,      0.5 },
      {    13.8,     -3099.3,    0.0,     -0.3 },
      {     0.2,      2860.3,    0.0,      0.3 },
      {     1.4,      2045.3,    0.0,      2.0 },
      {    -8.6,      2922.6,    0.0,      0.3 },
      {    -7.7,      2587.9,    0.0,      0.2 },
      {     8.8,     -1408.1,    0.0,      3.7 },
      {     1.4,      1517.5,    0.0,      1.5 },
      {    -1.9,     -1579.7,    0.0,      7.7 },
      {     1.3,     -2178.6,    0.0,     -0.2 },
      {    -4.8,      1286.8,    0.0,      1.3 },
      {     6.3,      1267.2,    0.0,     -4.0 },
      {    -1.0,      1669.3,    0.0,     -8.3 },
      {     2.4,     -1020.0,    0.0,     -0.9 },
      {     4.5,      -766.9,    0.0,      0.0 },
      {    -1.1,       756.5,    0.0,     -1.7 },
      {    -1.4,     -1097.3,    0.0,     -0.5 },
      {     2.6,      -663.0,    0.0,     -0.6 },
      {     0.8,      -714.1,    0.0,      1.6 },
      {     0.4,      -629.9,    0.0,     -0.6 },
      {     0.3,       580.4,    0.0,      0.6 },
      {    -1.6,       577.3,    0.0,      0.5 },
      {    -0.9,       644.4,    0.0,      0.0 },
      {     2.2,      -534.0,    0.0,     -0.5 },
      {    -2.5,       493.3,    0.0,      0.5 },
      {    -0.1,      -477.3,    0.0,     -2.4 },
      {    -0.9,       735.0,    0.0,     -1.7 },
      {     0.7,       406.2,    0.0,      0.4 },
      {    -2.8,       656.9,    0.0,      0.0 },
      {     0.6,       358.0,    0.0,      2.0 },
      {    -0.7,       472.5,    0.0,     -1.1 },
      {    -0.1,      -300.5,    0.0,      0.0 },
      {    -1.2,       435.1,    0.0,     -1.0 },
      {     1.8,      -289.4,    0.0,      0.0 },
      {     0.6,      -422.6,    0.0,      0.0 },
      {     0.8,      -287.6,    0.0,      0.6 },
      {   -38.6,      -392.3,    0.0,      0.0 },
      {     0.7,      -281.8,    0.0,      0.6 },
      {     0.6,      -405.7,    0.0,      0.0 },
      {    -1.2,       229.0,    0.0,      0.2 },
      {     1.1,      -264.3,    0.0,      0.5 },
      {    -0.7,       247.9,    0.0,     -0.5 },
      {    -0.2,       218.0,    0.0,      0.2 },
      {     0.6,      -339.0,    0.0,      0.8 },
      {    -0.7,       198.7,    0.0,      0.2 },
      {    -1.5,       334.0,    0.0,      0.0 },
      {     0.1,       334.0,    0.0,      0.0 },
      {    -0.1,      -198.1,    0.0,      0.0 },
      {  -106.6,         0.0,    0.0,      0.0 },
      {    -0.5,       165.8,    0.0,      0.0 },
      {     0.0,       134.8,    0.0,      0.0 },
      {     0.9,      -151.6,    0.0,      0.0 },
      {     0.0,      -129.7,    0.0,      0.0 },
      {     0.8,      -132.8,    0.0,     -0.1 },
      {     0.5,      -140.7,    0.0,      0.0 },
      {    -0.1,       138.4,    0.0,      0.0 },
      {     0.0,       129.0,    0.0,     -0.3 },
      {     0.5,      -121.2,    0.0,      0.0 },
      {    -0.3,       114.5,    0.0,      0.0 },
      {    -0.1,       101.8,    0.0,      0.0 },
      {    -3.6,      -101.9,    0.0,      0.0 },
      {     0.8,      -109.4,    0.0,      0.0 },
      {     0.2,       -97.0,    0.0,      0.0 },
      {    -0.7,       157.3,    0.0,      0.0 },
      {     0.2,       -83.3,    0.0,      0.0 },
      {    -0.3,        93.3,    0.0,      0.0 },
      {    -0.1,        92.1,    0.0,      0.0 },
      {    -0.5,       133.6,    0.0,      0.0 },
      {    -0.1,        81.5,    0.0,      0.0 },
      {     0.0,       123.9,    0.0,      0.0 },
      {    -0.3,       128.1,    0.0,      0.0 },
      {     0.1,        74.1,    0.0,     -0.3 },
      {    -0.2,       -70.3,    0.0,      0.0 },
      {    -0.4,        66.6,    0.0,      0.0 },
      {     0.1,       -66.7,    0.0,      0.0 },
      {    -0.7,        69.3,    0.0,     -0.3 },
      {     0.0,       -70.4,    0.0,      0.0 },
      {    -0.1,       101.5,    0.0,      0.0 },
      {     0.5,       -69.1,    0.0,      0.0 },
      {    -0.2,        58.5,    0.0,      0.2 },
      {     0.1,       -94.9,    0.0,      0.2 },
      {     0.0,        52.9,    0.0,     -0.2 },
      {     0.1,        86.7,    0.0,     -0.2 },
      {    -0.1,       -59.2,    0.0,      0.2 },
      {     0.3,       -58.8,    0.0,      0.1 },
      {    -0.3,        49.0,    0.0,      0.0 },
      {    -0.2,        56.9,    0.0,     -0.1 },
      {     0.3,       -50.2,    0.0,      0.0 },
      {    -0.2,        53.4,    0.0,     -0.1 },
      {     0.1,       -76.5,    0.0,      0.0 },
      {    -0.2,        45.3,    0.0,      0.0 },
      {     0.1,       -46.8,    0.0,      0.0 },
      {     0.2,       -44.6,    0.0,      0.0 },
      {     0.2,       -48.7,    0.0,      0.0 },
      {     0.1,       -46.8,    0.0,      0.0 },
      {     0.1,       -42.0,    0.0,      0.0 },
      {     0.0,        46.4,    0.0,     -0.1 },
      {     0.2,       -67.3,    0.0,      0.1 },
      {     0.0,       -65.8,    0.0,      0.2 },
      {    -0.1,       -43.9,    0.0,      0.3 },
      {     0.0,       -38.9,    0.0,      0.0 },
      {    -0.3,        63.9,    0.0,      0.0 },
      {    -0.2,        41.2,    0.0,      0.0 },
      {     0.0,       -36.1,    0.0,      0.2 },
      {    -0.3,        58.5,    0.0,      0.0 },
      {    -0.1,        36.1,    0.0,      0.0 },
      {     0.0,       -39.7,    0.0,      0.0 },
      {     0.1,       -57.7,    0.0,      0.0 },
      {    -0.2,        33.4,    0.0,      0.0 },
      {    36.4,         0.0,    0.0,      0.0 },
      {    -0.1,        55.7,    0.0,     -0.1 },
      {     0.1,       -35.4,    0.0,      0.0 },
      {     0.1,       -31.0,    0.0,      0.0 },
      {    -0.1,        30.1,    0.0,      0.0 },
      {    -0.3,        49.2,    0.0,      0.0 },
      {    -0.2,        49.1,    0.0,      0.0 },
      {    -0.1,        33.6,    0.0,      0.0 },
      {     0.1,       -33.5,    0.0,      0.0 },
      {     0.1,       -31.0,    0.0,      0.0 },
      {    -0.1,        28.0,    0.0,      0.0 },
      {     0.1,       -25.2,    0.0,      0.0 },
      {     0.1,       -26.2,    0.0,      0.0 },
      {    -0.2,        41.5,    0.0,      0.0 },
      {     0.0,        24.5,    0.0,      0.1 },
      {   -16.2,         0.0,    0.0,      0.0 },
      {     0.0,       -22.3,    0.0,      0.0 },
      {     0.0,        23.1,    0.0,      0.0 },
      {    -0.1,        37.5,    0.0,      0.0 },
      {     0.2,       -25.7,    0.0,      0.0 },
      {     0.0,        25.2,    0.0,      0.0 },
      {     0.1,       -24.5,    0.0,      0.0 },
      {    -0.1,        24.3,    0.0,      0.0 },
      {     0.1,       -20.7,    0.0,      0.0 },
      {     0.1,       -20.8,    0.0,      0.0 },
      {    -0.2,        33.4,    0.0,      0.0 },
      {    32.9,         0.0,    0.0,      0.0 },
      {     0.1,       -32.6,    0.0,      0.0 },
      {     0.0,        19.9,    0.0,      0.0 },
      {    -0.1,        19.6,    0.0,      0.0 },
      {     0.0,       -18.7,    0.0,      0.0 },
      {     0.1,       -19.0,    0.0,      0.0 },
      {     0.1,       -28.6,    0.0,      0.0 },
      {     4.0,       178.8,  -11.8,      0.3 },
      {    39.8,      -107.3,   -5.6,     -1.0 },
      {     9.9,       164.0,   -4.1,      0.1 },
      {    -4.8,      -135.3,   -3.4,     -0.1 },
      {    50.5,        75.0,    1.4,     -1.2 },
      {    -1.1,       -53.5,    1.3,      0.0 },
      {   -45.0,        -2.4,   -0.4,      6.6 },
      {   -11.5,       -61.0,   -0.9,      0.4 },
      {     4.4,       -68.4,   -3.4,      0.0 },
      {     7.7,       -47.1,   -4.7,     -1.0 },
      {   -42.9,       -12.6,   -1.2,      4.2 },
      {   -42.8,        12.7,   -1.2,     -4.2 },
      {    -7.6,       -44.1,    2.1,     -0.5 },
      {   -64.1,         1.7,    0.2,      4.5 },
      {    36.4,       -10.4,    1.0,      3.5 },
      {    35.6,        10.2,    1.0,     -3.5 },
      {    -1.7,        39.5,    2.0,      0.0 },
      {    50.9,        -8.2,   -0.8,     -5.0 },
      {     0.0,        52.3,    1.2,      0.0 },
      {   -42.9,       -17.8,    0.4,      0.0 },
      {     2.6,        34.3,    0.8,      0.0 },
      {    -0.8,       -48.6,    2.4,     -0.1 },
      {    -4.9,        30.5,    3.7,      0.7 },
      {     0.0,       -43.6,    2.1,      0.0 },
      {     0.0,       -25.4,    1.2,      0.0 },
      {     2.0,        40.9,   -2.0,      0.0 },
      {    -2.1,        26.1,    0.6,      0.0 },
      {    22.6,        -3.2,   -0.5,     -0.5 },
      {    -7.6,        24.9,   -0.4,     -0.2 },
      {    -6.2,        34.9,    1.7,      0.3 },
      {     2.0,        17.4,   -0.4,      0.1 },
      {    -3.9,        20.5,    2.4,      0.6 }
   };

/* Nutation series:  obliquity */
   static double eps[][4] = {
      { 9205365.8,  -1506.2,   885.7,  -0.2 },
      {  573095.9,   -570.2,  -305.0,  -0.3 },
      {   97845.5,    147.8,   -48.8,  -0.2 },
      {  -89753.6,     28.0,    46.9,   0.0 },
      {    7406.7,   -327.1,   -18.2,   0.8 },
      {   22442.3,    -22.3,   -67.6,   0.0 },
      {    -683.6,     46.8,     0.0,   0.0 },
      {   20070.7,     36.0,     1.6,   0.0 },
      {   12893.8,     39.5,    -6.2,   0.0 },
      {   -9593.2,     14.4,    30.2,  -0.1 },
      {   -6899.5,      4.8,    -0.6,   0.0 },
      {   -5332.5,     -0.1,     2.7,   0.0 },
      {    -125.2,     10.5,     0.0,   0.0 },
      {   -3323.4,     -0.9,    -0.3,   0.0 },
      {    3142.3,      8.9,     0.3,   0.0 },
      {    2552.5,      7.3,    -1.2,   0.0 },
      {    2634.4,      8.8,     0.2,   0.0 },
      {   -2424.4,      1.6,    -0.4,   0.0 },
      {    -123.3,      3.9,     0.0,   0.0 },
      {    1642.4,      7.3,    -0.8,   0.0 },
      {      47.9,      3.2,     0.0,   0.0 },
      {    1321.2,      6.2,    -0.6,   0.0 },
      {   -1234.1,     -0.3,     0.6,   0.0 },
      {   -1076.5,     -0.3,     0.0,   0.0 },
      {     -61.6,      1.8,     0.0,   0.0 },
      {     -55.4,      1.6,     0.0,   0.0 },
      {     856.9,     -4.9,    -2.1,   0.0 },
      {    -800.7,     -0.1,     0.0,   0.0 },
      {     685.1,     -0.6,    -3.8,   0.0 },
      {     -16.9,     -1.5,     0.0,   0.0 },
      {     695.7,      1.8,     0.0,   0.0 },
      {     642.2,     -2.6,    -1.6,   0.0 },
      {      13.3,      1.1,    -0.1,   0.0 },
      {     521.9,      1.6,     0.0,   0.0 },
      {     325.8,      2.0,    -0.1,   0.0 },
      {    -325.1,     -0.5,     0.9,   0.0 },
      {      10.1,      0.3,     0.0,   0.0 },
      {     334.5,      1.6,     0.0,   0.0 },
      {     307.1,      0.4,    -0.9,   0.0 },
      {     327.2,      0.5,     0.0,   0.0 },
      {    -304.6,     -0.1,     0.0,   0.0 },
      {     304.0,      0.6,     0.0,   0.0 },
      {    -276.8,     -0.5,     0.1,   0.0 },
      {     268.9,      1.3,     0.0,   0.0 },
      {     271.8,      1.1,     0.0,   0.0 },
      {     271.5,     -0.4,    -0.8,   0.0 },
      {      -5.2,      0.5,     0.0,   0.0 },
      {    -220.5,      0.1,     0.0,   0.0 },
      {     -20.1,      0.3,     0.0,   0.0 },
      {    -191.0,      0.1,     0.5,   0.0 },
      {      -4.1,      0.3,     0.0,   0.0 },
      {     130.6,     -0.1,     0.0,   0.0 },
      {       3.0,      0.3,     0.0,   0.0 },
      {     122.9,      0.8,     0.0,   0.0 },
      {       3.7,     -0.3,     0.0,   0.0 },
      {     123.1,      0.4,    -0.3,   0.0 },
      {     -52.7,     15.3,     0.0,   0.0 },
      {     120.7,      0.3,    -0.3,   0.0 },
      {       4.0,     -0.3,     0.0,   0.0 },
      {     126.5,      0.5,     0.0,   0.0 },
      {     112.7,      0.5,    -0.3,   0.0 },
      {    -106.1,     -0.3,     0.3,   0.0 },
      {    -112.9,     -0.2,     0.0,   0.0 },
      {       3.6,     -0.2,     0.0,   0.0 },
      {     107.4,      0.3,     0.0,   0.0 },
      {     -10.9,      0.2,     0.0,   0.0 },
      {      -0.9,      0.0,     0.0,   0.0 },
      {      85.4,      0.0,     0.0,   0.0 },
      {       0.0,    -88.8,     0.0,   0.0 },
      {     -71.0,     -0.2,     0.0,   0.0 },
      {     -70.3,      0.0,     0.0,   0.0 },
      {      64.5,      0.4,     0.0,   0.0 },
      {      69.8,      0.0,     0.0,   0.0 },
      {      66.1,      0.4,     0.0,   0.0 },
      {     -61.0,     -0.2,     0.0,   0.0 },
      {     -59.5,     -0.1,     0.0,   0.0 },
      {     -55.6,      0.0,     0.2,   0.0 },
      {      51.7,      0.2,     0.0,   0.0 },
      {     -49.0,     -0.1,     0.0,   0.0 },
      {     -52.7,     -0.1,     0.0,   0.0 },
      {     -49.6,      1.4,     0.0,   0.0 },
      {      46.3,      0.4,     0.0,   0.0 },
      {      49.6,      0.1,     0.0,   0.0 },
      {      -5.1,      0.1,     0.0,   0.0 },
      {     -44.0,     -0.1,     0.0,   0.0 },
      {     -39.9,     -0.1,     0.0,   0.0 },
      {     -39.5,     -0.1,     0.0,   0.0 },
      {      -3.9,      0.1,     0.0,   0.0 },
      {     -42.1,     -0.1,     0.0,   0.0 },
      {     -17.2,      0.1,     0.0,   0.0 },
      {      -2.3,      0.1,     0.0,   0.0 },
      {     -39.2,      0.0,     0.0,   0.0 },
      {     -38.4,      0.1,     0.0,   0.0 },
      {      36.8,      0.2,     0.0,   0.0 },
      {      34.6,      0.1,     0.0,   0.0 },
      {     -32.7,      0.3,     0.0,   0.0 },
      {      30.4,      0.0,     0.0,   0.0 },
      {       0.4,      0.1,     0.0,   0.0 },
      {      29.3,      0.2,     0.0,   0.0 },
      {      31.6,      0.1,     0.0,   0.0 },
      {       0.8,     -0.1,     0.0,   0.0 },
      {     -27.9,      0.0,     0.0,   0.0 },
      {       2.9,      0.0,     0.0,   0.0 },
      {     -25.3,      0.0,     0.0,   0.0 },
      {      25.0,      0.1,     0.0,   0.0 },
      {      27.5,      0.1,     0.0,   0.0 },
      {     -24.4,     -0.1,     0.0,   0.0 },
      {      24.9,      0.2,     0.0,   0.0 },
      {     -22.8,     -0.1,     0.0,   0.0 },
      {       0.9,     -0.1,     0.0,   0.0 },
      {      24.4,      0.1,     0.0,   0.0 },
      {      23.9,      0.1,     0.0,   0.0 },
      {      22.5,      0.1,     0.0,   0.0 },
      {      20.8,      0.1,     0.0,   0.0 },
      {      20.1,      0.0,     0.0,   0.0 },
      {      21.5,      0.1,     0.0,   0.0 },
      {     -20.0,      0.0,     0.0,   0.0 },
      {       1.4,      0.0,     0.0,   0.0 },
      {      -0.2,     -0.1,     0.0,   0.0 },
      {      19.0,      0.0,    -0.1,   0.0 },
      {      20.5,      0.0,     0.0,   0.0 },
      {      -2.0,      0.0,     0.0,   0.0 },
      {     -17.6,     -0.1,     0.0,   0.0 },
      {      19.0,      0.0,     0.0,   0.0 },
      {      -2.4,      0.0,     0.0,   0.0 },
      {     -18.4,     -0.1,     0.0,   0.0 },
      {      17.1,      0.0,     0.0,   0.0 },
      {       0.4,      0.0,     0.0,   0.0 },
      {      18.4,      0.1,     0.0,   0.0 },
      {       0.0,     17.4,     0.0,   0.0 },
      {      -0.6,      0.0,     0.0,   0.0 },
      {     -15.4,      0.0,     0.0,   0.0 },
      {     -16.8,     -0.1,     0.0,   0.0 },
      {      16.3,      0.0,     0.0,   0.0 },
      {      -2.0,      0.0,     0.0,   0.0 },
      {      -1.5,      0.0,     0.0,   0.0 },
      {     -14.3,     -0.1,     0.0,   0.0 },
      {      14.4,      0.0,     0.0,   0.0 },
      {     -13.4,      0.0,     0.0,   0.0 },
      {     -14.3,     -0.1,     0.0,   0.0 },
      {     -13.7,      0.0,     0.0,   0.0 },
      {      13.1,      0.1,     0.0,   0.0 },
      {      -1.7,      0.0,     0.0,   0.0 },
      {     -12.8,      0.0,     0.0,   0.0 },
      {       0.0,    -14.4,     0.0,   0.0 },
      {      12.4,      0.0,     0.0,   0.0 },
      {     -12.0,      0.0,     0.0,   0.0 },
      {      -0.8,      0.0,     0.0,   0.0 },
      {      10.9,      0.1,     0.0,   0.0 },
      {     -10.8,      0.0,     0.0,   0.0 },
      {      10.5,      0.0,     0.0,   0.0 },
      {     -10.4,      0.0,     0.0,   0.0 },
      {     -11.2,      0.0,     0.0,   0.0 },
      {      10.5,      0.1,     0.0,   0.0 },
      {      -1.4,      0.0,     0.0,   0.0 },
      {       0.0,      0.1,     0.0,   0.0 },
      {       0.7,      0.0,     0.0,   0.0 },
      {     -10.3,      0.0,     0.0,   0.0 },
      {     -10.0,      0.0,     0.0,   0.0 },
      {       9.6,      0.0,     0.0,   0.0 },
      {       9.4,      0.1,     0.0,   0.0 },
      {       0.6,      0.0,     0.0,   0.0 },
      {     -87.7,      4.4,    -0.4,  -6.3 },
      {      46.3,     22.4,     0.5,  -2.4 },
      {      15.6,     -3.4,     0.1,   0.4 },
      {       5.2,      5.8,     0.2,  -0.1 },
      {     -30.1,     26.9,     0.7,   0.0 },
      {      23.2,     -0.5,     0.0,   0.6 },
      {       1.0,     23.2,     3.4,   0.0 },
      {     -12.2,     -4.3,     0.0,   0.0 },
      {      -2.1,     -3.7,    -0.2,   0.1 },
      {     -18.6,     -3.8,    -0.4,   1.8 },
      {       5.5,    -18.7,    -1.8,  -0.5 },
      {      -5.5,    -18.7,     1.8,  -0.5 },
      {      18.4,     -3.6,     0.3,   0.9 },
      {      -0.6,      1.3,     0.0,   0.0 },
      {      -5.6,    -19.5,     1.9,   0.0 },
      {       5.5,    -19.1,    -1.9,   0.0 },
      {     -17.3,     -0.8,     0.0,   0.9 },
      {      -3.2,     -8.3,    -0.8,   0.3 },
      {      -0.1,      0.0,     0.0,   0.0 },
      {      -5.4,      7.8,    -0.3,   0.0 },
      {     -14.8,      1.4,     0.0,   0.3 },
      {      -3.8,      0.4,     0.0,  -0.2 },
      {      12.6,      3.2,     0.5,  -1.5 },
      {       0.1,      0.0,     0.0,   0.0 },
      {     -13.6,      2.4,    -0.1,   0.0 },
      {       0.9,      1.2,     0.0,   0.0 },
      {     -11.9,     -0.5,     0.0,   0.3 },
      {       0.4,     12.0,     0.3,  -0.2 },
      {       8.3,      6.1,    -0.1,   0.1 },
      {       0.0,      0.0,     0.0,   0.0 },
      {       0.4,    -10.8,     0.3,   0.0 },
      {       9.6,      2.2,     0.3,  -1.2 }
   };

/* Number of terms in the model */
   static int nterms = sizeof ( na ) / sizeof ( int ) /9;

   int j;
   double t, el, elp, f, d, om, ve, ma, ju, sa, theta, c, s, dp, de;



/* Interval between fundamental epoch J2000.0 and given epoch (JC). */
   t  =  ( date - DJM0 ) / DJC;

/* Mean anomaly of the Moon. */
   el  = 134.96340251 * DD2R +
         fmod ( t * ( 1717915923.2178 +
                t * (         31.8792 +
                t * (          0.051635 +
                t * (        - 0.00024470 ) ) ) ), TURNAS ) * DAS2R;

/* Mean anomaly of the Sun. */
   elp = 357.52910918 * DD2R +
         fmod ( t * (  129596581.0481 +
                t * (        - 0.5532 +
                t * (          0.000136 +
                t * (        - 0.00001149 ) ) ) ), TURNAS ) * DAS2R;

/* Mean argument of the latitude of the Moon. */
   f   =  93.27209062 * DD2R +
         fmod ( t * ( 1739527262.8478 +
                t * (       - 12.7512 +
                t * (        - 0.001037 +
                t * (          0.00000417 ) ) ) ), TURNAS ) * DAS2R;

/* Mean elongation of the Moon from the Sun. */
   d   = 297.85019547 * DD2R +
         fmod ( t * ( 1602961601.2090 +
                t * (        - 6.3706 +
                t * (          0.006539 +
                t * (        - 0.00003169 ) ) ) ), TURNAS ) * DAS2R;

/* Mean longitude of the ascending node of the Moon. */
   om  = 125.04455501 * DD2R +
         fmod ( t * (  - 6962890.5431 +
                t * (          7.4722 +
                t * (          0.007702 +
                t * (        - 0.00005939 ) ) ) ), TURNAS ) * DAS2R;

/* Mean longitude of Venus. */
   ve  = 181.97980085 * DD2R +
         fmod ( 210664136.433548 * t, TURNAS ) * DAS2R;

/* Mean longitude of Mars. */
   ma  = 355.43299958 * DD2R +
         fmod (  68905077.493988 * t, TURNAS ) * DAS2R;

/* Mean longitude of Jupiter. */
   ju  =  34.35151874 * DD2R +
         fmod (  10925660.377991 * t, TURNAS ) * DAS2R;

/* Mean longitude of Saturn. */
   sa  =  50.07744430 * DD2R +
         fmod (   4399609.855732 * t, TURNAS ) * DAS2R;

/* Geodesic nutation (Fukushima 1991) in microarcsec. */
   dp = - 153.1 * sin ( elp ) - 1.9 * sin( 2.0 * elp );
   de = 0.0;

/* Shirai & Fukushima (2001) nutation series. */
   for ( j = nterms - 1; j >= 0; j-- ) {
      theta = ( (double) na[j][0] ) * el +
              ( (double) na[j][1] ) * elp +
              ( (double) na[j][2] ) * f +
              ( (double) na[j][3] ) * d +
              ( (double) na[j][4] ) * om +
              ( (double) na[j][5] ) * ve +
              ( (double) na[j][6] ) * ma +
              ( (double) na[j][7] ) * ju +
              ( (double) na[j][8] ) * sa;
      c = cos ( theta );
      s = sin ( theta );
      dp += ( psi[j][0] + psi[j][2] * t ) * c +
              ( psi[j][1] + psi[j][3] * t ) * s;
      de += ( eps[j][0] + eps[j][2] * t ) * c +
              ( eps[j][1] + eps[j][3] * t ) * s;
   }

/* Change of units, and addition of the precession correction. */
   *dpsi = ( dp * 1e-6 - 0.042888 - 0.29856 * t ) * DAS2R;
   *deps = ( de * 1e-6 - 0.005171 - 0.02408 * t ) * DAS2R;

/* Mean obliquity of date (Simon et al. 1994). */
   *eps0  =  ( 84381.412 +
              ( - 46.80927 +
               ( - 0.000152 +
                 ( 0.0019989 +
               ( - 0.00000051 +
               ( - 0.000000025 ) * t ) * t ) * t ) * t ) * t ) * DAS2R;

}

void slaPreces ( char sys[4], double ep0, double ep1,
                 double *ra, double *dc )
  /*
  **  - - - - - - - - - -
  **   s l a P r e c e s
  **  - - - - - - - - - -
  **
  **  Precession - either FK4 (Bessel-Newcomb, pre-IAU1976) or
  **  FK5 (Fricke, post-IAU1976) as required.
  **
  **  Given:
  **     sys        char[]     precession to be applied: "FK4" or "FK5"
  **     ep0,ep1    double     starting and ending epoch
  **     ra,dc      double     RA,Dec, mean equator & equinox of epoch ep0
  **
  **  Returned:
  **     *ra,*dc    double     RA,Dec, mean equator & equinox of epoch ep1
  **
  **  Called:    slaDranrm, slaPrebn, slaPrec, slaDcs2c,
  **             slaDmxv, slaDcc2s
  **
  **  Notes:
  **
  **  1)  The epochs are Besselian if sys='FK4' and Julian if 'FK5'.
  **      For example, to precess coordinates in the old system from
  **      equinox 1900.0 to 1950.0 the call would be:
  **          slaPreces ( "FK4", 1900.0, 1950.0, &ra, &dc )
  **
  **  2)  This routine will not correctly convert between the old and
  **      the new systems - for example conversion from B1950 to J2000.
  **      For these purposes see slaFk425, slaFk524, slaFk45z and
  **      slaFk54z.
  **
  **  3)  If an invalid sys is supplied, values of -99.0,-99.0 will
  **      be returned for both ra and dc.
  **
  **  Last revision:   15 June 2001
  **
  **  Copyright P.T.Wallace.  All rights reserved.
  */
{
  double pm[3][3], v1[3], v2[3];
  
  /* Validate sys */
  if ( ( toupper ( (int) sys[0] ) != 'F' )
       || ( toupper ( (int) sys[1] ) != 'K' )
       || ( (int) sys[2] != '4' && (int) sys[2] != '5' ) ) {
    *ra = -99.0;          /* Error */
    *dc = -99.0;
  } else {
    
    /* Generate appropriate precession matrix */
    if ( (int) sys[2] == '4' )
      slaPrebn ( ep0, ep1, pm );
    else
      slaPrec ( ep0, ep1, pm );
    
    /* Convert RA,Dec to x,y,z */
    slaDcs2c ( *ra, *dc, v1 );
    
    /* Precess */
    slaDmxv ( pm, v1, v2 );
    
    /* Back to RA,Dec */
    slaDcc2s ( v2, ra, dc );
    *ra = slaDranrm ( *ra );
  }
}

void slaPrebn ( double bep0, double bep1, double rmatp[3][3] )
  /*
  **  - - - - - - - - -
  **   s l a P r e b n
  **  - - - - - - - - -
  **
  **  Generate the matrix of precession between two epochs,
  **  using the old, pre-IAU1976, Bessel-Newcomb model, using
  **  Kinoshita's formulation (double precision)
  **
  **  Given:
  **     BEP0    double        beginning Besselian epoch
  **     BEP1    double        ending Besselian epoch
  **
  **  Returned:
  **     RMATP   double[3][3]  precession matrix
  **
  **  The matrix is in the sense   v(bep1)  =  rmatp * v(bep0)
  **
  **  Reference:
  **     Kinoshita, H. (1975) 'Formulas for precession', SAO Special
  **     Report No. 364, Smithsonian Institution Astrophysical
  **     Observatory, Cambridge, Massachusetts.
  **
  **  Called:  slaDeuler
  **
  **  Defined in slamac.h:  DAS2R
  **
  **  Last revision:   30 October 1993
  **
  **  Copyright P.T.Wallace.  All rights reserved.
  */
{
  double bigt, t, tas2r, w, zeta, z, theta;
  
  /* Interval between basic epoch B1850.0 and beginning epoch in TC */
  bigt  = ( bep0 - 1850.0 ) / 100.0;
  
  /* Interval over which precession required, in tropical centuries */
  t = ( bep1 - bep0 ) / 100.0;
  
  /* Euler angles */
  tas2r = t * DAS2R;
  w = 2303.5548 + ( 1.39720 + 0.000059 * bigt ) * bigt;
  zeta = (w + ( 0.30242 - 0.000269 * bigt + 0.017996 * t ) * t ) * tas2r;
  z = (w + ( 1.09478 + 0.000387 * bigt + 0.018324 * t ) * t ) * tas2r;
  theta = ( 2005.1125 + ( - 0.85294 - 0.000365* bigt ) * bigt +
	    ( - 0.42647 - 0.000365 * bigt - 0.041802 * t ) * t ) * tas2r;
  
  /* Rotation matrix */
  slaDeuler ((char*)"ZYZ", -zeta, theta, -z, rmatp );
}

void slaPrec ( double ep0, double ep1, double rmatp[3][3] )
  /*
  **  - - - - - - - -
  **   s l a P r e c
  **  - - - - - - - -
  **
  **  Form the matrix of precession between two epochs (IAU 1976, FK5).
  **
  **  (double precision)
  **
  **  Given:
  **     ep0    double         beginning epoch
  **     ep1    double         ending epoch
  **
  **  Returned:
  **     rmatp  double[3][3]   precession matrix
  **
  **  Notes:
  **
  **  1)  The epochs are TDB (loosely ET) Julian epochs.
  **
  **  2)  The matrix is in the sense   v(ep1)  =  rmatp * v(ep0) .
  **
  **  3)  Though the matrix method itself is rigorous, the precession
  **      angles are expressed through canonical polynomials which are
  **      valid only for a limited time span.  There are also known
  **      errors in the IAU precession rate.  The absolute accuracy
  **      of the present formulation is better than 0.1 arcsec from
  **      1960AD to 2040AD, better than 1 arcsec from 1640AD to 2360AD,
  **      and remains below 3 arcsec for the whole of the period
  **      500BC to 3000AD.  The errors exceed 10 arcsec outside the
  **      range 1200BC to 3900AD, exceed 100 arcsec outside 4200BC to
  **      5600AD and exceed 1000 arcsec outside 6800BC to 8200AD.
  **      The SLALIB routine slaPrecl implements a more elaborate
  **      model which is suitable for problems spanning several
  **      thousand years.
  **
  **  References:
  **     Lieske,J.H., 1979. Astron. Astrophys.,73,282.
  **          equations (6) & (7), p283.
  **     Kaplan,G.H., 1981. USNO circular no. 163, pa2.
  **
  **  Called:  slaDeuler
  **
  **  Defined in slamac.h:  DAS2R
  **
  **  Last revision:   10 July 1994
  **
  **  Copyright P.T.Wallace.  All rights reserved.
  */
{
  double t0, t, tas2r, w, zeta, z, theta;
  
  /* Interval between basic epoch J2000.0 and beginning epoch (JC) */
  t0 = ( ep0 - 2000.0 ) / 100.0;
  
  /* Interval over which precession required (JC) */
  t =  ( ep1 - ep0 ) / 100.0;
  
  /* Euler angles */
  tas2r = t * DAS2R;
  w = 2306.2181 + ( ( 1.39656 - ( 0.000139 * t0 ) ) * t0 );
  zeta = (w + ( ( 0.30188 - 0.000344 * t0 ) + 0.017998 * t ) * t ) * tas2r;
  z = (w + ( ( 1.09468 + 0.000066 * t0 ) + 0.018203 * t ) * t ) * tas2r;
  theta = ( ( 2004.3109 + ( - 0.85330 - 0.000217 * t0 ) * t0 )
	    + ( ( -0.42665 - 0.000217 * t0 ) - 0.041833 * t ) * t ) * tas2r;
  
  /* Rotation matrix */
  slaDeuler ((char*) "ZYZ", -zeta, theta, -z, rmatp );
}

void slaPrenut ( double epoch, double date, double rmatpn[3][3] )
/*
**  - - - - - - - - - -
**   s l a P r e n u t
**  - - - - - - - - - -
**
**  Form the matrix of precession and nutation (IAU 1976/1980/FK5)
**
**  (double precision)
**
**  Given:
**     epoch   double         Julian epoch for mean coordinates
**     date    double         Modified Julian Date (JD-2400000.5)
**                            for true coordinates
**
**
**  Returned:
**     rmatpn  double[3][3]   combined precession/nutation matrix
**
**  Called:  slaPrec, slaEpj, slaNut, slaDmxm
**
**  Notes:
**
**  1)  The epoch and date are TDB (loosely ET).
**
**  2)  The matrix is in the sense   v(true)  =  rmatpn * v(mean) .
**
**  Last revision:   8 May 2000
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
   double rmatp[3][3], rmatn[3][3];

/* Precession */
   slaPrec ( epoch, slaEpj ( date ), rmatp );

/* Nutation */
   slaNut ( date, rmatn );

/* Combine the matrices:  pn = n x p */
   slaDmxm ( rmatn, rmatp, rmatpn );
}

void slaDeuler ( char* order, double phi, double theta,
                 double psi, double rmat[3][3] )
  /*
  **  - - - - - - - - - -
  **   s l a D e u l e r
  **  - - - - - - - - - -
  **
  **  Form a rotation matrix from the Euler angles - three successive
  **  rotations about specified Cartesian axes.
  **
  **  (double precision)
  **
  **  Given:
  **    *order char     specifies about which axes the rotations occur
  **    phi    double   1st rotation (radians)
  **    theta  double   2nd rotation (   "   )
  **    psi    double   3rd rotation (   "   )
  **
  **  Returned:
  **    rmat   double[3][3]  rotation matrix
  **
  **  A rotation is positive when the reference frame rotates
  **  anticlockwise as seen looking towards the origin from the
  **  positive region of the specified axis.
  **
  **  The characters of order define which axes the three successive
  **  rotations are about.  A typical value is 'zxz', indicating that
  **  rmat is to become the direction cosine matrix corresponding to
  **  rotations of the reference frame through phi radians about the
  **  old z-axis, followed by theta radians about the resulting x-axis,
  **  then psi radians about the resulting z-axis.
  **
  **  The axis names can be any of the following, in any order or
  **  combination:  x, y, z, uppercase or lowercase, 1, 2, 3.  Normal
  **  axis labelling/numbering conventions apply;  the xyz (=123)
  **  triad is right-handed.  Thus, the 'zxz' example given above
  **  could be written 'zxz' or '313' (or even 'zxz' or '3xz').  Order
  **  is terminated by length or by the first unrecognized character.
  **
  **  Fewer than three rotations are acceptable, in which case the later
  **  angle arguments are ignored.  Zero rotations leaves rmat set to the
  **  identity matrix.
  **
  **  Last revision:   9 December 1996
  **
  **  Copyright P.T.Wallace.  All rights reserved.
  */
{
  int j, i, l, n, k;
  double result[3][3], rotn[3][3], angle, s, c , w, wm[3][3];
  char axis;
  
  /* Initialize result matrix */
  for ( j = 0; j < 3; j++ ) {
    for ( i = 0; i < 3; i++ ) {
      result[i][j] = ( i == j ) ? 1.0 : 0.0;
    }
  }
  
  /* Establish length of axis string */
  l = strlen ( order );
  
  /* Look at each character of axis string until finished */
  for ( n = 0; n < 3; n++ ) {
    if ( n <= l ) {
      
      /* Initialize rotation matrix for the current rotation */
      for ( j = 0; j < 3; j++ ) {
	for ( i = 0; i < 3; i++ ) {
	  rotn[i][j] = ( i == j ) ? 1.0 : 0.0;
	}
      }
      
      /* Pick up the appropriate Euler angle and take sine & cosine */
      switch ( n ) {
      case 0 :
	angle = phi;
	break;
      case 1 :
	angle = theta;
	break;
      default:
	angle = psi;
	break;
      }
      s = sin ( angle );
      c = cos ( angle );
      
      /* Identify the axis */
      axis =  order[n];
      if ( ( axis == 'X' ) || ( axis == 'x' ) || ( axis == '1' ) ) {
	
	/* Matrix for x-rotation */
	rotn[1][1] = c;
	rotn[1][2] = s;
	rotn[2][1] = -s;
	rotn[2][2] = c;
      }
      else if ( ( axis == 'Y' ) || ( axis == 'y' ) || ( axis == '2' ) ) {
	
	/* Matrix for y-rotation */
	rotn[0][0] = c;
	rotn[0][2] = -s;
	rotn[2][0] = s;
	rotn[2][2] = c;
      }
      else if ( ( axis == 'Z' ) || ( axis == 'z' ) || ( axis == '3' ) ) {
	
	/* Matrix for z-rotation */
	rotn[0][0] = c;
	rotn[0][1] = s;
	rotn[1][0] = -s;
	rotn[1][1] = c;
      } else {
	
	/* Unrecognized character - fake end of string */
	l = 0;
      }
      
      /* Apply the current rotation (matrix rotn x matrix result) */
      for ( i = 0; i < 3; i++ ) {
	for ( j = 0; j < 3; j++ ) {
	  w = 0.0;
	  for ( k = 0; k < 3; k++ ) {
	    w += rotn[i][k] * result[k][j];
	  }
	  wm[i][j] = w;
	}
      }
      for ( j = 0; j < 3; j++ ) {
	for ( i= 0; i < 3; i++ ) {
	  result[i][j] = wm[i][j];
	}
      }
    }
  }
  
  /* Copy the result */
  for ( j = 0; j < 3; j++ ) {
    for ( i = 0; i < 3; i++ ) {
      rmat[i][j] = result[i][j];
    }
  }
}

void slaDcc2s ( double v[3], double *a, double *b )
  /*
  **  - - - - - - - - -
  **   s l a D c c 2 s
  **  - - - - - - - - -
  **
  **  Direction cosines to spherical coordinates.
  **
  **  (double precision)
  **
  **  Given:
  **     v      double[3]   x,y,z vector
  **
  **  Returned:
  **     *a,*b  double      spherical coordinates in radians
  **
  **  The spherical coordinates are longitude (+ve anticlockwise
  **  looking from the +ve latitude pole) and latitude.  The
  **  Cartesian coordinates are right handed, with the x axis
  **  at zero longitude and latitude, and the z axis at the
  **  +ve latitude pole.
  **
  **  If v is null, zero a and b are returned.
  **  At either pole, zero a is returned.
  **
  **  Last revision:   31 October 1993
  **
  **  Copyright P.T.Wallace.  All rights reserved.
  */
{
  double x, y, z, r;
  
  x = v[0];
  y = v[1];
  z = v[2];
  r = sqrt ( x * x + y * y );
  
  *a = ( r != 0.0 ) ? atan2 ( y, x ) : 0.0;
  *b = ( z != 0.0 ) ? atan2 ( z, r ) : 0.0;
}

float slaSep ( float a1, float b1, float a2, float b2 )
  /*
  **  - - - - - - -
  **   s l a S e p
  **  - - - - - - -
  **
  **  Angle between two points on a sphere.
  **
  **  (single precision)
  **
  **  Given:
  **     a1,b1     float     spherical coordinates of one point
  **     a2,b2     float     spherical coordinates of the other point
  **
  **  (The spherical coordinates are [RA,Dec], [Long,Lat] etc, in radians.)
  **
  **  The result is the angle, in radians, between the two points.  It is
  **  always positive.
  **
  **  Called:  slaDsep
  **
  **  Last revision:   7 May 2000
  **
  **  Copyright P.T.Wallace.  All rights reserved.
  */
{
  
  /* Use double precision version. */
  return (float) slaDsep( (double) a1, (double) b1,
			  (double) a2, (double) b2 );
}

double slaDsep ( double a1, double b1, double a2, double b2 )
  /*
  **  - - - - - - - -
  **   s l a D s e p
  **  - - - - - - - -
  **
  **  Angle between two points on a sphere.
  **
  **  (double precision)
  **
  **  Given:
  **     a1,b1    double    spherical coordinates of one point
  **     a2,b2    double    spherical coordinates of the other point
  **
  **  (The spherical coordinates are [RA,Dec], [Long,Lat] etc, in radians.)
  **
  **  The result is the angle, in radians, between the two points.  It
  **  is always positive.
  **
  **  Called:  slaDcs2c, slaDsepv
  **
  **  Last revision:   7 May 2000
  **
  **  Copyright P.T.Wallace.  All rights reserved.
  */
{
  double v1[3], v2[3];
  
  /* Convert coordinates from spherical to Cartesian. */
  slaDcs2c ( a1, b1, v1 );
  slaDcs2c ( a2, b2, v2 );
  
  /* Angle between the vectors. */
  return slaDsepv ( v1, v2 );
}

void slaDcs2c ( double a, double b, double v[3] )
  /*
  **  - - - - - - - - -
  **   s l a D c s 2 c
  **  - - - - - - - - -
  **
  **  Spherical coordinates to direction cosines.
  **
  **  (double precision)
  **
  **  Given:
  **     a,b       double      spherical coordinates in radians
  **                           (RA,Dec), (long,lat) etc
  **
  **  Returned:
  **     v         double[3]   x,y,z unit vector
  **
  **  The spherical coordinates are longitude (+ve anticlockwise
  **  looking from the +ve latitude pole) and latitude.  The
  **  Cartesian coordinates are right handed, with the x axis
  **  at zero longitude and latitude, and the z axis at the
  **  +ve latitude pole.
  **
  **  Last revision:   31 October 1993
  **
  **  Copyright P.T.Wallace.  All rights reserved.
  */
{
  double cosb;
  
  cosb = cos ( b );
  v[0] = cos ( a ) * cosb;
  v[1] = sin ( a ) * cosb;
  v[2] = sin ( b );
  return;
}

void slaCs2c ( float a, float b, float v[3] )
  /*
  **  - - - - - - - -
  **   s l a C s 2 c
  **  - - - - - - - -
  **
  **  Spherical coordinates to direction cosines.
  **
  **  (single precision)
  **
  **  Given:
  **     a,b      float     spherical coordinates in radians
  **                        (RA,Dec), (long,lat) etc
  **
  **  Returned:
  **     v        float[3]  x,y,z unit vector
  **
  **  The spherical coordinates are longitude (+ve anticlockwise
  **  looking from the +ve latitude pole) and latitude.  The
  **  Cartesian coordinates are right handed, with the x axis
  **  at zero longitude and latitude, and the z axis at the
  **  +ve latitude pole.
  **
  **  Last revision:   31 October 1993
  **
  **  Copyright P.T.Wallace.  All rights reserved.
  */
{
  float cosb;
  
  cosb = (float) cos ( b );
  v[0] = (float) cos ( a ) * cosb;
  v[1] = (float) sin ( a ) * cosb;
  v[2] = (float) sin ( b );
}

float slaSepv ( float v1[3], float v2[3] )
  /*
  **  - - - - - - - -
  **   s l a S e p v
  **  - - - - - - - -
  **
  **  Angle between two vectors.
  **
  **  (single precision)
  **
  **  Given:
  **     v1     float[3]     first vector
  **     v2     float[3]     second vector
  **
  **  The result is the angle, in radians, between the two vectors.  It
  **  is always positive.
  **
  **  Notes:
  **
  **  1  There is no requirement for the vectors to be unit length.
  **
  **  2  If either vector is null, zero is returned.
  **
  **  3  The simplest formulation would use dot product alone.  However,
  **     this would reduce the accuracy for angles near zero and pi.  The
  **     algorithm uses both cross product and dot product, which maintains
  **     accuracy for all sizes of angle.
  **
  **  Called:  slaDsepv
  **
  **  Last revision:   7 May 2000
  **
  **  Copyright P.T.Wallace.  All rights reserved.
  */
{
  int i;
  double dv1[3], dv2[3];
  
  /* Use double precision version. */
  for ( i = 0; i < 3; i++ ) {
    dv1[i] = (double) v1[i];
    dv2[i] = (double) v2[i];
  }
  return (float) slaDsepv ( dv1, dv2 );
}

double slaDsepv ( double v1[3], double v2[3] )
  /*
  **  - - - - - - - - -
  **   s l a D s e p v
  **  - - - - - - - - -
  **
  **  Angle between two vectors.
  **
  **  (double precision)
  **
  **  Given:
  **     v1      double[3]    first vector
  **     v2      double[3]    second vector
  **
  **  The result is the angle, in radians, between the two vectors.  It
  **  is always positive.
  **
  **  Notes:
  **
  **  1  There is no requirement for the vectors to be unit length.
  **
  **  2  If either vector is null, zero is returned.
  **
  **  3  The simplest formulation would use dot product alone.  However,
  **     this would reduce the accuracy for angles near zero and pi.  The
  **     algorithm uses both cross product and dot product, which maintains
  **     accuracy for all sizes of angle.
  **
  **  Called:  slaDvxv, slaDvn, slaDvdv
  **
  **  Last revision:   7 May 2000
  **
  **  Copyright P.T.Wallace.  All rights reserved.
  */
{
  double v1xv2[3], wv[3], s, c;
  
  /* Modulus of cross product = sine multiplied by the two moduli. */
  slaDvxv ( v1, v2, v1xv2 );
  slaDvn ( v1xv2, wv, &s );
  
  /* Dot product = cosine multiplied by the two moduli. */
  c = slaDvdv ( v1, v2 );
  
  /* Angle between the vectors. */
  return s != 0.0 ? atan2 ( s, c ) : 0.0;
}

void slaDvxv ( double va[3], double vb[3], double vc[3] )
  /*
  **  - - - - - - - -
  **   s l a D v x v
  **  - - - - - - - -
  **
  **  Vector product of two 3-vectors.
  **
  **  (double precision)
  **
  **  Given:
  **     va      double[3]     first vector
  **     vb      double[3]     second vector
  **
  **  Returned:
  **     vc      double[3]     vector result
  **
  **  Note:  the same vector may be specified more than once.
  **
  **  Last revision:   6 November 1999
  **
  **  Copyright P.T.Wallace.  All rights reserved.
  */
{
  double vw[3];
  int i;
  
  /* Form the vector product va cross vb */
  vw[0] = va[1] * vb[2] - va[2] * vb[1];
  vw[1] = va[2] * vb[0] - va[0] * vb[2];
  vw[2] = va[0] * vb[1] - va[1] * vb[0];
  
  /* Return the result */
  for ( i = 0; i < 3; i++ ) {
    vc[i] = vw[i];
  }
  return;
}

void slaDvn ( double v[3], double uv[3], double *vm )
  /*
  **  - - - - - - -
  **   s l a D v n
  **  - - - - - - -
  **
  **  Normalizes a 3-vector also giving the modulus.
  **
  **  (double precision)
  **
  **  Given:
  **     v       double[3]      vector
  **
  **  Returned:
  **     uv      double[3]      unit vector in direction of v
  **     *vm     double         modulus of v
  **
  **  Note:  v and uv may be the same array.
  **
  **  If the modulus of v is zero, uv is set to zero as well.
  **
  **  Last revision:   6 December 2001
  **
  **  Copyright P.T.Wallace.  All rights reserved.
  */
{
  int i;
  double w1, w2;
  
  /* Modulus */
  w1 = 0.0;
  for ( i = 0; i < 3; i++ ) {
    w2 = v[i];
    w1 += w2 * w2;
  }
  w1 = sqrt ( w1 );
  *vm = w1;
  
  /* Normalize the vector */
  w1 = ( w1 > 0.0 ) ? w1 : 1.0;
  
  for ( i = 0; i < 3; i++ ) {
    uv[i] = v[i] / w1;
  }
  return;
}

float slaVdv ( float va[3], float vb[3] )
  /*
  **  - - - - - - -
  **   s l a V d v
  **  - - - - - - -
  **
  **  Scalar product of two 3-vectors.
  **
  **  (single precision)
  **
  **  Given:
  **      va      float[3]     first vector
  **      vb      float[3]     second vector
  **
  **  The result is the scalar product va.vb  (single precision).
  **
  **  Last revision:   15 July 1993
  **
  **  Copyright P.T.Wallace.  All rights reserved.
  */
{
  return va[0] * vb[0] + va[1] * vb[1] + va[2] * vb[2];
}

double slaDvdv ( double va[3], double vb[3] )
  /*
  **  - - - - - - - -
  **   s l a D v d v
  **  - - - - - - - -
  **
  **  Scalar product of two 3-vectors.
  **
  **  (double precision)
  **
  **
  **  Given:
  **      va      double(3)     first vector
  **      vb      double(3)     second vector
  **
  **
  **  The result is the scalar product va.vb (double precision)
  **
  **
  **  Last revision:   31 October 1993
  **
  **  Copyright P.T.Wallace.  All rights reserved.
  */
{
  return va[0] * vb[0] + va[1] * vb[1] + va[2] * vb[2];
}


//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
#ifdef TEST_MAIN
int main(void)
{

  //struct timeval ExactTime;
  time_t CurrentTime;
  tm *ptm;

  time ( &CurrentTime );
  ptm = gmtime ( &CurrentTime );

  std::cout << "Year = " << (ptm->tm_year+1900) << std::endl;
  std::cout << "Month = " << ptm->tm_mon << std::endl;
  std::cout << "Day = " << ptm->tm_mday << std::endl;

  // Get Modified Julian date at 00:00:00 UT time
  int bogus;
  double mjd;
  slaCldj ( ptm->tm_year+1900, ptm->tm_mon+1, ptm->tm_mday, &mjd, &bogus );

  std::cout << "MJD = " << mjd << std::endl;

  // Now get the sideral time...
  double gmst = slaGmst ( mjd );
  std::cout << "Sidereal time  = " << gmst << " radians" << std::endl;
  
  //...

  return 0;
}
#endif
