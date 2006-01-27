/* kashdf5.h */

//Modified
//20/10/04 GHS
//  in mrec change i_threshold to ithphi

/* INCLUDE FILES */
#include "hdf5.h"
#include <cstdlib>
#include <cmath>
#include <cstdio>
using namespace std;


/* STRUCTURE DECLARATIONS */
/* pe_head data structure         */
typedef struct head_t {
	int	itype;
	float	tep;
	float	dli;
	float	dmi;
	char 	magnet_field[4];
	float	etr;
	float	depth;
	float	zobs;
	float	xinitial;
	float	yinitial;
	int	idfile;
	char	version_seg[8];
	float	dl;
	float	dm;
	float	xseg;
	float	yseg;
	float	hobs;
	float	x_offset;
	float	y_offset;
	char	petype[16];
	char	version_pe[16];
 }head;

/* pe_rec data structure         */
typedef struct pe_rec_t {
	short   nx;
        short   ny;
        float	time;
	float	dlr;
	float	dmr;
	int	nnext;
	float	xm;
	float	ym;
	short	spec;
	short	lambda;
	float	em_alt;
 }pe_rec;
/* m_head data structure         */
typedef struct m_head_t {
        int	itype;            /* Start of seg head stuff */
	float	tep;
	float	dli;
	float	dmi;
	char 	magnet_field[4];
	float	etr;
	float	depth;
	float	zobs;
	float	xinitial;
	float	yinitial;
	int	idfile;
	char	version_seg[8];
        float	dl;               /* Start of pe head stuff */
	float	dm;
	float	xseg;
	float	yseg;
	float	hobs;
	float	x_offset;
	float	y_offset;
	char	petype[16];
	char	version_pe[16];
        int     noise;             /* Start of m head stuff */
        float   disc_width;
        float   rate_noise;
        float   reflectivity;
        float   full_aperture;
        float   pesum;
        float   step_size;
        int     itheta_max;
        int     phi_steps;
        char    version_m[16];
        float   adc_gate;
        float   hmult;
        float   concentration;
        char    options[60];
}m_head;

//20/10/04 GHS
//  in mrec change i_threshold to ithphi

/* m_rec data structure         */
typedef struct m_rec_t {
  short     nx;
  short     ny;
  short     itheta;
  short     iphi;
  float     emission_alt;
  float     em_alt_sig;
  float     muon_ratio;
  float     median_time;
  float     fp_median_time;
  float     time_cfd;
  float     fp_time_cfd;
  float     ithphi;
  float     fp_threshold;
  float     i_mult;
  float     fp_mult;
}m_rec;

typedef struct w_data_t {              /* stuct for compressed pixel data */
  int ipix;                   /* Pixel # */
  hvl_t times;                /* pes hit times for this pixel */
}w_data;

typedef struct mf_t {
  hid_t     m_file;
  hid_t     mhead_type;
  hid_t     mdata_type;
  hid_t     mrec_type;
  hid_t     times_type;
  hid_t     pix_type;
  hid_t     ipix_type;
}mf;

typedef struct m_data_t {
  float     tep;
  int       idfile;
  float     x_offset;
  float     y_offset;
  m_rec     mrec;
  hvl_t     ipix;            /*pixel id array*/
  hvl_t     pix;             /* Pointer we can dynamically allocate 
			      *according to the
			      * number of hit pixels in this event */
}m_data;




/* FUNCTION DECLARATIONS:  Public */
extern "C" void peshdf5_create(char *pesfilename);
extern "C" void peshdf5_open(char* pesfilename,int *ierr);
extern "C" void peshdf5_pehead_write(head *pehead);
extern "C" void peshdf5_pehead_read(head *pehead);
extern "C" void peshdf5_out(int *recnum, pe_rec *pe);
extern "C" void peshdf5_in(int *recnum, pe_rec *pe, int *ierr);
extern "C" void peshdf5_close(void);
extern "C" void wcamera_psthdf5_get( char *dataset_name, short *pst_patterns,
				 int *ierr);
extern "C" void mhdf5_make_types(void);
extern "C" void mhdf5_create(char *mfilename, hid_t *m_file);
extern "C" void mhdf5_open(char *mfilename, hid_t *m_file, int *ierr);
extern "C" void mhdf5_mhead_write(m_head *mhead, hid_t *m_file);
extern "C" void mhdf5_mhead_read(m_head *mhead, hid_t *m_file);
extern "C" void mhdf5_mrec_size(hid_t *m_file, int *isize);
extern "C" void mhdf5_out(int *recnum, float *tep, int *idfile, float *x_offset, 
		      float *y_offset, m_rec *mrec_head, float *disc,
		      float *times, hid_t *m_file, int *npixels);
extern "C" void mhdf5_in(int *recnum,  float *tep, int *idfile, float *x_offset,
		     float *y_offset, m_rec *mrec_head, float* disc,
		     float *times, hid_t *m_file, int *ierr, int *npixels);
extern "C" void mhdf5_close(hid_t *m_file);
extern "C" void mhdf5_close_types(void);
extern "C" void mhdf5_data_print(m_rec *mrec_head, float *disc,float *time,
			     int *nhit_pixels);
extern "C" void mhdf5_data_make(float *tep, int *idfile, float *x_offset,
			    float *y_offset, m_rec *mrec_head, float *disc,
			    float *time, int *size, int *nhit_pixels);
