#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <sys/time.h>
#include <iostream>
#include <iomanip>
#include <cmath>
#include <complex>
#include <vector>
#include <cassert>
#include <omp.h>

/*************************************************************************
* *


This code computes the number of zeros on the critical line of the Zeta function.
https://en.wikipedia.org/wiki/Riemann_zeta_function 

This is one of the most important and non resolved problem in mathematics : https://www.science-et-vie.com/article-magazine/voici-les-7-plus-grands-problemes-de-mathematiques-jamais-resolus

This problem has been the subject of one of the most important distributed computing project in the cloud : more than 10000 machines during 2 years. 
They used this algorithm: very well optimized.
This project failed, bitten by a smaller team that used a far better algorithm. 
The code is based on the Thesis of Glendon Ralph Pugh (1992) : https://web.viu.ca/pughg/thesis.d/masters.thesis.pdf

We can optimize the code in numerous ways, and obviously parallelize it. 

Remark: we do not compute the zeros: we count them to check that they are on the Riemann Line.
Remark: Andrew Odlyzko created a method that is far more efficient but too complex to be the subject of an algorithmetical tuning exercice. 

The exercise is to sample a region on the critical line to count how many times the function changes sign, so that there is at least 1 zero between 2 sampling points.
Here we use a constant sampling but you can recode entirely the way to proceed.

Only a correct (right) count matters, and the performance.

comPIle g++ RiemannSiegel.cpp -O -o RiemannSiegel
--------------------------------------------------------------------------
./RiemannSiegel 10 1000 100
I found 10142 Zeros in 3.459 seconds     # OK 
--------------------------------------------------------------------------
./RiemannSiegel 10 10000 10 
I found 10142 Zeros in 0.376 seconds     # OK
--------------------------------------------------------------------------
./RiemannSiegel 10 100000 10
I found 137931 Zeros in 6.934 seconds    # INCORRECT
--------------------------------------------------------------------------
./RiemannSiegel 10 100000 100
I found 138069 Zeros in 56.035 seconds   # OK
--------------------------------------------------------------------------
RiemannSiegel 10 1000000     need to find : 1747146     zeros
RiemannSiegel 10 10000000    need to find : 21136125    zeros
RiemannSiegel 10 100000000   need to find : 248888025   zeros
RiemannSiegel 10 1000000000  need to find : 2846548032  zeros
RiemannSiegel 10 10000000000 need to find : 32130158315 zeros


The more regions you validate and with the best timing, the more points you get.

The official world record of the zeros computed is 10^13 but with some FFTs and the method from Odlyzsko.
Compute time 1 year-core so an algortihm 10000*2*40 times more efficient than ZetaGrid's one. 

* *
*************************************************************************/

typedef unsigned long      ui32;
typedef unsigned long long ui64;

constexpr double PI            = M_PI;         // PI
constexpr double TWO_PI        = 2.0 * PI;     // 2PI
constexpr double PI_2          = M_PI_2;       // PI/2
constexpr double PI_8          = M_PI / 8.0;   // PI/8
constexpr double TWO_PI_INV    = 1.0 / TWO_PI; // 1 / 2PI

static double * MEM_INV_SQRT_J = NULL;   // Memoization des 1/sqrt(j)
static double * MEM_LOG_J      = NULL;   // Memoization des log(j)

double dml_micros()
{
        static struct timezone tz;
        static struct timeval  tv;
        gettimeofday(&tv,&tz);
        return((tv.tv_sec*1000000.0)+tv.tv_usec);
}

static inline int even(int n)
{
	if (n%2 == 0) return(1);
	else          return(-1);
}

static inline double theta(double t)
{
	double tau_1 = 1.0 / t;        // 1/t
	double tau_2 = tau_1 * tau_1;  // 1/t^2
	double acc, aux;

	acc = 0.00042005339856902;
	acc = 0.00029529389880952 + tau_2 * acc;
	acc = 0.0003844246031746  + tau_2 * acc;
	acc = 0.0012152777777778  + tau_2 * acc;
	acc = 0.020833333333333   + tau_2 * acc;
	acc = -PI_8               + tau_1 * acc;

	// aux = -1.418939           + 0.5   * log(t);
	aux = 0.5 * (log(t * TWO_PI_INV) - 1.0);
	acc = acc                 + t * aux;

	return acc;

	// aux = (- PI/8.0 + 1.0/48.0/t + 7.0/5760.0/pow(t,3.0) + 31.0/80640.0/powl(t,5.0) +127.0/430080.0/powl(t,7.0)+511.0/1216512.0/powl(t,9.0));
	// aux = (t/2.0*log(t/2.0/PI) - t/2.0);
	// printf("%f %f \n", aux ,acc);	

	// return (t/2.0*log(t/2.0/PI) - t/2.0 - PI/8.0 + 1.0/48.0/t + 7.0/5760.0/pow(t,3.0) + 31.0/80640.0/powl(t,5.0) +127.0/430080.0/powl(t,7.0)+511.0/1216512.0/powl(t,9.0));
	//https://oeis.org/A282898  // numerators
	//https://oeis.org/A114721  // denominators
}

static inline void C(double z, double res[5]) {
	double acc;

	double z_2 = z * z;

	acc = +.00000000000000000015;
	acc = +.00000000000000000058 + z_2 * acc;
	acc = -.00000000000000003412 + z_2 * acc;
	acc = -.00000000000000033507 + z_2 * acc;
	acc = +.00000000000000522184 + z_2 * acc;
	acc = +.00000000000009327423 + z_2 * acc;
	acc = -.00000000000037851093 + z_2 * acc;
	acc = -.00000000001632663390 + z_2 * acc;
	acc = -.00000000003391414390 + z_2 * acc;
	acc = +.00000000178810838580 + z_2 * acc;
	acc = +.00000001235792708386 + z_2 * acc;
	acc = -.00000010354847112313 + z_2 * acc;
	acc = -.00000143272516309551 + z_2 * acc;
	acc = +.00000046556124614505 + z_2 * acc;
	acc = -.00000143272516309551 + z_2 * acc;
	acc = +.00007943300879521470 + z_2 * acc;
	acc = +.00029705353733379691 + z_2 * acc;
	acc = -.00162372532314446528 + z_2 * acc;
	acc = -.01356762197010358089 + z_2 * acc;
	acc = -.01360502604767418865 + z_2 * acc;
	acc = +.13237657548034352332 + z_2 * acc;
	acc = +.43724046807752044936 + z_2 * acc;
	acc = +.38268343236508977173 + z_2 * acc;

	res[0] = acc;

	// res[0] = (.38268343236508977173 * pow(z, 0.0)
	// 		+.43724046807752044936 * pow(z, 2.0)
	// 		+.13237657548034352332 * pow(z, 4.0)
	// 		-.01360502604767418865 * pow(z, 6.0)
	// 		-.01356762197010358089 * pow(z, 8.0)
	// 		-.00162372532314446528 * pow(z,10.0)
	// 		+.00029705353733379691 * pow(z,12.0)
	// 		+.00007943300879521470 * pow(z,14.0)
	// 		+.00000046556124614505 * pow(z,16.0)
	// 		-.00000143272516309551 * pow(z,18.0)
	// 		-.00000010354847112313 * pow(z,20.0)
	// 		+.00000001235792708386 * pow(z,22.0)
	// 		+.00000000178810838580 * pow(z,24.0)
	// 		-.00000000003391414390 * pow(z,26.0)
	// 		-.00000000001632663390 * pow(z,28.0)
	// 		-.00000000000037851093 * pow(z,30.0)
	// 		+.00000000000009327423 * pow(z,32.0)
	// 		+.00000000000000522184 * pow(z,34.0)
	// 		-.00000000000000033507 * pow(z,36.0)
	// 		-.00000000000000003412 * pow(z,38.0)
	// 		+.00000000000000000058 * pow(z,40.0)
	// 		+.00000000000000000015 * pow(z,42.0));

	acc = +.00000000000000000001;
	acc = +.00000000000000000036 + z_2 * acc;
	acc = -.00000000000000000084 + z_2 * acc;
	acc = -.00000000000000008663 + z_2 * acc;
	acc = -.00000000000000028786 + z_2 * acc;
	acc = +.00000000000001458378 + z_2 * acc;
	acc = +.00000000000012119942 + z_2 * acc;
	acc = -.00000000000158301728 + z_2 * acc;
	acc = -.00000000002343762601 + z_2 * acc;
	acc = +.00000000007785288654 + z_2 * acc;
	acc = +.00000000270963508218 + z_2 * acc;
	acc = +.00000000446708756272 + z_2 * acc;
	acc = -.00000018334733722714 + z_2 * acc;
	acc = -.00000096413224561698 + z_2 * acc;
	acc = +.00000598024258537345 + z_2 * acc;
	acc = +.00005922748701847141 + z_2 * acc;
	acc = -.00001320794062487696 + z_2 * acc;
	acc = -.00146478085779541508 + z_2 * acc;
	acc = -.00331075976085840433 + z_2 * acc;
	acc = +.00987106629906207647 + z_2 * acc;
	acc = +.03849125048223508223 + z_2 * acc;
	acc = +.01378477342635185305 + z_2 * acc;
	acc = -.02682510262837534703 + z_2 * acc;
	acc = 0                      + z   * acc;

	res[1] = acc;

	// res[1] = (-.02682510262837534703 * pow(z, 1.0)
	// 		+.01378477342635185305 * pow(z, 3.0)
	// 		+.03849125048223508223 * pow(z, 5.0)
	// 		+.00987106629906207647 * pow(z, 7.0)
	// 		-.00331075976085840433 * pow(z, 9.0)
	// 		-.00146478085779541508 * pow(z,11.0)
	// 		-.00001320794062487696 * pow(z,13.0)
	// 		+.00005922748701847141 * pow(z,15.0)
	// 		+.00000598024258537345 * pow(z,17.0)
	// 		-.00000096413224561698 * pow(z,19.0)
	// 		-.00000018334733722714 * pow(z,21.0)
	// 		+.00000000446708756272 * pow(z,23.0)
	// 		+.00000000270963508218 * pow(z,25.0)
	// 		+.00000000007785288654 * pow(z,27.0)
	// 		-.00000000002343762601 * pow(z,29.0)
	// 		-.00000000000158301728 * pow(z,31.0)
	// 		+.00000000000012119942 * pow(z,33.0)
	// 		+.00000000000001458378 * pow(z,35.0)
	// 		-.00000000000000028786 * pow(z,37.0)
	// 		-.00000000000000008663 * pow(z,39.0)
	// 		-.00000000000000000084 * pow(z,41.0)
	// 		+.00000000000000000036 * pow(z,43.0)
	// 		+.00000000000000000001 * pow(z,45.0));

	acc = +.00000000000000000003 + z_2 * acc;
	acc = +.00000000000000000044 + z_2 * acc;
	acc = -.00000000000000000416 + z_2 * acc;
	acc = -.00000000000000011188 + z_2 * acc;
	acc = +.00000000000000023333 + z_2 * acc;
	acc = +.00000000000001975014 + z_2 * acc;
	acc = +.00000000000005398265 + z_2 * acc;
	acc = -.00000000000238069625 + z_2 * acc;
	acc = -.00000000001616407246 + z_2 * acc;
	acc = +.00000000017815649583 + z_2 * acc;
	acc = +.00000000209115148595 + z_2 * acc;
	acc = -.00000015161199700941 + z_2 * acc;
	acc = -.00000016423838362436 + z_2 * acc;
	acc = +.00000592766549309654 + z_2 * acc;
	acc = +.00002088839221699276 + z_2 * acc;
	acc = -.00010229972547935857 + z_2 * acc;
	acc = -.00059106484274705828 + z_2 * acc;
	acc = +.00034399144076208337 + z_2 * acc;
	acc = +.00519663740886233021 + z_2 * acc;
	acc = +.00223304574195814477 + z_2 * acc;
	acc = -.01133594107822937338 + z_2 * acc;
	acc = +.00030946583880634746 + z_2 * acc;
	acc = +.00518854283029316849 + z_2 * acc;

	res[2] = acc;

	// res[2] = (+.00518854283029316849 * pow(z, 0.0)
	// 		+.00030946583880634746 * pow(z, 2.0)
	// 		-.01133594107822937338 * pow(z, 4.0)
	// 		+.00223304574195814477 * pow(z, 6.0)
	// 		+.00519663740886233021 * pow(z, 8.0)
	// 		+.00034399144076208337 * pow(z,10.0)
	// 		-.00059106484274705828 * pow(z,12.0)
	// 		-.00010229972547935857 * pow(z,14.0)
	// 		+.00002088839221699276 * pow(z,16.0)
	// 		+.00000592766549309654 * pow(z,18.0)
	// 		-.00000016423838362436 * pow(z,20.0)
	// 		-.00000015161199700941 * pow(z,22.0)
	// 		-.00000000590780369821 * pow(z,24.0)
	// 		+.00000000209115148595 * pow(z,26.0)
	// 		+.00000000017815649583 * pow(z,28.0)
	// 		-.00000000001616407246 * pow(z,30.0)
	// 		-.00000000000238069625 * pow(z,32.0)
	// 		+.00000000000005398265 * pow(z,34.0)
	// 		+.00000000000001975014 * pow(z,36.0)
	// 		+.00000000000000023333 * pow(z,38.0)
	// 		-.00000000000000011188 * pow(z,40.0)
	// 		-.00000000000000000416 * pow(z,42.0)
	// 		+.00000000000000000044 * pow(z,44.0)
	// 		+.00000000000000000003 * pow(z,46.0));

	acc = +.00000000000000000004;
	acc = +.00000000000000000033 + z_2 * acc;
	acc = -.00000000000000000713 + z_2 * acc;
	acc = -.00000000000000009420 + z_2 * acc;
	acc = +.00000000000000079608 + z_2 * acc;
	acc = +.00000000000001763981 + z_2 * acc;
	acc = -.00000000000003627687 + z_2 * acc;
	acc = -.00000000000224267385 + z_2 * acc;
	acc = -.00000000000443783768 + z_2 * acc;
	acc = +.00000000018763137453 + z_2 * acc;
	acc = +.00000000095701162109 + z_2 * acc;
	acc = -.00000000941468508130 + z_2 * acc;
	acc = -.00000007890884245681 + z_2 * acc;
	acc = +.00000021919289102435 + z_2 * acc;
	acc = +.00000333165985123995 + z_2 * acc;
	acc = +.00000059853667915386 + z_2 * acc;
	acc = -.00006865733449299826 + z_2 * acc;
	acc = -.00010128858286776622 + z_2 * acc;
	acc = +.00060100384589636039 + z_2 * acc;
	acc = +.00095484999985067304 + z_2 * acc;
	acc = -.00226546607654717871 + z_2 * acc;
	acc = -.00133031789193214681 + z_2 * acc;
	acc = +.00374421513637939370 + z_2 * acc;
	acc = -.00133971609071945690 + z_2 * acc;
	acc = 0                      + z   * acc;

	res[3] = acc;

	// res[3] = (-.00133971609071945690 * pow(z, 1.0)
	// 		+.00374421513637939370 * pow(z, 3.0)
	// 		-.00133031789193214681 * pow(z, 5.0)
	// 		-.00226546607654717871 * pow(z, 7.0)
	// 		+.00095484999985067304 * pow(z, 9.0)
	// 		+.00060100384589636039 * pow(z,11.0)
	// 		-.00010128858286776622 * pow(z,13.0)
	// 		-.00006865733449299826 * pow(z,15.0)
	// 		+.00000059853667915386 * pow(z,17.0)
	// 		+.00000333165985123995 * pow(z,19.0)
	// 		+.00000021919289102435 * pow(z,21.0)
	// 		-.00000007890884245681 * pow(z,23.0)
	// 		-.00000000941468508130 * pow(z,25.0)
	// 		+.00000000095701162109 * pow(z,27.0)
	// 		+.00000000018763137453 * pow(z,29.0)
	// 		-.00000000000443783768 * pow(z,31.0)
	// 		-.00000000000224267385 * pow(z,33.0)
	// 		-.00000000000003627687 * pow(z,35.0)
	// 		+.00000000000001763981 * pow(z,37.0)
	// 		+.00000000000000079608 * pow(z,39.0)
	// 		-.00000000000000009420 * pow(z,41.0)
	// 		-.00000000000000000713 * pow(z,43.0)
	// 		+.00000000000000000033 * pow(z,45.0)
	// 		+.00000000000000000004 * pow(z,47.0));

		acc = +.00000000000000000004;
		acc = +.00000000000000000011 + z_2 * acc;
		acc = -.00000000000000000807 + z_2 * acc;
		acc = -.00000000000000005179 + z_2 * acc;
		acc = +.00000000000000105160 + z_2 * acc;
		acc = +.00000000000001119589 + z_2 * acc;
		acc = -.00000000000008915418 + z_2 * acc;
		acc = -.00000000000151915445 + z_2 * acc;
		acc = +.00000000000351377004 + z_2 * acc;
		acc = +.00000000013312279416 + z_2 * acc;
		acc = -.00000000750521420704 + z_2 * acc;
		acc = -.00000002391582476734 + z_2 * acc;
		acc = +.00000024456561422485 + z_2 * acc;
		acc = +.00000117811136403713 + z_2 * acc;
		acc = -.00000410746443891574 + z_2 * acc;
		acc = -.00002557906251794953 + z_2 * acc;
		acc = +.00003260214424386520 + z_2 * acc;
		acc = +.00023212290491068728 + z_2 * acc;
		acc = -.00020365286803084818  + z_2 * acc;
		acc = -.00076578610717556442 + z_2 * acc;
		acc = +.00102830861497023219 + z_2 * acc;
		acc = +.00024044856573725793 + z_2 * acc;
		acc = -.00100566073653404708 + z_2 * acc;
		acc = +.00046483389361763382 + z_2 * acc;

		res[4] = acc;
		
		// res[4] = (+.00046483389361763382 * pow(z, 0.0)
		// 	-.00100566073653404708 * pow(z, 2.0)
		// 	+.00024044856573725793 * pow(z, 4.0)
		// 	+.00102830861497023219 * pow(z, 6.0)
		// 	-.00076578610717556442 * pow(z, 8.0)
		// 	-.00020365286803084818 * pow(z,10.0)
		// 	+.00023212290491068728 * pow(z,12.0)
		// 	+.00003260214424386520 * pow(z,14.0)
		// 	-.00002557906251794953 * pow(z,16.0)
		// 	-.00000410746443891574 * pow(z,18.0)
		// 	+.00000117811136403713 * pow(z,20.0)
		// 	+.00000024456561422485 * pow(z,22.0)
		// 	-.00000002391582476734 * pow(z,24.0)
		// 	-.00000000750521420704 * pow(z,26.0)
		// 	+.00000000013312279416 * pow(z,28.0)
		// 	+.00000000013440626754 * pow(z,30.0)
		// 	+.00000000000351377004 * pow(z,32.0)
		// 	-.00000000000151915445 * pow(z,34.0)
		// 	-.00000000000008915418 * pow(z,36.0)
		// 	+.00000000000001119589 * pow(z,38.0)
		// 	+.00000000000000105160 * pow(z,40.0)
		// 	-.00000000000000005179 * pow(z,42.0)
		// 	-.00000000000000000807 * pow(z,44.0)
		// 	+.00000000000000000011 * pow(z,46.0)
		// 	+.00000000000000000004 * pow(z,48.0));
}

static inline double Z(double t)
//*************************************************************************
// Riemann-Siegel Z(t) function implemented per the Riemenn Siegel formula.
// See http://mathworld.wolfram.com/Riemann-SiegelFormula.html for details
//*************************************************************************
{
	double p; /* fractional part of sqrt(t/(2.0*PI))*/
	//double C(int, double); /* coefficient of (2*PI/t)^(k*0.5) */
	double tau = sqrt(t * TWO_PI_INV); // sqrt(t/2PI)
	
	int N = (int) tau;
	int n = 4;
	p = tau - N;

	double tt = theta(t);

	double ZZ = 0.0;

	for (int j = 1; j <= N; j++) {
		double x = tt - t * MEM_LOG_J[j];

		double alpha = fma(trunc(x / TWO_PI), -TWO_PI, x);
		alpha = abs(alpha) - PI;
		double beta = PI_2 - abs(alpha);
		double beta_2 = beta * beta;

		double aux; // Calcul de sin(beta)
		aux = 2.75573192E-6;
		aux = -0.000198412698 + beta_2 * aux;
		aux = 0.00833333333   + beta_2 * aux;
		aux = -0.166666667    + beta_2 * aux;
		aux = 1.0             + beta_2 * aux;
		aux = 0.0             + beta   * aux;

		ZZ = ZZ + MEM_INV_SQRT_J[j] * (-aux);
		// ZZ = ZZ + MEM_INV_SQRT_J[j] * cos(fmod(tt -t * MEM_LOG_J[j], TWO_PI));
		// ZZ = ZZ + 1.0/sqrt((double) j ) * cos(fmod(tt -t*log((double) j),2.0*PI));
	} 
	
	ZZ = 2.0 * ZZ; 
	
	
	double C_mem[5];
	C(2.0 * p - 1.0, C_mem);

	double rau = 1.0 / tau; // pow(TWO_PI / t, 0.5) = sqrt(TWO_PI / t) = 1 / sqrt(t / TWO_PI) = 1 / tau
	double sqrt_rau = sqrt(rau);

	double R  = 0.0; 
	// Remplacement des puissances par des racines carrès + évaluation polynomiale (Horner)
	// R = R + C_mem[4] * pow(rau, 2.0);
	// R = R + C_mem[3] * pow(rau, 1.5);
	// R = R + C_mem[2] * pow(rau, 1.0);
	// R = R + C_mem[1] * pow(rau, 0.5);
	// R = R + C_mem[0] * pow(rau, 0.0);
	R = C_mem[4];
	R = C_mem[3] + sqrt_rau * R;
	R = C_mem[2] + sqrt_rau * R;
	R = C_mem[1] + sqrt_rau * R;
	R = C_mem[0] + sqrt_rau * R;

	// R = even(N-1) * pow(2.0 * PI / t,0.25) * R; 
	R = even(N - 1) * sqrt_rau * R;
	return (ZZ + R);
}

/*
	An option to better the performance of Z(t) for large values of t is to simplify the equations
	to validate we present a function that tests the known zeros :  look at https://www.lmfdb.org/zeros/zeta/?limit=10&N=10
	We should obtain 0.0
        no need to test many zeros. In case of a bug the column 2 will show large values instead of values close to 0 like with the original code
	Observe that when t increases the accuracy increases until the limits of the IEEE 754 norm block us, we should work with extended precision
	But here a few digits of precision are enough to count the zeros, only on rare cases the _float128 should be used
	But this limitation only appears very far and with the constraint of resources it won't be possible to reach this region. 
	----------------------------------------------------------------------------------------------------------------------
	value in double			should be 0.0		 value in strings: LMFDB all the digits are corrects
        14.13472514173469463117        -0.00000248590756340983   14.1347251417346937904572519835625
        21.02203963877155601381        -0.00000294582959536882   21.0220396387715549926284795938969
        25.01085758014568938279        -0.00000174024500421144   25.0108575801456887632137909925628
       178.37740777609997167019         0.00000000389177887139   178.3774077760999772858309354141843
       179.91648402025700193008         0.00000000315651035865   179.9164840202569961393400366120511
       182.20707848436646258961         0.00000000214091858131   182.207078484366461915407037226988
 371870901.89642333984375000000         0.00000060389888876036   371870901.8964233245801283081720385309201
 371870902.28132432699203491211        -0.00000083698274928878   371870902.2813243157291041227177012243450
 371870902.52132433652877807617        -0.00000046459056067712   371870902.5213243412580878836297930128983
*/

int main(int argc,char **argv)
{
	double LOWER, UPPER, SAMP;
	const double PI = 3.1415926535897932385;

	try {
		LOWER=std::atof(argv[1]);
		UPPER=std::atof(argv[2]);
		SAMP =std::atof(argv[3]);
	}
	catch (...) {
                std::cout << argv[0] << " START END SAMPLING" << std::endl;
                return -1;
        }
	double estimate_zeros = theta(UPPER)/PI;
	printf("I estimate I will find %1.3lf zeros\n",estimate_zeros);

	double STEP = 1.0/SAMP;
	ui64   NUMSAMPLES=floor((UPPER-LOWER)*SAMP+1.0);
	double prev = 0.0;
	double count = 0.0;

	// Pré-calculs des 1/sqrt(j) et log(j)
	int Nmax = ceil( sqrt(UPPER / TWO_PI) ); //Modif NMAX

	MEM_INV_SQRT_J = (double*) malloc(sizeof(double) * (Nmax + 1));
	MEM_LOG_J = (double*) malloc(sizeof(double) * (Nmax + 1));

	// Recupération nom de threads
	int num_chunks = 0;

	#pragma omp single
	num_chunks = omp_get_max_threads(); 

	#pragma omp parallel for schedule(static, num_chunks)
	for (int j = 0; j <= Nmax; ++j) {
		double sqrt_j = 1.0 / sqrt( static_cast<double>(j) );
		double log_j = log( static_cast<double>(j) );

		MEM_INV_SQRT_J[j] = sqrt_j;
		MEM_LOG_J[j] = log_j;
	}

    int chunk_size = 1 + (NUMSAMPLES + num_chunks - 1) / num_chunks;
	
	bool is_first = true;
	double prev_private = 0.0;
	
	double t1 = dml_micros();
    #pragma omp parallel
    {
        int num_threads = omp_get_num_threads();

        #pragma omp for schedule(static, chunk_size) nowait firstprivate(is_first, prev_private) reduction(+: count)
        for (int i = 1; i <= NUMSAMPLES; i++) {
            double t = LOWER + i * STEP;

            if(is_first && i>1){
				prev_private = Z(t-STEP);
				is_first = false;
			}

            double zout = Z(t);

            if (t > LOWER) {
                if (((zout < 0.0) && (prev_private > 0.0)) || ((zout > 0.0) && (prev_private < 0.0))) {
                    count++;
                }
            }

            prev_private = zout;
        }
    }

	double t2 = dml_micros();

	printf("I found %1.0lf Zeros in %.3lf seconds\n",count,(t2-t1)/1000000.0);

	// Clean-up
	free(MEM_INV_SQRT_J);
	free(MEM_LOG_J);

	return(0);
}



