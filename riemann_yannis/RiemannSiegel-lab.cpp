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

compile g++ RiemannSiegel.cpp -O -o RiemannSiegel
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

const double pi = 3.1415926535897932385;
typedef unsigned long      ui32;
typedef unsigned long long ui64;
const double two_pi = 2.0*pi;

static inline double dml_micros()
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

inline double theta(double t)
{
	// diviser le calcul ici ? remplacer les pow
	float temp = t*t*t;
	return(t*0.5*log(t*0.159154943) - t*0.5 - 0.392699082
 + 0.0208333333
/t + 0.00121527778
/temp + 0.000384424603
/(temp*t*t) +0.000295293899/(temp*t*t*t*t)+0.000420053399
/(temp*t*t*t*t*t*t));
	//https://oeis.org/A282898  // numerators
	//https://oeis.org/A114721  // denominators
}

static inline double C(int n, double z){
	const double z2 = z * z;
	const double z4 = z2 * z2;
	const double z6 = z4 * z2;
	const double z8 = z6 * z2;
	const double z10 = z8 * z2;
	const double z12 = z10 * z2;
	const double z14 = z12 * z2;
	const double z16 = z14 * z2;
	const double z18 = z16 * z2;
	const double z20 = z18 * z2;
	const double z22 = z20 * z2;
	const double z24 = z22 * z2;
	const double z26 = z24 * z2;
	const double z28 = z26 * z2;
	const double z30 = z28 * z2;
	const double z32 = z30 * z2;
	const double z34 = z32 * z2;
	const double z36 = z34 * z2;
	const double z38 = z36 * z2;
	const double z40 = z38 * z2;
	const double z42 = z40 * z2;
	const double z44 = z42 * z2;
	const double z46 = z44 * z2;


	if (n==0)
		  return(.38268343236508977173
			+.43724046807752044936 * z2
			+.13237657548034352332 * z4
			-.01360502604767418865 * z6
			-.01356762197010358089 * z8
			-.00162372532314446528 * z10
			+.00029705353733379691 * z12
			+.00007943300879521470 * z14
			+.00000046556124614505 * z16
			-.00000143272516309551 * z18
			-.00000010354847112313 * z20
			+.00000001235792708386 * z22
			+.00000000178810838580 * z24
			-.00000000003391414390 * z26
			-.00000000001632663390 * z28
			-.00000000000037851093 * z30
			+.00000000000009327423 * z32
			+.00000000000000522184 * z34
			-.00000000000000033507 * z36
			-.00000000000000003412 * z38
			+.00000000000000000058 * z40
			+.00000000000000000015 * z42);
	else if (n==1)
		 return(-.02682510262837534703 * z
			+.01378477342635185305 * (z2*z)
			+.03849125048223508223 * (z4*z)
			+.00987106629906207647 * (z6*z)
			-.00331075976085840433 * (z8*z)
			-.00146478085779541508 * (z10*z)
			-.00001320794062487696 * (z12*z)
			+.00005922748701847141 * (z14*z)
			+.00000598024258537345 * (z16*z)
			-.00000096413224561698 * (z18*z)
			-.00000018334733722714 * (z20*z)
			+.00000000446708756272 * (z22*z)
			+.00000000270963508218 * (z24*z)
			+.00000000007785288654 * (z26*z)
			-.00000000002343762601 * (z8*z)
			-.00000000000158301728 * (z30*z)
			+.00000000000012119942 * (z32*z)
			+.00000000000001458378 * (z34*z)
			-.00000000000000028786 * (z36*z)
			-.00000000000000008663 * (z38*z)
			-.00000000000000000084 * (z40*z)
			+.00000000000000000036 * (z42*z)
			+.00000000000000000001 * (z44*z));
else if (n==2)
		 return(+.00518854283029316849 
			+.00030946583880634746 * z2
			-.01133594107822937338 * z4
			+.00223304574195814477 * z6
			+.00519663740886233021 * z8
			+.00034399144076208337 * z10
			-.00059106484274705828 * z12
			-.00010229972547935857 * z14
			+.00002088839221699276 * z16
			+.00000592766549309654 * z18
			-.00000016423838362436 * z20
			-.00000015161199700941 * z22
			-.00000000590780369821 * z24
			+.00000000209115148595 * z26
			+.00000000017815649583 * z28
			-.00000000001616407246 * z30
			-.00000000000238069625 * z32
			+.00000000000005398265 * z34
			+.00000000000001975014 * z36
			+.00000000000000023333 * z38
			-.00000000000000011188 * z40
			-.00000000000000000416 * z42
			+.00000000000000000044 * z44
			+.00000000000000000003 * z46);
else if (n==3)
		 return(-.00133971609071945690 * z
			+.00374421513637939370 * (z2 * z)
			-.00133031789193214681 * (z4 * z)
			-.00226546607654717871 * (z6 * z)
			+.00095484999985067304 * (z8 * z)
			+.00060100384589636039 * (z10 * z)
			-.00010128858286776622 * (z12 * z)
			-.00006865733449299826 * (z14 * z)
			+.00000059853667915386 * (z16 * z)
			+.00000333165985123995 * (z18 * z)
			+.00000021919289102435 * (z20 * z)
			-.00000007890884245681 * (z22 * z)
			-.00000000941468508130 * (z24 * z)
			+.00000000095701162109 * (z26 * z)
			+.00000000018763137453 * (z28 * z)
			-.00000000000443783768 * (z30 * z)
			-.00000000000224267385 * (z32 * z)
			-.00000000000003627687 * (z34 * z)
			+.00000000000001763981 * (z36 * z)
			+.00000000000000079608 * (z38 * z)
			-.00000000000000009420 * (z40 * z)
			-.00000000000000000713 * (z42 * z)
			+.00000000000000000033 * (z44 * z)
			+.00000000000000000004 * (z46 * z));
else
		 return(+.00046483389361763382
			-.00100566073653404708 * z2
			+.00024044856573725793 * z4
			+.00102830861497023219 * z6
			-.00076578610717556442 * z8
			-.00020365286803084818 * z10
			+.00023212290491068728 * z12
			+.00003260214424386520 * z14
			-.00002557906251794953 * z16
			-.00000410746443891574 * z18
			+.00000117811136403713 * z20
			+.00000024456561422485 * z22
			-.00000002391582476734 * z24
			-.00000000750521420704 * z26
			+.00000000013312279416 * z28
			+.00000000013440626754 * z30
			+.00000000000351377004 * z32
			-.00000000000151915445 * z34
			-.00000000000008915418 * z36
			+.00000000000001119589 * z38
			+.00000000000000105160 * z40
			-.00000000000000005179 * z42
			-.00000000000000000807 * z44
			+.00000000000000000011 * z46
			+.00000000000000000004 * (z46 * z2));
}

double Z(double t, int n, std::vector<double>tab_j_sqrt, std::vector<double> tab_j_log)
//*************************************************************************
// Riemann-Siegel Z(t) function implemented per the Riemenn Siegel formula.
// See http://mathworld.wolfram.com/Riemann-SiegelFormula.html for details
//*************************************************************************
{
	double p; /* fractional part of sqrt(t/(2.0*pi))*/
	double C(int,double); /* coefficient of (2*pi/t)^(k*0.5) */
	int N = sqrt(t/two_pi); 
	p = sqrt(t/two_pi) - N; 
	double tt = theta(t);
	double ZZ = 0.0; 
	double t_invert = 1.0/t; 
	double tinvert2pi = two_pi * t_invert;
	int j = 1;
 
	// #pragma omp parallel for \
    //     reduction(+:ZZ) private(j) shared(tab_j_sqrt, tab_j_log, tt, t) \
    //     schedule(static, 16) \
    //     if (N >= 200)
	for (j=1;j <= N;j++) {
		// ZZ += 1.0/sqrt((double) j ) * cos(fmod(tt -t*log((double) j),2.0*pi));
		ZZ += tab_j_sqrt[j] * cos(fmod(tt -t* tab_j_log[j],two_pi));
	}

    // double temp3, temp4;

    // for (int j=1;j <= N;j++) {
	// 	// temp3 = fmod(tt -t*tab_j_log[j],two_pi) - pi;
	// 	temp3 = abs(abs(fmod(tt -t*tab_j_log[j],two_pi)) - pi);
		
	// 	double beta = 0.5 * pi - temp3;
	// 	double beta2 = beta * beta;
	// 	double beta3 = beta2 * beta;
	// 	//printf("temp3 = %f ; Beta = %f \n", temp3, beta);
	// 	double beta5 = beta3 * beta2;
	// 	temp4 = beta - 0.166666667 * beta3 + 0.00833333333 * beta5;
	// 	if (abs(beta) >= 1.2) {
	// 		double beta7 = beta5 * beta2;
	// 		double beta9 = beta7 * beta2;
	// 		temp4 = temp4 - 0.000198412698 * beta7 + (2.75573192E-6) * beta9;
	// 	}

	// 	ZZ = ZZ + tab_j_sqrt[j] * -temp4;
	// } 

	ZZ *= 2.0; 
	double R  = 0.0; 

	//n est tjrs egal à 4 lors des appels, on enleve la boucle 
	R = C(0, 2.0 * p - 1.0) 
    + C(1, 2.0 * p - 1.0) * pow(tinvert2pi, 0.5)
    + C(2, 2.0 * p - 1.0) * (tinvert2pi)
    + C(3, 2.0 * p - 1.0) * pow(tinvert2pi, 1.5)
    + C(4, 2.0 * p - 1.0) * (tinvert2pi) * (tinvert2pi);

	R = even(N - 1) * pow(tinvert2pi, 0.25) * R;
	
	return(ZZ + R);
}

/*
	Code to compute Zeta(t) with high precision
	Only works in IEEE 754 for t<1000
	This can help to validate that the Riemann Siegel function for small values but since we are mainly interrested to the behavior for large values of t,
	the best method is to compute zeros that are known
	As you may observe, the accuracy of Z(t) gets better with large values of t until being limited by the IEEE 754 norm and the double format. 
*/
std::complex <double> test_zerod(const double zero,const int N)
{
        const std::complex <double> un(1.0,0);  
        const std::complex <double> deux(2.0,0); 
        const std::complex <double> c1(0.5,zero);
        std::complex <double> sum1(0.0,0.0);
        std::complex <double> sum2(0.0,0.0);
        std::complex <double> p1=un/(un-pow(deux,un-c1));

        for(int k=1;k<=N;k++){
                 std::complex <double> p2=un/pow(k,c1);
                 if(k%2==0)sum1+=p2;
                 if(k%2==1)sum1-=p2;
        }
        std::vector<double   > V1(N);
        std::vector<double   > V2(N);
        double coef=1.0;
        double up=N;
        double dw=1.0;
        double su=0.0;
        for(int k=0;k<N;k++){
                coef*=up;up-=1.0;
                coef/=dw;dw+=1.0;
                V1[k]=coef;
                su+=coef;
        }
        for(int k=0;k<N;k++){
                V2[k]=su;
                su-=V1[k];
        }
        for(int k=N+1;k<=2*N;k++){
                 std::complex <double> p2=un/pow(k,c1);
                 double ek=V2[k-N-1];
                 std::complex <double> c3(ek,0.0);
                 std::complex <double> c4=p2*c3;
                 if(k%2==0)sum2+=c4;
                 if(k%2==1)sum2-=c4;
        }

        std::complex <double> rez=(sum1+sum2/pow(deux,N))*p1;
        return(rez);
}

void test_one_zero(double t, std::vector<double>tab_j_sqrt, std::vector<double> tab_j_log)
{
	double RS=Z(t,4,tab_j_sqrt, tab_j_log);
	std::complex <double> c1=test_zerod(t,10);
	std::complex <double> c2=test_zerod(t,100);
	std::complex <double> c3=test_zerod(t,1000);
	std::cout << std::setprecision(15);
        std::cout << "RS= "<<" "<<RS<<" TEST10= "<< c1 << " TEST100=" << c2 << " TEST1000=" << c3 << std::endl;
	
}

void tests_zeros(std::vector<double>tab_j_sqrt, std::vector<double> tab_j_log)
{
	test_one_zero(14.1347251417346937904572519835625, tab_j_sqrt, tab_j_log);
    test_one_zero(101.3178510057313912287854479402924, tab_j_sqrt, tab_j_log);
    test_one_zero(1001.3494826377827371221033096531063, tab_j_sqrt, tab_j_log);
    test_one_zero(10000.0653454145353147502287213889928, tab_j_sqrt, tab_j_log);

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

char line[1024];
void test_fileof_zeros(const char *fname, std::vector<double>tab_j_sqrt, std::vector<double> tab_j_log)
{
	FILE *fi=fopen(fname,"r");
	assert(fi!=NULL);
	for(;;){
		double t,RS;
		fgets(line,1000,fi);
		if(feof(fi))break;
		sscanf(line,"%lf",&t);
		RS=Z(t,4, tab_j_sqrt, tab_j_log);
		printf(" %30.20lf %30.20lf   %s",t,RS,line);
	}
	fclose(fi);
}

std::vector<double> calculateValuesSqrt(int Nmax) {
    std::vector<double> tab_j;

    for (int j = 0; j <= Nmax; ++j) {
        double sqrt_j = 1.0 / sqrt(static_cast<double>(j));
        tab_j.push_back({sqrt_j});
    }

    return tab_j;
}

std::vector<double> calculateValuesLog(int Nmax) {
    std::vector<double> tab_j;

    for (int j = 0; j <= Nmax; ++j) {
        double log_j = log(static_cast<double>(j));

        tab_j.push_back({log_j});
    }

    return tab_j;
}


int main(int argc,char **argv)
{
	double LOWER,UPPER,SAMP;
	//tests_zeros();
	//test_fileof_zeros("ZEROS");
	try {
		LOWER=std::atof(argv[1]);
		UPPER=std::atof(argv[2]);
		SAMP =std::atof(argv[3]);
	}
	catch (...) {
                std::cout << argv[0] << " START END SAMPLING" << std::endl;
                return -1;
        }
	double estimate_zeros=(UPPER/2.0*log(UPPER/2.0/pi) - UPPER/2.0 - pi/8.0 + 1.0/48.0/UPPER + 7.0/5760.0/pow(UPPER,3.0) + 31.0/80640.0/powl(UPPER,5.0) +127.0/430080.0/powl(UPPER,7.0)+511.0/1216512.0/powl(UPPER,9.0))/pi;
	printf("I estimate I will find %1.3lf zeros\n",estimate_zeros);

	double STEP = 1.0/SAMP;
	ui64   NUMSAMPLES=floor((UPPER-LOWER)*SAMP+1.0);
	double prev=0.0;
	double count=0.0;
	double t1=dml_micros();
	
	int maxN = ceil(sqrt(UPPER / two_pi)); //Modif NMAX 
	std::vector<double> tab_j_sqrt = calculateValuesSqrt(maxN); //précalcul des log(j) et 1/sqrt(j)
	std::vector<double> tab_j_log = calculateValuesLog(maxN);

    int maxI = ceil((UPPER - LOWER) / SAMP);
    int i, chunk;
    double t;

    int chunk_size = 100000;
    int num_chunks = (NUMSAMPLES + chunk_size - 1) / chunk_size;
	double chunk_step = chunk_size * STEP;
	printf("num samples = %llu \n", NUMSAMPLES);
	printf("chunk size  = %d \n", chunk_size);
	printf("num chunks  = %d \n", num_chunks);
	printf("chunk step  = %f \n", chunk_step);

    #pragma omp parallel for \
        reduction(+ : count) private(chunk, t, prev) shared(tab_j_sqrt, tab_j_log) \
        schedule(static)
    for (chunk = 0; chunk < num_chunks; chunk += 1) {

        // for (i = 0; i < NUMSAMPLES; i++) {
        double lower = LOWER + chunk * chunk_step - STEP;
        double upper = std::min(UPPER, lower + chunk_step + STEP * 0.99);
		// printf("chunk = %d, lower = %f, upper = %f \n", chunk, lower, upper);

        for (t = lower; t <= upper; t += STEP) {
            double zout = Z(t, 4, tab_j_sqrt, tab_j_log);
			// printf("%f \n", zout);
            if(t > lower){
                if(   ((zout<0.0)and(prev>0.0))
                    or((zout>0.0)and(prev<0.0))){
                    // printf("%20.6lf  %20.12lf %20.12lf\n",t,prev,zout);
                    count += 1.0;
                }
            }
            prev = zout;
        }

    }

	double t2=dml_micros();
	printf("I found %1.0lf Zeros in %.3lf seconds\n",count,(t2-t1)/1000000.0);
	return(0);
}



