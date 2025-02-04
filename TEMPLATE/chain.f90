MODULE nlte_type
  INTEGER, PARAMETER :: I4B = SELECTED_INT_KIND(9)
  INTEGER, PARAMETER :: SP = KIND(1.0)
  INTEGER, PARAMETER :: DP = KIND(1.0D0)
END MODULE nlte_type     

PROGRAM CHAIN
!
!
!     09/30/2002 changed to f90
!
!     06/2004 	several changes with respect to previous versions,
!		this one to be used with nlte_8.3.1 
!	
!     12/2007   changed for simpler use with FASTWIND9.1
!
!     05/2021   changed use of epshe by yhe

Use nlte_type
IMPLICIT NONE
!
REAL(DP) ::  TEFF,XMLOSS,VINF,GLOG,R,RLAST,T0,BETA,EPSHE,VTURB,FMET, &
&            VMIN,VDIV,ELHE,YHE,CLF,CLF0,CLF1
INTEGER(I4B) :: ITEFF,IVINF,IT1,ITF,ITC0,ITC1
LOGICAL ::   L1,L2,L3,L4,L5,L6,L7,L8,L9,LT1
CHARACTER*80 LINEA,DIR2
CHARACTER*35  NAME
!     ..


OPEN(3,FILE='CHAINFIL',FORM='FORMATTED')
READ(3,9001) NAME,IT1,ITF,ITEFF,GLOG,R,XMLOSS, &
!&            VINF,BETA,EPSHE,VTURB,FMET,CLF,CLF0,CLF1
&            VINF,BETA,YHE,VTURB,FMET,CLF,CLF0,CLF1
9001 	FORMAT(A35,1X,2(I3,1X),I5,1X,F4.2,1X,F5.1, &
&              1X,E8.2,1X,F5.0,1X,F4.2,1X,F4.2,1X,F5.1,1X,F4.2,&
&              1X,F5.2,1X,F4.2,1X,F4.2)
!
! old variables appearing in CHAINFIL_DAT. Here for compatibility
!
L1=1      ! update Ne

RLAST= 120. ! maximum radius
T0= 0.6     ! minimum T(r)
L2=0      ! always F (optmod)
L3=1      ! Lucy-approx?
L4=0      ! generate megasalida file?
L5=1      ! forward extrapolation for accelerated convergence?
L6=1        ! CMF-treatment after iteration 20? (if not, only Sobolev)
L7=0      ! Hopf-parameters from HOPFPARA file
L8=1      ! approximate line-blocking?
L9=1      ! approximate line-blocking also in model?
LT1=1     ! T-correction?
ITC0= 1     ! iteration for begin of T-correction
ITC1= 2     ! iteration step for T update
!
TEFF= ITEFF


!	XMLOSS= -(MLOSS/1000.)
!	XMLOSS= 10.**XMLOSS
!       xmloss = xmloss *1.d-6
!	VINF= IVINF*10.

PRINT*,' '
PRINT*,'MODEL: ',TRIM(NAME)
PRINT*,'DATA : ',IT1,ITF,TEFF,GLOG,R,RLAST,T0,XMLOSS, &
!&      VINF,BETA,EPSHE,L2,L3,L4,L5,L6,L7, &
&      VINF,BETA,YHE,L2,L3,L4,L5,L6,L7, &
&      VTURB,L8,L9,FMET,LT1,ITC0,ITC1,CLF,CLF0,CLF1
!
OPEN(2,FILE='INDAT.DAT',FORM='FORMATTED')
OPEN(4,FILE='SUBDIR.LIS',FORM='FORMATTED')
LINEA= TRIM(NAME)
WRITE(2,*) TRIM(LINEA)
WRITE(4,*) TRIM(NAME)
!
!     output, option for HeI, initial iteration, final iteration
!
LINEA='T' !always consider He in the input file
WRITE(2,'(A2,1X,L1,1X,2(I3,1X))') TRIM(LINEA),L1,IT1,ITF
!
!     optimization
!
LINEA='0.'
WRITE(2,*) TRIM(LINEA)
!
!     teff, log g, rstar
! 
WRITE(2,'(F6.0,1X,F4.2,1X,F5.1,1X)') TEFF,GLOG,R
!
!     rmax/rstar, tmin/teff
!
WRITE(2,'(F4.0,1X,F3.2,1X)') RLAST,T0
!
!     mdot, vmin(start), vinf, beta, v(div)
!
VMIN=.1		! starting value for vmin in km/s
VDIV= .1	
WRITE(2,'(E8.3,1X,F2.1,1X,F5.0,1X,F4.2,1X,F2.1)') &
&                XMLOSS,VMIN,VINF,BETA,VDIV
!
!     yhe, elec/He
!

ELHE=0.
IF(TEFF.GE.9.E3) ELHE=1.
IF(TEFF.GE.2.E4) ELHE=2.
!YHE= EPSHE/(1.-EPSHE)
WRITE(2,'(F5.2,1X,F3.0,1X)') YHE,ELHE
!
!     optmod, tlucy, megasalida, accel, CMF
!
WRITE(2,'(5(L1,1X))') L2,L3,L4,L5,L6
WRITE(2,'(F8.4,1X,F5.2,2(1X,L1))') VTURB,FMET,L8,L9
!	
!     temperature correction
!
linea='F'	! always false presently (refers to adiabatic expansion)
write(2,'(1x,l1,1x,A2,2(3x,i3))') LT1,trim(linea),itc0,itc1
!
!     clumping 
!		
!linea='   1.00       0.20      	0.80'     
!write(2,*) linea(1:30)
WRITE(2,'(2X,F11.9,7X,F4.1,7X,F4.1)') CLF,CLF0,CLF1
!
!	hopf-parameters from HOPFPARA file
!		
!WRITE(2,'(1X,L1)') L7
!
READ(3,9002,END=9999) DIR2

IF(DIR2(1:6).EQ.'FORMAL') THEN
   OPEN(8,FILE='FORMALDATA',FORM='FORMATTED')
   WRITE(8,*) TRIM(NAME)
   WRITE(8,'(F4.0)') VTURB
   WRITE(8,'(A1)') DIR2(8:8)
ENDIF

 9002	FORMAT(A80)
 9999	CONTINUE

CLOSE(2)
CLOSE(3)
CLOSE(4)
CLOSE(8)
END
