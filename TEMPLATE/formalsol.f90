MODULE formalsol_var
!
USE nlte_type
USE nlte_dim
IMPLICIT NONE
!
!
INTEGER(I4B), ALLOCATABLE, DIMENSION(:,:) :: INVERTED
!----------------------------------------------------------------------
!cprepr
!
REAL(DP), DIMENSION(ID_DEPFI) :: VZRP  ! changed Sept 2015 (previous dim too large)
REAL(DP), DIMENSION(2*ID_DEPFI) :: XCMFP
REAL(DP), DIMENSION(ID_CORES+2+4*ID_NOCOR) :: WP,WP1
!----------------------------------------------------------------------
!cvoigt etc
!
REAL(DP), ALLOCATABLE, DIMENSION(:) :: GAMMAL,GAMMAC,WEIG,XNUE
!
!----------------------------------------------------------------------
!doppler,nemin 
!
REAL(DP) :: VMAX,XNEMIN,VTURB,VTURBMIN,VTURBMAX
REAL(DP), ALLOCATABLE, DIMENSION(:) :: VTURBV
!
!----------------------------------------------------------------------
!exi 
!
INTEGER(I4B) ::  NFCMF  
REAL(DP), DIMENSION(ID_NFESC) :: XCMFE
REAL(DP), DIMENSION(ID_DEPFI,ID_NFESC) :: SCONTE
!----------------------------------------------------------------------
!exi1
!
REAL(DP), DIMENSION(ID_NFESC) :: SCE,DESCE
REAL(DP), DIMENSION(2*ID_DEPFI,ID_NFESC) :: SCERAY
!----------------------------------------------------------------------
!file 
!
CHARACTER FILE*60,LINE*20  
!
!----------------------------------------------------------------------
!starkin 
!
INTEGER(I4B), ALLOCATABLE, DIMENSION(:) :: NSTARK,NLEVL,NLEVU,NWS,NTS,NES  
!
!----------------------------------------------------------------------
!starklo 
!
LOGICAL, ALLOCATABLE, DIMENSION(:) :: QHALF
!----------------------------------------------------------------------
!starkre 
!
REAL(DP), ALLOCATABLE, DIMENSION(:,:) :: DWS
REAL(DP), ALLOCATABLE, DIMENSION(:,:) :: TS
REAL(DP), ALLOCATABLE, DIMENSION(:,:) :: ES
REAL(DP), ALLOCATABLE, DIMENSION(:,:) :: PS
!
!additional clumping parameters for optically thick clumping
REAL(DP), DIMENSION(ID_NDEPT) ::  FIC,TCL_FAC_LINE,TCL_FAC_CONT

LOGICAL OPTTHICK

END MODULE formalsol_var
!
!***********************************************************************
!
! main program
!
!***********************************************************************
PROGRAM FORSOL  
!
USE nlte_type
USE nlte_dim
USE fund_const, ONLY: CLIGHT
USE ffr_error
USE formalsol_var, ONLY: NSTARK,GAMMAL,GAMMAC,WEIG,XNUE,VMAX,VTURB,FILE,LINE, &
& XNEMIN,VTURBMIN,VTURBMAX,VTURBV
USE formalsol_var, ONLY: INVERTED, NLEVL, NLEVU, QHALF, NWS, NTS, NES, &
& DWS, TS, ES, PS, OPTTHICK, FIC, TCL_FAC_LINE, TCL_FAC_CONT
USE preformal_var, ONLY: AT

IMPLICIT NONE
!
!     formal solution for singlets/doublets/multiplets with continuum
!
!
!     stark broadening and doppler broadening included
!     including depth dependendance through t and n_e
!
!
!---- from a program by puls, modified and generalized by
!---- e. santolaya (1993) and j.p. (1996)
!
!---- present philosophy: if stark-broadening, optiov = .true.
!----                     else: dependency on delta
!
!---- version 1.1 april 30th  1997 (j.puls) inclusion of preformal,
!         different input
!
!     version 1.2 july 4th 1997
!             gf-values instead of f values, broadening option 2
!             for voigt profiles
!
!     version 1.3 dec 16th 1997
!             new input-structure(formal_input)
!             new frequency grid
!
!     version 1.4 feb 17th 1998
!             polished version, i4 now possible
!             exi files replaced by scratch files
!
!     version 3.0.0 jan 22nd 1999
!             changed to f90, modifications by Chr. Wiethaus incorpo-
!             rated: calc. of xmax,  wavelength in air
!
!     version 3.0.1 march 2nd 1999
!             f90 polished
!
!     version 3.1.0 march 12th 1999
!             more f90 changes, obsolete features removed
!
!     version 3.1.1 march 31st 1999
!             no large vturb, if stark-option
!
!     version 4.0 april 16th 1999
!
!             different treatment of vturb (now separated from temp), 
!             "all" values (however constant) allowed
!
!     version 4.1 april 20th 1999
!             calculation of nemin changed. Now evaluated at
!             tauc = taucmin (0.03)
!
!     version 4.2 may 20th 1999
!             inclusion of NSTARK = 3 option 
!
!     version 4.3 may  4th 2000
!             "cheat" to force overlap 
!
!     version 4.4 may  30th 2000
!             treatment of inverted levels changed: SL = 0. set  
!             different xmax for Stark broadening  
!
!     version 4.5 oct 18th 2000 catalog can be 30 characters long
!
!     version 5.0 july 2003 inclusion of approx. treatment of clumping
!                 (to be consistent with version nlte_8.2 and higher
!                 NOTE: until further evidence, the profile functions
!                       are evaluated at the given (i.e., increased) n_e's
!
!     version 5.1 april 2004 YHe read for use in preformal
!
!     version 5.2 febr  2005 log(xnemin) set always to 10.5, &
!                            perfil marginally modified to allow to be called
!                            by calcxmax when xnemin has not been calculated
!
!     version 5.3 oct   2005 xgrid changed for NB=2, to allow for sufficient
!                            resolution of 2nd component
!
!     version 5.3.1 sept  2006 new formulation to allaw for a better (though
!                         not perfect) treatment of inverted levels
!
!     version 5.3.2 june 2009 treatment of inverted levels even improved
!                         (CONTIN: interpolation of SL and OPAL, EXPUNO)
!
!                   STILL NOT TESTED: case of doublets, where one comp is in inversion
!                   but note: since opa*sl > 0 always, this is also true for
!                   Stot=Sum(opai*si)/Sum(opai): If Sum(opai) <0, then also Stot!
!
!     version 6.0 july 2009 generalization to arbitrary number of components
!                   (in case, change MAXNCOMP=10)
!                 for NB > 2, general approach (obsfram/obsfram1) always 
!
!     version 6.0.1 sept 2009 some constants taken from nlte_type(v1.2)
!
!     version 6.1   april 2012: approximate treatment of transitions between
!                   high lying levels (mm and submm range), for ALMA
!                   coding by NSTARK = 0 or 1 and LINENO=1 (NCOMP=1).
!                   used departures are controlled by OPTDEP in CONTIN
!                   OPTDEP=.FALSE -> DEP = 1, LTE
!                   OPTDEP=.TRUE  -> DEP_LOW = DEP(19), DEP_UP = DEP(20)
!                                    to obtain similar ratios.
!
!     version 7.0 april/may 2013: improvement of incoherent electron scattering:
!                   modified approach allows to consider more than one line!
!                   Inclusion of Stark-profiles from Lemke (1997, A&AS)
!                   So far, only Balmer lines can be used.
!                   To use them, uncomment lines after 'BROAD_BALMER'
!                   in main program, and provide corresponding INPUT keyword
!                   Files IX.DAT and STARK.BIN are now saved in model-directory,
!                   to allow for simultaneous jobs
!
!
!     version 7.1.0 november 2014: inclusion of optically thick clumping
!                   (programmed by Jon Sundqvist), i.e. porosity and vorosity
!                   needs nlte.f90 v10.3.0/nlte_approx.f90 v7.4.0 and higher
!                   (CONT_FORMAL now includes OPA_EFF_RAT = Chi_eff/<Chi>,
!                   additional file OUT_CLUMPING required).
!                   incoherent e-scattering adapted for optically thick clumping
!
!
!     version 8.0 july 2015: emergent spectrum in specific range, from
!                   occupation numbers resulting from cmf-calculations
!                   line-name = 'UV'
!                   function perfil renamed to perfil1 (same name as in princesa)
!                   missing: vturb(r)
!                            correct treatment of inversion
!                            voigt broadening
!                            unpack packed lines in expl. elements
!                            e-scat???
!     NOTE: so far, we calculate individual lines using CONT_FORMAL,
!           whilst for the 'complete' spectra we use CONT_FORMAL_CMF.
!           The difference is that CONT_FORMAL uses pseudo-continua
!           for OPAC and SCONT (somewhat inconsistently calculated with J(cmf)),
!           whilst CONT_FORMAL_CMF uses pure continuum values. This gives,
!           for unbroadened spectra, slight differences due to a different
!           normalization and background, but incorporates the effects from the 
!           additional lines present in the complete treatment. We checked that
!           using CONT_FORMAL_CMF for individual lines gives profiles that are
!           identical with those from the complete approach.
!
!     version 8.1 july 2016: inclusion of depth-dependent micro-turbulence
!                   for standard path (from v7.1 in path standard_xrays_v10.2.1)
!                   NOTE: preformal (Stark etc. profiles) not affected,
!                   since photospheric, i.e., calculated with Min(vturb).
!                   IN THIS FIRST VERSION, ONLY ESCAT = 0 can be treated with
!                   vturb(r). For ESCAT = 1, use constant vturb.
!                   UPDATE of corresponding 'ranges' still required
!
!           THIS VERSION SHOULD BE CONSISTENT with
!           standard_v10.4.1/formalsol v7.2 
!
!----------------------------------------------------------------------
!
!     .. parameters ..
!
!
INTEGER(I4B), PARAMETER :: MAXNO=500  
INTEGER(I4B), PARAMETER :: MAXNCOMP=70  
INTEGER(I4B), PARAMETER :: ND1=ID_NDEPT  
INTEGER(I4B), PARAMETER :: NDM=ID_DEPFI,LTOM=2*NDM,NC=ID_CORES
INTEGER(I4B), PARAMETER :: NP=NC+2+4*ID_NOCOR  
INTEGER(I4B), PARAMETER :: NFOBS=ID_NFOBS  
INTEGER(I4B), PARAMETER :: MAXW=ID_MAXWW,MAXT=ID_MAXTT,MAXNE=ID_MAXNE, &
&          MAXPS=ID_MAXPS
!     ..
!     .. local scalars ..
REAL(DP) ::  DELTA,DELTAX,REALO,RMAX,SR,SRVMAX,RELXMAX, &
&                 TEM,TGRAD,VSINI,VTURB1,XCBLUE,XCRED,YHE,DUM,DUM1, &
&                 LAM_BLU, LAM_RED, RESOL, TIME_CPU, &
&                 VTMI, VTMA
INTEGER(I4B) ::  I,I1,I2,I3,IESCAT,INTEG,IS,ITURB, &
&        IU,L,NB,NCH,NCO,NLI,NLIN,NSUM,J,ND,IBLU,IRED

LOGICAL ESCAT,OPTIOV,EXI,ESCAT_UV,FIRST

LOGICAL BALMER_LEMKE  
! if true, Balmer Stark profiles from Lemke will be used 

CHARACTER DC*6,KEY*6,SPEC*11,RET*4,SPEC1*18  
CHARACTER BROAD_BALMER*6,VTURB_STR*60
!     ..
!     .. local arrays ..
REAL(DP) ::       DVDR(NDM),P(NP),PROFABS(NFOBS), &
&                 PROFEM(NFOBS),PROFILE(NFOBS),PROFROT(NFOBS), &
&                 R(NDM),R1(ND1),RHO(NDM),TEMP(NDM),V(NDM), X0(NFOBS), &
&                 XNE(ND1),XNEFI(NDM),CLF(ND1), &
&                 Z(NDM,NP),ZG(NP,NFOBS),ZRAY(LTOM), &
&                 OPACON(NDM,2),SCONT(NDM,2),OPACRAY(LTOM,2),SCONRAY(LTOM,2), &
&                 AIC(NFOBS,2), &
&                 CLF_TEST(ND1),FVEL(ND1),HPOR(ND1),FVOL(ND1), &
&                 V1(ND1),DVDR1(ND1),TEMP1(ND1)

INTEGER(I4B) ::  INDEX1(ND1), LMAX(NP), LTOT(NP), NCOMP(MAXNO)

INTEGER(I4B) :: LINENO(MAXNCOMP,MAXNO),NSTARKL(MAXNCOMP,MAXNO)

REAL(DP) :: XLAM_BLU(MAXNO), XLAM_RED(MAXNO), XRESOL(MAXNO)

CHARACTER LEVEL(MAXNCOMP,MAXNO)*6,LEVEU(MAXNCOMP,MAXNO)*6,LINES(MAXNO)*20  

INTEGER(I4B), ALLOCATABLE, DIMENSION(:) :: NL,NU

REAL(DP), ALLOCATABLE, DIMENSION(:) :: GFLU,GL,GU,VDOP,XMAX, &
&                                      XMAXDOP,XMAXDOP_MIN,DELTAARR
REAL(DP), ALLOCATABLE, DIMENSION(:,:) :: OPAL, SLINE
REAL(DP), ALLOCATABLE, DIMENSION(:,:) :: OPALRAY, SRAY, XCMF
REAL(DP), ALLOCATABLE, DIMENSION(:) :: VTURBRAY


!     ..
!     .. external subroutines ..
EXTERNAL CONTIN,ELSCAT,FFRACC,FFRCDC,FFRKEY,FFRNCD,FFRNUM,FORMAL, &
&         MODEL,PREFORMAL,STAREAD
!     ..
!     .. intrinsic functions ..
INTRINSIC ABS,CHAR,INDEX,INT,SQRT  
!     ..
FIRST=.TRUE.

PRINT *,' INPUT CATALOGUE NAME FOR FORMAL CALC.'  
READ (*,FMT='(A)') FILE

PRINT *,' Either INPUT of constant VTURB (KM/S)'  
PRINT *,' or INPUT VTURBMIN (KM/S), VTURBMAX (KM/S or IN UNITS OF VINF)'  
READ (*,FMT='(A)') VTURB_STR
! trick to read unknown number of variables (including description)
OPEN (2,STATUS='SCRATCH',FORM='FORMATTED')
WRITE(2,*) VTURB_STR
REWIND(2)
READ (2,FMT=*,END=10,ERR=10) VTMI, VTMA
CLOSE(2)
GOTO 20
! in case that only one input value for vturb (old approach)
10 VTMA=VTMI

20 PRINT *,' INPUT IESCAT (=0, NO; = 1 WITH ESCAT)'  

IF(VTMI.EQ.0.D0 .AND. VTMA.NE.0.D0) &
  STOP' VTURBMIN = 0 AND VTURBMAX NE 0! NOT ALLOWED'

READ (*,FMT=*) IESCAT  
IF(IESCAT.EQ.1 .AND. VTMI.NE.VTMA) &
  STOP' DEPTH DEPENDENT TURBULENCE ONLY FOR NO ESCAT'

VTURB = VTMI*1.D5 ! for photosphere
!VTURBMIN and VTURBMAX will obtain correct units in sub. MODEL

BROAD_BALMER='BUTLER'
!uncomment for tests with Lemke broadening functions
!PRINT *,' IN CASE, SPECIFY STARK BROADENING OF BALMER LINES:'
!PRINT *,' BUTLER (default) or LEMKE'
!READ (*,FMT='(A)',END=5) BROAD_BALMER

!5 IF(BROAD_BALMER.EQ.'') BROAD_BALMER ='BUTLER'

IF(BROAD_BALMER.NE.'BUTLER' .AND. BROAD_BALMER.NE.'LEMKE ') STOP' WRONG BALMER BROADENING'
 
BALMER_LEMKE=.FALSE.
IF (BROAD_BALMER.EQ.'LEMKE') BALMER_LEMKE=.TRUE.

PRINT*,' BALMER LINE BROADENING FOLLOWING ',BROAD_BALMER

DC = ':T'  

OPEN (1,FILE='FORMAL_INPUT',STATUS='OLD',FORM='FORMATTED')  
IU = 1  
CALL FFRACC(IU,RET)  
SELECT CASE(RET)
CASE('RET1')
  GOTO 110
CASE('RET0')
  CONTINUE
CASE DEFAULT
  STOP ' WRONG RETURN IN FORMALSOL'
END SELECT

CALL FFRCDC(DC)  
!
!---  allowing for all characters in string 'file'
!
CALL FFRNCD(RET)  
IF (ON_ERROR(RET)) GOTO 120

CALL FFRNUM(REALO,INTEG,RET)  
IF (ON_ERROR(RET)) GOTO 120
VSINI = REALO  

IF (IESCAT.EQ.0) THEN  
     ESCAT = .FALSE.  
ELSE IF (IESCAT.EQ.1) THEN  
     ESCAT = .TRUE.  
ELSE  
     STOP ' ERROR IN ESCAT OPTION'  
END IF  

NLIN = 0  
PRINT *,' LINES TO BE TREATED'  

COMPLOOP: DO

NLIN = NLIN + 1  
IF (NLIN.GT.MAXNO) STOP 'CHANGE MAXNO'  

CALL FFRKEY(LINES(NLIN),NCH,RET)  
SELECT CASE(RET)
CASE('RET1')
  GOTO 30
CASE('RET2')
  GOTO 120
CASE('RET0')
  CONTINUE
CASE DEFAULT
  STOP ' WRONG RETURN IN FORMALSOL'
END SELECT

PRINT *,LINES(NLIN)  

IF(TRIM(LINES(NLIN)).NE.'UV') THEN
! standard treatment
CALL FFRNUM(REALO,NCO,RET)  
IF (ON_ERROR(RET)) GOTO 120

NCOMP(NLIN) = NCO  

DO I = 1,NCO  
     CALL FFRKEY(KEY,NCH,RET)  
     IF (ON_ERROR(RET)) GOTO 120
     LEVEL(I,NLIN) = KEY  

     CALL FFRKEY(KEY,NCH,RET)  
     IF (ON_ERROR(RET)) GOTO 120
     LEVEU(I,NLIN) = KEY  

     CALL FFRNUM(REALO,INTEG,RET)  
     IF (ON_ERROR(RET)) GOTO 120
     LINENO(I,NLIN) = INTEG  

     CALL FFRNUM(REALO,INTEG,RET)  
     IF (ON_ERROR(RET)) GOTO 120
     NSTARKL(I,NLIN) = INTEG

END DO  

ELSE
! spectral range
     CALL FFRNUM(REALO,INTEG,RET)  
     IF (ON_ERROR(RET)) GOTO 120
     XLAM_BLU(NLIN) = REALO 

     CALL FFRNUM(REALO,INTEG,RET)  
     IF (ON_ERROR(RET)) GOTO 120
     XLAM_RED(NLIN) = REALO 

     CALL FFRNUM(REALO,INTEG,RET)  
     IF (ON_ERROR(RET)) GOTO 120
     XRESOL(NLIN) = REALO 
     
     DELTA=XLAM_RED(NLIN)-XLAM_BLU(NLIN)
     IF(DELTA.LE.0.D0) STOP' LAMBDA_BLUE > LAMBDA_RED, MODIFY INPUT'
     IF(DELTA.LE.RESOL) STOP' RESOL TOO LARGE, MODIFY INPUT'
     
ENDIF
  
END DO COMPLOOP
!
!     end of file formal-input
!
   30 CONTINUE  
WRITE (*,FMT='(A)') ' END OF FORMAL-INPUT, NO MORE LINES '  
CLOSE (1)  

NLIN = NLIN - 1  

OPEN (1,FILE=TRIM(FILE)//'/MODEL',STATUS='OLD',FORM='UNFORMATTED')  
REWIND 1  
READ (1) DUM,DUM,DUM,YHE
CLOSE(1)

ALL_LINES: DO NLI = 1,NLIN  

  IF(TRIM(LINES(NLI)).NE.'UV') THEN
! standard treatment  
     NCO = NCOMP(NLI)  
     IF (NCO .GT. MAXNCOMP) STOP' TOO MANY COMPONENTS (INCREASE MAXNCOMP)'
     
     ALLOCATE(INVERTED(ID_NDEPT,NCO))
     ALLOCATE(GAMMAL(NCO),GAMMAC(NCO),WEIG(NCO),XNUE(NCO))
     ALLOCATE(NSTARK(NCO),NLEVL(NCO),NLEVU(NCO),NWS(NCO),NTS(NCO),NES(NCO))
     ALLOCATE(NL(NCO),NU(NCO))
     ALLOCATE(GFLU(NCO),GL(NCO),GU(NCO),VDOP(NCO))
     ALLOCATE(QHALF(NCO))
     ALLOCATE(DWS(ID_MAXWW,NCO))
     ALLOCATE(TS(ID_MAXTT,NCO))
     ALLOCATE(ES(ID_MAXNE,NCO))
     ALLOCATE(PS(ID_MAXPS,NCO))

     ALLOCATE(OPAL(NDM,NCO),SLINE(NDM,NCO))
     ALLOCATE(XMAX(NCO),XMAXDOP(NCO),XMAXDOP_MIN(NCO),DELTAARR(NCO))

     ALLOCATE(OPALRAY(LTOM,NCO),SRAY(LTOM,NCO),XCMF(LTOM,NCO),VTURBRAY(LTOM))

     LINE = LINES(NLI)  
     DO I = 1,NCO  
          NSTARK(I) = NSTARKL(I,NLI)  
     END DO
!
!---- begin of calculation for different lines
!---- for PREFORMAL, WE USE ONLY VTURB = VTURBMIN
!
!    ARGUMENT .FALSE. CORRESPONDS TO SRANGE IN PREFORMAL
     CALL PREFORMAL(FILE,NCO,LEVEL(1,NLI),LEVEU(1,NLI),LINENO(1,NLI),NSTARK,VTURB,YHE,BALMER_LEMKE,.FALSE.)

     OPEN (1,FILE=TRIM(FILE)//'/IX.DAT',STATUS='UNKNOWN')  
     REWIND 1  
     READ (1,FMT=*) NB  
     IF (NB.NE.NCO) STOP ' SOMETHING WRONG WITH NO OF COMPONENTS'  
     DO I = 1,NB  
          READ (1,FMT=*) XNUE(I)  
          READ (1,FMT=*) GL(I),GU(I)  
          READ (1,FMT=*) GFLU(I)  
          READ (1,FMT=*) NL(I),NU(I)  
          READ (1,FMT=*) WEIG(I)  
          READ (1,FMT=*) NSTARK(I)  
          IF (NSTARK(I).EQ.2.OR.NSTARK(I).EQ.3) &
&             READ (1,FMT=*) GAMMAL(I),GAMMAC(I)  
     END DO
     CLOSE (1)  
!
! approximate treatment so far only for ESCAT = .FALSE.
     IF (AT.AND.ESCAT) STOP' Approximate Treatment (AT) AND ESCAT NOT POSSIBLE YET'

!Read in clumping params from OUTPUT_CLUMPING
     OPEN (1,FILE=TRIM(FILE)//'/CLUMPING_OUTPUT',STATUS='OLD')
     READ(1,*) !header
     DO I=1,ND1 
        READ(1,FMT=*) J,DUM1,DUM1,DUM1,CLF_TEST(I),FIC(I),FVEL(I),HPOR(I),FVOL(I),&
&                TCL_FAC_LINE(I),TCL_FAC_CONT(I)
     ENDDO
     CLOSE(1) 

     OPTTHICK=.TRUE.
     IF(MAXVAL(TCL_FAC_LINE).EQ.0. .AND. MAXVAL(TCL_FAC_CONT).EQ.0.) OPTTHICK=.FALSE. 
     IF(OPTTHICK) THEN
       PRINT*
       PRINT*,'MODEL WITH OPTICALLY THICK CLUMPING'
     ENDIF  
!
!------------------------------------
!     output-filename and open output; still named with vturb=vturbmin
!     but profiles with variable vturb denoted by 'VTV' instead of 'VT'
!
     IS = INDEX(LINE,' ')  
     VTURB1 = VTURB*1.D-5  
     IF (VTURB1.EQ.0.) THEN  
          IF (ESCAT) THEN  
               SPEC = 'ESC'  
               OPEN (2,FILE=TRIM(FILE)//'/OUT.'//LINE(1:IS-1)//'_'// &
&                SPEC, STATUS='UNKNOWN')
          ELSE  
               SPEC = ' '  
               OPEN (2,FILE=TRIM(FILE)//'/OUT.'//LINE(1:IS-1), &
&                STATUS= 'UNKNOWN')
          END IF  
     ELSE  
          ITURB = INT(VTURB1)  
          IF (ITURB.GE.1000) STOP ' VTURB > 1000 KM/S'  
          I1 = ITURB/100  
          I2 = (ITURB-I1*100)/10  
          I3 = ITURB - I1*100 - I2*10  

          IF (ESCAT) THEN  
               SPEC = 'ESC_VT'//CHAR(48+I1)//CHAR(48+I2)//CHAR(48+I3)
          ELSE  
               IF(VTMI.EQ.VTMA) THEN
               SPEC = 'VT'//CHAR(48+I1)//CHAR(48+I2)//CHAR(48+I3)  
               ELSE
               SPEC = 'VTV'//CHAR(48+I1)//CHAR(48+I2)//CHAR(48+I3)
               ENDIF
          END IF  

          OPEN (2,FILE=TRIM(FILE)//'/OUT.'//LINE(1:IS-1)//'_'//SPEC, &
&           STATUS='UNKNOWN')
       END IF
!
!------------------------------------
!
!   initialize xnemin (to be checked in perfil)
! 
     XNEMIN=0.
! 
     NSUM = 0  
     DO I = 1,NB  
          IF (NSTARK(I).EQ.1) NSUM = NSUM + 1  
!
!------- xnue in cm-1
!
          XNUE(I) = 1.D8/XNUE(I)  
!
!------- calculation of mass dependent part of VDOP (rest is calculated
!        in subroutine model)
!
          VDOP(I) = SQRT(1./WEIG(I))
     END DO
!
!---- stark profiles are read
!
     IF (NSUM.GT.0) THEN 
       CALL STAREAD(FILE,NB,NSUM,NL,NU)  
     ENDIF

     DO I = 1,NB  
          IF (NSTARK(I).EQ.2.OR.NSTARK(I).EQ.3) NSUM = NSUM + 10  
     END DO
!
!---- hence: first digit in nsum: number of voigt components
!----          2nd digit in nsum: number of stark components
!
     IF (ESCAT) CALL ELSCAT(XNUE,NL,NU,GFLU,GL,GU,NB)  
!

     WRITE (*,FMT=9000) LINE  
     PRINT *  
     PRINT *,'VSINI: ',VSINI,' KM/S, VTURBMIN: ',VTURB*1.D-5,' KM/S'  

     CALL MODEL(R1,R,V1,V,DVDR1,RHO,RMAX,NDM,ND,VMAX,VDOP,INDEX1, &
      SRVMAX,SR,NB,XNE,CLF,CLF_TEST,VTMI,VTMA)
!
!    from here on, VDOP is correct (at TEFF, corrected for VTURB), in VMAX
!    VDOP calculated with VTURB=VTURBMIN;
!
     DELTA = 0.D0  
! changed from v 6.0, since x-grid w.r.t. xnue(1)
     IF (NB.GE.2) THEN
       DELTA = (XNUE(1)-XNUE(NB))/XNUE(1)*CLIGHT/VMAX
       IF(DELTA.LE.0.D0) STOP' ERROR IN DELTA'
       DO I=2,NB-1
         DELTAARR(I) = (XNUE(1)-XNUE(I))/XNUE(1)*CLIGHT/VMAX
         IF(DELTAARR(I).LE.0.D0) STOP' ERROR IN DELTA'
       ENDDO
       DELTAARR(NB)=DELTA
     ENDIF

     PRINT *  
     PRINT *,' DELTA = ',DELTA  
!
!---- determination of continuum quantities
!
     CALL CONTIN(XNUE,NL,NU,GFLU,SRVMAX,INDEX1,R,OPAL,OPACON, &
      SLINE, SCONT,GL,GU,NB,SR,XCRED,XCBLUE,TEM,TGRAD,XMAX, &
      XMAXDOP,XMAXDOP_MIN,TEMP,XNE,XNEFI,CLF,ESCAT)
!
!---- determination of the implicit overlapping option
!

 99  DELTAX = DELTA - XMAX(1) - XMAX(NB)  
     IF (DELTAX.GT.0.D0) THEN  
          WRITE (*,FMT=9010) DELTAX,DELTA  
          OPTIOV = .FALSE.  

     ELSE  
          IF (DELTA.EQ.0.D0) THEN  
               WRITE (*,FMT=9030) XMAX(1)  
               OPTIOV = .TRUE.  
          ELSE  
               WRITE (*,FMT=9020) DELTAX,DELTA  
               OPTIOV = .TRUE.  
          END IF  
     END IF  
!
!-----more than 2 pure Doppler profiles calculated with general approach
!    
     IF (NSUM.EQ.0 .AND. NB .GT. 2) OPTIOV=.TRUE.
!
!-----in present philosophy, stark/voigt broadening requires
!-----optiov = .true.
!
     IF (NSUM.GT.0 .AND. .NOT.OPTIOV) THEN
!
!    CHEAT TO OBTAIN ALWAYS OVERLAP (=> DELTAX=-0.01): RENORMALIZE XMAX 
!
       RELXMAX=XMAX(NB)/XMAX(1)
       XMAX(1)=(DELTA+0.01)/(1.+RELXMAX)
       XMAX(NB)=XMAX(1)*RELXMAX
       GOTO 99 
    ENDIF  
!
!-----formal integral
    !
              !JS-NEW: ADDING INTENSITY AS OUTPUT 
      OPEN (22,FILE=TRIM(FILE)//'/OUT_IEM.'//LINE(1:IS-1), &
           &                STATUS= 'UNKNOWN')
      CALL FORMAL(ND,NP,NC,NB,NFOBS,RMAX,DELTA,ESCAT,R1,R,V,OPAL, &
      SLINE,P,Z,LMAX,X0,VMAX,OPALRAY,SRAY,ZRAY,VTURBRAY,LTOT, XCMF,PROFILE, &
      PROFABS,PROFEM,ZG,AIC,XMAX,XMAXDOP,XMAXDOP_MIN,OPTIOV,NSUM,OPACON, &
      OPACRAY,SCONT,SCONRAY,TEM, XCRED,XCBLUE,TGRAD,XNUE(1),VDOP, &
      TEMP,XNEFI,PROFROT,VSINI,DELTAARR)
     CLOSE (2)
     !JS-NEW APR 2018 - INTENSITY OUTPUT 
     CLOSE (22) 

     DEALLOCATE(INVERTED)
     DEALLOCATE(GAMMAL,GAMMAC,WEIG,XNUE)
     DEALLOCATE(NSTARK,NLEVL,NLEVU,NWS,NTS,NES,NL,NU)
     DEALLOCATE(GFLU,GL,GU,VDOP)
     DEALLOCATE(QHALF)
     DEALLOCATE(DWS,TS,ES,PS)

     DEALLOCATE(OPAL,SLINE)
     DEALLOCATE(VTURBV)
     DEALLOCATE(XMAX,XMAXDOP,XMAXDOP_MIN,DELTAARR)

     DEALLOCATE(OPALRAY,SRAY,XCMF,VTURBRAY)

  ELSE
! spectral range
! so far, constant vturb and no incoherent e-scat
     IF(VTMI.NE.VTMA) STOP' DEPTH DEPENDENT TURBULENCE ONLY FOR STANDARD APPROACH'
     
     LAM_BLU=XLAM_BLU(NLI)
     LAM_RED=XLAM_RED(NLI)
     RESOL=XRESOL(NLI)
     
     ESCAT_UV=.FALSE.
!     IF(VTURB.LT.5.D0) STOP' UV AND VTURB < 5 KM/S'
     IF(VTURB.LT.1.D0) STOP' UV AND VTURB < 1 KM/S'
     
     INQUIRE(FILE=TRIM(FILE)//'/CONT_FORMAL_CMF', EXIST=EXI)
     IF(.NOT.EXI) STOP' UV range not possible, since CONT_FORMAL_CMF not existent!'

     NB=1
     ALLOCATE(VDOP(NB))
     ALLOCATE(OPALRAY(LTOM,1),SRAY(LTOM,1))
     
!Read in clumping params from OUTPUT_CLUMPING
     OPEN (1,FILE=TRIM(FILE)//'/CLUMPING_OUTPUT',STATUS='OLD')
     READ(1,*) !header
     DO I=1,ND1 
        READ(1,FMT=*) J,DUM1,DUM1,DUM1,CLF_TEST(I),FIC(I),FVEL(I),HPOR(I),FVOL(I),&
&                TCL_FAC_LINE(I),TCL_FAC_CONT(I)
     ENDDO
     CLOSE(1) 

     OPTTHICK=.TRUE.
     IF(MAXVAL(TCL_FAC_LINE).EQ.0. .AND. MAXVAL(TCL_FAC_CONT).EQ.0.) OPTTHICK=.FALSE. 
     IF(OPTTHICK) THEN
       PRINT*
       PRINT*,'MODEL WITH OPTICALLY THICK CLUMPING'
     ENDIF  
!
!
!------------------------------------
!     output-filename and open output
!
     LINE=LINES(NLI)
     IS = INDEX(LINE,' ')  
     VTURB1 = VTURB*1.D-5
     IF(RESOL.LT.10.) THEN
       WRITE(SPEC1,FMT='(I5.5,A1,I5.5,A2,F5.3)') INT(LAM_BLU),'_',INT(LAM_RED),'__',RESOL
     ELSE IF(RESOL.LT.100.) THEN
       WRITE(SPEC1,FMT='(I5.5,A1,I5.5,A1,F6.3)') INT(LAM_BLU),'_',INT(LAM_RED),'_',RESOL
     ELSE
       STOP' RESOL TOO LARGE, CHANGE FORMAT'
     ENDIF  
     
     IF (VTURB1.EQ.0.) THEN  
          IF (ESCAT_UV) THEN  
               SPEC = 'ESC'  
               OPEN (2,FILE=TRIM(FILE)//'/OUT.'//LINE(1:IS-1)//'_'// &
&                SPEC1//'_'//SPEC, STATUS='UNKNOWN')
          ELSE  
               SPEC = ' '  
               OPEN (2,FILE=TRIM(FILE)//'/OUT.'//LINE(1:IS-1)//'_'//SPEC1, &
&                STATUS= 'UNKNOWN')
          END IF  
     ELSE  
          ITURB = INT(VTURB1)  
          IF (ITURB.GE.1000) STOP ' VTURB > 1000 KM/S'  
          I1 = ITURB/100  
          I2 = (ITURB-I1*100)/10  
          I3 = ITURB - I1*100 - I2*10  

          IF (ESCAT_UV) THEN  
               SPEC = 'ESC_VT'//CHAR(48+I1)//CHAR(48+I2)//CHAR(48+I3)
          ELSE  
               SPEC = 'VT'//CHAR(48+I1)//CHAR(48+I2)//CHAR(48+I3)  
          END IF  

          OPEN (2,FILE=TRIM(FILE)//'/OUT.'//LINE(1:IS-1)//'_'//SPEC1//'_'//SPEC, &
&           STATUS='UNKNOWN')
       END IF

     WRITE (*,FMT=9000) LINE  
     PRINT*,' SPECTRAL RANGE:',LAM_BLU,' ',LAM_RED
     PRINT*,' RESOL = ',RESOL,' ANGSTROM' 
     PRINT *  
     PRINT *,'VSINI: ',VSINI,' KM/S, VTURB: ',VTURB*1.D-5,' KM/S'  

!----mass dependent part of VDOP (rest is calculated in subroutine model)
     VDOP(NB)=0.
     CALL MODEL(R1,R,V1,V,DVDR1,RHO,RMAX,NDM,ND,VMAX,VDOP,INDEX1, &
      SRVMAX,SR,NB,XNE,CLF,CLF_TEST,VTMI,VTMA)
!on output, vdop(nb)=vdop(1)=vturb/vinf

     
     IF(FIRST) THEN
! call prince only once! (2nd call will not work)
       CALL PRINCE
       FIRST=.FALSE.
     ENDIF  
     
     CALL CALC_LINES(ND1,XNE,TEMP1,CLF,R1,V1,DVDR1,SR,SRVMAX, &
&                    LAM_BLU,LAM_RED,IBLU,IRED, &
&                    VTURB,YHE,BALMER_LEMKE,MAXNCOMP)

     CALL FORMAL_RANGE(ND,NP,NC,VDOP(NB),RMAX,R,P,Z,V,LTOT,LMAX, &
 &                     R1,V1,XNE,TEMP1,INDEX1,IBLU,IRED, &
 &                     LAM_BLU,LAM_RED,RESOL,SR,VMAX, &
 &                     OPACON,SCONT, &
 &                     ZRAY,OPACRAY(1,1),SCONRAY(1,1),OPALRAY(1,1),SRAY(1,1))
     
     CLOSE(2)

     DEALLOCATE(VDOP,OPALRAY,SRAY)
     CALL DEALLOC
!     PRINT*,' ALL ARRAYS DEALLOCATED'
     PRINT*
     
  ENDIF

END DO ALL_LINES  

CALL CPU_TIME(TIME_CPU) 
PRINT*
PRINT* , ' CPU time: ',TIME_CPU 

STOP  
!
!     error in input unit exit
!
  110 CONTINUE  
WRITE (*,FMT='(A)') ' ERROR IN INPUT UNIT NUMBER '  
STOP  
!
!       error conditions
120 SELECT CASE(RET)
CASE ('RET1') !       end of file "no esperado" exit  
  WRITE (*,FMT='(A)') ' EOF NO ESPERADO. ALGO FALLA! '  
  STOP
CASE ('RET2') !       error in ffr subroutines exit  
  WRITE (*,FMT='(A)') ' ERROR IN FFR SUBROUTINES '  
  STOP
CASE DEFAULT
  STOP ' WRONG ERROR CONDITION IN FORMALSOL'
END SELECT

 9000 FORMAT ('LINE: ',A)  
 9010 FORMAT (' DELTAX =',F7.2,'  DELTA =',F7.2,' WELL SEPARATED')  
 9020 FORMAT (' DELTAX =',F7.2,'  DELTA =',F7.2,' OVERLAP !!')  
 9030 FORMAT (' SINGLE WITH XMAX =',F7.4)  
END
!
!***********************************************************************
!
! subroutines: simple ones
!
!***********************************************************************
!
SUBROUTINE CONTIN(XNUE,NL,NU,GFLU,SRVMAX,INDEX1,R,OPAL,OPACON, &
&                  SLINE,SCONT,GL,GU,NB,SR,XCRED,XCBLUE,TEM,TGRAD, &
&                  XMAX,XMAXDOP,XMAXDOP_MIN,TEMP,XNE,XNEFI,CLF,ESCAT)
  
!
! includes clumping
! NOTE: continuum opacities already corrected
!       line quantities need to be corrected (assumption: resonance zone
!       large compared to clumps)
! 
! sline and opal are now calculated from interpolated nl, nu values
! 
! approximate treatment if AT = .TRUE.
! used departures for AT-case are controlled by OPTDEP
!      OPTDEP=.FALSE -> DEP = 1, LTE
!      OPTDEP=.TRUE  -> DEP_LOW = DEP(19), DEP_UP = DEP(20)
!                                    to obtain similar ratios.
!
USE nlte_type
USE nlte_dim
USE fund_const, ONLY: CLIGHT
USE formalsol_var, ONLY: GAMMAL,GAMMAC,XCMFE,SCONTE,NFCMF,VTURB,FILE,INVERTED, &
&                        FIC,TCL_FAC_LINE,TCL_FAC_CONT,OPTTHICK

USE preformal_var, ONLY: AT,NLEVH21
IMPLICIT NONE
!
!---- reads and calculates continuum quantities
!
!     .. parameters ..
!
LOGICAL, PARAMETER :: OPTDEP=.false.
!
INTEGER(I4B), PARAMETER :: ND=ID_NDEPT  
INTEGER(I4B), PARAMETER :: IFRETOT=ID_FREC1  
INTEGER(I4B), PARAMETER :: ND1=ID_DEPFI,NFMAX=ID_NFESC  

REAL(DP), PARAMETER :: C1 = 0.02654D0, C2 = 3.97285D-16  

!     ..
!     .. scalar arguments ..
REAL(DP) ::  SR,SRVMAX,TEM,TGRAD,XCBLUE,XCRED  
INTEGER(I4B) ::  NB  
LOGICAL ESCAT  
!     ..
!     .. array arguments ..
REAL(DP) ::  GFLU(NB),GL(NB),GU(NB),OPACON(ND1,2),OPAL(ND1,NB), &
&                 R(ND1),SCONT(ND1,2),SLINE(ND1,NB),TEMP(ND1), &
&                 XMAX(NB),XMAXDOP(NB),XMAXDOP_MIN(NB), &
&                 XNE(ND),XNEFI(ND1),CLF(ND),XNUE(NB)

INTEGER(I4B) ::  INDEX1(ND),NL(NB),NU(NB)  
!     ..
!     .. local scalars ..
REAL(DP) ::  AUX,DUMMY,FACTOR,OC,OC1,OC2,OL,OL1, &
&                 OL2,OLE,OOPA,RLJ,RLJ1,RLJ2,SC,SC1,SC2,SL,SL1,SL2, &
&                 SLE,TL,TL1,TL2,XKL,XL,XL1,XL2,XNL,XNU,XXX, &
&                 OOPA1,TAUCON,DEPL,DEPU

REAL(DP) :: TCL,FAC2,EFF_RAT_INT

INTEGER(I4B) ::  I,IK,J,J1,J2,K,K0,KI, &
&                L,LI,NFRE,NREC,NRECET,NX,ITAUC1,IFRE
!     ..
!     .. local arrays ..
REAL(DP) ::  BLEVEL(ID_LLEVS),ALEVEL(ID_LLEVS), &
&            FREQC(IFRETOT),OPAC(ND,IFRETOT), &
&            SCON(ND,IFRETOT),T(ND), &
&            TAUROSS(ND),XNELTE(ND)

REAL(DP), DIMENSION(ND,NB) :: EFF_RAT 
REAL(DP), DIMENSION(ND,IFRETOT) :: OPA_EFF_RAT

INTEGER(I4B), ALLOCATABLE, DIMENSION(:) :: DEPTHL
REAL(DP), ALLOCATABLE, DIMENSION(:) :: XMAXL, XLAMB
REAL(DP), ALLOCATABLE, DIMENSION(:,:) :: XNLL, XNUL

!     ..
!     .. external subroutines ..
EXTERNAL CALCXMAX  
!     ..
!     .. intrinsic functions ..
INTRINSIC ABS,EXP,LOG,SIGN  
!     ..
!cw----xmaxl: maximal 'linestrength'
!cw----depthl: depth of maximal 'linestrength'
!
!consistency check for approximate treatment (AT)
IF (.NOT.AT .AND. NLEVH21.NE.0) STOP' NOT AT AND NLEVH21 NE 0'
IF (AT) THEN
   IF (NLEVH21.EQ.0) STOP' AT AND NLEVH21 EQ 0'
   IF (NL(1).NE.ID_LLEVS+1) STOP' AT: ERROR IN NL'
   IF (NU(1).NE.ID_LLEVS+2) STOP' AT: ERROR IN NU'
   IF (NB.NE.1) STOP' AT: ERROR IN NB'
ENDIF

ALLOCATE(DEPTHL(NB),XMAXL(NB),XLAMB(NB))
ALLOCATE(XNLL(ND,NB),XNUL(ND,NB))

NREC = ID_LLEVS  
NRECET = 8*NREC  

! now including OPA_EFF_RAT
OPEN (1,FILE=TRIM(FILE)//'/CONT_FORMAL',STATUS='OLD',FORM='UNFORMATTED')  
REWIND 1  
READ (1) NFRE, (FREQC(I),I=1,IFRETOT), &
&  ((SCON(I,J),I=1,ND),J=1,IFRETOT), &
&  ((OPAC(I,J),I=1,ND),J=1,IFRETOT), (T(I),I=1,ND), &
&  ((OPA_EFF_RAT(I,J),I=1,ND),J=1,IFRETOT)
CLOSE (1)  

! for tests, when individual lines are compared with those from the
! complete approach and should be identical
!OPEN (1,FILE=TRIM(FILE)//'/CONT_FORMAL_CMF',STATUS='OLD',FORM='UNFORMATTED')  
!REWIND 1  
!READ (1) ((SCON(I,J),I=1,ND),J=1,IFRETOT), &
!&  ((OPAC(I,J),I=1,ND),J=1,IFRETOT)
!CLOSE (1)  

!CHECK CONSISTENCY
DO I=1,IFRETOT
  IF(FREQC(I).EQ.0.) EXIT
ENDDO  
IFRE=I

!do i=1,nd
!  do j=1,ifretot
!    print*,i,j,opa_eff_rat(i,j)
!  enddo
!enddo  

IF(.NOT.OPTTHICK.AND.MAXVAL(ABS(OPA_EFF_RAT(1:ND,1:IFRE)-1.D0)).GT.1.D-6) &
  STOP' OPTICALLY THIN CLUMPING AND OPA_EFF_RAT NE 1, subr. contin!!!'

TEM = T(ND)  

IF(AT) THEN
OPEN (1,FILE=TRIM(FILE)//'/TAU_ROS',STATUS='OLD',FORM='FORMATTED')  
REWIND 1  
DO L = 1,ND  
     READ (1,FMT=*) TAUROSS(L),XNELTE(L)  
END DO  
CLOSE (1)
ENDIF


OPEN (1,FILE=TRIM(FILE)//'/NLTE_POP',STATUS='OLD',ACCESS='DIRECT',RECL=NRECET)
OPEN (11,FILE=TRIM(FILE)//'/LTE_POP',STATUS='OLD',ACCESS='DIRECT',RECL=NRECET)
!
!---- xnue in cm-1
!---- it is assumed without any check that xnue(1) > xnue(2) (i.e.,
!---- xnue(1) corresponds to the blue component)
!
IF (XNUE(1).GT.FREQC(NFRE) .OR. XNUE(1).LT.FREQC(1)) &
& STOP 'FREQ. NOT CALCULATED IN CONTINUUM TRANSFER'

K0 = NFRE  
XXX = FREQC(NFRE)  

DO WHILE (XNUE(1).LT.XXX)  
   K0=K0-1  
   XXX=FREQC(K0)  
END DO  
!
!---- calculation of cmf-frequencies for continuum interpolation
!---- (they corresponds to the limits of the subinterval defined
!---- by k0 and k0+1)
!
XCRED  = (FREQC(K0)-XNUE(1))*CLIGHT*SRVMAX/SR/XNUE(1)  
XCBLUE = (FREQC(K0+1)-XNUE(1))*CLIGHT*SRVMAX/SR/XNUE(1)  
!
!---- lambda in cm
!
DO I = 1,NB  
  XLAMB(I) = 1.D0/XNUE(I)
END DO  

DO I=1,NB
     XMAXL(I) = 0.D0
END DO

INVERTED=0

TAUCON=0.
ITAUC1=0

EFF_RAT = 0.

DO I = 1,ND  

     READ (1,REC=I) (BLEVEL(J),J=1,NREC)  
     READ (11,REC=I) (ALEVEL(J),J=1,NREC)  
! recheck
     SCONT(INDEX1(I),1) = SCON(I,K0)  
     SCONT(INDEX1(I),2) = SCON(I,K0+1)  
     OPAC(I,K0) = OPAC(I,K0) / OPA_EFF_RAT(I,K0)
     OPAC(I,K0+1) = OPAC(I,K0+1) / OPA_EFF_RAT(I,K0+1)      
     OPACON(INDEX1(I),1) = OPAC(I,K0)*SR
     OPACON(INDEX1(I),2) = OPAC(I,K0+1)*SR  
! OPAC IS ALREADY EFFECTIVE QUANTITY, so needs to be back-corrected.
! Then follow philosophy  of correcting with largest TCL 
! -- either line or from blended line+cont BG
! (i.e. opac is corrected again below) 

     
! ASSUMING RED OPACITY IS HIGHER     
!     IF(I.LT.ND.AND.ITAUC1.EQ.0) THEN
!     TAUCON=TAUCON+ &
!&      .5*(OPACON(INDEX1(I),1)+OPAC(I+1,K0)*SR)* &
!&      (R(INDEX1(I))-R(INDEX1(I+1)))
!     IF(TAUCON.GE.0.66) ITAUC1=I
!     ENDIF
! Do this calculation AFTER it's been decided which TCL reduction to use 
     
     DO IK = 1,NB  
      
          IF(.NOT.AT) THEN
! standard treatment
            XNL = BLEVEL(NL(IK))
            IF(XNL.EQ.0.) XNL = 1.D-15*ALEVEL(NL(IK))
     
            XNU = BLEVEL(NU(IK))  
            IF(XNU.EQ.0.) XNU = 1.D-15*ALEVEL(NU(IK))
          ELSE
! approximate treatment
            IF(OPTDEP) THEN
              AUX=ALEVEL(NLEVH21)/BLEVEL(NLEVH21)*XNELTE(I)/XNE(I)
!             LAST TWO DEPARTURES, CORRESPONDING TO H119 and H120 in A10HHe
              DEPL = BLEVEL(NLEVH21-2)/ALEVEL(NLEVH21-2) * AUX              
              DEPU = BLEVEL(NLEVH21-1)/ALEVEL(NLEVH21-1) * AUX
            ENDIF
            CALL OCCUP_AT(NL(IK),NU(IK),XNE(I),T(I),BLEVEL(NLEVH21),XNL,XNU)
            IF(OPTDEP) THEN
              XNL=XNL*DEPL
              XNU=XNU*DEPU
            ENDIF
          ENDIF

          XNLL(I,IK)=XNL
          XNUL(I,IK)=XNU

          XKL = C1*GFLU(IK)* (XNL/GL(IK)-XNU/GU(IK))  
          SLINE(INDEX1(I),IK) = C2* (XNUE(IK)**3)/(XNL/XNU*GU(IK)/GL(IK)-1.D0)
!CAN BE MODIFIED FOR TESTS
!Note: in most cases, this should give a very similar result to the "exact"
! approach (as long as tau_inv small, i.e., a dominating source term)
! IN CASE, DON'T FORGET TO INSERT THE ABS BELOW (FOR INTERPOLATED VALUES!
!          XKL = ABS(C1*GFLU(IK)* (XNL/GL(IK)-XNU/GU(IK)) ) 
!          SLINE(INDEX1(I),IK) = ABS(C2* (XNUE(IK)**3)/(XNL/XNU*GU(IK)/GL(IK)-1.D0))

          IF (XKL.LT.0.) THEN
! that was the old formulation; new one allows for a better (though not
! perfect) treatment of inverted levels
!            XKL = C1*GFLU(IK)* XNL/GL(IK)  
!            SLINE(INDEX1(I),IK)=0.
            INVERTED(I,IK)=1
          ENDIF  
! here comes the correction (only for lines!)
          XKL=XKL/CLF(I)
          OPAL(INDEX1(I),IK) = XKL*SR  
! correction for thick clumping + consistent inversion
          TCL = ABS(XKL)*XLAMB(IK)*TCL_FAC_LINE(I)
! Note: SRVMAX term included in tcl_fac_line multiplier 
          FAC2 = (1.+FIC(I)*TCL)/(1.+TCL)
          EFF_RAT(I,IK) = MIN (FAC2, OPA_EFF_RAT(I,K0), OPA_EFF_RAT(I,K0+1) )
          IF (EFF_RAT(I,IK).LE.0.0D0.OR.EFF_RAT(I,IK).GT.1.0D0) THEN 
            PRINT*,EFF_RAT(I,IK)
            PRINT*,TCL,FAC2,I,IK
            STOP' OPA_EFF > <OPA>, CONTIN'
          ENDIF
          OPAL(INDEX1(I),IK) = OPAL(INDEX1(I),IK) * EFF_RAT(I,IK)
! Now effective line opacity, reduce continuum with *same* factor 
          OPAC(I,K0) = OPAC(I,K0) * EFF_RAT(I,IK)
          OPAC(I,K0+1) = OPAC(I,K0+1) * EFF_RAT(I,IK) 
          OPACON(INDEX1(I),1) = OPAC(I,K0)*SR
          OPACON(INDEX1(I),2) = OPAC(I,K0+1)*SR

          OOPA = ABS(XKL*EFF_RAT(I,IK))/OPAC(I,K0)  
!
!cw----search for maximum linestrength
!
          IF(I .GE. 1) THEN
              IF(OOPA .GT. XMAXL(IK)) THEN
                XMAXL(IK) = OOPA
                DEPTHL(IK) = I
              END IF
          END IF

     END DO

END DO

! Need to calculate TAUCON scale here, since might have been modified,
! and OPAC(I+1) only known after end of previous I loop 
! ASSUMING RED OPACITY IS HIGHER     
DO I=1,ND
     IF(I.LT.ND.AND.ITAUC1.EQ.0) THEN
        IF(OPACON(INDEX1(I+1),1).NE.OPAC(I+1,K0)*SR) &
          STOP' ERROR IN OPACON/OPAC -- subr. CONTIN'
        TAUCON=TAUCON+ &
&       .5*(OPACON(INDEX1(I),1)+OPAC(I+1,K0)*SR)* &
&       (R(INDEX1(I))-R(INDEX1(I+1)))
        IF(TAUCON.GE.0.66) ITAUC1=I
     ENDIF
ENDDO
     
CLOSE (1)  
CLOSE (11)  
!
IF(ITAUC1.EQ.0) STOP' TAUC = 0.66 NOT FOUND'
PRINT* 
PRINT*,' TAUCON = ',TAUCON,' AT LOG NE = ',LOG10(XNE(ITAUC1))
!
!cw----calculation of xmax at depth of maximal linestrength
!
DO IK=1,NB

     OOPA1=ABS(OPAL(INDEX1(ITAUC1),IK))/OPACON(INDEX1(ITAUC1),1)  
! both opacities (line and cont) corrected
     
     PRINT *
     PRINT *, "DEPTHL =", DEPTHL(IK)
     PRINT *, "LOG(XMAXL)  =", LOG10(XMAXL(IK))
! calcxmax remains unaffected, since we assume that profile is
! formed inside clumps, i.e., at increased densities 
     CALL CALCXMAX(XMAX(IK),XMAXDOP(IK),XMAXDOP_MIN(IK),IK,T(DEPTHL(IK)),&
&      XMAXL(IK),XLAMB(IK),XNE(DEPTHL(IK)),GAMMAL(IK),GAMMAC(IK), &
&      OOPA1,T(ITAUC1),XNE(ITAUC1))
     PRINT *, "XMAX  =", XMAX(IK)
END DO

DO IK=1,NB
  IF (XMAX(IK).EQ.0.D0) STOP 'ERROR IN XMAX - CONTIN'
ENDDO
!
!---- interpolation (linear in log-log) in the micro-grid
!-----interpolate *linearly* in EFF_RAT, to avoid certain problems 
!
TEMP(1) = T(1)  
XNEFI(1) = XNE(1)  
ILOOP: DO I = 2,ND  
     J1 = INDEX1(I-1)  
     J2 = INDEX1(I)  
     TEMP(J2) = T(I)  
     XNEFI(J2) = XNE(I)
     
JLOOP: DO J = J1 + 1,J2 - 1  
          RLJ = LOG(R(J))  
          RLJ1 = LOG(R(J1))  
          RLJ2 = LOG(R(J2))  
          FACTOR = (RLJ-RLJ1)/ (RLJ2-RLJ1)  
          TL1 = LOG(TEMP(J1))  
          TL2 = LOG(TEMP(J2))  
          TL = TL1 + FACTOR* (TL2-TL1)  
          TEMP(J) = EXP(TL)  
          XL1 = LOG(XNEFI(J1))  
          XL2 = LOG(XNEFI(J2))  
          XL = XL1 + FACTOR* (XL2-XL1)  
          XNEFI(J) = EXP(XL)  
          DO K = 1,2  
               SC1 = LOG(SCONT(J1,K))  
               SC2 = LOG(SCONT(J2,K))  
               SC = SC1 + FACTOR* (SC2-SC1)  
               SCONT(J,K) = EXP(SC)  
               OC1 = LOG(OPACON(J1,K))  
               OC2 = LOG(OPACON(J2,K))  
               OC = OC1 + FACTOR* (OC2-OC1)  
               OPACON(J,K) = EXP(OC)  
          END DO

          DO K = 1,NB  
! linear in EFF_RAT_INT
               RLJ = R(J) 
               RLJ1 = R(J1) 
               RLJ2 = R(J2)
               FAC2 = (RLJ-RLJ1)/ (RLJ2-RLJ1)  
               OL1 = EFF_RAT(I-1,K)
               OL2 = EFF_RAT(I,K)
               EFF_RAT_INT = OL1 + FAC2* (OL2-OL1)  

               OL1 = LOG(XNLL(I-1,K)/CLF(I-1))  
               OL2 = LOG(XNLL(I,K)/CLF(I))  
               OL =  OL1 + FACTOR* (OL2-OL1)  
               XNL = EXP(OL)  
               OL1 = LOG(XNUL(I-1,K)/CLF(I-1))  
               OL2 = LOG(XNUL(I,K)/CLF(I))  
               OL = OL1 + FACTOR* (OL2-OL1)  
               XNU = EXP(OL)  
               OPAL(J,K)  = C1*GFLU(K)* (XNL/GL(K)-XNU/GU(K))*SR * EFF_RAT_INT
! Now effective 
               IF(OPAL(J,K).EQ.0.) STOP' OPAL = 0 IN INTERPOLATION; CHANGE SCHEME'
               SLINE(J,K) = C2* (XNUE(K)**3)/(XNL/XNU*GU(K)/GL(K)-1.D0)
! IN CASE (IF ABS HAS BEEN USED ABOVE
!               OPAL(J,K)  = ABS(C1*GFLU(K)* (XNL/GL(K)-XNU/GU(K))*SR)  
!               SLINE(J,K) = ABS(C2* (XNUE(K)**3)/(XNL/XNU*GU(K)/GL(K)-1.D0))
               IF (SLINE(J,K)*OPAL(J,K).LT.0.) THEN
                 PRINT*,R(J),' ',J,' ',K,' OPAL*SL < 0!'
                 STOP' OPAL*SL < 0!'
               ENDIF  
          END DO
!          WRITE(*,1001) J1,J2,J,OPAL(J1,1),OPAL(J,1),OPAL(J2,1)
!          WRITE(*,1001)J1,J2,J,SLINE(J1,1),SLINE(J,1),SLINE(J2,1)
1001 FORMAT(3(I4,2X),3(E9.3,2X))          
     END DO JLOOP     

END DO ILOOP

TGRAD = ABS(T(ND)-T(ND-1))/ (R(INDEX1(ND-1))-R(INDEX1(ND)))  

DEALLOCATE(DEPTHL,XMAXL,XLAMB)
DEALLOCATE(XNLL,XNUL)

IF (.NOT.ESCAT) RETURN  
!
!     interpolation of scont (incl. non-coherent el.scat term)
!     on micro grid
!
!!    open(1,file=TRIM(FILE)//'/exi_'//line,status='unknown',form='formatted')
!
REWIND 17  
READ (17,FMT=*) NX,NFCMF  

IF (NX.NE.ND) STOP 'NX .NE. ND'  
IF (NFCMF.GT.NFMAX) STOP 'NFCMF > NFMAX'  

DO K = 1,NFCMF  
     READ (17,FMT=*) KI,XCMFE(K),DUMMY  
END DO  

DO K = 1,NFCMF  
     DO L = 1,ND  
          READ (17,FMT=*) KI,LI,DUMMY,SCONTE(INDEX1(L),K),DUMMY  
     END DO
END DO

CLOSE (17)  

DO I = 2,ND  
     J1 = INDEX1(I-1)  
     J2 = INDEX1(I)  
     DO J = J1 + 1,J2 - 1  
          RLJ = LOG(R(J))  
          RLJ1 = LOG(R(J1))  
          RLJ2 = LOG(R(J2))  
          FACTOR = (RLJ-RLJ1)/ (RLJ2-RLJ1)  
          DO K = 1,NFCMF  
               SC1 = LOG(SCONTE(J1,K))  
               SC2 = LOG(SCONTE(J2,K))  
               SC = SC1 + FACTOR* (SC2-SC1)  
               SCONTE(J,K) = EXP(SC)  
          END DO
     END DO
END DO  

RETURN  

END
!
!-----------------------------------------------------------------------
!
SUBROUTINE CALCXMAX(XMAX,XMAXDOP,XMAXDOP_MIN, &
&                   IK,T,OPA,XLAMBDA,XNE,GAMMAL,GAMMAC, &
&                   OPA1,TETAUC1,XNETAUC1)  
!
USE nlte_type
USE nlte_dim
USE fund_const, ONLY: AKB,XMH=>AMH,PI,CLIGHT
USE formalsol_var, ONLY: NSTARK,WEIG,VMAX,VTURB,VTURBMIN,VTURBMAX
IMPLICIT NONE
!
!---- calculates xmax for component 'ik'
!---- now with vturbmax for Doppler;
!---- for STARK and Voigt, calculate with vturb=vturbmin (photosphere),
!-----and then take maximum (xmax, xmaxdop) except for const. vturb
!
!     .. parameters ..
REAL(DP), PARAMETER :: WPI1=0.56418958D0  
!     ..
!     .. scalar arguments ..
REAL(DP) ::  GAMMAC,GAMMAL,OPA,T,XLAMBDA,XMAX,XMAXDOP,XMAXDOP_MIN, &
&            XNE,OPA1,TETAUC1,XNETAUC1  
INTEGER(I4B) ::  IK  
!     ..
!     .. local scalars ..
REAL(DP) ::  AUX,AVOIGT,EPS,VTHER,XKL,PHII,XI,XIMIN  
LOGICAL FLAG
!     ..
!     ..
!     .. external functions ..
REAL(DP) ::  PERFIL1  
!     ..

IF(VTURB.NE.VTURBMIN) STOP' VTURB NE VTURBMIN in CALCXMAX'

! minimum for xmax according to dlam = 10. A or 0.3 vinf 
!
XIMIN=10.D-8/XLAMBDA*CLIGHT/VMAX
XIMIN=MAX(XIMIN,0.3D0)

!new: calculated XMAXDOP_MIN as before, with VTURB=VTURBMIN,
!to be used in core region of frequency grid
VTHER = SQRT(2.D0*AKB*T/XMH/WEIG(IK)+VTURB**2)  
!
!------- doppler profile(overall or in wind)
!
XKL = OPA*XLAMBDA/VTHER  
IF (XKL.GT.1.D0) THEN  
!
!##----  modifiied by jp, assumimg line profile 1/100 at xmax
!
     AUX = XKL*WPI1*100.D0  
     AUX = LOG(AUX)  
     XMAXDOP_MIN = VTHER/VMAX*SQRT(AUX)  
ELSE  
     XMAXDOP_MIN = MAX(1.D-3,3.D0*VTHER/VMAX)  
END IF  

!new
!NOW, for XMAX, use VTURBMAX instead of VTURB;
!remains identical to old version for constant vturb,
!since then VTURBMAX=VTURBMIN=VTURB
VTHER = SQRT(2.D0*AKB*T/XMH/WEIG(IK)+VTURBMAX**2)  
!
!------- doppler profile(overall or in wind)
!
XKL = OPA*XLAMBDA/VTHER  
IF (XKL.GT.1.D0) THEN  
!
!##----  modifiied by jp, assumimg line profile 1/100 at xmax
!
     AUX = XKL*WPI1*100.D0  
     AUX = LOG(AUX)  
     XMAXDOP = VTHER/VMAX*SQRT(AUX)  
ELSE  
     XMAXDOP = MAX(1.D-3,3.D0*VTHER/VMAX)  
END IF  

IF (NSTARK(IK).EQ.0) THEN  
     XMAX = XMAXDOP  
ELSE IF(NSTARK(IK) .EQ. 1) THEN
!
! stark profile: find x where stark profile is below threshold
! note that doppler width is included into perfil
! 
! THUS: opal*perfil(xmax) le eps*opac gives desired xmax 
!
! first for tauc = 0.66
!
EPS=5.d-3 !previous version 1.d-2
!
!AUX=EPS/OPA1 !old version
! in the new version, we use the maximum of the line-strength, to be on the
! safe side (together with the conditions at tauc=0.66)
AUX=EPS/OPA

XI=0.05D0
PHII=PERFIL1(IK,XI,TETAUC1,XNETAUC1,VTURB,FLAG)

DO WHILE(PHII.GT.AUX)
  XI=XI+0.05D0
  PHII=PERFIL1(IK,XI,TETAUC1,XNETAUC1,VTURB,FLAG)
ENDDO

XMAX=MAX(XI,XIMIN)
!JO: might need to be changed
IF(VTURBMAX.GT.VTURBMIN) XMAX=MAX(XMAX,XMAXDOP)
ELSE
!
!cw------ voigt profil
!
     IF(NSTARK(IK).NE.2.AND.NSTARK(IK).NE.3) STOP ' ERROR IN NSTARK' 
     AVOIGT=(GAMMAL+XNE*GAMMAC)*XLAMBDA/VTHER
     AUX=XKL*100.*AVOIGT/PI
     XMAX=VTHER/VMAX*SQRT(AUX)
         
     XMAX=MAX(XMAX,XIMIN)
!JO: might need to be changed
     IF(VTURBMAX.GT.VTURBMIN) XMAX=MAX(XMAX,XMAXDOP)
END IF  

RETURN  
END
!
!-----------------------------------------------------------------------
!
SUBROUTINE MODEL(R,RPRIM,V,VPRIM,DVDR,RHOPRI,RMAX,ND1,ND2,VMAX,VDOP,INDEX1, &
                 SRVMAX,SR,NB,XNE,CLF,CLF_TEST,VTMI,VTMA)
!!
! enhancement factor clf now read from 'model'
! NOTE: ne, nh and occupation numbers include clf, rho NOT
USE nlte_type
USE nlte_dim
USE fund_const, ONLY: AKB,XMH=>AMH
USE formalsol_var, ONLY: VTURB,FILE,VTURBMIN,VTURBMAX,VTURBV

IMPLICIT NONE
!
!---- enrique santolaya, 23 de junio de 1993 (noche de san juan)
!
!---- modified (5-12-94): no hamman velocity law is used. instead of
!---- that, the velocity is linearly interpolated in the radial grid to
!---- obtain the micro-grid.
!---- any velocity law can then be used (e. santolaya).
!
!---- file 'model' is read. it contain all information about stellar
!---- parameters
!##   xne only approximative in 'model', actual value from 'enion'
!##   has to be used
!
!     .. parameters ..
INTEGER(I4B), PARAMETER :: ND=ID_NDEPT,NDM=ID_DEPFI  
INTEGER(I4B), PARAMETER :: KEL=ID_ATOMS,KIS=ID_KISAT  
REAL(DP), PARAMETER :: RSUN=6.96D10  
!     ..
!     .. scalar arguments ..
REAL(DP) ::  RMAX,SR,SRVMAX,VMAX,VTMI,VTMA  
INTEGER(I4B) ::  NB,ND1,ND2  
!     ..
!     .. array arguments ..
REAL(DP) ::  R(ND),RHOPRI(NDM),RPRIM(NDM),VDOP(NB),VPRIM(NDM), &
&                 XNE(ND),CLF(ND),V(ND),DVDR(ND)
REAL(DP), DIMENSION(ND) :: CLF_TEST(ND)

INTEGER(I4B) ::  INDEX1(ND)  
!     ..
!     .. local scalars ..
REAL(DP) ::  A,AUX,BETA,CTECEQ,DEL,DVMIN,DVRHO,GGRAV, &
&                 VMIN,VVDOP,VD,X,XMLOSS,XMU,YHE,DUM1,TEFF
INTEGER(I4B) ::  I,J,K,NN,NRHO,NV  
!     ..
!     .. local arrays ..
REAL(DP) ::  ENIONND(KEL,KIS+1,ND),RHO(ND),XNH(ND)

!     ..
!     .. intrinsic functions ..
INTRINSIC DBLE,INT,LOG10,MAX,MIN,SQRT  
!     ..

OPEN (1,FILE=TRIM(FILE)//'/MODEL',STATUS='OLD',FORM='UNFORMATTED')  
REWIND 1  
READ (1) TEFF,GGRAV,SR,YHE,XMU,VMAX,XMLOSS,BETA, (R(I),I=1,ND), &
&  (V(I),I=1,ND), (DVDR(I),I=1,ND), (RHO(I),I=1,ND), &
&  (XNE(I),I=1,ND), (XNH(I),I=1,ND), (CLF(I),I=1,ND), (INDEX1(I),I=1,ND)
CLOSE (1)  

OPEN (1,FILE=TRIM(FILE)//'/ENION',STATUS='OLD',FORM='UNFORMATTED')  
READ (1) ENIONND,XNE  
CLOSE (1)  

!test consistency
DO I=1,ND 
! lower precision in CLUMPING_OUTPUT   
   IF (ABS(CLF_TEST(I)-CLF(I)).GT.1.D-3) STOP' Error in in clumping params!, model_1' 
ENDDO

RMAX = R(1)  
!
!---- calculation of doppler width(s) using VTURBMIN
!
VTURBMIN=VTMI*1.D5 
IF (VTURBMIN.NE.VTURB) STOP' SOMETHING WRONG WITH VTURB -- SUBR. MODEL'
IF (VTMA.LE.1.D0) THEN
  VTURBMAX=VTMA*VMAX
ELSE 
  VTURBMAX=VTMA*1.D5
ENDIF

IF(VTURBMAX.LT.VTURBMIN) STOP ' VTURBMAX < VTURBMIN, NOT ALLOWED!!'  

PRINT*,' MIN(VTURB) = ',VTURBMIN/1.D5,' MAX(VTURB) = ',VTURBMAX/1.D5
PRINT*

VVDOP = 1.D50  
A = SQRT(2.D0*AKB*TEFF/XMH)  
DO I = 1,NB  
     VD = A*VDOP(I)  
     VDOP(I) = SQRT(VD**2+VTURB**2)/VMAX  ! using vturb=vturbmin
     VVDOP = MIN(VVDOP,VDOP(I))  
END DO  

SRVMAX = SR/VMAX  

VMIN = V(ND)*VMAX  

DVMIN = MIN(VVDOP/5.D0,1.D-2)  
!
!-----interpolation on velocity values
!
!---- calculation of micro-grid using VTURBMIN (since min(dvmin 0.01 anyway)
!
DVRHO = 5.D-2  
RPRIM(1) = R(1)  
VPRIM(1) = V(1)  
RHOPRI(1) = RHO(1)  
CTECEQ = RHO(1)*V(1)*R(1)*R(1)  

J = 1  

DO I = 2,ND  
     NV = INT((V(I-1)-V(I))/DVMIN) + 1  
     NRHO = INT(LOG10(RHO(I)/RHO(I-1))/DVRHO) + 1  
     NN = MAX(NV,NRHO)  
     X = LOG10(R(I-1)/R(I))/DBLE(NN+1)  
     A = (V(I-1)-V(I))/ (R(I-1)-R(I))  

     DO K = 1,NN  
          J = J + 1  
          AUX = LOG10(R(I-1)) - DBLE(K)*X  
          RPRIM(J) = 10.D0**AUX  
          DEL = RPRIM(J) - R(I)  
          VPRIM(J) = V(I) + A*DEL  
          RHOPRI(J) = CTECEQ/VPRIM(J)/RPRIM(J)/RPRIM(J)  
     END DO

     J = J + 1  
     INDEX1(I) = J  
     RPRIM(J) = R(I)  
     VPRIM(J) = V(I)  
     RHOPRI(J) = RHO(I)  
END DO  

PRINT *  
PRINT *,'RSTAR= ',SR/RSUN,'    R-STAR/VMAX=',SRVMAX  
PRINT *  
WRITE (*,FMT=9000) XMLOSS,V(1)*VMAX/1.D5,V(ND)*VMAX/1.D5,RHO(ND)  
PRINT *  
ND2 = J  
PRINT *,'TOTAL NUMBER OF DEPTH POINTS=',ND2  

IF (ND2.GT.ND1) STOP 'TOO MANY DEPTH POINTS'  

! CALCULATION OF DEPTH DEPENDENT VTURB = VTURBV propto V(I) and VDOPV
ALLOCATE(VTURBV(ND2))

IF(VTURBMAX.NE.VTURBMIN) THEN
  DO I=1,ND2
    VTURBV(I) = MAX(VTURBMIN,VPRIM(I)*VTURBMAX)
!    PRINT*,I,VTURBV(I)/1.D5
  ENDDO
ELSE
  VTURBV=VTURBMIN
ENDIF

RETURN  

 9000 FORMAT ('MLOSS/YEAR=',E12.6,'   VMAX(RMAX)=',F7.1,/,'    VMIN=', &
&       F11.7,'    RHOPHO=',E13.6)

END
!
!-----------------------------------------------------------------------
!
SUBROUTINE STAREAD(FILE,NB,NSUM,NL,NU)  
!
USE nlte_type
USE nlte_dim
USE formalsol_var, ONLY: NSTARK,NLEVL,NLEVU,NWS,NTS,NES,QHALF, &
& DWS,TS,ES,PS

IMPLICIT NONE
!
!---- it reads and stores  stark profiles from binary file 'stark.bin'
!
!     .. parameters ..
INTEGER(I4B), PARAMETER :: MAXW=ID_MAXWW,MAXT=ID_MAXTT,MAXNE=ID_MAXNE, &
&          MAXPS=ID_MAXPS
!     ..
!     .. scalar arguments ..
INTEGER(I4B) ::  NB,NSUM  
!     ..
!     .. array arguments ..
INTEGER(I4B) ::  NL(NB),NU(NB)  
!     ..
!     .. local scalars ..
INTEGER(I4B) ::  I,J,NN  
!     ..
CHARACTER*60 :: FILE

OPEN (1,FILE=TRIM(FILE)//'/STARK.BIN',STATUS='OLD',FORM='UNFORMATTED')  
REWIND 1  

IF (NSUM.EQ.0. .OR. NSUM.GT.NB) STOP' STAREAD CALLED ERRONEOUSLY'  

DO I = 1,NB  

  IF(NSTARK(I).NE.1) CYCLE

  READ (1,END=20,ERR=30) NLEVL(I),NLEVU(I),QHALF(I), NWS(I), &
&   (DWS(J,I),J=1,NWS(I)),NTS(I), (TS(J,I),J=1,NTS(I)), NES(I), &
&   (ES(J,I),J=1,NES(I)),NN, (PS(J,I),J=1,NN)
!
!---- checking data
!
     IF (NWS(I).GT.MAXW) STOP 'TOO MANY FREQUENCIES'  
     IF (NTS(I).GT.MAXT) STOP 'TOO MANY TEMPERATURES'  
     IF (NES(I).GT.MAXNE) STOP 'TOO MANY ELECTRON DENSITIES'  
     IF (NN.GT.MAXPS) STOP 'TOO MANY POINTS IN PROFILE'  
     IF (NLEVL(I).NE.NL(I)) STOP 'LOWER LEVEL NOT CORRECT'  
     IF (NLEVU(I).NE.NU(I)) STOP 'UPPER LEVEL NOT CORRECT'  
END DO  

CLOSE (1)  

RETURN  
!
!---- end of file not expected
!
   20 CONTINUE  
PRINT *,'END OF FILE STARK.BIN NOT EXPECTED'  
STOP  
!
!---- error in reading
!
   30 CONTINUE  
PRINT *,'ERROR IN READING STARK.BIN'  
STOP  

END
!
!***********************************************************************
!
! subroutines: complex ones
! elscat and related
!
!***********************************************************************
!
SUBROUTINE ELSCAT(XNUE,NL,NU,GFLU,GL,GU,NB)  
!
USE nlte_type
USE nlte_dim
USE fund_const, ONLY: AKB, XMH=>AMH,CLIGHT, SIGMAE
USE formalsol_var, ONLY: WEIG,VMAX,XNEMIN,VTURB,FILE,OPTTHICK,FIC,TCL_FAC_LINE
IMPLICIT NONE
!
!     calculates consistent mean intensities for non-coherent electron
!     scattering on the macro grid. transfer in the cmf.
!     el. scattering treated as in rybicki and hummer, aa 290, 553.
!
!     CLUMPING INCLUDED (THIN AND THICK)
!
!     .. parameters ..
INTEGER(I4B), PARAMETER :: ND=ID_NDEPT,NP=ID_NPOIN  
INTEGER(I4B), PARAMETER :: KEL=ID_ATOMS,KIS=ID_KISAT  
INTEGER(I4B), PARAMETER :: IFRETOT=ID_FREC1,NFMAX=ID_NFESC  

REAL(DP), PARAMETER :: C1 = 0.02654D0, C2 = 3.97285D-16  
!     ..
!     .. scalar arguments ..
INTEGER(I4B) ::  NB  
!     ..
!     .. array arguments ..
REAL(DP) ::  GFLU(NB),GL(NB),GU(NB),XNUE(NB)  
INTEGER(I4B) ::  NL(NB),NU(NB)  
!     ..
!     .. local scalars ..
REAL(DP) ::  A1,A2,AIC,AUX,B1,B2,BETA,BETAT,CONST, &
&                 CORRFC,DBDR,DX,DXCOARSE,DXFINE, &
&                 ERR,GGRAV,QBLUE,QRED,REL,SCNEW,SCOLD,SR, &
&                 SRVMAX,TEFF,VTHEL,XCBLUE,XCRED, &
&                 XKL,XLAMBDA,XMLOSS,XMU,XNL,XNU,XNUE0, &
&                 XXX,YHE,Z2, & 
&                 THOMSON_1,THOMSON_2,THOMSON_11,THOMSON_21,STRUEK,STRUEK1, &
&                 LAMMIN,LAMMAX,VREF,LAM_CMF, &
&                 XMAX,XMIN,BORDER,FAC1,FAC2,XXDIFF,PHIKL, &
&                 PHIKL1,PHIKL2,PHIKL3,PHIKL4,DUM1,TCL

INTEGER(I4B) ::  I,IIT,IK,J,JP,K,K0,K1,L,LMAX,LZ,NC,NF,NF1,NF2,NF3,NF4, &
&        NFCMF,NFRE,NREC,NRECET,K01,IDV,INEXT,ILAST,IDX,IFRE
LOGICAL FLAG  
!     ..
!     .. local arrays ..
REAL(DP) ::       BLEVEL(ID_LLEVS),ALEVEL(ID_LLEVS),DVDR(ND), &
&                 ENIONND(KEL,KIS+1,ND),FREQC(IFRETOT), &
&                 P(NP),R(ND),Z(ND,NP), &
&                 V(ND),T(ND),XNE(ND),CLF(ND),DUM(ND), &
&                 OPAC(ND),OPACON(ND,IFRETOT), &
&                 ST(ND,IFRETOT),STRUE(ND), &
&                 THOMS(ND,IFRETOT),THOMSON(ND), &
&                 UBLUWI(ND,NP-1),UCMF(ND),VCMF(ND-1), &
&                 W0(ND,NP-1),W2(ND,NP-1), &
&                 XJALL(ND,IFRETOT),XJOLD(ND),XX(NFMAX), &
&                 OPA_EFF_RAT(ND,IFRETOT)

REAL(DP), ALLOCATABLE:: AICK(:),DBDRK(:),EOLD(:),X(:),XI(:), &
&                 FI(:),ETAC(:,:),AJ(:,:),E(:,:),XJNEW(:,:), &
&                 OPAL_TOT(:,:),ETAL_TOT(:,:)

REAL(DP), ALLOCATABLE :: OPAL(:,:),SLINE(:,:),XLAMB(:),XC(:)

!     ..
!     .. external functions ..
REAL(DP) ::  PERFIL1  
EXTERNAL PERFIL1  
!     ..
!     .. external subroutines ..
EXTERNAL CONT1,DIFFUS,RAY,SCATSOL  
!     ..
!     .. intrinsic functions ..
INTRINSIC ABS,DBLE,INT,LOG,MAX,MIN,MIN0,MOD,SQRT  
!     ..
!     .. data statements ..
!
DATA A1,A2/1.69070371729D0,-.69070371729D0/  
DATA B1,B2/1.614249968779D0,2.154326524957D0/  
!     ..

ALLOCATE(OPAL(ND,NB),SLINE(ND,NB),XLAMB(NB),XC(NB))

XNEMIN = 10.5D0  
XNEMIN = 10.D0**XNEMIN  

NREC = ID_LLEVS  
NRECET = 8*NREC  
!
!     this subroutine assumes in any case and without checking
!     that index1(i)=: i
!
OPEN (1,FILE=TRIM(FILE)//'/MODEL',STATUS='OLD',FORM='UNFORMATTED')  
REWIND 1  
READ (1) TEFF,GGRAV,SR,YHE,XMU,VMAX,XMLOSS,BETA, (R(I),I=1,ND), &
& (V(I),I=1,ND), (DVDR(I),I=1,ND), (DUM(I),I=1,ND), &
& (DUM(I),I=1,ND), (DUM(I),I=1,ND), (CLF(I),I=1,ND)
CLOSE (1)  

OPEN (1,FILE=TRIM(FILE)//'/ENION',STATUS='OLD',FORM='UNFORMATTED')  
READ (1) ENIONND,XNE  
CLOSE (1)  

SRVMAX = SR/VMAX  

! ALL QUANTITIES ALREADY CORRECTED FOR CLUMPING
OPEN (1,FILE=TRIM(FILE)//'/CONT_FORMAL_ALL',STATUS='OLD',FORM='UNFORMATTED')
REWIND 1  
READ (1) NFRE, (FREQC(I),I=1,IFRETOT), &
&  ((OPACON(I,J),I=1,ND),J=1,IFRETOT), &
&  ((THOMS(I,J),I=1,ND),J=1,IFRETOT), &
&  ((ST(I,J),I=1,ND),J=1,IFRETOT), &
&  ((XJALL(I,J),I=1,ND),J=1,IFRETOT), (T(I),I=1,ND), &
&  ((OPA_EFF_RAT(I,J),I=1,ND),J=1,IFRETOT)
CLOSE (1)  

!CHECK CONSISTENCY
DO I=1,IFRETOT
  IF(FREQC(I).EQ.0.) EXIT
ENDDO  
IFRE=I

IF(.NOT.OPTTHICK.AND.MAXVAL(ABS(OPA_EFF_RAT(1:ND,1:IFRE)-1.D0)).GT.1.D-6) &
  STOP' OPTICALLY THIN CLUMPING AND OPA_EFF_RAT NE 1, subr. elscat!!!'


OPEN (1,FILE=TRIM(FILE)//'/NLTE_POP',STATUS='OLD',ACCESS='DIRECT',RECL=NRECET)
OPEN (11,FILE=TRIM(FILE)//'/LTE_POP',STATUS='OLD',ACCESS='DIRECT',RECL=NRECET)
!
!---- xnue in cm-1
!---- it is assumed without any check that xnue(1) > xnue(2) (i.e.,
!---- xnue(1) corresponds to the blue component)
!

IF (XNUE(1).GT.FREQC(NFRE) .OR. XNUE(1).LT.FREQC(1)) &
& STOP 'FREQ. NOT CALCULATED IN CONTINUUM TRANSFER'

K0 = NFRE  
XXX = FREQC(NFRE)  

DO WHILE (XNUE(1).LT.XXX)  
   K0=K0-1  
   XXX=FREQC(K0)  
END DO

K01=K0+1
!commented out by JP (June 2013), since only bluest component decisive
!XXX = FREQC(K01)  
!DO WHILE (XNUE(NB).LT.XXX)  
!   K0=K0-1  
!   XXX=FREQC(K0)  
!END DO

!PRINT*,1.d8/FREQC(K01),1.d8/XNUE(1),1.d8/XNUE(NB),1.d8/XXX
!
!---- calculation of cmf-frequencies for continuum interpolation
!---- (they correspond to the limits of the subinterval defined
!---- by k0 and k0+1)
!
!NOTE: from here on, all frequencies displayed as X (i.e., in units of vinf)
!      are calculated with respect to the bluemost line frequency (XNUE(1))

XCRED  = (FREQC(K0)-XNUE(1))*CLIGHT*SRVMAX/SR/XNUE(1)  
XCBLUE = (FREQC(K01)-XNUE(1))*CLIGHT*SRVMAX/SR/XNUE(1)  
!
!---- lambda in cm
!
DO I = 1,NB  
     XLAMB(I) = 1.D0/XNUE(I)  
END DO  
!
!     up to now linear interpolation to line freq of bluemost componnent (x=0),
!     to be consistent with treatment in formacon
!
DX = XCBLUE - XCRED  
QBLUE = 1.D0 - XCBLUE/DX  
QRED = 1.D0 - QBLUE  

DO I = 1,ND  

! recalculate thomson factor, separating electron-scat. and bg-line contribution
! re-correct for OPA_EFF_RAT
     THOMSON_1=(XNE(I)*SIGMAE*XMH/CLF(I))/OPACON(I,K0)*OPA_EFF_RAT(I,K0)
     THOMSON_2=THOMS(I,K0)-THOMSON_1
     IF(THOMSON_2.LT.-1.d-14) STOP' ERROR IN THOMSON_1(K0)'
     THOMSON_2=AMAX1(0.,THOMSON_2)

     THOMSON_11=(XNE(I)*SIGMAE*XMH/CLF(I))/OPACON(I,K01)*OPA_EFF_RAT(I,K01)
     THOMSON_21=THOMS(I,K01)-THOMSON_11             
     IF(THOMSON_21.LT.-1.d-14) STOP' ERROR IN THOMSON_11(K1)'
     THOMSON_21=AMAX1(0.,THOMSON_21)

     OPAC(I) = (QRED*OPACON(I,K0)+QBLUE*OPACON(I,K01)) ! already corrected 
     DUM1 = (QRED*OPACON(I,K0)/OPA_EFF_RAT(I,K0)+QBLUE*OPACON(I,K01)/OPA_EFF_RAT(I,K01))
     THOMSON(I)=(XNE(I)*SIGMAE*XMH/CLF(I))/DUM1
     OPAC(I) = OPAC(I)*SR
!
!--------possibility that thoms > 1 due to updated electr. densities
!        in nlte code.
!
     IF (THOMSON(I).GT.1.D0) THEN
       PRINT*,'WARNING THOMSON > 1 ',I,' ',THOMSON(I)
       THOMSON(I) = 1.D0  
     ENDIF
! correct strue for bg-line contribution
! since THOMSON_2 has been consistently calculated, no additional correction required
     STRUEK=ST(I,K0)+THOMSON_2*XJALL(I,K0)
     STRUEK1=ST(I,K01)+THOMSON_21*XJALL(I,K01)

     STRUE(I) = QRED*STRUEK + QBLUE*STRUEK1  
     XJOLD(I) = QRED*XJALL(I,K0) + QBLUE*XJALL(I,K01)  

     READ (1,REC=I) (BLEVEL(J),J=1,NREC)  
     READ (11,REC=I) (ALEVEL(J),J=1,NREC)  
     DO IK = 1,NB  
          XNL = BLEVEL(NL(IK))  
          IF(XNL.EQ.0.) XNL = 1.D-15*ALEVEL(NL(IK))
          XNU = BLEVEL(NU(IK))  
          IF(XNU.EQ.0.) XNU = 1.D-15*ALEVEL(NU(IK))
! corrected
          XKL = C1*GFLU(IK)* (XNL/GL(IK)-XNU/GU(IK))/CLF(I)  
          TCL = ABS(XKL)*XLAMB(IK)*TCL_FAC_LINE(I) ! consistent inversion treatment
! Note: SRVMAX term included in tcl_fac_line multiplier 
          FAC2 = (1.+FIC(I)*TCL)/(1.+TCL)
! correction for optically thick clumping as in nlte.f90,
! no back-correction of background          
          FAC2 = MIN (FAC2, OPA_EFF_RAT(I,K0), OPA_EFF_RAT(I,K01) )
          IF (FAC2.LE.0.0D0.OR.FAC2.GT.1.0D0) THEN 
            PRINT*,FAC2
            PRINT*,TCL,I,IK
            STOP' OPA_EFF > <OPA>, ELSCAT'
          ENDIF
          OPAL(I,IK) = XKL*SRVMAX*XLAMB(IK)*FAC2  
          SLINE(I,IK) = C2* (XNUE(IK)**3)/ (XNL/XNU*GU(IK)/GL(IK)- 1.D0)
          IF (OPAL(I,IK).LT.0.) THEN
            PRINT*,'INVERSION OF COMPONENT',IK,' AT L = ',I
          ENDIF  
     END DO
END DO

CLOSE (1)  
CLOSE (11)  
!
!     calculation of the p-grid
!
NC = NP - ND  
DO L = 1,NC  
     P(L) = .99D0*DBLE(L-1)/DBLE(NC-1)  
END DO  

DO L = NC + 1,NP  
     P(L) = R(NP+1-L)  
END DO  
!
!     calculation of the z-grid
!
DO L = 1,NP  
     LMAX = MIN0(NP+1-L,ND)  
     DO J = 1,LMAX  
          Z2 = R(J)*R(J) - P(L)*P(L)  
          IF (L.GT.NC .AND. J.EQ.LMAX) THEN  
               IF (Z2.GT.1.D-5) STOP 'ERROR IN Z=0'  
               Z2 = 0.D0  
          END IF  
          Z(J,L) = SQRT(Z2)  
     END DO
END DO  
!
!     set up of cmf freq. grid ----------------------------------
!
!---  assumes that turbulence velocity is negligible for electrons
!
VTHEL = 5.5056D5*SQRT(TEFF)  
print*,'vmax = ',vmax/1.d5,' vth_el = ',vthel/1.d5

! from x = 4*vthel,-2*vinf-3*vthel
! min(lam) from xlamb(1), max(lam) from xlamb(nb)

LAMMIN=XLAMB(1)/(4.*VTHEL/CLIGHT+1.)
LAMMAX=XLAMB(NB)/((-3.-2.*VMAX/VTHEL)*VTHEL/CLIGHT+1.)
!PRINT*,LAMMIN*1.D8,LAMMAX*1.D8,XLAMB*1.D8

VREF= MAX(VTURB,5.D5)
VREF= VREF/3.D0 ! 3 points per vth is sufficient
PRINT*
PRINT*,'freq. separation inside doppler core = ',VREF/1.D5,' km/s'

!x for lammin/lammax (again: w.r.t. blue component)
XMAX=(XLAMB(1)/LAMMIN -1.D0)*CLIGHT/VMAX
XMIN=(XLAMB(1)/LAMMAX -1.D0)*CLIGHT/VMAX

! transition frequencies in X, w.r.t. blue component
DO IK=1,NB
  XC(IK)=(XLAMB(1)/XLAMB(IK)-1.D0)*CLIGHT/VMAX
ENDDO

DX=0.05! 5% is typical maximum separation in V (do not use larger DX values
       ! in non core region; otherwise => oscillations in the red
DX=AMAX1(DX, 50.D5/VMAX) ! minimum dx corresponding to  50 km/s
DX=AMIN1(DX,100.D5/VMAX) ! maximum dx corresponding to 100 km/s
DXCOARSE=DX
DXFINE=VREF/VMAX

! account for fine-resolution around line-cores
IDV=6*3 !6 Doppler width * 3*vref; might need to be changed to 10

ALL_COMP: DO IK=1,NB
  IF (IK.EQ.1) THEN
      XX(1)=XC(IK) ! line-center(IK=1) 
      INEXT=2
      DO I=INEXT,INEXT+IDV-1  
        XX(I)=XC(IK)+(I+1-INEXT)*DXFINE ! 6 vth towards blue
      ENDDO
      INEXT=INEXT+IDV
  ENDIF 

  IF (IK.EQ.NB) THEN 
      DO I=INEXT,INEXT+IDV-1  
        XX(I)=XC(IK)-(I+1-INEXT)*DXFINE ! 6 vth towards red
      ENDDO
      ILAST=INEXT+IDV-1
      EXIT ALL_COMP
  ENDIF 

  IF(XC(IK)-XC(IK+1).GT.(2*IDV*DXFINE)) THEN
      DO I=INEXT,INEXT+IDV-1 
        XX(I)=XC(IK)-(I+1-INEXT)*DXFINE ! 6 vth towards red of IK
      ENDDO
      INEXT=INEXT+IDV
      XX(INEXT)=XC(IK+1) ! line-center(IK+1) 
      INEXT=INEXT+1
      DO I=INEXT,INEXT+IDV-1  
        XX(I)=XC(IK+1)+(I+1-INEXT)*DXFINE ! 6 vth towards blue of IK+1
      ENDDO
      INEXT=INEXT+IDV
   ELSE    
      IF(XC(IK).EQ.XC(IK+1)) CYCLE ALL_COMP! identical frequencies
      IDX=(XC(IK)-XC(IK+1))/DXFINE+1
      DX=(XC(IK)-XC(IK+1))/IDX
      DO I=INEXT,INEXT+IDX-1   
        XX(I)=XC(IK)-(I+1-INEXT)*DX ! towards red, including XC(IK+1)
      ENDDO
      INEXT=INEXT+IDX
   ENDIF
ENDDO ALL_COMP

ILAST=ILAST+1
XX(ILAST)=XMAX
ILAST=ILAST+1
XX(ILAST)=XMIN

XX=-XX
CALL SORT(ILAST,XX)
XX=-XX


! check that transition frequencies are included
DO IK=1,NB
   DO I=1,ILAST
     IF(ABS(XX(I)-XC(IK)).LT.1.D-15) GOTO 100
   ENDDO
   STOP' XC(IK) NOT FOUND IN XX!'
100 CONTINUE   
ENDDO

! now fill the gaps with DXCOARSE

INEXT=ILAST+1
DO I=1,ILAST-1
  XXDIFF=XX(I)-XX(I+1)
  IF(XXDIFF.GT.1.2*DXCOARSE) THEN
      IDX=XXDIFF/DXCOARSE+1
      DX=XXDIFF/IDX
      DO K=INEXT,INEXT+IDX-2 ! only inside interval 
        XX(K)=XX(I)-(K+1-INEXT)*DX 
      ENDDO
      INEXT=INEXT+IDX-1
   ENDIF
ENDDO
ILAST=INEXT-1

IF(ILAST.GT.NFMAX) STOP'NFMAX=ID_NFESC TOO SMALL'

NFCMF=ILAST

XX=-XX
CALL SORT(NFCMF,XX)
XX=-XX

! for tests
!DO I=1,NFCMF
!  PRINT*,I,XX(I),XX(I)-XX(I+1)
!ENDDO

PRINT*,'number of cmf-frequency points = ',NFCMF

ALLOCATE(AICK(NFCMF),DBDRK(NFCMF),EOLD(NFCMF), &
&        X(NFCMF),XI(NFCMF),FI(NFCMF))

ALLOCATE(ETAC(NFCMF,ND),AJ(NFCMF,ND),E(NFCMF,ND),XJNEW(NFCMF,ND), &
&        OPAL_TOT(NFCMF,ND),ETAL_TOT(NFCMF,ND))
!
!     now xi = ln (nue)
!
!     nue_0 (corresponding to x=0) taken from first component
!
DO I=1,NFCMF
   X(I)=XX(I)
   LAM_CMF=XLAMB(1)/(X(I)*VMAX/CLIGHT+1.D0)
   XNU = CLIGHT/LAM_CMF  
   XI(I) = LOG(XNU)  
ENDDO
!
!     end set up of cmf freq. grid ------------------------------
!
!
!     blue wing boundary (remains fixed during subsequent iteration)
!
XLAMBDA = XLAMB(1)*1.D8/ (VMAX*X(1)/CLIGHT+1.D0)  
CALL DIFFUS(XLAMBDA,T,R,ND,AIC,DBDR)  
CORRFC = 1.D0  

CALL CONT1(ND,NP,R,P,Z,STRUE,OPAC,THOMSON,AIC,DBDR,CORRFC, &
           XJALL(1,1),UBLUWI,W0,W2)
! now xjall(i,1) is continuum J at x=0

ERR = 0.D0

DO L = 1,ND-1  
! last point differs, due to irradiation from x(1) instead of x=0
!      print*,l,' ',xjall(l,1)/xjold(l)
!      write(*,fmt='(i2,2x,f10.5,3(2x,e11.5),2x,f10.5)') &
!&       l,xjall(l,1)/xjold(l),xjold(l),strue(l),opac(l),thomson(l)
   ERR = MAX(ERR,ABS(1.D0-XJALL(L,1)/XJOLD(L)))  
END DO

PRINT *  
PRINT *,' MAX. INCONSISTENCY IN JNUE(CONT): ',ERR  
PRINT *  

IF(ERR.GT.0.01) STOP'INCONSISTENCY IN JNUE(CONT) TOO LARGE!'

!for tests of stark-broadening
!PRINT*,'lambda, 20000/1D13 40000/1D14 20000/1D14 40000/1D14'
!DO K = 1,NFCMF  
!      XLAMBDA = XLAMB(2)*1.D8/ (VMAX*X(K)/CLIGHT+1.D0)  
!      PHIKL1 = PERFIL1(2,X(K),20000.d0,1.D13,VTURB,FLAG)
!      PHIKL2 = PERFIL1(2,X(K),40000.d0,1.D13,VTURB,FLAG)
!      PHIKL3 = PERFIL1(2,X(K),20000.d0,1.D14,VTURB,FLAG)
!      PHIKL4 = PERFIL1(2,X(K),40000.d0,1.D14,VTURB,FLAG)
!      WRITE(*,FMT='(5(E12.6,2X))'),XLAMBDA-XLAMB(2)*1.D8,LOG10(PHIKL1), &
!&       LOG10(PHIKL2),LOG10(PHIKL3),LOG10(PHIKL4)
!ENDDO
!STOP


DO L = 1,ND  
   XJNEW(:,L) = XJALL(L,1)  

   DO K = 1,NFCMF  
!
!-----perfil with respect to opal*sr instead of opal*srvmax*lambda
!
      PHIKL = PERFIL1(1,X(K),T(L),XNE(L),VTURB,FLAG)*VMAX/XLAMB(1)
      AUX=OPAL(L,1)*PHIKL      
      OPAL_TOT(K,L)=AUX 
      ETAL_TOT(K,L)=SLINE(L,1)*AUX   
      DO IK=2,NB
        PHIKL = PERFIL1(IK,X(K)-XC(IK),T(L),XNE(L),VTURB,FLAG)*VMAX/XLAMB(IK)
        AUX=OPAL(L,IK)*PHIKL      
        OPAL_TOT(K,L)=OPAL_TOT(K,L)+AUX  
        ETAL_TOT(K,L)=ETAL_TOT(K,L)+SLINE(L,IK)*AUX  
      ENDDO   
! remember the old bug for the bluest frequencies: k1 instead of k
      XLAMBDA = XLAMB(1)*1.D8/ (VMAX*X(K)/CLIGHT+1.D0)  
      CALL DIFFUS(XLAMBDA,T,R,ND,AICK(K),DBDRK(K))  
   ENDDO

END DO


!DELTAX = VREF/VMAX  !old version; now set in subr. RAY

!total opacity (does not change during iteration)
DO K = 1,NFCMF
   OPAL_TOT(K,:)=OPAL_TOT(K,:)+OPAC
ENDDO
!now, opal_tot is total opacity (lines + cont)

DO K = 1,NFCMF
   DUM=OPAL_TOT(K,:)
   WHERE(DUM.LE.0.)
! check for inversion in total opacity;
! intermed. hack: use only cont. quantities
     OPAL_TOT(K,:)=OPAC
     ETAL_TOT(K,:)=0.
   ENDWHERE  
ENDDO

!     begin of iteration 

ITLOOP: DO IIT = 1,20
!
!=======================================
!     cmf solution
!=======================================
!

     REL = 0.D0  
     AJ  = 0.D0

     IF (IIT.EQ.1) THEN
! for inversion, etal_tot has been set to zero
       DO K = 1,NFCMF
           ETAC(K,:) = (STRUE + THOMSON*XJNEW(K,:))*OPAC + ETAL_TOT(K,:) 
       ENDDO 

     ELSE
       DO K = 1,NFCMF
           ETAC(K,:) = (STRUE + THOMSON*E(K,:))*OPAC + ETAL_TOT(K,:) 
       ENDDO
     ENDIF
!now, etac is total emissivity (lines + cont); changes during iteration

!for tests
!do k=1,nfcmf
!    do l=1,nd
!      WRITE (*,FMT='(2(i3,2x),3(E12.6,2x))') k,l,x(k),opal_tot(k,l),etac(k,l)
!    enddo  
!enddo
!stop

JPLOOP1: DO JP = 1,NP - 1  
          LMAX = MIN0(NP+1-JP,ND)  
          LZ = LMAX - 1  
!
!     bluewing boundary condition
!
          UCMF(1) = UBLUWI(1,JP)  

          DO L = 1,LZ  
               UCMF(L+1) = UBLUWI(L+1,JP)  
               AUX = .5D0* (OPAC(L)+OPAC(L+1))  
               VCMF(L) = (UCMF(L+1)-UCMF(L))/AUX/ (Z(L,JP)-Z(L+1,JP))
          END DO

          CALL RAY(JP,Z,R,V,DVDR,ND,NP,UCMF,VCMF,OPAL_TOT,ETAC, &
           NFCMF,AICK,DBDRK,AJ,W0(1,JP),X)

     END DO JPLOOP1

!     DO K = 1,NFCMF  
!          ERR = 0.D0  
!          DO L = 1,ND  
!               ERR = MAX(ERR,ABS(1.D0-AJ(K,L)/XJOLD(L)))  
!          END DO
!      print*
!      print*,' max. inconsistency in jnue: ',err,' at x(',k,') = ',x(k)
!     END DO

XJNEW=AJ
! hack to account for edge effects (inconsistency cont-transfer/cmf-transfer)
XJNEW(1,:)=XJNEW(2,:)

!normalize J(cmf) to (more accurate) J(cont)
!in the ideal case, xjnew (cmf) should be equal to j(cont) at blue and red edges
!(assuming that freq. dependent lower boundary makes no difference expect for lowermost point)
DO L=1,ND-1
  FAC1=XJNEW(1,L)/XJOLD(L)
  FAC2=XJNEW(NFCMF,L)/XJOLD(L)
  IF(L.EQ.1 .AND. (ABS(1.-FAC1).GT.0.05 .OR. ABS(1.-FAC2).GT.0.05)) THEN
    PRINT*,FAC1,FAC2
    PRINT*,'WARNING !!! LARGE INCONSISTENCY BETWEEN J(CMF) AND J(CONT)!!!'
  ENDIF
  IF(ABS(1.-FAC1).GT.0.1) STOP 'LARGE INCONSISTENCY BETWEEN J(CMF) AND J(CONT) AT BLUE EDGE'
  IF(ABS(1.-FAC2).GT.0.1) STOP 'LARGE INCONSISTENCY BETWEEN J(CMF) AND J(CONT) AT RED EDGE'
  IF(ABS(1.-FAC1/FAC2).GT.0.02) THEN
    PRINT*,FAC1,FAC2
    STOP 'INCONSISTENCY AT BLUE AND RED EDGE FREQUENCY'
  ENDIF
  FAC1=0.5*(FAC1+FAC2)
  FAC1=1./FAC1
  XJNEW(:,L)=XJNEW(:,L)*FAC1
ENDDO  
!
!     now rybicki and hummer algorithm at cmf frequencies
!     (dipole approx)
!
!goto 510
LLOOP: DO L = 1,ND  

          IF (IIT.GE.2) EOLD=E(:,L)

          BETAT = 1.84D-5*SQRT(T(L))  
          CONST = (BETAT/B1)**2  
! AJ(:,I),I=1,3 as dummy for TA,TB,TC to save storage
          CALL SCATSOL(XI,XJNEW(1,L),FI,CONST,AJ(:,1),AJ(:,2),AJ(:,3),NFCMF)  

          DO K = 1,NFCMF  
               E(K,L) = A1*FI(K)  
          END DO

          CONST = (BETAT/B2)**2  
          CALL SCATSOL(XI,XJNEW(1,L),FI,CONST,AJ(:,1),AJ(:,2),AJ(:,3),NFCMF)  

          DO K = 1,NFCMF  
               E(K,L) = E(K,L) + A2*FI(K)  
               IF (IIT.GE.2) REL = MAX(REL,ABS(1.D0-E(K,L)/EOLD(K)))
          END DO

     END DO LLOOP

     IF (IIT.GE.2) PRINT *,' ITERATION NO.',IIT, &
&        ' MAX DEV. IN E(K,L) = ',REL
     IF (IIT.GE.2 .AND. REL.LE.1.D-3) THEN  
          PRINT *  
          PRINT *,' CONVERGENCY ACHIEVED'  
          PRINT *  
          GO TO 510  
     END IF  

END DO ITLOOP  
!
PRINT *  
PRINT *,' ITERATION NOT CONVERGED IN 20 ITERATIONS'  
PRINT *  

  510 CONTINUE  

!
!     change of first and last freq. to be consistent with xgrid
!     assuming that pure continuum is reached
!
IF (X(1)-X(2).LT.1.) X(1) = X(2) + 1.01D0  

IF (X(NFCMF-1)-X(NFCMF).LT.1.) X(NFCMF) = X(NFCMF-1) - 1.01D0  

!!      open(1,file=TRIM(FILE)//'/exi_'//line,
!!     *status='unknown',form='formatted')

OPEN (17,STATUS='SCRATCH',FORM='FORMATTED')  
WRITE (17,FMT=*) ND,NFCMF  
DO K = 1,NFCMF  
     WRITE (17,FMT=*) K,' ',X(K),' ',XI(K)  
END DO  

DO K = 1,NFCMF  
     DO L = 1,ND  
          SCOLD = STRUE(L) + THOMSON(L)*XJOLD(L)  
          SCNEW = STRUE(L) + THOMSON(L)*E(K,L)  
          WRITE (17,FMT=*) K,' ',L,' ',E(K,L),' ',SCNEW,' ', &
           SCNEW/SCOLD
!          WRITE (17,FMT=*) K,' ',L,' ',E(K,L),' ',SCOLD,' ', &
!           SCNEW/SCOLD
! if we replace here scnew by scold, we can test whether the result is
! consistent with a calculation without elscat

!          WRITE (*,FMT='(E12.6,1x,I3,2(1x,E12.6))') X(K),L,E(K,L),SCNEW/SCOLD
!          WRITE (*,FMT='(E12.6,1x,I3,2(1x,E12.6))') X(K),L,E(K,L),XJNEW(K,L)/XJOLD(L)
     END DO
END DO  

!!      close(1)
!stop

DEALLOCATE(AICK,DBDRK,EOLD,X,XI,FI)
DEALLOCATE(ETAC,AJ,E,XJNEW,OPAL_TOT,ETAL_TOT)
DEALLOCATE(OPAL,SLINE,XLAMB,XC)

RETURN  
END
!
!-----------------------------------------------------------------------
!
SUBROUTINE CONT1(ND,NP,R,P,Z,ST,OPA,THOMSON,XIC1,XIC2,CORR,XJ,U,VP,VP2)
!
USE nlte_type
USE nlte_dim
IMPLICIT NONE
!
!.......................................................................
!
!     adaptation from nonplusultra. only j is calculated
!
!     attention!!! subroutine cont1 changed for fast and consistent
!     continuum transfer in program nonplusultra. so far, only case
!     i) can be treated.
!
!     calculates the solution of the following equation applying the
!     rybicki algorithm (including the solution of moments' equation,
!                        if necessary , for case i) )
!
!     xj = lambda (st+thomson*xj) ,
!
!     or, explicitly written
!
!     1)     j = lamdda (s)
!     2) dj/dt = lambda (ds/dt)
!
!     where s is the usual nlte-source function including electron
!     scattering
!
!     upper boundary condition : valid to 3rd order including self-
!     consistent calculated i-minus  (dj/dtau neglected)
!
!     lower boundary condition: valid to 2nd order
!     diffusion approximation possible : xic1 = bnue
!                                        xic2 = dbnue/dr
!     rescaling of lower input flux possible : corr = h-true/h-actual
!
!
!     case 1) input:
!
!          st = (eta - opa-thomson*j)/opa
!         opa = total opacity * stellar radius
!     thomson = opa-thomson/opa
!        xic1 = i-plus at lower boundary (diff.approx. = bnue(t))
!        xic2 = 0. (diff. approx. = dbnue/dr)
!
!     output : xj...mean intensity
!              xh...eddington flux (optflux=.true.)
!
!
!     case 2) input:
!
!          st = d/dt((eta - opa-thomson*j)/opa) + d/dt(thomson) *j
!          opa , thomson as  above
!        xic1 = d/dt(i-plus)  (diff. approx. = d/dt(bnue(t))
!        xic2 = 0. (diff. approx. = d2/drdt (bnue(t)) )
!
!     output : xj = dj/dt
!
!
!.......................................................................
!
!
!     .. parameters ..
INTEGER(I4B), PARAMETER :: ND1=ID_NDEPT,NP1=ID_NPOIN  
INTEGER(I4B), PARAMETER :: IMAX=10  
!     ..
!     .. scalar arguments ..
REAL(DP) ::  CORR,XIC1,XIC2  
INTEGER(I4B) ::  ND,NP  
!     ..
!     .. array arguments ..
REAL(DP) ::  OPA(ND1),P(NP1),R(ND1),ST(ND1),THOMSON(ND1), &
&                 U(ND1,NP1-1),VP(ND1,NP1-1),VP2(ND1,NP1-1), &
&                 XJ(ND1),Z(ND1,NP1)
!     ..
!     .. local scalars ..
REAL(DP) ::  E0,E1,EDMUE,EPSMAX,HEDDI,HEDDO,HIN,HOUT,PI2,UX  
INTEGER(I4B) ::  III,JP,KI,L,LI,LMAX  
LOGICAL CONV,CORE  
!     ..
!     .. local arrays ..
REAL(DP) ::  AKP(ND1),AKSUM(ND1),F(ND1+3),QQ(ND1),TA(ND1), &
&                 TAUM(NP1),TB(ND1),TB1(ND1),TC(ND1),TC1(ND1), &
&                 TP(ND1,ND1),TSUM(ND1,ND1),UP(ND1), &
&                 WP(ND1+1,NP1-1),XJOLD(ND1),XK(ND1)
!     ..
!     .. external subroutines ..
EXTERNAL DEV,INV,INVTRI,INVTRI3,MADD,MDMV,MDV,MOMCONT,MVMD, &
&         MVV,SETUP1,VADD,VSUB,WEIGH11
!     ..
!     .. intrinsic functions ..
INTRINSIC ACOS,ATAN,EXP,MIN0  
!     ..

PI2 = ACOS(0.D0)  
CONV = .FALSE.  

CALL WEIGH11(ND,NP,R,Z,P,VP,VP2,WP)  

AKSUM = .0D0  
TSUM  = .0D0  

DO JP = 1,NP - 1  

     IF (JP.EQ.1) THEN  
          TAUM(1) = OPA(1)*R(1)* (1.D0/3.D0+2.D0*THOMSON(1)/3.D0)  
     ELSE  
          TAUM(JP) = OPA(1)*R(1)*R(1)* (THOMSON(1)/P(JP)* (PI2- &
           ATAN(Z(1,JP)/P(JP)))+ (1.D0-THOMSON(1))*R(1)*R(1)/2.D0/ &
           P(JP)**3.D0* (PI2-ATAN(Z(1,JP)/P(JP))-Z(1, JP)*P(JP)/R(1)/R(1)))
     END IF  

     IF (TAUM(JP).LT.0.D0) STOP 'TAUM NEGATIVE!'  

     LMAX = MIN0(NP+1-JP,ND)  
     CORE = (LMAX.EQ.ND)  
!
!     calculation of tp,akp
!
     CALL SETUP1(LMAX,CORE,Z(1,JP),ST,OPA,THOMSON,XIC1,XIC2,CORR, &
      UP,AKP,TA,TB,TC,TAUM(JP))
!
!     rybicki algorithm to obtain ajc
!
     CALL INVTRI3(TA,TB,TC,TP,LMAX,ND)  
     CALL MDMV(TP,VP(1,JP),LMAX,ND)  
     CALL MVV(QQ,TP,AKP,LMAX,LMAX,ND)  
     CALL VADD(AKSUM,QQ,LMAX)  
!
!     tp=:(vp*tp-1)*u
!
     CALL MVMD(TP,UP,LMAX,ND)  
     CALL MADD(TSUM,TP,LMAX,ND)  

END DO  
!
!     addition of unity-matrix to tsum
!
DO L = 1,ND  
     TSUM(L,L) = TSUM(L,L) + 1.D0  
END DO  

CALL INV(ND,ND,TSUM)  
CALL MVV(XJ,TSUM,AKSUM,ND,ND,ND)  

QQ=XJ
XJOLD=XJ
!
!     backsubstitution to obtain u
!
III = 0  

   50 CONTINUE  

III = III + 1  

DO JP = 1,NP - 1  

     LMAX = MIN0(NP+1-JP,ND)  
     CORE = (LMAX.EQ.ND)  

     CALL SETUP1(LMAX,CORE,Z(1,JP),ST,OPA,THOMSON,XIC1,XIC2,CORR, &
      UP,AKP,TA,TB,TC,TAUM(JP))

     DO L = 1,LMAX  
          TA(L) = -TA(L)  
          TC(L) = -TC(L)  
     END DO
!
!     recalculation of total source-function
!
     CALL MDV(QQ,UP,LMAX)  
     CALL VSUB(AKP,UP,LMAX)  

     CALL INVTRI(TA,TB,TC,AKP,LMAX)  

     DO L = 1,LMAX  
          U(L,JP) = AKP(L)  
     END DO

END DO  
!
!     recalculation of j and calculation of k
!
XJ = 0.D0  
XK = 0.D0  

DO JP = 1,NP - 1  
     LMAX = MIN0(NP+1-JP,ND)  
     DO L = 1,LMAX  
          UX = U(L,JP)  
          XJ(L) = XJ(L) + VP(L,JP)*UX  
          XK(L) = XK(L) + VP2(L,JP)*UX  
     END DO
END DO  

DO L = 1,ND  
     F(L) = XK(L)/XJ(L)  
END DO  
!
!     calculation of inner and outer eddington factors respective to h
!
!---- this is the old form for boundary conditions (i.e., achim's form
!---- instead of gudrun's). i prefer it because the first one takes into
!---- account the new radiation field in the calculation of h-nue as the
!---- outer boundary condition.
!
HIN = 0.D0  
HOUT = 0.D0  
EDMUE = 0.D0  

DO JP = 1,NP - 1  
     LMAX = MIN0(NP+1-JP,ND)  
!
!     outer boundary
!
     IF (TAUM(JP).LT.100.D0) THEN  
          E0 = EXP(-TAUM(JP))  
          E1 = 1.D0 - E0  
     ELSE  
          E1 = 1.D0  
     END IF  
!
!      dxi=u(1,jp)-e1*(st(1)+thomson(1)*xj(1))
!---- this line has been masked (21-10-92). it corresponds to case
!---- i_plus<i_minus (at r_max), and now there is no difference
!     between c---- both cases.(also, now is equivalent to gudrun's
!     formulation but c---- the comment stated above).
!     if(dxi.lt.0.d0) e1=u(1,jp)/(st(1)+thomson(1)*xj(1))
!
     HOUT = HOUT + U(1,JP)*WP(1,JP)  
     EDMUE = EDMUE + E1*WP(1,JP)  
!
!---- with the last modification, edmue is always i_minus(1)/s_total(1)
!---- integrated over mue
!
!     inner boundary
!
     IF (LMAX.EQ.ND) THEN  
          HIN = HIN + U(ND,JP)*WP(ND+1,JP)  
     END IF  

END DO  

HEDDO = HOUT/XJ(1)  
HEDDI = HIN/XJ(ND)  
F(ND+1) = HEDDO  
F(ND+2) = HEDDI  
F(ND+3) = EDMUE  

CALL MOMCONT(ND,R,OPA,THOMSON,ST,XIC1,XIC2,CORR,QQ,TA,TB,TC,TB1,TC1,AKP,XJ,F)

IF (CONV .OR. III.EQ.IMAX) THEN  
!     PRINT *,' ACHIEVED ACCURACY IN CONT. TRANSPORT  = ',EPSMAX  
!     PRINT *,' IN ',III,' ITERATIONS'  
     RETURN  
END IF  
!
!-----------------------------------------------------------------------
!
CALL DEV(R,XJ,XJOLD,ND,EPSMAX,.FALSE.)  

IF (EPSMAX.LT.1.D-3 .OR. III.EQ.IMAX) CONV = .TRUE.  

QQ=XJ  
XJOLD=XJ  

GO TO 50  

END
!
!-----------------------------------------------------------------------
!
SUBROUTINE DEV(R,A1,A,ND,EPSMAX,OPT)  
!
USE nlte_type
USE nlte_dim
IMPLICIT NONE
!
!
!     .. scalar arguments ..
REAL(DP) ::  EPSMAX  
INTEGER(I4B) ::  ND  
LOGICAL OPT  
!     ..
!     .. array arguments ..
REAL(DP) ::  A(ND),A1(ND),R(ND)  
!     ..
!     .. local scalars ..
REAL(DP) ::  E1,E2,E3,E4,E5,EPS1,EPS2,EPS3,EPS4,EPS5  
INTEGER(I4B) ::  L  
!     ..
!     .. intrinsic functions ..
INTRINSIC ABS,MAX  
!     ..

EPS1 = 0.D0  
EPS2 = 0.D0  
EPS3 = 0.D0  
EPS4 = 0.D0  
EPS5 = 0.D0  

DO L = 1,ND  
     IF (R(L).GE.50.D0) THEN  
          E1 = ABS(1.D0-A1(L)/A(L))  
          IF (E1.GT.EPS1) EPS1 = E1  

     ELSE IF (R(L).LT.50.D0 .AND. R(L).GE.10.D0) THEN  
          E2 = ABS(1.D0-A1(L)/A(L))  
          IF (E2.GT.EPS2) EPS2 = E2  

     ELSE IF (R(L).LT.10.D0 .AND. R(L).GE.5.D0) THEN  
          E3 = ABS(1.D0-A1(L)/A(L))  
          IF (E3.GT.EPS3) EPS3 = E3  

     ELSE IF (R(L).LT.5.D0 .AND. R(L).GE.2.D0) THEN  
          E4 = ABS(1.D0-A1(L)/A(L))  
          IF (E4.GT.EPS4) EPS4 = E4  

     ELSE IF (R(L).LT.2.D0) THEN  
          E5 = ABS(1.D0-A1(L)/A(L))  
          IF (E5.GT.EPS5) EPS5 = E5  

     END IF  
END DO  

EPSMAX = MAX(EPS1,EPS2,EPS3,EPS4,EPS5)  

IF (OPT) THEN  
    PRINT *  
    PRINT *,' MAXIMUM DEVIATION ',EPSMAX  
    PRINT *  
    PRINT *,'  1. < R <   2.  : ',EPS5  
    PRINT *,'  2. < R <   5.  : ',EPS4  
    PRINT *,'  5. < R <  10.  : ',EPS3  
    PRINT *,' 10. < R <  50.  : ',EPS2  
    PRINT *,' 50. < R < 100.  : ',EPS1  
END IF  

RETURN  
END
!
!-----------------------------------------------------------------------
!
SUBROUTINE MOMCONT(ND,R,OPA,THOMSON,ST,XIC1,XIC2,CORR,Q,TA,TB,TC, &
&                   TB1,TC1,AKP,XJ,F)
!
USE nlte_type
USE nlte_dim
IMPLICIT NONE
!
!
!-----solves moments equation for continuum
!
!     .. parameters ..
INTEGER(I4B), PARAMETER :: ND1=ID_NDEPT  
!     ..
!     .. scalar arguments ..
REAL(DP) ::  CORR,XIC1,XIC2  
INTEGER(I4B) ::  ND  
!     ..
!     .. array arguments ..
REAL(DP) ::  AKP(ND1),F(ND1+3),OPA(ND1),Q(ND1),R(ND1),ST(ND1), &
&                 TA(ND1),TB(ND1),TB1(ND1),TC(ND1),TC1(ND1), &
&                 THOMSON(ND1),XJ(ND1)
!     ..
!     .. local scalars ..
REAL(DP) ::  DT0,DTM,DTP,EDMUE,FL,FLP,HI,HO,RL,RL2,RLP,RRQ  
INTEGER(I4B) ::  L  
!     ..
!     .. external subroutines ..
EXTERNAL INVTRI  
!     ..
!     .. intrinsic functions ..
INTRINSIC EXP  
!     ..

HO = F(ND+1)  
HI = F(ND+2)  
EDMUE = F(ND+3)  

Q(ND) = 1.D0  
RRQ = 1.D0  
FL = 3.D0 - 1.D0/F(ND)  

DO L = ND - 1,1,-1  
     RL = R(L)  
     RLP = R(L+1)  
     FLP = FL  
     FL = 3.D0 - 1.D0/F(L)  
     RRQ = RRQ*EXP(FL-FLP)* (RL/RLP)** ((FLP*RL-FL*RLP)/ (RL-RLP))  
     Q(L) = RRQ/RL/RL  
END DO  
!
!     feautrier scheme to solve moments equation :
!
!     (-ta,tb,-tc) * rr * xj = akp
!
!     outer boundary condition
!
DTP = 2.D0/ ((Q(1)*OPA(1)+Q(2)*OPA(2))* (R(1)-R(2)))  
TB(1) = (F(1)*Q(1)*DTP+HO-EDMUE*THOMSON(1))  
TB1(1) = TB(1) + EDMUE*THOMSON(1)  
TC(1) = F(2)*Q(2)*DTP  
TC1(1) = TC(1)  
AKP(1) = R(1)*R(1)*ST(1)*EDMUE  
!
!     non boundary points
!
DO L = 2,ND - 1  
     DTM = DTP  
     DTP = 2.D0/ ((Q(L)*OPA(L)+Q(L+1)*OPA(L+1))* (R(L)-R(L+1)))  
     DT0 = 2.D0/ (1.D0/DTP+1.D0/DTM)  
     TA(L) = F(L-1)*Q(L-1)*DT0*DTM  
     TC(L) = F(L+1)*Q(L+1)*DT0*DTP  
     TC1(L) = TC(L)  
     TB(L) = F(L)*Q(L)*DT0* (DTM+DTP) + (1.D0-THOMSON(L))/Q(L)  
     TB1(L) = TB(L) + THOMSON(L)/Q(L)  
     AKP(L) = R(L)*R(L)*ST(L)/Q(L)  
END DO  
!
!     inner boundary condition
!
L = ND  
TA(L) = F(L-1)*Q(L-1)*DTP  
TB(L) = F(L)*Q(L)*DTP + HI  
TB1(L) = TB(L)  
AKP(L) = .5D0*XIC1 + XIC2*CORR/ (3.D0*OPA(L))  
TC1(ND) = AKP(L)  

CALL INVTRI(TA,TB,TC,AKP,ND)  

DO L = 1,ND  
     RL2 = R(L)*R(L)  
     XJ(L) = AKP(L)/RL2  
END DO  

RETURN  
END
!
!-----------------------------------------------------------------------
!
SUBROUTINE SETUP1(LMAX,CORE,Z,ST,OPA,THOMSON,XIC1,XIC2,CORR,UP, &
&                  AKP,TA,TB,TC,TAUM)
!
USE nlte_type
USE nlte_dim
IMPLICIT NONE
!
!     sets up matrix elements for subroutine cont1
!
!     .. parameters ..
INTEGER(I4B), PARAMETER :: ND1=ID_NDEPT  
!     ..
!     .. scalar arguments ..
REAL(DP) ::  CORR,TAUM,XIC1,XIC2  
INTEGER(I4B) ::  LMAX  
LOGICAL CORE  
!     ..
!     .. array arguments ..
REAL(DP) ::  AKP(ND1),OPA(ND1),ST(ND1),TA(ND1),TB(ND1), &
&                 TC(ND1),THOMSON(ND1),UP(ND1),Z(ND1)
!     ..
!     .. local scalars ..
REAL(DP) ::  AK,AKDZ,BB,CC,DT0,DTM,DTP,DZ,E0,E1  
INTEGER(I4B) ::  L,LZ  
!     ..
!     .. intrinsic functions ..
INTRINSIC EXP  
!     ..

LZ = LMAX - 1  
!
!     outer boundary, 3rd order, corrected for i-minus
!
AK = .5D0* (OPA(1)+OPA(2))  
DZ = Z(1) - Z(2)  
AKDZ = .5D0*AK*DZ  
DTP = 1.D0/AK/DZ  
BB = 1.D0/DTP/3.D0  
CC = .5D0*BB  

IF (TAUM.LT.100.D0) THEN  
     E0 = EXP(-TAUM)  
     E1 = 1.D0 - E0  
ELSE  
     E1 = 1.D0  
END IF  

IF (AKDZ.LT..5D0) THEN  
     UP(1) = THOMSON(1)* (BB+E1) + THOMSON(2)*CC  
     AKP(1) = -ST(1)* (BB+E1) - ST(2)*CC  
     TB(1) = -1.D0 - DTP - BB  
     TC(1) = DTP - CC  
ELSE  
     UP(1) = THOMSON(1)*E1  
     AKP(1) = -ST(1)*E1  
     TB(1) = -1.D0 - DTP  
     TC(1) = DTP  
END IF  
!
!     non boundary points
!
DO L = 2,LZ  
     DTM = DTP  
     AK = .5D0* (OPA(L)+OPA(L+1))  
     DZ = Z(L) - Z(L+1)  
     DTP = 1.D0/AK/DZ  
     DT0 = 2.D0/ (1.D0/DTM+1.D0/DTP)  
     UP(L) = THOMSON(L)  
     AKP(L) = -ST(L)  
     TA(L) = DT0*DTM  
     TC(L) = DT0*DTP  
     TB(L) = -DT0* (DTM+DTP) - 1.D0  
END DO  

L = LMAX  
!
!     inner boundary,2nd order
!
IF (CORE) THEN  
     UP(L) = 0.D0  
     TA(L) = DTP  
     AKP(L) = -XIC1 - Z(L)*XIC2/OPA(L)*CORR  
     TB(L) = -DTP - 1.D0  
ELSE  
     AKP(L) = -ST(L)  
     UP(L) = THOMSON(L)  
     TA(L) = 2.D0*DTP*DTP  
     TB(L) = -2.D0*DTP*DTP - 1.D0  
END IF  

RETURN  
END
!
!-----------------------------------------------------------------------
!
SUBROUTINE WEIGH11(ND,NP,R,Z,P,W0,W2,W1)  
!
USE nlte_type
USE nlte_dim
IMPLICIT NONE
!
!***  calculation of angular integration weights
!***  the weights w1  are calculated for intermesh points
!***  (in this context only for boundary conditions)
!
!
!     .. parameters ..
INTEGER(I4B), PARAMETER :: ND1=ID_NDEPT,NP1=ID_NPOIN  
!     ..
!     .. scalar arguments ..
INTEGER(I4B) ::  ND,NP  
!     ..
!     .. array arguments ..
REAL(DP) ::  P(NP1),R(ND1),W0(ND1,NP1-1),W1(ND1+1,NP1-1), &
&                 W2(ND1,NP1-1),Z(ND1,NP1)
!     ..
!     .. local scalars ..
REAL(DP) ::  A,AA,B,BB,C,CC,RL,RL2,RRR12,RZ,RZQ6,W1LZ,WW1  
INTEGER(I4B) ::  JP,L,LMAX,LZ  
!     ..
!     .. intrinsic functions ..
INTRINSIC MIN0  
!     ..

JPLOOP: DO JP = 1,NP - 1  

     LMAX = MIN0(NP+1-JP,ND)  
     LZ = LMAX - 1  
!***
!***  0. and 2. moment. the integration is performed in the z variable
     DO L = 1,LMAX  
          RL = R(L)  
          RL2 = RL + RL  
          RRR12 = RL*RL*RL*12.D0  
!***  first step if jp=1
          IF (JP.EQ.1) THEN  
               B = Z(L,1)  
               A = Z(L,2)  
               W0(L,JP) = (B-A)/RL2  
               AA = A*A  
               BB = B*B  
               W2(L,JP) = (B* (3.D0*BB-AA)-A* (BB+AA))/RRR12  
          ELSE  
               IF (L.NE.LMAX .OR. JP.LE. (NP-ND)) THEN  
!***  intermediate step
                    A = Z(L,JP+1)  
                    B = Z(L,JP)  
                    C = Z(L,JP-1)  
                    W0(L,JP) = (C-A)/RL2  
                    AA = A*A  
                    BB = B*B  
                    CC = C*C  
                    W2(L,JP) = (B* (CC-AA)+C* (CC+BB)-A* (BB+AA))/ RRR12
               ELSE  
!***  last step, implying z(l,jmax)=0
                    B = Z(L,JP-1)  
                    W0(L,JP) = B/RL2  
                    W2(L,JP) = B*B*B/RRR12  
               END IF  
          END IF  
     END DO    
!****
!**** 1.moment.
!****  first step
!****
     IF (JP.EQ.1) THEN  
          WW1 = P(2)*P(2)  
     ELSE  
!***  intermediate steps
          A = P(JP-1)  
          B = P(JP)  
          C = P(JP+1)  
          WW1 = (A+B+C)* (C-A)  
!***  for the last interval (l=lz to the p axis), the next point is an
!***  intermesh-point in p
          C = .5D0* (B+C)  
          W1LZ = (A+B+C)* (C-A)  
     END IF  
!***  no weight for the z=0 point is calculated, as v=0 there for symmetry.
!***  loop over depth index l

     DO L = 1,LZ  
          RZ = .5D0* (R(L)+R(L+1))  
          RZQ6 = RZ*RZ*6.D0  
          IF (L.NE.LZ .OR. JP.LE. (NP-ND)) THEN  
               W1(L+1,JP) = WW1/RZQ6  
          ELSE  
               W1(L+1,JP) = W1LZ/RZQ6  
          END IF  
     END DO
!***  special weights at the outer boundary for h
     RL = R(1)
     W1(1,JP) = WW1/RL/RL/6.D0  
!***  special weights at the inner boundary
     IF (LMAX.LT.ND) CYCLE JPLOOP  

     IF (JP.GT. (NP-ND)) THEN  
!******  core tangent ray, last step of integration
          WW1 = (B-A)* (2.D0*B+A)  
     END IF  

     W1(ND+1,JP) = WW1/6.D0  

END DO JPLOOP  

RETURN  
END
!
!-----------------------------------------------------------------------
!
SUBROUTINE DIFFUS(XLAMBDA,T,R,ND,AIC,DBDR)  
!
USE nlte_type
USE nlte_dim
IMPLICIT NONE
!
!     gives the planck function aic and its radius derivative dbdr
!     at the inner boundary from the given temperature-stratification.
!
!     .. parameters ..
INTEGER(I4B), PARAMETER :: ND1=ID_NDEPT  
!     ..
!     .. scalar arguments ..
REAL(DP) ::  AIC,DBDR,XLAMBDA  
INTEGER(I4B) ::  ND  
!     ..
!     .. array arguments ..
REAL(DP) ::  R(ND1),T(ND1)  
!     ..
!     .. local scalars ..
REAL(DP) ::  DTDR  
!     ..
!     .. external functions ..
REAL(DP) ::  BNUE,DBDT  
EXTERNAL BNUE,DBDT  
!     ..

AIC = BNUE(XLAMBDA,T(ND))  
DTDR = (T(ND)-T(ND-1))/ (R(ND-1)-R(ND))  
DBDR = DBDT(XLAMBDA,T(ND))*DTDR  

RETURN  
END
!
!-----------------------------------------------------------------------
!
FUNCTION BNUE(XLAMBDA,T)  
!
USE nlte_type
USE nlte_dim
IMPLICIT NONE
!
!
!     planck function,lambda in angstroem,t in kelvin
!     bnue in erg per (cm**2 * sec * hertz )
!
!     constanten: c1=h*c/k,c2=2*h*c
!
!
!     .. parameters ..
REAL(DP), PARAMETER :: C1=1.4388354967334D8,C2=3.972970127D8  
!     ..
!     .. scalar arguments ..
REAL(DP) ::  T,XLAMBDA,BNUE  
!     ..
!     .. intrinsic functons ..
INTRINSIC EXP  
!     ..

BNUE = C2/ (EXP(C1/XLAMBDA/T)-1.D0)/XLAMBDA/XLAMBDA/XLAMBDA  

RETURN  
END
!
!-----------------------------------------------------------------------
!
FUNCTION DBDT(X,T)  
!
USE nlte_type
USE nlte_dim
IMPLICIT NONE
!
!
!     .. parameters ..
REAL(DP), PARAMETER :: C1=1.4388354967334D8  
!     ..
!     .. scalar arguments ..
REAL(DP) ::  T,X,DBDT
!     ..
!     .. external functions ..
REAL(DP) ::  BNUE  
EXTERNAL BNUE  
!     ..
!     .. intrinsic functions ..
INTRINSIC EXP  
!     ..

DBDT = BNUE(X,T)*C1/X/T/T/ (1.D0-EXP(-C1/X/T))  

RETURN  
END
!
!-----------------------------------------------------------------------
!
SUBROUTINE RAY(JP,Z,R,VELO,GRADV,ND,NP,U,V,OPAL,ETAL, &
&              NF,AIC,DBDR,AJ,W0,X)
!
USE nlte_type
USE nlte_dim
IMPLICIT NONE
!
!
!     line radiation transfer in the comoving frame from a
!     given source function for a given impact parameter jp.
!     the integration is carried out in space to
!     yield the mean intensity
!
!     j-nue
!
!     note, that here the cont. emissivity is varying
!
!
!     .. parameters ..
INTEGER(I4B), PARAMETER :: ND1=ID_NDEPT  
!     ..
!     .. scalar arguments ..
INTEGER(I4B) ::  JP,ND,NF,NP  
!     ..
!     .. array arguments ..
REAL(DP) ::  AIC(NF),AJ(NF,ND),DBDR(NF), &
&            OPAL(NF,ND),ETAL(NF,ND), &
&            U(ND),V(ND-1),Z(ND,NP), &
&            R(ND),VELO(ND),GRADV(ND),W0(ND),X(NF)
!     ..
!     .. local scalars ..
REAL(DP) ::  RL,XMUE,XMUE2,DELTAX  
INTEGER(I4B) ::  K,L,LMAX,LZ  
!     ..
!     .. local arrays ..
REAL(DP) ::  GA(ND1),H(ND1),PP(ND1),PP1(ND1),QQ(ND1),S(ND1),TA(ND1), &
&                 TB(ND1),TC(ND1),UB(ND1),VA(ND1), &
&                 VB(ND1)
!     ..
!     .. external subroutines ..
EXTERNAL CMFSET,GMALU,INVTRI,MDV,VADD,VMALV  
!     ..
!     .. intrinsic functions ..
INTRINSIC MIN0  
!     ..

LMAX = MIN0(NP+1-JP,ND)  
LZ = LMAX - 1  
!
!     pp(l)=velocity gradient,projected on the present ray,/deltax
!
DO L = 1,LMAX  
     RL = R(L)  
     XMUE = Z(L,JP)/RL  
     XMUE2 = XMUE*XMUE  
     PP1(L) = (XMUE2*GRADV(L)+ (1.D0-XMUE2)*VELO(L)/RL) !here, without DELTAX
END DO  
!
!     loop for all original frequency points
!
KLOOP: DO K = 1,NF  
     IF (K.EQ.1) GO TO 40

     DELTAX=X(K-1)-X(K)
     PP=PP1/DELTAX
! THIS IS THE MOST IMPORTANT NEW FEATURE.
! For K=1, BW values are taken (as usual)
! But for K=2, we set PP articifically to zero, and decouple K=2 from K=1.
! In this way, a consistent BW boundary condition is calculated, which
! does not spoil the overall solution. Otherwise, we always encounter
! small (0.005) inconsistencies in the pseudo-continuum, because the
! slight differences between BW (continuum, 2nd order, Rybicki)
! and CMF solution (1st order, formal) are not damped out if no strong line
! or optically thick continuum is present.
! These differences are set to zero by the new method.
! Note that this trick requires K=2 to be pure continuum!
     IF(K.EQ.2) PP=0.

     CALL CMFSET(Z(1,JP),ND,LMAX,TA,TB,TC,UB,VA,VB,GA,H,S, &
      OPAL(K,:),ETAL(K,:),PP,DBDR(K),AIC(K))

     CALL VMALV(VA,VB,V,QQ,LMAX)  
     CALL VADD(QQ,S,LMAX)  

     U(1:LMAX)=UB(1:LMAX)*U(1:LMAX)

     CALL VADD(U,QQ,LMAX)  
     CALL INVTRI(TA,TB,TC,U,LMAX)  

!tested; improves results
     DO L = 1,LMAX  
          IF (U(L).LT.0.) U(L) = 0.D0  
     END DO
!
!     now u is the new field at index k
!
     CALL MDV(H,V,LZ)  
     CALL GMALU(GA,U,LMAX)  
     CALL VADD(V,GA,LZ)  
!
!     now v is the new field at index k
!
!     adding the new u to 0th moments aj
!
   40      CONTINUE  


     DO L = 1,LMAX  
          AJ(K,L) = AJ(K,L) + W0(L)*U(L)  
!          AJ(K,L) = AJ(K,L) + W0(L)*AMAX1(U(L),0.)  
!          print*,k,l,u(l),v(l),aj(k,l) 
     END DO

END DO KLOOP  

RETURN  
END
!
!-----------------------------------------------------------------------
!
SUBROUTINE CMFSET(Z,ND,LMAX,TA,TB,TC,UB,VA,VB,GA,H,S, &
&                 OPAL,ETAL,PP,DBDR,AIC)
!
USE nlte_type
USE nlte_dim
IMPLICIT NONE
!
!
!
!     sets up the array elements for the cmf formalism
!
!
!     ..
!     .. scalar arguments ..
INTEGER(I4B) ::  LMAX,ND  
REAL(DP) :: AIC,DBDR

!     ..
!     .. array arguments ..
REAL(DP) ::  GA(ND),H(ND),S(ND), &
&            OPAL(ND),ETAL(ND),&
&            PP(ND),TA(ND),TB(ND),TC(ND),UB(ND), &
&            VA(ND),VB(ND),Z(ND)
!     ..
!     .. local scalars ..
REAL(DP) ::  AK,AZ,DAZ,DAZM,DBZ,DBZM,DT,DTZ,DTZM,DX,DXZ,DXZ1,DXZM,DXZM1, &
&                 EK,TAU,TAUZ
INTEGER(I4B) ::  L,LZ  
!     ..
!     .. intrinsic functions ..
INTRINSIC ABS  
!     ..

LZ = LMAX - 1  
!
!***  outer boundary condition  -  2nd order
!
AK = OPAL(1)
AZ = 0.5D0* (AK + OPAL(2))  
TAUZ = Z(1) - Z(2)  
DX = PP(1)/AK  
S(1) = 0.D0  
TC(1) = 1.D0/AK/TAUZ  
TB(1) = TC(1) + DX + 1.D0  
UB(1) = DX  
VB(1) = 0.0D0  
!
!***  for g and h, the matrix element s are not different from inner
!     points
DTZM = 1.D0/AZ/TAUZ  
DXZM = (PP(1)+PP(2))/AZ/2.D0  
DXZM1 = 1.D0/ (1.D0+DXZM)  
DAZM = DTZM*DXZM1  
DBZM = DXZM*DXZM1  
GA(1) = -DAZM  
H(1) = DBZM  
!
!***  non-boundary points
!
DO L = 2,LZ  
     AK = OPAL(L)  
     EK = ETAL(L)  
     S(L) = EK/AK  
     AZ = 0.5D0* (AK + OPAL(L+1))  
     TAU = 0.5D0* (Z(L-1)-Z(L+1))  
     TAUZ = Z(L) - Z(L+1)  
     DT = 1.D0/AK/TAU  
     DTZ = 1.D0/AZ/TAUZ  
     DX = PP(L)/AK  
     DXZ = (PP(L)+PP(L+1))/AZ/2.D0  
     DXZ1 = 1.D0/ (1.D0+DXZ)  
     DAZ = DTZ*DXZ1  
     DBZ = DXZ*DXZ1  
     TA(L) = DT*DAZM  
     TC(L) = DT*DAZ  
     TB(L) = TA(L) + TC(L) + DX + 1.D0  
     UB(L) = DX  
     VA(L) = -DT*DBZM  
     VB(L) = DT*DBZ  
     GA(L) = -DAZ  
     H(L) = DBZ  
     DAZM = DAZ  
     DBZM = DBZ  

END DO  

L = LMAX  

IF (LMAX.LT.ND) GO TO 30  
!
!***  inner boundary condition (core rays)  -  only to first order
!***   diffusion-approximation
!
AK = OPAL(L)  
S(L) = AIC + Z(L)/AK*DBDR  
TAUZ = Z(L-1) - Z(L)  
DX = PP(L)/AK  
DT = 1.D0/TAUZ/AK  
TA(L) = DT  
TB(L) = DT + DX + 1.D0  
UB(L) = DX  
VA(L) = 0.0D0  
VB(L) = 0.0D0  

RETURN  
!
!***  inner boundary condition (non-core rays)  -  second order
!
   30 CONTINUE  

AK = OPAL(L) 
EK = ETAL(L)  
S(L) = EK/AK  
TAUZ = Z(LZ)  
DT = 1.D0/AK/TAUZ  
DX = PP(L)/AK  

TA(L) = 2.D0*DT*DAZM !INSTEAD OF DAZ  
TB(L) = TA(L) + DX + 1.D0  
UB(L) = DX  
VA(L) = -2.D0*DT*DBZM !INSTEAD OF DBZ

RETURN  
END
!
!-----------------------------------------------------------------------
!
SUBROUTINE SCATSOL(XI,XJ,FI,CONST,TA,TB,TC,NF)  
!
USE nlte_type
USE nlte_dim
IMPLICIT NONE
!
!-----set up of matrix elements (note, that here in reverse order, i.e.
!     from high to low frequencies)
!
!-----note: used scheme here is -ta tb -tc in order to use invtri
!
!     ..
!     .. scalar arguments ..
REAL(DP) ::  CONST  
INTEGER(I4B) ::  NF  
!     ..
!     .. array arguments ..
REAL(DP) ::  FI(NF),XI(NF),XJ(NF),TA(NF),TB(NF),TC(NF)  
!     ..
!     .. local scalars ..
REAL(DP) ::  DM,DDP,DQ,TA1,TB1  
INTEGER(I4B) ::  L  
!     ..
!     .. external subroutines ..
EXTERNAL INVTRI  
!     ..
!
!     first frequency
!
DDP = XI(2) - XI(1)  
TB1 = 2.D0*CONST/DDP**2  
TB(1) = TB1 + 1.D0  
TC(1) = TB1  
FI(1) = XJ(1)  

DO L = 2,NF - 1  
     DM = DDP  
     DDP = XI(L+1) - XI(L)  
     DQ = .5D0* (DDP+DM)  
     TA(L) = CONST/ (DM*DQ)  
     TB(L) = 2.D0*CONST/ (DDP*DM) + 1.D0  
     TC(L) = CONST/ (DDP*DQ)  
     FI(L) = XJ(L)  
END DO  
!
!     last freq.
!
TA1 = 2.D0*CONST/DDP**2  
TA(NF) = TA1  
TB(NF) = TA1 + 1.D0  
FI(NF) = XJ(NF)  

CALL INVTRI(TA,TB,TC,FI,NF)  

RETURN  
END
!
!***********************************************************************
!
! subroutines: complex ones
! formal and related
!
!***********************************************************************
!
SUBROUTINE FORMAL(ND,NP,NC,NB,NFOBS,RMAX,DELTA,ESCAT,R1,R,V,OPAL, &
&                  SLINE,P,Z,LMAX,X0,VMAX,OPALRAY,SRAY,ZRAY,VTURBRAY,LTOT, &
&                  XCMF,PROFILE,PROFABS,PROFEM,ZG,AIC, &
&                  XMAX,XMAXDOP,XMAXDOP_MIN, &
&                  OPTIOV,NSUM,OPACON,OPACRAY,SCONT,SCONRAY,TEM, &
&                  XCRED,XCBLUE,TGRAD,XNUE0,VDOP,TEMP,XNE,PROFROT, &
&                  VSINI,DELTAARR)
!
USE nlte_type
USE nlte_dim
USE fund_const, ONLY: CLIGHT,PI
USE formalsol_var, ONLY: XNEMIN,INVERTED,FILE
IMPLICIT NONE
!
!
!     .. parameters ..
REAL(DP), PARAMETER :: TAUCMIN = 0.03 !XNEMIN CHOSEN FROM THIS VALUE

INTEGER(I4B), PARAMETER :: ND1=ID_NDEPT,NDM=ID_DEPFI,NF=ID_NFOBS,LTO1=2*NDM  
INTEGER(I4B), PARAMETER :: NC1=ID_CORES  
INTEGER(I4B), PARAMETER :: NP1=NC1+2+4*ID_NOCOR,NFMAX=ID_NFESC  
!     ..
!     .. scalar arguments ..
REAL(DP) ::  DELTA,RMAX,TEM,TGRAD,VMAX,VSINI,XCBLUE,XCRED, &
&                 XNUE0
INTEGER(I4B) ::  NB,NC,ND,NFOBS,NP,NSUM,MAXNCOMP  
LOGICAL ESCAT,OPTIOV  
!     ..
!     .. array arguments ..
REAL(DP) ::  AIC(NF,2),OPACON(NDM,2),OPACRAY(LTO1,2), &
&                 OPAL(NDM,NB),OPALRAY(LTO1,NB),P(NP1),PROFABS(NF), &
&                 PROFEM(NF),PROFILE(NF),PROFROT(NF),R(NDM), &
&                 R1(ND1),SCONRAY(LTO1,2),SCONT(NDM,2), &
&                 SLINE(NDM,NB),SRAY(LTO1,NB),VTURBRAY(LTO1),TEMP(NDM),V(NDM), &
&                 VDOP(NB),X0(NF),XCMF(LTO1,NB), &
&                 XMAX(NB),XMAXDOP(NB),XMAXDOP_MIN(NB), &
&                 XNE(NDM),Z(NDM,NP1),ZG(NP1,NF),ZRAY(LTO1),DELTAARR(NB)
INTEGER(I4B) ::  LMAX(NP1),LTOT(NP1),INVER(ND1)
!NEW-JS APR 2018: PRINTING INTENSITIES AS WELL, SEE BELOW
REAL(DP) :: IEMER_ABS(NFOBS,NP), IEMER_EM(NFOBS,NP), FLUX_WEIGHTS(NFOBS,NP), &
&                FLUX_WEIGHTS1(NFOBS,NP) !,PRAY(JP) 
!     ..
!     .. local scalars ..
REAL(DP) ::  AEQUIT,DD,DELP,EMINT,EMINT1,ERRMAX, &
&                 RELEM,TAUC,TAUK,VPLUS,VVDOP,W,W1,WW,WW1,XCMFMIN,XICOR, &
&                 XKW,XN,XXLAM,XXLAM1,XXX0,Z2
INTEGER(I4B) ::  I,IJP,IK, &
&        IREST,ISTART,IZB,IZB1,IZNEB,IZNER,IZR,IZR2,J,JJ,JP,K,L, &
&        LEMIN,LM,LM1,LTAUMAX,NDELT,III
LOGICAL CORE,FIRST,PUREDOP  
CHARACTER LINE*20  
!     ..
!     .. local arrays ..
REAL(DP) ::  CONABS(NF),CONEM1(NF),CONEM2(NF),FSCON(LTO1), &
&                 PROFCON(NF),PROFRED(NF),S1PABS(NF),S1PEM(NF), &
&                 S2PEM(NF),TAU(NF),TAUCON(LTO1),TEMPRAY(LTO1), &
&                 XNERAY(LTO1)
!     ..
!     .. external subroutines ..
EXTERNAL FORMACON,OBSFRAM,OBSFRAM1,OBSFRAM2,PREPRAY,RCONV,XGRID  
!     ..
!     .. intrinsic functions ..
INTRINSIC ABS,DBLE,EXP,LOG10,MAX,MIN,SQRT  
!     ..

IF (ND.GT.NDM) STOP ' ND > NDM IN FORMAL'  
IF (NF.NE.NFOBS) STOP ' NF .NE. NFOBS IN FORMAL'  

VVDOP = 1.D50  

DO I = 1,NB  
     VVDOP = MIN(VVDOP,VDOP(I)) !calculated with VTURB=VTURBMIN 
END DO  
!
!---- open output file for resonant zones with tau=1
!
!      open(20,file=TRIM(FILE)//'/tauuno_'//line,status='unknown',form='formatted')
!
!-----sets up grid variables
!
!     calculation of the p-grid
!
DO JP = 1,NC  
     P(JP) = .99D0*DBLE(JP-1)/DBLE(NC-1)  
END DO  

P(NC+1) = 1.D0  
P(NC+2) = 1.D0 + 1.D-4  

NDELT = (NP- (NC+2))/4  

IREST = (4*NDELT+1-ND1)  
IF (IREST.EQ.0) THEN  
     ISTART = 1  

ELSE IF (IREST.EQ.2) THEN  
     ISTART = -1  

ELSE  
     STOP ' SOMETHING WRONG WITH NUMBER OF P-RAYS'  
END IF  

DO JP = 1,NDELT  
     IJP = NC + 2 + 4*JP  
     P(IJP) = R1(ND1+1- (ISTART+4*JP))  
     DELP = P(IJP) - P(IJP-4)  
     DD = DELP/4.D0  
     DO J = 1,3  
          P(IJP-J) = P(IJP) - J*DD  
     END DO
END DO  

P(NP) = RMAX  
!
!     calculation of the z-grid
!
DO JP = 1,NP - 1  
     DO L = 1,ND  
          Z2 = R(L)**2 - P(JP)**2  

          IF (Z2.GE.-1.D-10) THEN  
               IF (Z2.LT.0.D0) Z2 = 0.D0  
               Z(L,JP) = SQRT(Z2)  
               LMAX(JP) = L  
          ELSE  
               EXIT  
          END IF  
     END DO
END DO

Z(1,NP) = 0.D0  
LMAX(NP) = 1  

VPLUS = 0.D0  
DO I = 1,NB  
     VPLUS = MAX(VPLUS,XMAX(I))  
END DO  

PRINT *  
PRINT *,' STARK/DOPPLER BROADENING EFFECTIVE FOR XMAX = ',VPLUS  
PRINT *  

CALL XGRID(DELTA,NFOBS,XMAXDOP,XMAXDOP_MIN,X0,AIC,OPACON,ND,TEM,TGRAD,XCRED, &
&           XCBLUE,XNUE0,VMAX,NB,VPLUS,ESCAT)

AEQUIT = 0.D0  
ERRMAX = 0.D0  

DO K = 1,NFOBS  
     TAU(K) = 0.D0  
     S1PABS(K) = 0.D0  
     S1PEM(K) = 0.D0  
     S2PEM(K) = 0.D0  
     CONABS(K) = 0.D0  
     CONEM1(K) = 0.D0  
     CONEM2(K) = 0.D0  
END DO  
!
!-----find XNEMIN from continuum depth at TAUCMIN (minimum value 10.5)
!-----tauc calculated assuming red opacity is larger
!
TAUC=0.
DO I=1,ND-1    
  TAUC=TAUC+0.5*(OPACON(I,1)+OPACON(I+1,1))*(R(I)-R(I+1))
  IF (TAUC.GT.TAUCMIN) GOTO 105
END DO
STOP ' TAUC > TAUCMIN NOT FOUND!'

105 XNEMIN=MAX(3.2D10,XNE(I-1)) 
XNEMIN=3.2D10

110 CONTINUE

!JS-NEW APR 2018: Intensiyt output, see below
IEMER_ABS = 0.0D0 
IEMER_EM = 0.0D0
FLUX_WEIGHTS = 0.0D0
FLUX_WEIGHTS1 = 0.0D0
!
JPLOOP: DO JP = 1,NP - 1  
!JPLOOP: DO JP = 1,1  
     LM = LMAX(JP)  
     CORE = LM .EQ. ND  
     FIRST = .TRUE.  

     KLOOP: DO K = 1,NFOBS  
!     KLOOP: DO K = 1,1  
!     KLOOP: DO K = 34,34  

          XICOR = AIC(K,1) + Z(LM,JP)*AIC(K,2)  
          ZG(JP,K) = -10.D0*RMAX  

          CALL PREPRAY(Z(1,JP),P,NP,NB,JP,X0(K),LTOT,LM,CORE,W,W1, &
           V, VVDOP,R,OPAL,SLINE,OPALRAY,SRAY,ZRAY,VTURBRAY,XCMF, DELTA, &
           XMAXDOP,IZR,IZR2,IZB1,IZB,IZNER,IZNEB, FIRST,OPTIOV, &
           NSUM,SCONT,SCONRAY,OPACON, OPACRAY,TEMP,TEMPRAY,XNE, &
           XNERAY,ESCAT,DELTAARR)
          !JS-NEW APR 2018
          !PRAY(JP) = P 
!
!           correction to achim's integration weights is necessary to
!           obtain absolute fluxes
!
!           ww=w*2.d0*pi*0.5d0/(r(1)*r(1))
!           ww1=w1*2.d0*pi*0.5d0/(r(1)*r(1))
!
          WW = W*PI/ (R(1)*R(1))  
          WW1 = W1*PI/ (R(1)*R(1))  
!
!---- continuum can be included in three differents ways:
!     1) keeping opa and source function constant over the whole
!        frequency range (twice v_inf);
!     2) allowing it to vary over the ray for every observer frequency;
!     3) allowing it to vary over the ray but calculating it only with
!        a certain observer frequency (the central one, for example).
!     to get a faster code, option 3) has been taken. if desired,
!     option 2) can be considered if the 'if(first)' condition is
!     avoided, and the line masked with ccc (in earlier versions)
!     is uncommented.
          
          XXX0 = 0.D0  

          IF (ESCAT) XXX0 = X0(K)  

          IF (FIRST .OR. ESCAT) CALL FORMACON(TAUCON,LTAUMAX, &
           FSCON, OPACRAY,SCONRAY,ZRAY,LTOT(JP),XXX0,XCRED,XCBLUE, &
           ESCAT)
!     check for xnemin
           
          IF (FIRST .AND. JP.EQ.1) THEN  
               DO JJ = 1,LTOT(JP)  
                    IF (TAUCON(JJ).GT.TAUCMIN) GOTO 130
               END DO
               STOP ' TAUC > TAUCMIN  NOT FOUND'  

  130          CONTINUE  

               PRINT *,' LOG NE = ',LOG10(XNE(JJ)),' AT TAUC = ',TAUCON(JJ)
               PRINT *,' CHOSEN LOG NEMIN = ',LOG10(XNEMIN)  
               IF (XNEMIN.GT.XNE(JJ) .AND. XNEMIN.NE.3.2D10) THEN  
                 PRINT *,' SOMETHING WRONG WITH XNEMIN !!!'
!     reset xnemin
                 XNEMIN = 3.2D10
                 GOTO 110
               ELSE
                 PRINT*,' XNEMIN OK!'
               ENDIF  
          END IF  
          FIRST = .FALSE.  
          LM1 = LTOT(JP)  
!
!---- check whether stark zones must me considered
!
          IF (IZNER.EQ.1 .AND. IZNEB.EQ.1) THEN  
               PUREDOP = .TRUE.  
          ELSE  
               PUREDOP = .TRUE.  
               DO IK = 1,NB  
                    XCMFMIN = MIN(ABS(XCMF(IZNER,IK)), ABS(XCMF(IZNEB,IK)))
                    IF (XCMFMIN.LT.XMAX(IK)) PUREDOP = .FALSE.  
               END DO
          END IF  

          IF (((IZR.EQ.1.AND.IZB.EQ.1).OR. &
&           (IZR.EQ.LM1.AND.IZB.EQ.LM1)) .AND. PUREDOP) THEN
               IF (CORE) THEN  
                    EMINT = FSCON(1)  
                    EMINT1 = XICOR*EXP(-TAUCON(LM1))  
               ELSE  
                    EMINT = FSCON(1)  
                    EMINT1 = 0.D0  
               END IF  

          ELSE IF (OPTIOV) THEN  

               IF (PUREDOP) THEN  
!
!-----only doppler-zones in the wind
!
                    CALL OBSFRAM1(LM1,NB,CORE,EMINT,OPALRAY,SRAY, &
                     ZRAY,VTURBRAY,XCMF,IZR,IZB,EMINT1,ZG(JP,K),TAUK, &
                     XICOR,TAUCON,FSCON,TEMPRAY,XNERAY, LTAUMAX)
!
!-----doppler zones in the wind + stark broadening zone, or NB > 2
!
               ELSE  
                    CALL OBSFRAM(LM1,NB,CORE,EMINT,OPALRAY,SRAY, &
                     ZRAY,VTURBRAY,XCMF,IZR,IZB,IZNER,IZNEB,EMINT1, &
                     ZG(JP,K),TAUK,XICOR,TAUCON,FSCON,TEMPRAY, &
                     XNERAY,LTAUMAX)

               END IF  

               IF (CORE .AND. JP.EQ.1) TAU(K) = TAUK  
               IF (.NOT.CORE .AND. JP.EQ.NC+2) TAU(K) = TAUK  

          ELSE  
!
!-----separated doublets: only doppler broadening!
!
               DO IK = 1,NB  
                 IF (XMAX(IK).NE.XMAXDOP(IK)) STOP &
&                ' SOMETHING WRONG WITH XMAX FOR DOPPLER-DOUBLETS!'
               END DO

               CALL OBSFRAM2(LM1,NB,CORE,EMINT,OPALRAY,SRAY,ZRAY,VTURBRAY, &
                XCMF,IZR,IZR2,IZB1,IZB,EMINT1,ZG(JP,K),TAUK,XICOR, &
                TAUCON,FSCON,TEMPRAY,XNERAY,LTAUMAX)

               IF (CORE .AND. JP.EQ.1) TAU(K) = TAUK  
               IF (.NOT.CORE .AND. JP.EQ.NC+2) TAU(K) = TAUK  
          END IF  
!
!           write(20,*) zg(jp,k)
!

          !JS-NEW APR 2018: Adding variables for printing intensities
          !as functions of frequency and p-ray, for Michael's project
          !using surface intensity maps.
          IEMER_ABS(K,JP) = EMINT1 
          IEMER_EM(K,JP) = EMINT
          FLUX_WEIGHTS(K,JP) = WW 
          FLUX_WEIGHTS1(K,JP) = WW1 
          !----------------------------------

          S1PABS(K) = S1PABS(K) + WW*EMINT1  
          S1PEM(K) = S1PEM(K) + WW*EMINT  
          IF (CORE) CONABS(K) = CONABS(K) + WW*XICOR*EXP(-TAUCON(LM1))
          CONEM1(K) = CONEM1(K) + WW*FSCON(1)
          
          IF (JP.GE.NC+2) THEN  
               S2PEM(K) = S2PEM(K) + WW1*EMINT  
               CONEM2(K) = CONEM2(K) + WW1*FSCON(1)  
          END IF  

!          print*,S1PEM(K),WW,IEMER_ABS(K,JP)  
     END DO KLOOP

END DO JPLOOP  

DO K = 1,NFOBS  

     PROFABS(K) = S1PABS(K)  
     PROFEM(K) = S1PEM(K) + S2PEM(K)  

     IF (S2PEM(K).EQ.0. .AND. PROFEM(K).EQ.0.) THEN  
          RELEM = 0.D0  
     ELSE  
          RELEM = ABS(S2PEM(K)/PROFEM(K))  
     END IF  

     ERRMAX = MAX(ERRMAX,RELEM)  
     PROFILE(K) = PROFABS(K) + PROFEM(K)  
     PROFCON(K) = CONEM1(K) + CONEM2(K) + CONABS(K)  
     PROFRED(K) = PROFILE(K)/PROFCON(1)  
!
!     now: profile contains wavelengths
!
     PROFILE(K) = 1.D8/XNUE0/ (X0(K)*VMAX/CLIGHT+1.D0)  
END DO  
!
!     rot. convolution
!
!TEST JS ! - works fine, doesn't change anything 
CALL RCONV(PROFILE,PROFRED,PROFROT,NFOBS,VSINI)  
!PROFROT = PROFRED 
!
!     conversion to wavelength in air, xkw is wavenumber in microns,
!     formula a la lang, is done in preformal/hallcl
!
DO K = 1,NFOBS  
   XXLAM = PROFILE(K)  

   IF (K.NE.1) THEN  
      XXLAM1 = PROFILE(K-1)  
      AEQUIT = AEQUIT+(.5D0*(PROFROT(K-1)+PROFROT(K))-1.D0)*(XXLAM-XXLAM1)
   END IF
   
   WRITE (*,FMT=9000) K,X0(K),XXLAM,PROFCON(K),PROFRED(K),PROFROT(K)
   WRITE (2,FMT=9000) K,X0(K),XXLAM,PROFCON(K),PROFRED(K),PROFROT(K)
END DO

!JS-NEW APR 2018: ADDING OUTPUT FOR INTENSITES, SEE ABOVE
DO K = 1,NFOBS
   XXLAM = PROFILE(K)  
   DO JP = 1,NP
      !JS-NEW: Something weird with format here !  hack for now...
      IF (IEMER_ABS(K,JP).LT.1.D-15) IEMER_ABS(K,JP)=0.0D0
      IF (IEMER_EM(K,JP).LT.1.D-15) IEMER_EM(K,JP)=0.0D0
      WRITE (22,FMT=10000) JP,P(JP),K,XXLAM,IEMER_ABS(K,JP),IEMER_EM(K,JP),FLUX_WEIGHTS(K,JP),FLUX_WEIGHTS1(K,JP)!,PROFCON(K) 
   ENDDO
ENDDO
10000 FORMAT (1X,I3,F14.6,1X,I3,F14.6,4(2X,G17.6))  
!------------------------------
!
!#    pessimistic error (from delta i/i with step halfing)
!
ERRMAX = 15.D0*ERRMAX  
IF (ERRMAX.GT..03) PRINT *,' WARNING!!! WARNING!!! WARNING!!! WARNING!!!'

PRINT *  
PRINT *,' MAXIMUM ERROR IN ANGULAR INTEGRATION ',ERRMAX  
PRINT *  

IF (ERRMAX.GT..03) THEN  
     PRINT *,' WARNING!!! WARNING!!! WARNING!!! WARNING!!!'  
     PRINT *  
END IF  

PRINT *,' OBSERVED AEQUIVALENT WIDTH = ',AEQUIT  
PRINT *  
WRITE (2,FMT=*) AEQUIT  


DO IK=1,NB
  DO L=1,ND1
    IF(INVERTED(L,IK).NE.0) GOTO 8990
  ENDDO
ENDDO

RETURN

8990 PRINT*,' WARNING: INVERTED LEVELS FOUND!!!'
DO IK=1,NB
  INVER=0
  K=0
  DO L=1,ND1
  IF(INVERTED(L,IK).NE.0) THEN
    K=K+1
    INVER(K)=L
  ENDIF 
  ENDDO
  IF(K.NE.0) THEN
    WRITE(*,9001) IK,(INVER(L),L=1,K)
    WRITE(2,9002) IK,(INVER(L),L=1,K)
  ENDIF
ENDDO
PRINT*

RETURN  

 9000 FORMAT (1X,I3,5 (2X,G14.6))  
 9001 FORMAT ('  FOR COMPONENT ',I2,' AT L = ',101(I2))
 9002 FORMAT ('  INVERSION FOR COMPONENT ',I2,' AT L = ',101(I2))

END
!
!-----------------------------------------------------------------------
!

SUBROUTINE FORMACON(TAUCON,LTAUMAX,FSCON,OPACRAY,SCONRAY,ZRAY, &
&                    LTOTAL,X0,XCRED,XCBLUE,ESCAT)
!
USE nlte_type
USE nlte_dim
USE formalsol_var, ONLY: XCMFP,VZRP,WP,WP1  
IMPLICIT NONE
!
!---- calculates comoving continuum opacity and source function over
!---- the ray. it calculates also the pure continuum formal solution
!---- (that is, the so called 'f_s(tau-cont)' for every ray.
!---- a linear expansion of source function is taken over every
!---- subinterval
!-----regard, that in the elscat case the source function was already
!-----interpolated to cmf frequencies in prepray
!-----
!
!     .. parameters ..
INTEGER(I4B), PARAMETER :: NDM=ID_DEPFI,LTO1=2*NDM,NC=ID_CORES  
INTEGER(I4B), PARAMETER :: NP1=NC+2+4*ID_NOCOR  
REAL(DP), PARAMETER :: TAUMAX=10.D0  
!     ..
!     .. scalar arguments ..
REAL(DP) ::  X0,XCBLUE,XCRED  
INTEGER(I4B) ::  LTAUMAX,LTOTAL  
LOGICAL ESCAT  
!     ..
!     .. array arguments ..
REAL(DP) ::  FSCON(LTO1),OPACRAY(LTO1,2),SCONRAY(LTO1,2), &
&                 TAUCON(LTO1),ZRAY(LTO1)
!     ..
!     .. local scalars ..
REAL(DP) ::  DT,DX0,DX00,DXC,E0,E1,W1,W2  
INTEGER(I4B) ::  I
!     ..
!     .. local arrays ..
REAL(DP) ::  OPAC(LTO1),SCO(LTO1)  
!     ..
!     .. intrinsic functions ..
INTRINSIC EXP  
!     ..

DXC = XCBLUE - XCRED  

IF (ESCAT) THEN  

     DO I = 1,LTOTAL  
!
!    changed as elscat assumes constant opacities
!
          DX00 = (0.D0+XCMFP(I)-XCRED)/DXC  
          OPAC(I) = OPACRAY(I,1) + (OPACRAY(I,2)-OPACRAY(I,1))*DX00
          SCO(I) = SCONRAY(I,1)  
     END DO 

ELSE  
     DO I = 1,LTOTAL  
!
!    changed as elscat assumes constant opacities
!
          DX00 = (0.D0+XCMFP(I)-XCRED)/DXC  
          DX0 = (X0+XCMFP(I)-XCRED)/DXC  
!    if strongly varying cont. opacities, the next statement needs to
!    be changed (OPAC with DX0 as well)
          OPAC(I) = OPACRAY(I,1) + (OPACRAY(I,2)-OPACRAY(I,1))*DX00
          SCO(I) = SCONRAY(I,1) + (SCONRAY(I,2)-SCONRAY(I,1))*DX0  
     END DO 

END IF  

TAUCON(1) = 0.D0  

DO I = 2,LTOTAL  
     TAUCON(I) = TAUCON(I-1) + 0.5D0* (OPAC(I-1)+OPAC(I))* &
      (ZRAY(I-1)-ZRAY(I))
!     print*,i,taucon(i),opac(i-1),opac(i),zray(i-1),zray(i)
     IF (TAUCON(I).LT.TAUMAX) LTAUMAX = I  
END DO  

LTAUMAX = I + 1  
FSCON(LTOTAL) = 0.D0  

DO I = LTOTAL - 1,1,-1  
     DT = TAUCON(I+1) - TAUCON(I)  

     IF (DT.LT.1.D-8) THEN  
          W1 = .5D0*DT  
          W2 = .5D0*DT  
     ELSE  
          E0 = EXP(-DT)  
          E1 = (1.D0-E0)/DT  
          W1 = 1.D0 - E1  
          W2 = E1 - E0  
     END IF  

     FSCON(I) = FSCON(I+1) + EXP(-TAUCON(I))* (SCO(I)*W1+SCO(I+1)*W2)
END DO  
RETURN  
END
!
!-----------------------------------------------------------------------
!
SUBROUTINE OBSFRAM(LM,NB,CORE,EMINT,OPALRAY,SRAY,ZRAY,VTURBRAY,XCMF,IZR, &
&                   IZB,IZNER,IZNEB,EMINT1,Z1,TAU,XICOR,TAUCON, &
&                   FSCON,TEMPRAY,XNERAY,LTAUMAX)
!
USE nlte_type
USE nlte_dim
IMPLICIT NONE
!
!
!     integration of the emergent intensities in the observers frame
!     for singlets and overlapping multiplets:
!     pure doppler-case for ne < nemin, stark-broadening inside
!     "stark-broadening zone from izner...izneb
!
!     seven different pathes possible:two discontinuous, five conitnuous
!
!     path 1: izr-izb, izner-izneb
!     path 2: izner-izneb, izr-izb
!     path 3: izr-izner-izb-izneb
!     path 4: izr-izner-izneb-izb
!     path 5: izner-izr-izb-izneb
!     path 6: izner-izr-izneb-izb
!     path 7  izner-izneb for (izr and izb) = (1 or lm)
!
!     tauout: resonance zones of tau=tauout stored in z1
!
!
!---- initialization
!
!     .. parameters ..
INTEGER(I4B), PARAMETER :: NDM=ID_DEPFI,LTOM=2*NDM  
REAL(DP), PARAMETER :: TAUMAX=10.D0,TAUOUT=1.D0  
!     ..
!     .. scalar arguments ..
REAL(DP) ::  EMINT,EMINT1,TAU,XICOR,Z1  
INTEGER(I4B) ::  IZB,IZNEB,IZNER,IZR,LM,LTAUMAX,NB  
LOGICAL CORE  
!     ..
!     .. array arguments ..
REAL(DP) ::  FSCON(LTOM),OPALRAY(LTOM,NB),SRAY(LTOM,NB), &
&                 TAUCON(LTOM),TEMPRAY(LTOM),XCMF(LTOM,NB), &
&                 XNERAY(LTOM),ZRAY(LTOM),VTURBRAY(LTOM)
!     ..
!     .. local scalars ..
REAL(DP) ::  DT,FBLUE,FQ,FRED,OPABLUE,OPAI,OPAIS,OPARED,PHII, &
&                 SBLUE,SI,SIS,SRED,TAUBLU,TAURED,XI,ZI,ZINEW
INTEGER(I4B) ::  IEND,IEND1,IEND2,ISTA,ISTA1,ISTA2,K,L  
LOGICAL FLAG,OPTOUT  
!     ..
!     .. external functions ..
REAL(DP) ::  EXPUNO,PERFIL1  
EXTERNAL EXPUNO,PERFIL1  
!     ..
!     .. intrinsic functions ..
INTRINSIC EXP,MAX,MIN  
!     ..

OPTOUT = .TRUE.  
EMINT = .0D0  
TAU = .0D0  
SIS = 0.D0  

OPAIS = 0.D0  
!
!---- decision, whether continuous or discontinuous pathes
!

IF (IZR.EQ.1 .AND. IZB.EQ.1 .OR. IZR.EQ.LM .AND. IZB.EQ.LM) THEN  

!--path7
     ISTA = IZNER  
     IEND = IZNEB  

!--path1
ELSE IF (IZB.LT.IZNER) THEN  
     ISTA1 = IZR  
     IEND1 = IZB  
     ISTA2 = IZNER  
     IEND2 = IZNEB  
     GO TO 50  

!--path2
ELSE IF (IZNEB.LT.IZR) THEN  
     ISTA1 = IZNER  
     IEND1 = IZNEB  
     ISTA2 = IZR  
     IEND2 = IZB  
     GO TO 50  

!-- remaining pathes
ELSE  
     ISTA = MIN(IZR,IZNER)  
     IEND = MAX(IZB,IZNEB)  
END IF  
!
!-- continous path
!
L = ISTA  
ZI = ZRAY(L)  

DO K = 1,NB  
     XI = XCMF(L,K)  
     PHII = PERFIL1(K,XI,TEMPRAY(L),XNERAY(L),VTURBRAY(L),FLAG)  
     IF (FLAG) CYCLE  
     OPAI = PHII*OPALRAY(L,K)  
     SI = OPAI*SRAY(L,K)  
     OPAIS = OPAIS + OPAI  
     SIS = SIS + SI  
END DO  

OPARED = OPAIS  
TAURED = TAU  

IF (OPAIS.NE.0.D0) THEN  
     SRED = SIS/OPAIS  
     FRED = SRED*EXP(-TAUCON(L)) - FSCON(L)  
ELSE  
     FRED = -FSCON(L)  
END IF  

L = L + 1  

   20 CONTINUE  

SIS = 0.D0  
OPAIS = 0.D0  
ZINEW = ZRAY(L)  

DO K = 1,NB  
     XI = XCMF(L,K)  
     PHII = PERFIL1(K,XI,TEMPRAY(L),XNERAY(L),VTURBRAY(L),FLAG)  
     IF (FLAG) CYCLE  
     OPAI = PHII*OPALRAY(L,K)  
     SI = OPAI*SRAY(L,K)  
     OPAIS = OPAIS + OPAI  
     SIS = SIS + SI  
END DO  

OPABLUE = OPAIS  

IF (OPAIS.NE.0.D0) THEN  
     SBLUE = SIS/OPAIS  
     FBLUE = SBLUE*EXP(-TAUCON(L)) - FSCON(L)  
ELSE  
     FBLUE = -FSCON(L)  
END IF  

DT = .5D0* (OPARED+OPABLUE)* (ZI-ZINEW)  
TAUBLU = TAURED + DT  

IF (DT.EQ.0.D0) THEN  
!     if(nb.eq.1) print*,' warning!! error in resonance zone - obsfram'
     GO TO 40  
END IF  
!
!     print*,zi,'  ',zinew,'  ',tau,'  ',dt,'  macrogrid'
!
IF (DT.GT.1.D0) THEN  
     FQ = FRED  
ELSE  
     FQ = .5D0* (FBLUE+FRED)  
END IF  


EMINT = EMINT + EXP(-TAURED)*FQ*EXPUNO(DT)  

IF (EMINT.GT.10.D0) STOP ' EMINT > 10 IN OBSFRAM'  

TAU = TAU + DT  

IF (OPTOUT .AND. TAU.GE.TAUOUT) THEN  
     Z1 = ZINEW  
     OPTOUT = .FALSE.  
END IF  

   40 CONTINUE  

IF (TAU.GT.TAUMAX .OR. L.EQ.IEND .OR. L.EQ.LTAUMAX) THEN  
     EMINT1 = 0.D0  
     IF (CORE) EMINT1 = XICOR*EXP(-TAU-TAUCON(LM))  
     EMINT = EMINT + FSCON(1)  
     RETURN  
END IF  

OPARED = OPABLUE  
FRED = FBLUE  
TAURED = TAUBLU  
ZI = ZINEW  
L = L + 1  
GO TO 20  
!
!-- discontinous paths
!
!-- 1st inegration range
!
   50 CONTINUE  
L = ISTA1  
ZI = ZRAY(L)  

DO K = 1,NB  
     XI = XCMF(L,K)  
     PHII = PERFIL1(K,XI,TEMPRAY(L),XNERAY(L),VTURBRAY(L),FLAG)  
     IF (FLAG) CYCLE  
     OPAI = PHII*OPALRAY(L,K)  
     SI = OPAI*SRAY(L,K)  
     OPAIS = OPAIS + OPAI  
     SIS = SIS + SI  
END DO  

OPARED = OPAIS  
TAURED = TAU  

IF (OPAIS.NE.0.D0) THEN  
     SRED = SIS/OPAIS  
     FRED = SRED*EXP(-TAUCON(L)) - FSCON(L)  
ELSE  
     FRED = -FSCON(L)  
END IF  

L = L + 1  

   70 CONTINUE  
SIS = 0.D0  
OPAIS = 0.D0  
ZINEW = ZRAY(L)  

DO K = 1,NB  
     XI = XCMF(L,K)  
     PHII = PERFIL1(K,XI,TEMPRAY(L),XNERAY(L),VTURBRAY(L),FLAG)  
     IF (FLAG) CYCLE  
     OPAI = PHII*OPALRAY(L,K)  
     SI = OPAI*SRAY(L,K)  
     OPAIS = OPAIS + OPAI  
     SIS = SIS + SI  
END DO  

OPABLUE = OPAIS  

IF (OPAIS.NE.0.D0) THEN  
     SBLUE = SIS/OPAIS  
     FBLUE = SBLUE*EXP(-TAUCON(L)) - FSCON(L)  
ELSE  
     FBLUE = -FSCON(L)  
END IF  

DT = .5D0* (OPARED+OPABLUE)* (ZI-ZINEW)  
TAUBLU = TAURED + DT  

IF (DT.EQ.0.D0) THEN  
!     if(nb.eq.1) print*,' warning!! error in resonance zone - obsfram'
     GO TO 90  
END IF  
!
!     print*,zi,'  ',zinew,'  ',tau,'  ',dt,'  macrogrid'
!
IF (DT.GT.1.D0) THEN  
     FQ = FRED  
ELSE  
     FQ = .5D0* (FBLUE+FRED)  
END IF  


EMINT = EMINT + EXP(-TAURED)*FQ*EXPUNO(DT)  

IF (EMINT.GT.10.D0) STOP ' EMINT > 10 IN OBSFRAM'  

TAU = TAU + DT  
IF (OPTOUT .AND. TAU.GE.TAUOUT) THEN  
     Z1 = ZINEW  
     OPTOUT = .FALSE.  
END IF  

IF (TAU.GT.TAUMAX .OR. L.EQ.LTAUMAX) THEN  
     EMINT1 = 0.D0  
     EMINT = EMINT + FSCON(1)  
     RETURN  
END IF  

   90 CONTINUE  
IF (L.EQ.IEND1) GO TO 100  

OPARED = OPABLUE  
FRED = FBLUE  
TAURED = TAUBLU  
ZI = ZINEW  
L = L + 1  
GO TO 70  
!
!------------------------------
!
!-- 2nd integration range!
!
  100 CONTINUE  

IF (ISTA2.EQ.LM) THEN  
     IF (IEND2.NE.LM) STOP ' ERROR IN OBSFRAM'  
     EMINT1 = 0.D0  
     IF (CORE) EMINT1 = XICOR*EXP(-TAU-TAUCON(LM))  
     EMINT = EMINT + FSCON(1)  
     RETURN  
END IF  

TAURED = TAU  
L = ISTA2  
ZI = ZRAY(L)  

DO K = 1,NB  
     XI = XCMF(L,K)  
     PHII = PERFIL1(K,XI,TEMPRAY(L),XNERAY(L),VTURBRAY(L),FLAG)  
     IF (FLAG) CYCLE  
     OPAI = PHII*OPALRAY(L,K)  
     SI = OPAI*SRAY(L,K)  
     OPAIS = OPAIS + OPAI  
     SIS = SIS + SI  
END DO  

OPARED = OPAIS  
TAURED = TAU  

IF (OPAIS.NE.0.D0) THEN  
     SRED = SIS/OPAIS  
     FRED = SRED*EXP(-TAUCON(L)) - FSCON(L)  
ELSE  
     FRED = -FSCON(L)  
END IF  

L = L + 1  

  120 CONTINUE  

SIS = 0.D0  
OPAIS = 0.D0  
ZINEW = ZRAY(L)  

DO K = 1,NB  
     XI = XCMF(L,K)  
     PHII = PERFIL1(K,XI,TEMPRAY(L),XNERAY(L),VTURBRAY(L),FLAG)  
     IF (FLAG) CYCLE  
     OPAI = PHII*OPALRAY(L,K)  
     SI = OPAI*SRAY(L,K)  
     OPAIS = OPAIS + OPAI  
     SIS = SIS + SI  
END DO  

OPABLUE = OPAIS  

IF (OPAIS.NE.0.D0) THEN  
     SBLUE = SIS/OPAIS  
     FBLUE = SBLUE*EXP(-TAUCON(L)) - FSCON(L)  
ELSE  
     FBLUE = -FSCON(L)  
END IF  

DT = .5D0* (OPARED+OPABLUE)* (ZI-ZINEW)  
TAUBLU = TAURED + DT  

IF (DT.EQ.0.D0) THEN  
!     if(nb.eq.1) print*,' warning!! error in resonance zone - obsfram'
     GO TO 140  
END IF  
!
!     print*,zi,'  ',zinew,'  ',tau,'  ',dt,'  macrogrid'
!
IF (DT.GT.1.D0) THEN  
     FQ = FRED  
ELSE  
     FQ = .5D0* (FBLUE+FRED)  
END IF  


EMINT = EMINT + EXP(-TAURED)*FQ*EXPUNO(DT)  

IF (EMINT.GT.10.D0) STOP ' EMINT > 10 IN OBSFRAM'  

TAU = TAU + DT  
IF (OPTOUT .AND. TAU.GE.TAUOUT) THEN  
     Z1 = ZINEW  
     OPTOUT = .FALSE.  
END IF  

  140 CONTINUE  

IF (TAU.GT.TAUMAX .OR. L.EQ.IEND2 .OR. L.EQ.LTAUMAX) THEN  
     EMINT1 = 0.D0  
     IF (CORE) EMINT1 = XICOR*EXP(-TAU-TAUCON(LM))  
     EMINT = EMINT + FSCON(1)  
     RETURN  
END IF  

OPARED = OPABLUE  
FRED = FBLUE  
TAURED = TAUBLU  
ZI = ZINEW  
L = L + 1  
GO TO 120  

END
!
!-----------------------------------------------------------------------
!
SUBROUTINE OBSFRAM1(LM,NB,CORE,EMINT,OPALRAY,SRAY,ZRAY,VTURBRAY,XCMF,IZR, &
&                    IZB,EMINT1,Z1,TAU,XICOR,TAUCON,FSCON,TEMPRAY, &
&                    XNERAY,LTAUMAX)
!
USE nlte_type
USE nlte_dim
IMPLICIT NONE
!
!
!     integration of the emergent intensities in the observers frame
!     for singlets and overlapping multiplets, pure doppler-case
!
!     tauout: resonance zones of tau=tauout stored in z1
!
!
!     .. parameters ..
INTEGER(I4B), PARAMETER :: NDM=ID_DEPFI,LTOM=2*NDM  
REAL(DP), PARAMETER :: TAUMAX=10.D0,TAUOUT=1.D0  
!     ..
!     .. scalar arguments ..
REAL(DP) ::  EMINT,EMINT1,TAU,XICOR,Z1  
INTEGER(I4B) ::  IZB,IZR,LM,LTAUMAX,NB  
LOGICAL CORE  
!     ..
!     .. array arguments ..
REAL(DP) ::  FSCON(LTOM),OPALRAY(LTOM,NB),SRAY(LTOM,NB), &
&                 TAUCON(LTOM),TEMPRAY(LTOM),XCMF(LTOM,NB), &
&                 XNERAY(LTOM),ZRAY(LTOM),VTURBRAY(LTOM)
!     ..
!     .. local scalars ..
REAL(DP) ::  DT,FBLUE,FQ,FRED,OPABLUE,OPAI,OPAIS,OPARED,PHII, &
&                 SBLUE,SI,SIS,SRED,TAUBLU,TAURED,XI,ZI,ZINEW
INTEGER(I4B) ::  K,L  
LOGICAL FLAG,OPTOUT  
!     ..
!     .. external functions ..
REAL(DP) ::  EXPUNO,PERFIL1  
EXTERNAL EXPUNO,PERFIL1  
!     ..
!     .. intrinsic functions ..
INTRINSIC EXP  
!     ..

OPTOUT = .TRUE.  

EMINT = .0D0  
TAU = .0D0  

SIS = 0.D0  
OPAIS = 0.D0  
L = IZR  
ZI = ZRAY(L)  

DO K = 1,NB  
     XI = XCMF(L,K)  
     PHII = PERFIL1(K,XI,TEMPRAY(L),XNERAY(L),VTURBRAY(L),FLAG)  
     IF (FLAG) CYCLE  
     OPAI = PHII*OPALRAY(L,K)  
     SI = OPAI*SRAY(L,K)  
     OPAIS = OPAIS + OPAI  
     SIS = SIS + SI  
END DO  

OPARED = OPAIS  
TAURED = TAU  
IF (OPAIS.NE.0.D0) THEN  
     SRED = SIS/OPAIS  
     FRED = SRED*EXP(-TAUCON(L)) - FSCON(L)  
ELSE  
     FRED = -FSCON(L)  
END IF  

L = L + 1  

   20 CONTINUE  

SIS = 0.D0  
OPAIS = 0.D0  
ZINEW = ZRAY(L)  

DO K = 1,NB  
     XI = XCMF(L,K)  
     PHII = PERFIL1(K,XI,TEMPRAY(L),XNERAY(L),VTURBRAY(L),FLAG)  
     IF (FLAG) CYCLE  
     OPAI = PHII*OPALRAY(L,K)  
     SI = OPAI*SRAY(L,K)  
     OPAIS = OPAIS + OPAI  
     SIS = SIS + SI  
END DO  

OPABLUE = OPAIS  
IF (OPAIS.NE.0.D0) THEN  
     SBLUE = SIS/OPAIS  
     FBLUE = SBLUE*EXP(-TAUCON(L)) - FSCON(L)  
ELSE  
     FBLUE = -FSCON(L)  
END IF  

DT = .5D0* (OPARED+OPABLUE)* (ZI-ZINEW)  
TAUBLU = TAURED + DT  

IF (DT.EQ.0.D0) THEN  
!     if(nb.eq.1) print*,' warning!! error in resonance zone - obsfram1'
     GO TO 40  
END IF  
!
!    print*,zi,' ',zinew,' ',tau,' ',dt,'  macrogrid'
!
!      aux1=exp(-taured)-exp(-taublu)
!      aux2=taured*exp(-taured)-taublu*exp(-taublu)
!      auxa=(fblue-fred)/dt
!      auxb=(fred*taublu-fblue*taured)/dt
!
!      emint=emint+aux1*(auxa+auxb)+auxa*aux2
IF (DT.GT.1.D0) THEN  
     FQ = FRED  
ELSE  
     FQ = .5D0* (FBLUE+FRED)  
END IF  


EMINT = EMINT + EXP(-TAURED)*FQ*EXPUNO(DT)  

IF (EMINT.GT.10.D0) STOP ' EMINT > 10 IN OBSFRAM1'  

!     write(*,1001) l,zi,zinew,tau,dt,sblue,fscon(l),taucon(l),fq,emint
!1001 format(i4,2x,2(f8.3,2x),7(e9.3,2x))

TAU = TAU + DT  

IF (OPTOUT .AND. TAU.GE.TAUOUT) THEN  
     Z1 = ZINEW  
     OPTOUT = .FALSE.  
END IF  

   40 CONTINUE  

IF (TAU.GT.TAUMAX .OR. L.EQ.IZB .OR. L.EQ.LTAUMAX) THEN  
     EMINT1 = 0.D0  
     IF (CORE) EMINT1 = XICOR*EXP(-TAU-TAUCON(LM))  
     EMINT = EMINT + FSCON(1)  
!     print*,'emint ',emint
     RETURN  
END IF  

OPARED = OPABLUE  
FRED = FBLUE  
TAURED = TAUBLU  
ZI = ZINEW  
L = L + 1  
GO TO 20  

END
!
!-----------------------------------------------------------------------
!
SUBROUTINE OBSFRAM2(LM,NB,CORE,EMINT,OPALRAY,SRAY,ZRAY,VTURBRAY,XCMF,IZR, &
&                    IZR2,IZB1,IZB,EMINT1,Z1,TAU,XICOR,TAUCON, &
&                    FSCON,TEMPRAY,XNERAY,LTAUMAX)
!
USE nlte_type
USE nlte_dim
IMPLICIT NONE
!
!     integration of the emergent intensities in the observers frame
!     for non-overlapping doublets, pure doppler-case
!
!     tauout: resonance zones of tau=tauout stored in z1
!
!
!     .. parameters ..
INTEGER(I4B), PARAMETER :: NDM=ID_DEPFI,LTOM=2*NDM  
REAL(DP), PARAMETER :: TAUMAX=10.D0,TAUOUT=1.D0  
!     ..
!     .. scalar arguments ..
REAL(DP) ::  EMINT,EMINT1,TAU,XICOR,Z1  
INTEGER(I4B) ::  IZB,IZB1,IZR,IZR2,LM,LTAUMAX,NB  
LOGICAL CORE  
!     ..
!     .. array arguments ..
REAL(DP) ::  FSCON(LTOM),OPALRAY(LTOM,NB),SRAY(LTOM,NB), &
&                 TAUCON(LTOM),TEMPRAY(LTOM),XCMF(LTOM,NB), &
&                 XNERAY(LTOM),ZRAY(LTOM),VTURBRAY(LTOM)
!     ..
!     .. local scalars ..
REAL(DP) ::  DT,FBLUE,FQ,FRED,OPABLUE,OPARED,PHII,SBLUE,SRED, &
&                 TAUBLU,TAURED,XI,ZI,ZINEW
INTEGER(I4B) ::  K,L  
LOGICAL FLAG,OPTOUT  
!     ..
!     .. external functions ..
REAL(DP) ::  EXPUNO,PERFIL1  
EXTERNAL EXPUNO,PERFIL1  
!     ..
!     .. intrinsic functions ..
INTRINSIC EXP  
!     ..

IF(NB.GT.2) STOP' NB > 2 AND OBSFRAM2!'

OPTOUT = .TRUE.  

EMINT = .0D0  
TAU = .0D0  

IF (IZR.EQ.1 .AND. IZR2.EQ.1) GO TO 50  
!
!     red component!!!
!
TAURED = TAU  
L = IZR  
ZI = ZRAY(L)  

K = 2  
XI = XCMF(L,2)  
PHII = PERFIL1(K,XI,TEMPRAY(L),XNERAY(L),VTURBRAY(L),FLAG)  

IF (FLAG) THEN  
     FRED = -FSCON(L)  
     OPARED = 0.D0  
ELSE  
     OPARED = PHII*OPALRAY(L,2)  
     SRED = SRAY(L,2)  
     FRED = SRED*EXP(-TAUCON(L)) - FSCON(L)  
END IF  

   10 CONTINUE  

L = L + 1  

   20 CONTINUE  

ZINEW = ZRAY(L)  

XI = XCMF(L,2)  
PHII = PERFIL1(K,XI,TEMPRAY(L),XNERAY(L),VTURBRAY(L),FLAG)  

IF (FLAG) THEN  
     FBLUE = -FSCON(L)  
     OPABLUE = 0.D0  
ELSE  
     OPABLUE = PHII*OPALRAY(L,2)  
     SBLUE = SRAY(L,2)  
     FBLUE = SBLUE*EXP(-TAUCON(L)) - FSCON(L)  
END IF  

   30 CONTINUE  

DT = .5D0* (OPARED+OPABLUE)* (ZI-ZINEW)  
TAUBLU = TAURED + DT  

IF (DT.EQ.0.D0) THEN  
!     print*,' warning!! error in resonance zone - obsfram2'
     GO TO 40  
END IF  
!
!     print*,zi,'  ',zinew,'  ',tau,'  ',dt,'  macrogrid'
!
!      aux1=exp(-taured)-exp(-taublu)
!      aux2=taured*exp(-taured)-taublu*exp(-taublu)
!      auxa=(fblue-fred)/dt
!      auxb=(fred*taublu-fblue*taured)/dt
!
!      emint=emint+aux1*(auxa+auxb)+auxa*aux2
IF (DT.GT.1.D0) THEN  
     FQ = FRED  
ELSE  
     FQ = 0.5D0* (FRED+FBLUE)  
END IF  


EMINT = EMINT + EXP(-TAURED)*FQ*EXPUNO(DT)  

IF (EMINT.GT.10.D0) STOP ' EMINT > 10 IN OBSFRAM2'  

TAU = TAU + DT  

IF (OPTOUT .AND. TAU.GE.TAUOUT) THEN  
     Z1 = ZINEW  
     OPTOUT = .FALSE.  
END IF  

IF (TAU.GT.TAUMAX .OR. L.EQ.LTAUMAX) THEN  
     EMINT1 = 0.D0  
     EMINT = EMINT + FSCON(1)  
     RETURN  
END IF  

   40 CONTINUE  

IF (L.EQ.IZR2) GO TO 50  

FRED = FBLUE  
TAURED = TAUBLU  
OPARED = OPABLUE  
ZI = ZINEW  
L = L + 1  
GO TO 20  
!
!------------------------------
!
!     blue component!!!
!
   50 CONTINUE  

IF (IZB1.EQ.LM) THEN  
     IF (IZB.NE.LM) STOP ' ERROR IN OBSFRAM2'  
     EMINT1 = 0.D0  
     IF (CORE) EMINT1 = XICOR*EXP(-TAU-TAUCON(LM))  
     EMINT = EMINT + FSCON(1)  
     RETURN  
END IF  

TAURED = TAU  
L = IZB1  
ZI = ZRAY(L)  

K = 1  
XI = XCMF(L,1)  
PHII = PERFIL1(K,XI,TEMPRAY(L),XNERAY(L),VTURBRAY(L),FLAG)  

IF (FLAG) THEN  
     FRED = -FSCON(L)  
     OPARED = 0.D0  
ELSE  
     OPARED = PHII*OPALRAY(L,1)  
     SRED = SRAY(L,1)  
     FRED = SRED*EXP(-TAUCON(L)) - FSCON(L)  
END IF  

   60 CONTINUE  

L = L + 1  

   70 CONTINUE  

ZINEW = ZRAY(L)  

XI = XCMF(L,1)  
PHII = PERFIL1(K,XI,TEMPRAY(L),XNERAY(L),VTURBRAY(L),FLAG)  

IF (FLAG) THEN  
     FBLUE = -FSCON(L)  
     OPABLUE = 0.D0  
ELSE  
     OPABLUE = PHII*OPALRAY(L,1)  
     SBLUE = SRAY(L,1)  
     FBLUE = SBLUE*EXP(-TAUCON(L)) - FSCON(L)  
END IF  

   80 CONTINUE  

DT = .5D0* (OPARED+OPABLUE)* (ZI-ZINEW)  
TAUBLU = TAURED + DT  

IF (DT.EQ.0.D0) THEN  
!     print*,' warning!! error in resonance zone - obsfram2'
     GO TO 90  
END IF  
!
!     print*,zi,'  ',zinew,'  ',tau,'  ',dt,'  macrogrid'
!
!      aux1=exp(-taured)-exp(-taublu)
!      aux2=taured*exp(-taured)-taublu*exp(-taublu)
!      auxa=(fblue-fred)/dt
!      auxb=(fred*taublu-fblue*taured)/dt
!
!      emint=emint+aux1*(auxa+auxb)+auxa*aux2
IF (DT.GT.1.D0) THEN  
     FQ = FRED  
ELSE  
     FQ = 0.5D0* (FRED+FBLUE)  
END IF  


EMINT = EMINT + EXP(-TAURED)*FQ*EXPUNO(DT)  

IF (EMINT.GT.10.D0) STOP ' EMINT > 10 IN OBSFRAM2'  

TAU = TAU + DT  
IF (OPTOUT .AND. TAU.GE.TAUOUT) THEN  
     Z1 = ZINEW  
     OPTOUT = .FALSE.  
END IF  

   90 CONTINUE  

IF (TAU.GT.TAUMAX .OR. L.EQ.IZB .OR. L.EQ.LTAUMAX) THEN  
     EMINT1 = 0.D0  
     IF (CORE) EMINT1 = XICOR*EXP(-TAU-TAUCON(LM))  
     EMINT = EMINT + FSCON(1)  
     RETURN  
END IF  

FRED = FBLUE  
TAURED = TAUBLU  
OPARED = OPABLUE  
ZI = ZINEW  
L = L + 1  
GO TO 70  

END
!
!-----------------------------------------------------------------------
!
SUBROUTINE PREPRAY(Z,P,NP,NB,JP,X0,LTOT,LMAX,CORE,W,W1,V,VVDOP,R, &
&                   OPAL,SOURCE,OPALRAY,SRAY,ZRAY,VTURBRAY,XCMF,DELTA,XMAX, &
&                   IZR,IZR2,IZB1,IZB,IZNER,IZNEB,FIRST,OPTIOV, &
&                   NSUM,SCONT,SCONRAY,OPACON,OPACRAY,TEMP, &
&                   TEMPRAY,XNE,XNERAY,ESCAT,DELTAARR)
!
USE nlte_type
USE nlte_dim
USE formalsol_var, ONLY: XCMFP,VZRP,WP,WP1, &
& NFCMF,XCMFE,SCONTE,SCERAY,SCE,DESCE,XNEMIN,XNUE,VTURBV  

IMPLICIT NONE
!
!     defining whole rays including backward half-sphere
!     calculation of doppler-(freq. dependent) and stark zones
!
!     note here: xmax corresponds to doppler-broadening
!
!     .. parameters ..
INTEGER(I4B), PARAMETER :: NDM=ID_DEPFI,LTO1=2*NDM,NC=ID_CORES  
INTEGER(I4B), PARAMETER :: NP1=NC+2+4*ID_NOCOR,NFMAX=ID_NFESC  
!     ..
!     .. scalar arguments ..
REAL(DP) ::  DELTA,VVDOP,W,W1,X0  
INTEGER(I4B) ::  IZB,IZB1,IZNEB,IZNER,IZR,IZR2,JP,LMAX,NB,NP,NSUM  
LOGICAL CORE,ESCAT,FIRST,OPTIOV  
!     ..
!     .. array arguments ..
REAL(DP) ::  OPACON(NDM,2),OPACRAY(LTO1,2),OPAL(NDM,NB), &
&                 OPALRAY(LTO1,NB),P(NP1),R(NDM),SCONRAY(LTO1,2), &
&                 SCONT(NDM,2),SOURCE(NDM,NB),SRAY(LTO1,NB), &
&                 TEMP(NDM),TEMPRAY(LTO1),V(NDM), &
&                 XCMF(LTO1,NB),XMAX(NB),XNE(NDM),XNERAY(LTO1), &
&                 Z(NDM),ZRAY(LTO1),VTURBRAY(LTO1),DELTAARR(NB)

REAL (DP), DIMENSION(10) :: X01,OPAI2,DELOPA2,SI2,DELS2

INTEGER(I4B) ::  LTOT(NP1)  
!     ..
!     .. local scalars ..
REAL(DP) ::  DELLIN,DELOPA1,DELS1,DELT1,DELVT1, &
&                 DELTAZ,DELV,DELXN1,DELZ,DELZI,DEOPC1,DEOPC2, &
&                 DESC1,DESC2,DDP,DPOLD,DPP3,DPP3OLD,OPAI1, &
&                 OPC1,OPC2,Q1,SC1,SC2,SI1,SUM1,SUM2,SUM3, &
&                 SUMEX,T1,VT1,VDOPMAX,VDOPMIN,VZR,VZR1,XCMFL, &
&                 XMAXI,XN1,Z1,ZI
INTEGER(I4B) ::  I,IP,IREST,K,KK,L,L0,L1,LL,LM, &
&        LM1,NDEL
LOGICAL OPTZ0  
!     ..
!     .. intrinsic functions ..
INTRINSIC ABS,DBLE,INT,MAX,MOD  
!     ..

IF (FIRST) THEN  

     VDOPMIN = VVDOP/5.D0  
     VDOPMAX = VDOPMIN*2.D0  
! modified from V6.0 on
     IF (NB.GE.2) THEN
       IF(XNUE(1)/XNUE(NB).LE.1.D0) STOP' PREFORMAL: ERROR IN XNUE'
       IF(NB.GT.10) STOP' TOO MANY COMPONENTS (> 10) IN PREFORMAL'
       DO I=2,NB 
         X01(I) = (X0 + DELTAARR(I))*XNUE(1)/XNUE(I)  
       ENDDO
     ENDIF

     OPTZ0 = .FALSE.  
!
!-----core rays and first point of non-core rays without interpolation
!
     IF (CORE) THEN  
          LTOT(JP) = LMAX  
          LM1 = LMAX  
     ELSE  
          LM1 = 1  
     END IF  

     DO L = 1,LM1  
          VZR = V(L)*Z(L)/R(L)  
          XCMFP(L) = -VZR  
          XCMF(L,1) = X0 - VZR  
          SRAY(L,1) = SOURCE(L,1)  
          OPALRAY(L,1) = OPAL(L,1)  
          OPACRAY(L,1) = OPACON(L,1)  
          SCONRAY(L,1) = SCONT(L,1)  
          OPACRAY(L,2) = OPACON(L,2)  
          SCONRAY(L,2) = SCONT(L,2)  
          TEMPRAY(L) = TEMP(L)  
          XNERAY(L) = XNE(L)  
          ZRAY(L) = Z(L)  
          VTURBRAY(L)=VTURBV(L)
     END DO

     IF (ESCAT) THEN  
          DO K = 1,NFCMF  
               DO L = 1,LM1  
                    SCERAY(L,K) = SCONTE(L,K)  
               END DO
          END DO
     END IF  

     IF (NB.GE.2) THEN  
          DO L = 1,LM1  
               DO K=2,NB
                 XCMF(L,K) = X01(K) + XCMFP(L)  
                 SRAY(L,K) = SOURCE(L,K)  
                 OPALRAY(L,K) = OPAL(L,K)  
               ENDDO
          END DO
     END IF  
!
!-----non core rays with interpolation
!
     IF (.NOT.CORE) THEN  

          IF (Z(LMAX).EQ.0.D0) THEN  
               OPTZ0 = .TRUE.  
               LM1 = LMAX  
          ELSE  
               LM1 = LMAX + 1  
          END IF  

          DO L = 1,LMAX  
               VZRP(L) = V(L)*Z(L)/R(L)  
          END DO

          IF (.NOT.OPTZ0) VZRP(LM1) = 0.D0  

          I = 1  

LLOOP1:   DO L = 1,LM1 - 1  
               L1 = L + 1  
               VZR = VZRP(L)  
               VZR1 = VZRP(L1)  
               DELV = VZR - VZR1  

               IF (DELV.LT.VDOPMAX) THEN  
!
!     no interpolation necessary
!
                    I = I + 1  
                    IF (I.GT.LTO1) STOP &
&                    ' MORE THAN LTO1 POINTS USED IN INTERPOLATION!'
                    XCMFP(I) = -VZR1  

                    IF (OPTZ0 .OR. L1.NE.LM1) THEN  
                         XCMF(I,1) = X0 - VZR1  
                         SRAY(I,1) = SOURCE(L1,1)  
                         OPALRAY(I,1) = OPAL(L1,1)  
                         OPACRAY(I,1) = OPACON(L1,1)  
                         OPACRAY(I,2) = OPACON(L1,2)  
                         SCONRAY(I,1) = SCONT(L1,1)  
                         SCONRAY(I,2) = SCONT(L1,2)  

                         IF (ESCAT) THEN  
                            DO K = 1,NFCMF  
                               SCERAY(I,K) = SCONTE(L1,K)  
                            END DO
                         END IF  

                         TEMPRAY(I) = TEMP(L1)  
                         XNERAY(I) = XNE(L1)  
                         ZRAY(I) = Z(L1)  
                         VTURBRAY(I)=VTURBV(L1)

                         IF (NB.GE.2) THEN  
                            DO K=2,NB
                              XCMF(I,K) = X01(K) - VZR1  
                              SRAY(I,K) = SOURCE(L1,K)  
                              OPALRAY(I,K) = OPAL(L1,K)  
                            ENDDO
                         END IF                            
!
!     different treatment for last point with optz0 = false, assuming
!     constant source function and opacity from z(lmax) to z=0.
!
                    ELSE  
                         XCMF(I,1) = X0 - VZR1  
                         SRAY(I,1) = SOURCE(L,1)  
                         OPALRAY(I,1) = OPAL(L,1)  
                         OPACRAY(I,1) = OPACON(L,1)  
                         OPACRAY(I,2) = OPACON(L,2)  
                         SCONRAY(I,1) = SCONT(L,1)  
                         SCONRAY(I,2) = SCONT(L,2)  

                         IF (ESCAT) THEN  
                            DO K = 1,NFCMF  
                               SCERAY(I,K) = SCONTE(L,K)  
                            END DO
                         END IF  

                         TEMPRAY(I) = TEMP(L)  
                         XNERAY(I) = XNE(L)  
                         VTURBRAY(I) = VTURBV(L)
                         ZRAY(I) = 0.D0  
                         IF (NB.GE.2) THEN  
                            DO K=2,NB 
                              XCMF(I,K) = X01(K) - VZR1  
                              SRAY(I,K) = SOURCE(L,K)  
                              OPALRAY(I,K) = OPAL(L,K)  
                            ENDDO
                         END IF  

                    END IF  
!
!     interpolation linearly with z
!
               ELSE  

                    NDEL = INT(DELV/VDOPMIN) + 1  
                    IF (NDEL.LT.2) STOP ' NDEL < 2'  
                    ZI = Z(L)  

                    IF (OPTZ0 .OR. L1.NE.LM1) THEN  
                         Z1 = Z(L1)  
                         DELTAZ = ZI - Z1  
                         DELZ = DELTAZ/DBLE(NDEL)  
                         DELTAZ = 1.D0/DELTAZ  
                         OPAI1 = OPAL(L,1)  
                         DELOPA1 = OPAL(L1,1) - OPAL(L,1)  
                         SI1 = SOURCE(L,1)  
                         DELS1 = SOURCE(L1,1) - SOURCE(L,1)  
                         OPC1 = OPACON(L,1)  
                         DEOPC1 = OPACON(L1,1) - OPACON(L,1)  
                         SC1 = SCONT(L,1)  
                         DESC1 = SCONT(L1,1) - SCONT(L,1)  
                         OPC2 = OPACON(L,2)  
                         DEOPC2 = OPACON(L1,2) - OPACON(L,2)  
                         SC2 = SCONT(L,2)  
                         DESC2 = SCONT(L1,2) - SCONT(L,2)  

                         IF (ESCAT) THEN  
                            DO K = 1,NFCMF  
                               SCE(K) = SCONTE(L,K)  
                               DESCE(K) = SCONTE(L1,K) - SCONTE(L,K)  
                            END DO
                         END IF  

                         T1 = TEMP(L)  
                         DELT1 = TEMP(L1) - TEMP(L)  
                         XN1 = XNE(L)  
                         DELXN1 = XNE(L1) - XNE(L)  
                         VT1 = VTURBV(L)  
                         DELVT1 = VTURBV(L1) - VTURBV(L)  
                         IF (NB.GE.2) THEN  
                            DO K=2,NB
                               OPAI2(K) = OPAL(L,K)  
                               DELOPA2(K) = OPAL(L1,K) - OPAL(L,K)  
                               SI2(K) = SOURCE(L,K)  
                               DELS2(K) = SOURCE(L1,K) - SOURCE(L,K)  
                            ENDDO
                         END IF  
!
                    ELSE  
!
!     different treatment for last point with optz0 = false, assuming
!     constant source function and opacity from z(lmax) to z=0.
!
                         Z1 = 0.D0  
                         DELTAZ = ZI - Z1  
                         DELZ = DELTAZ/DBLE(NDEL)  
                         DELTAZ = 1.D0/DELTAZ  
                         OPAI1 = OPAL(L,1)  
                         DELOPA1 = 0.D0  
                         SI1 = SOURCE(L,1)  
                         DELS1 = 0.D0  
                         OPC1 = OPACON(L,1)  
                         DEOPC1 = 0.D0  
                         SC1 = SCONT(L,1)  
                         DESC1 = 0.D0  
                         OPC2 = OPACON(L,2)  
                         DEOPC2 = 0.D0  
                         SC2 = SCONT(L,2)  
                         DESC2 = 0.D0  

                         IF (ESCAT) THEN  
                            DO K = 1,NFCMF  
                               SCE(K) = SCONTE(L,K)  
                               DESCE(K) = 0.D0  
                            END DO
                         END IF  

                         T1 = TEMP(L)  
                         DELT1 = 0.D0  
                         XN1 = XNE(L)  
                         DELXN1 = 0.D0  
                         VT1 = VTURBV(L)  
                         DELVT1 = 0.D0  
                         IF (NB.GE.2) THEN  
                            DO K=2,NB
                               OPAI2(K) = OPAL(L,K)  
                               DELOPA2(K) = 0.D0  
                               SI2(K) = SOURCE(L,K)  
                               DELS2(K) = 0.D0  
                            ENDDO
                         END IF  

                    END IF  

                    KLOOP: DO K = 1,NDEL  
                         I = I + 1  
                         IF (I.GT.LTO1) STOP &
&                         ' MORE THAN LTO1 POINTS USED IN INTERPOLATION!'
                         DELZI = K*DELZ  
                         DELLIN = DELZI*DELTAZ  
                         ZRAY(I) = ZI - DELZI  
!
!     attention!! explicitly accounted for sign of delv
!
                         XCMFP(I) = -VZR + DELV*DELLIN  
                         XCMF(I,1) = X0 + XCMFP(I)  
                         SRAY(I,1) = SI1 + DELS1*DELLIN  
                         OPALRAY(I,1) = OPAI1 + DELOPA1*DELLIN  
                         OPACRAY(I,1) = OPC1 + DEOPC1*DELLIN  
                         OPACRAY(I,2) = OPC2 + DEOPC2*DELLIN  
                         SCONRAY(I,1) = SC1 + DESC1*DELLIN  
                         SCONRAY(I,2) = SC2 + DESC2*DELLIN  

                         IF (ESCAT) THEN  
                            DO KK = 1,NFCMF  
                               SCERAY(I,KK) = SCE(KK) + DESCE(KK)*DELLIN
                            END DO
                         END IF  

                         TEMPRAY(I) = T1 + DELT1*DELLIN  
                         XNERAY(I) = XN1 + DELXN1*DELLIN  
                         VTURBRAY(I) = VT1 + DELVT1*DELLIN  

                         IF (NB.GE.2) THEN  
                            DO KK=2,NB 
                               XCMF(I,KK) = X01(KK) + XCMFP(I)  
                               SRAY(I,KK) = SI2(KK) + DELS2(KK)*DELLIN  
                               OPALRAY(I,KK) = OPAI2(KK) + DELOPA2(KK)*DELLIN  
                            ENDDO
                         END IF  
                    
                    END DO KLOOP

               END IF  

          END DO LLOOP1  
!
!     defining quantities on the back hemisphere
!
          LM1 = 2*I - 1  
          IF (LM1.GT.LTO1) STOP &
&                   ' MORE THAN LTO1 POINTS USED IN INTERPOLATION!'
          LTOT(JP) = LM1  

          DO L = 1,I - 1  
               LL = 2*I - L  
               VZR = -XCMFP(L)  
               XCMFP(LL) = VZR  
               XCMF(LL,1) = X0 + VZR  
               SRAY(LL,1) = SRAY(L,1)  
               OPALRAY(LL,1) = OPALRAY(L,1)  
               OPACRAY(LL,1) = OPACRAY(L,1)  
               OPACRAY(LL,2) = OPACRAY(L,2)  
               SCONRAY(LL,1) = SCONRAY(L,1)  
               SCONRAY(LL,2) = SCONRAY(L,2)  
               TEMPRAY(LL) = TEMPRAY(L)  
               XNERAY(LL) = XNERAY(L)  
               ZRAY(LL) = -ZRAY(L)  
               VTURBRAY(LL) = VTURBRAY(L)  
          END DO

          IF (ESCAT) THEN  
               DO K = 1,NFCMF  
                    DO L = 1,I - 1  
                         LL = 2*I - L  
                         SCERAY(LL,K) = SCERAY(L,K)  
                    END DO
               END DO
          END IF  

          IF (NB.GE.2) THEN  
               DO L = 1,I - 1  
                    LL = 2*I - L  
                    DO K=2,NB
                       XCMF(LL,K) = X01(K) + XCMFP(LL)  
                       SRAY(LL,K) = SRAY(L,K)  
                       OPALRAY(LL,K) = OPALRAY(L,K)  
                    ENDDO
               END DO
          END IF  
!
!     end of non-core ray treatment
!
     END IF  
!
!     w = weights for flux integral
!
     IF (JP.EQ.1) THEN  
          W = P(2)*P(2)/3.D0  
          W1 = 0.D0  

     ELSE IF (JP.LT.NC+2) THEN  
          W = (P(JP-1)+P(JP)+P(JP+1))* (P(JP+1)-P(JP-1))/3.D0  
          W1 = 0.D0  

     ELSE IF (JP.EQ.NC+2) THEN  
          W = (P(JP)-P(JP-1))* (2*P(JP)+P(JP-1))/3.D0  
          DDP = P(JP+1) - P(JP)  
          DPP3 = P(JP)*DDP*2.D0/3.D0  
          W = W + DPP3  
          W1 = -DPP3/15.D0  

     ELSE  
          IP = JP - (NC+2)  
          IREST = MOD(IP,4)  
          DDP = P(JP+1) - P(JP)  
          DPP3 = P(JP)*DDP*2.D0/3.D0  

          IF (IREST.EQ.0) THEN  
               DPOLD = P(JP) - P(JP-1)  
               DPP3OLD = P(JP)*DPOLD*2.D0/3.D0  
               W = DPP3 + DPP3OLD  
               W1 = -W/15.D0  

          ELSE IF (IREST.EQ.1 .OR. IREST.EQ.3) THEN  
               W = 4.D0*DPP3  
               W1 = W/15.D0  
          ELSE  
               W = 2.D0*DPP3  
               W1 = -3.D0*W/15.D0  
          END IF  

     END IF  

     WP(JP) = W  
     WP1(JP) = W1  

     IF (JP.EQ.NP-1) THEN  
          DDP = P(JP+1) - P(JP)  
          DPP3 = P(JP+1)*DDP*2.D0/3.D0  
          WP(NP) = DPP3  
          WP1(NP) = -DPP3/15.D0  
!
!-----test of integration weights
!
          SUM1 = 0.D0  
          SUM2 = 0.D0  
          SUM3 = 0.D0  
          DO IP = 1,NP  
               SUM1 = SUM1 + WP(IP)  
               SUM3 = SUM3 + WP1(IP)  
               IF (IP.GE.NC+2) SUM2 = SUM2 + WP1(IP)/P(IP)  
          END DO

          IF (ABS(SUM2).GT.1.D-5) STOP 'ERROR IN SUM2'  
          SUMEX = R(1)**2  

          IF (ABS(SUM1+SUM3-SUMEX).GT.1.D-2) THEN  
               PRINT *,SUM1 + SUM3 - SUMEX  
               STOP 'ERROR IN SUM1'  
          END IF  
     END IF  

     IZNER = 1  
     IZNEB = 1  
!
!     definition of "stark-broadening zone"
!
     IF (NSUM.GT.0) THEN  

          IF (CORE) THEN  
               DO L = 1,LTOT(JP)  
                    IF (XNERAY(L).GE.XNEMIN) GO TO 190  
               END DO
               STOP 'XNEMIN NOT FOUND IN CORE-RAYS'  

  190                CONTINUE  
               IZNER = L - 1  
               IZNEB = LTOT(JP)  

          ELSE  
               L0 = (LTOT(JP)-1)/2  
               DO L = 1,L0  
                    IF (XNERAY(L).GE.XNEMIN) GO TO 210  
               END DO
!              print*,'xnemin not found in non core-ray with p =',p(jp)
               GO TO 240  

  210          CONTINUE  

               IZNER = L - 1  
               DO L = LTOT(JP),L0,-1  
                    IF (XNERAY(L).GE.XNEMIN) GO TO 230  
               END DO
               STOP &
   &            'XNEMIN NOT FOUND IN NON CORE-RAY, SOMETHING WRONG!!!'

  230          CONTINUE  

               IZNEB = L + 1  

  240          CONTINUE  

          END IF  
     END IF  
!
!     if not first
!
ELSE  

     LM = LTOT(JP)  

     DO K = 1,NB  
          IF (K.EQ.1) X01(1) = X0  
!changed
          IF (K.GE.2) X01(K) = (X0 + DELTAARR(K))*XNUE(1)/XNUE(K)  
!#------  do 290 l=izr,lm changed!!!!
          DO L = 1,LM  
               XCMF(L,K) = XCMFP(L) + X01(K)  
          END DO
     END DO
!
!     w = weights for flux integral
!
     W = WP(JP)  
     W1 = WP1(JP)  

END IF  

LM = LTOT(JP)  
!
!     defining continuum source function at appropriate cmf frequency
!     corresponding to x0. the result is written to sconray(l,1). the
!     algorithm exploits the monotonicity of xcmf (decreasing with z)
!     note, that sce and xcmfe are defined from blue to red
!
IF (ESCAT) THEN  

     XCMFL = XCMFP(LM) + X0  
     DO K = 1,NFCMF - 1  
          IF (XCMFL.LE.XCMFE(K) .AND. XCMFL.GE.XCMFE(K+1)) GO TO 280
     END DO
     PRINT*,XCMFL
     PRINT*
     PRINT*,XCMFE 
     STOP ' XCMF(LM) NOT FOUND IN XCMFE'  

  280  CONTINUE  

     Q1 = (XCMFL-XCMFE(K+1))/ (XCMFE(K)-XCMFE(K+1))  
     SCONRAY(LM,1) = Q1*SCERAY(LM,K) + (1.D0-Q1)*SCERAY(LM,K+1)  
     SCONRAY(LM,2) = 0.D0  
     KK = K  

     DO L = LM,1,-1  
          XCMFL = XCMFP(L) + X0  
          DO K = KK,NFCMF - 1  
               IF (XCMFL.LE.XCMFE(K) .AND. XCMFL.GE.XCMFE(K+1)) GO TO 310
          END DO

          DO K = 1,KK - 1  
               IF (XCMFL.LE.XCMFE(K) .AND. XCMFL.GE.XCMFE(K+1)) GO TO 310
          END DO
          PRINT*,L,' ',XCMFL
          PRINT*,XCMFE 
          STOP ' XCMF(L) NOT FOUND IN XCMFE'  


  310     CONTINUE  

          Q1 = (XCMFL-XCMFE(K+1))/ (XCMFE(K)-XCMFE(K+1))  
          SCONRAY(L,1) = Q1*SCERAY(L,K) + (1.D0-Q1)*SCERAY(L,K+1)  
          SCONRAY(L,2) = 0.D0  
          KK = K  
     END DO

END IF  
!
!     location of resonance zone(s) for singlets and overlapping
!     multiplets (between comp. 1 and NB)
!
IF (OPTIOV) THEN  

     IF (FIRST) THEN  
          IZR = 1  
          IZB = 1  
     END IF  

     K = NB  

     XMAXI = MAX(XMAX(1),XMAX(NB))  

     IF (XCMF(1,K).GE.-XMAXI) THEN  
          IZR = 1  
     ELSE  
          DO L = IZR,LM - 1  
               IF (XCMF(L,K).LE.-XMAXI .AND. XCMF(L+1,K).GT.-XMAXI) EXIT
          END DO
          IZR = L  
     END IF  

     IF (XCMF(1,1).GE.XMAXI) THEN  
          IZB = 1  
     ELSE IF (XCMF(LM,1).LT.XMAXI) THEN  
          IZB = LM  
     ELSE  
          DO L = MAX(1,IZB-1),LM - 1  
               IF (XCMF(L,1).LT.XMAXI .AND. XCMF(L+1,1).GE.XMAXI) EXIT
          END DO
          IZB = L + 1  

          IF (IZB.GT.LM) STOP ' IZB > LM'  
     END IF  
!
!     print*,jp,' ',x0,' ',izr,' ',izb,' ',izner,' ',izneb

     RETURN  

ELSE  
!
!     location of resonance zone(s) for non-overlapping doublets
!

     IF (FIRST) THEN  
          IZR = 1  
          IZR2 = 1  
          IZB1 = 1  
          IZB = 1  
     END IF  
!
!     red component(outer one)
!
     IF (XCMF(1,2).GE.-XMAX(2)) THEN  
          IZR = 1  
     ELSE  
          DO L = IZR,LM - 1  
               IF (XCMF(L,2).LE.-XMAX(2) .AND. XCMF(L+1,2).GT.-XMAX(2)) EXIT
          END DO
          IZR = L  
     END IF  

     IF (XCMF(1,2).GE.XMAX(2)) THEN  
          IZR2 = 1  
     ELSE IF (XCMF(LM,2).LT.XMAX(2)) THEN  
          IZR2 = LM  
     ELSE  
          DO L = MAX(1,IZR2-1),LM - 1  
               IF (XCMF(L,2).LT.XMAX(2) .AND. XCMF(L+1,2).GE.XMAX(2)) EXIT
          END DO
          IZR2 = L + 1  

          IF (IZR2.GT.LM) STOP ' IZR2 > LM'  
     END IF  
!
!     blue component(inner one)
!
     IF (XCMF(1,1).GE.-XMAX(1)) THEN  
          IZB1 = 1  
     ELSE  
          DO L = IZB1,LM - 1  
               IF (XCMF(L,1).LE.-XMAX(1) .AND. XCMF(L+1,1).GT.-XMAX(1)) EXIT
          END DO
          IZB1 = L  
     END IF  

     IF (XCMF(1,1).GE.XMAX(1)) THEN  
          IZB = 1  
     ELSE IF (XCMF(LM,1).LT.XMAX(1)) THEN  
          IZB = LM  
     ELSE  
          DO L = MAX(1,IZB-1),LM - 1  
               IF (XCMF(L,1).LT.XMAX(1) .AND. XCMF(L+1,1).GE.XMAX(1)) EXIT
          END DO
          IZB = L + 1  

          IF (IZB.GT.LM) STOP ' IZB > LM'  
     END IF  
!
!      print*,
!     *jp,' ',x0,' ',izr,' ',izr2,' ',izb1,' ',izb,' ',izner,' ',izneb
!
     RETURN  

END IF  

END
!
!-----------------------------------------------------------------------
!
SUBROUTINE XGRID(DELTA,NFOBS,XMAXDOP,XMAXDOP_MIN,X0,AIC,OPACON,ND,TEM,TGRAD, &
&                 XCRED,XCBLUE,XNUE0,VMAX,NB,VPLUS,ESCAT)
!
! new version, from V7.0 on.
!
USE nlte_type
USE nlte_dim
USE fund_const, ONLY: CLIGHT
USE formalsol_var, ONLY: NFCMF,XCMFE,XREST=>XNUE
IMPLICIT NONE
!
!
!     .. parameters ..
INTEGER(I4B), PARAMETER :: NDM1=ID_DEPFI,NF=ID_NFOBS,NFMAX=ID_NFESC  
!     ..
!     .. scalar arguments ..
REAL(DP) ::  DELTA,TEM,TGRAD,VMAX,VPLUS,XCBLUE,XCRED,XNUE0  
INTEGER(I4B) ::  NB,ND,NFOBS  
LOGICAL ESCAT  
!     ..
!     .. array arguments ..
REAL(DP) ::  AIC(NFOBS,2),OPACON(ND,2),X0(NFOBS),XMAXDOP(NB),XMAXDOP_MIN(NB)  
!     ..
REAL(DP), DIMENSION(NB) :: XLAM1


!     .. local scalars ..
REAL(DP) ::  C2,DXS1,DXS2,FMAX,FMIN,FXLOG,OPA,SAT,VM, &
&                 VMM,VMP,XLAM,XSAT,XX,XX0,YY,DX,FMAX1,FMIN1
INTEGER(I4B) ::  I,IE,IEB,IER,K,M,M1,MBLUE,MRED,NFOBS1,MFIRST,NPERLINE  
!     ..
!     .. external functions ..
REAL(DP) ::  BPLANC  
EXTERNAL BPLANC  
!     ..
!     .. external subroutines ..
EXTERNAL SORT  
!     ..
!     .. intrinsic functions ..
INTRINSIC ABS,DBLE,EXP,LOG10,MAX  
!     ..
!
IF(NF.NE.NFOBS) STOP' NF NE NFOBS IN XGRID'

IF(NB.EQ.1 .AND. DELTA .NE. 0.D0) STOP' NB=1 AND DELTA NE 0' 

!C2 = 1.439426D0/TEM
!changed by JO July 2015
C2=1.4388354967334D0/TEM
!
!     definition of satellite components for hei-lines
!
XLAM = 1.D8/XNUE0  

IF (ABS(XLAM-4026.D0).LT.3.) THEN  
     PRINT *  
     PRINT *,'SATELLITE-COMPONENT AT -0.6 A!!'  
     PRINT *  
     SAT = -.6D0  

ELSE IF (ABS(XLAM-4387.D0).LT.3.) THEN  
     PRINT *  
     PRINT *,'SATELLITE-COMPONENT AT -0.525 A!!'  
     PRINT *  
     SAT = -0.525D0  

ELSE IF (ABS(XLAM-4471.D0).LT.3.) THEN  
     PRINT *  
     PRINT *,'SATELLITE-COMPONENT AT -1.5 A!!'  
     PRINT *  
     SAT = -1.5D0  

ELSE IF (ABS(XLAM-4922.D0).LT.3.) THEN  
     PRINT *  
     PRINT *,'SATELLITE-COMPONENT AT -1.4 A!!'  
     PRINT *  
     SAT = -1.4D0  

ELSE  
     SAT = 0.D0  
END IF  

XSAT = (XLAM/ (XLAM+SAT)-1.D0)*CLIGHT/VMAX  
IF (SAT.EQ.0.) XSAT = 0.D0  
!
!     defining maximum observer's frame bandwidth
!
VM = 1.D0  
VMP = VM + XMAXDOP(1)  !calculated with VDOPMAX
VMM = VM + XMAXDOP(NB) !calculated with VDOPMAX 

PRINT *  
PRINT *,' VMAX + VD_MAX = ',VMP  
PRINT *  

FMAX = MAX(VMP,VPLUS)  !vplus calculated with XMAX
FMIN = MAX(VMM,VPLUS) + DELTA  

FMAX1=FMAX
FMIN1=FMIN

IF(ESCAT) THEN 
! IN CASE OF VERY BROAD LINES, CHANGE FIRST AND LAST XCMFE-FREQ. POINTS (faked anyway)
  IF(FMAX1.GT.XCMFE(1)-1.D0) XCMFE(1)=FMAX1+1.01
  IF(FMIN1.GT.-XCMFE(NFCMF)-1.D0) XCMFE(NFCMF)=-FMIN1-1.01

  FMAX1=MAX(FMAX1,XCMFE(1)-1.D0)
  FMIN1=MAX(FMIN1,-XCMFE(NFCMF)-1.D0)
ENDIF  

! so far, equidistant grid, resolved at line center
NPERLINE=32 !sufficient for 3 lines
M1=MAX(60,NFOBS-NB*NPERLINE)

IF (M1.EQ.60) THEN
   NPERLINE=(NFOBS-60)/NB
   IF (MOD(NPERLINE,2).NE.0) NPERLINE=NPERLINE-1
   M1=NFOBS-NB*NPERLINE
   PRINT*
   PRINT*,' WARNING!!!! WARNING!!!! WARNING!!!!'
   PRINT*,' ONLY ',NPERLINE,' HIGHLY RESOLVED FREQUENCY POINTS PER LINE' 
   PRINT*,' WARNING!!!! WARNING!!!! WARNING!!!!'
   PRINT*
ENDIF
M1=M1-NB
    
DO I=1,NB
   XLAM1(I) = 1.D8/XREST(I)  
! XLAM1 in x-units, w.r.t. bluemost component
   XLAM1(I) = (XLAM/XLAM1(I)-1.D0)*CLIGHT/VMAX  ! nu/nu0 corresponds to lam0/lam
ENDDO

IF(FMAX.EQ.FMAX1) THEN ! either no elscat, or elscat wings narrow
! equidistant grid with M1 points
   XX = (FMAX+FMIN)/DBLE(M1-1)  
   DO I = 1,M1  
      X0(I) = FMAX - (I-1)*XX
   END DO
   M=M1

ELSE
   IF(.NOT.ESCAT) STOP' ERROR IN ESCAT PHILOSOPHY - SUBR. XGRID'
! equidistant grid with M1-20 points
   M1=M1-20
   XX = (FMAX+FMIN)/DBLE(M1-1)  
   DO I = 1,M1  
      X0(I) = FMAX - (I-1)*XX
   END DO
! equidistant grid between FMAX1 and FMAX with 10 points
   M=M1
   XX = (FMAX1-FMAX)/DBLE(10)  
   DO I = 1,10  
      M=M+1
      X0(M) = FMAX1 - (I-1)*XX !(excl. FMAX)
   END DO
! equidistant grid between FMIN and FMIN1 with 10 points 
   XX = (FMIN1-FMIN)/DBLE(10)  
   DO I = 1,10  
      M=M+1
      X0(M) = -FMIN - I*XX !(excl. FMIN)
   END DO
ENDIF

! higher resolution around line cores, logarithmically distributed
M=M+1
DO K=1,NB
   DXS1=ALOG10(4./0.05)/DBLE(NPERLINE/2-1) !from 0.05 to 4 (previously 2) max-doppler units (usually xmaxdop=3 vdop)  
   X0(M)=XLAM1(K)
   M=M+1
   DO I=1,NPERLINE/2
     DX=0.05*XMAXDOP_MIN(K)*10.**((I-1)*DXS1) ! calculated with XMAXDOP_MIN,
                                              ! to assure resolution of core
     X0(M)=XLAM1(K)+DX
!     print*,i,nperline,x0(m),dx,xmaxdop(k),xmax(k)
     M=M+1
     X0(M)=XLAM1(K)-DX
     M=M+1
   ENDDO
ENDDO  

X0=-X0
CALL SORT(NFOBS,X0)  
X0=-X0

IF (XSAT.EQ.0.D0) GO TO 270  

!     set in satelite component

DO K = 1,NFOBS - 1  
     IF (XSAT.LT.X0(K) .AND. XSAT.GE.X0(K+1)) GO TO 260  
END DO  

STOP ' SATELLITE COMPONENT NOT FOUND'  

260 CONTINUE  

DXS1 = X0(K) - XSAT  
DXS2 = XSAT - X0(K+1)  

IF (DXS2.LT.DXS1) THEN  
     X0(K+1) = XSAT  
ELSE  
     X0(K) = XSAT  
END IF  
!
!     loop for every observers-frame-frequency
!
270 CONTINUE  

DO I = 1,NFOBS  
!
!---- i-core calculated under difussion appr. with limb darkening
!
!         opa=opacon(nd,1)+(opacon(nd,2)-opacon(nd,1))/(xcblue-
!     *         xcred)*(x0(i)-xcred)
!
!     changed to be consistent with constant opacity approx. used
!     in elscat
!
     OPA = OPACON(ND,1) + (OPACON(ND,2)-OPACON(ND,1))/ (XCBLUE- &
      XCRED)* (0.D0-XCRED)
     XX0 = X0(I)*VMAX*XNUE0/CLIGHT + XNUE0  

     AIC(I,1) = BPLANC(XX0,TEM)  
     AIC(I,2) = AIC(I,1)*C2*XX0*TGRAD/TEM/OPA/ (1.D0-EXP(-C2*XX0))  

END DO  

RETURN  

END
!
!-----------------------------------------------------------------------
!
SUBROUTINE XGRID_OLD(DELTA,NFOBS,XMAXDOP,X0,AIC,OPACON,ND,TEM,TGRAD, &
&                 XCRED,XCBLUE,XNUE0,VMAX,NB,VPLUS,ESCAT)
!
! old version, until V6.1
USE nlte_type
USE nlte_dim
USE fund_const, ONLY: CLIGHT
USE formalsol_var, ONLY: NFCMF,XCMFE,XREST=>XNUE
IMPLICIT NONE
!
!
!     .. parameters ..
INTEGER(I4B), PARAMETER :: NDM1=ID_DEPFI,NF=ID_NFOBS,NFMAX=ID_NFESC  
!     ..
!     .. scalar arguments ..
REAL(DP) ::  DELTA,TEM,TGRAD,VMAX,VPLUS,XCBLUE,XCRED,XNUE0  
INTEGER(I4B) ::  NB,ND,NFOBS  
LOGICAL ESCAT  
!     ..
!     .. array arguments ..
REAL(DP) ::  AIC(NFOBS,2),OPACON(ND,2),X0(NFOBS),XMAXDOP(NB)  
!     ..
!REAL(DP), DIMENSION(NF) :: AUX
REAL(DP), DIMENSION(NB) :: XLAM1


!     .. local scalars ..
REAL(DP) ::  C2,DXS1,DXS2,FMAX,FMIN,FXLOG,OPA,SAT,VM, &
&                 VMM,VMP,XLAM,XSAT,XX,XX0,YY,DX
INTEGER(I4B) ::  I,IE,IEB,IER,K,M,M1,MBLUE,MRED,NFOBS1,MFIRST,NPERLINE  
!     ..
!     .. external functions ..
REAL(DP) ::  BPLANC  
EXTERNAL BPLANC  
!     ..
!     .. external subroutines ..
EXTERNAL SORT  
!     ..
!     .. intrinsic functions ..
INTRINSIC ABS,DBLE,EXP,LOG10,MAX  
!     ..
!
IF(NF.NE.NFOBS) STOP' NF NE NFOBS IN XGRID'

IF(NB.EQ.1 .AND. DELTA .NE. 0.D0) STOP' NB=1 AND DELTA NE 0' 

!changed by JO July 2015
C2=1.4388354967334D0/TEM
!
!     definition of satellite components for hei-lines
!
XLAM = 1.D8/XNUE0  

IF (ABS(XLAM-4026.D0).LT.3.) THEN  
     PRINT *  
     PRINT *,'SATELLITE-COMPONENT AT -0.6 A!!'  
     PRINT *  
     SAT = -.6D0  

ELSE IF (ABS(XLAM-4387.D0).LT.3.) THEN  
     PRINT *  
     PRINT *,'SATELLITE-COMPONENT AT -0.525 A!!'  
     PRINT *  
     SAT = -0.525D0  

ELSE IF (ABS(XLAM-4471.D0).LT.3.) THEN  
     PRINT *  
     PRINT *,'SATELLITE-COMPONENT AT -1.5 A!!'  
     PRINT *  
     SAT = -1.5D0  

ELSE IF (ABS(XLAM-4922.D0).LT.3.) THEN  
     PRINT *  
     PRINT *,'SATELLITE-COMPONENT AT -1.4 A!!'  
     PRINT *  
     SAT = -1.4D0  

ELSE  
     SAT = 0.D0  
END IF  

XSAT = (XLAM/ (XLAM+SAT)-1.D0)*CLIGHT/VMAX  
IF (SAT.EQ.0.) XSAT = 0.D0  
!
!     defining maximum observer's frame bandwidth
!
VM = 1.D0  
VMP = VM + XMAXDOP(1)  
VMM = VM + XMAXDOP(NB)  

PRINT *  
PRINT *,' VMAX + VD_MAX = ',VMP  
PRINT *  

FMAX = MAX(VMP,VPLUS)  
FMIN = MAX(VMM,VPLUS) + DELTA  
print*,'vmp',vmp,vmm
print*,'vplus',vplus


IF (.NOT.ESCAT) XCMFE(1) = 0.D0  

IF(NB.GT.2) GOTO 200

! path for singlets and doublets

IF (.NOT.ESCAT .OR. FMAX.GE.XCMFE(1)) THEN  
     M = NFOBS/3  
!
!---- blue wing
!
     M1 = 2*M + 1  
     MBLUE = NFOBS - M1  
     MBLUE = MBLUE/2  
     MRED = NFOBS - (M1+MBLUE)  

     IF (VPLUS.LT.VMP) THEN  
!
!     1/3 of points from vmp to .5 and from -.5 to -vmm linear
!     2/3 of points from .5 to -.5 logarithmic for NB=1
!     1/3 of points from .5 to -.5 logarithmic for each component if NB=2
!
!
          XX = (FMAX-.5D0)/DBLE(MBLUE)  
          DO I = 1,MBLUE  
               X0(I) = FMAX - (I-1)*XX  
          END DO
          K = MBLUE  

          FXLOG = LOG10(0.5D0)  

          IF(NB.EQ.1) THEN
!ONE COMPONENT
          XX = (FXLOG+4.D0)/DBLE(M-1)  
          DO I = 1,M  
               K = K + 1  
               X0(K) = 1.D1** (FXLOG-DBLE(I-1)*XX)  
          END DO
!
!-----one point at x=0.
!
          K = K + 1  
          X0(K) = 0.D0  
!
!---- red wing
!
          FXLOG = LOG10(0.5D0)  
          YY = (FXLOG+4.D0)/DBLE(M-1)  
          DO I = 1,M  
               K = K + 1  
               X0(K) = -1.D1** (-4.D0+DBLE(I-1)*YY)  
          END DO

          XX = (FMIN-.5D0)/DBLE(MRED)  
          DO I = 1,MRED  
               K = K + 1  
               X0(K) = -0.5D0 - I*XX  
          END DO

!-----
          ELSE
!TWO COMPONENTS
          IF (NB.NE.2) STOP' SOMETHING WRONG WITH NB(1)'
          MFIRST=M/2
          XX = (FXLOG+4.D0)/DBLE(MFIRST-1)  
          DO I = 1,MFIRST  
               K = K + 1  
               X0(K) = 1.D1** (FXLOG-DBLE(I-1)*XX)  
               K = K + 1  
               X0(K) = 1.D1** (FXLOG-DBLE(I-1)*XX) - DELTA
          END DO
!
!-----one point at x=0.
!
          K = K + 1  
          X0(K) = 0.D0  
          K = K + 1  
          X0(K) = -DELTA  
!
!---- red wing
!
          FXLOG = LOG10(0.5D0)  
          YY = (FXLOG+4.D0)/DBLE(MFIRST-1)  
          DO I = 1,MFIRST  
               K = K + 1  
               X0(K) = -1.D1** (-4.D0+DBLE(I-1)*YY)  
               K = K + 1  
               X0(K) = -1.D1** (-4.D0+DBLE(I-1)*YY) - DELTA  
          END DO

          IF (MRED+K .NE. NFOBS-1) STOP' SOMETHING WRONG IN MRED PHILOSOPHY' 
          MRED=MRED+1

          XX = (FMIN-.5D0)/DBLE(MRED)  
          DO I = 1,MRED  
               K = K + 1  
               X0(K) = -0.5D0 - I*XX  
          END DO
          
          DO I = 1,NFOBS  
               X0(I) = -X0(I)  
          END DO

          CALL SORT(NFOBS,X0)  

          DO I = 1,NFOBS  
               X0(I) = -X0(I)  
          END DO

          IF (K.NE.NFOBS) STOP 'ERROR IN OBSERV. FREQUENCY GRID, K.NE.NF(1)'
   
          ENDIF ! ONE OR TWO COMPONENT TREATMENT
     ELSE  
!
!     2/3 of points from vplus to -vplus logarithmic for NB=1
!     1/3 of points from vplus to -vplus logarithmic for each component if NB=2
!     1/3 of points from vmp to .5 and from -.5 to -vmm
!     linear additionally
!
          FXLOG = LOG10(FMAX)  
          IF(NB.EQ.1) THEN
!ONE COMPONENT
          XX = (FXLOG+4.D0)/DBLE(M-1)  
          K = 0  
          DO I = 1,M  
               K = K + 1  
               X0(K) = 1.D1** (FXLOG-DBLE(I-1)*XX)  
          END DO
!
!-----one point at x=0.
!
          K = K + 1  
          X0(K) = 0.D0  
!
!---- red wing
!
          FXLOG = LOG10(FMIN)  
          YY = (FXLOG+4.D0)/DBLE(M-1)  
          DO I = 1,M  
               K = K + 1  
               X0(K) = -1.D1** (-4.D0+DBLE(I-1)*YY)  
          END DO
!
          ELSE
!TWO COMPONENTS
          IF (NB.NE.2) STOP' SOMETHING WRONG WITH NB(2)'
          MFIRST=M/2
          XX = (FXLOG+4.D0)/DBLE(MFIRST-1)  
          K = 0  
          DO I = 1,MFIRST  
               K = K + 1  
               X0(K) = 1.D1** (FXLOG-DBLE(I-1)*XX)  
               K = K + 1  
               X0(K) = 1.D1** (FXLOG-DBLE(I-1)*XX) - DELTA
          END DO
!
!-----one point at x=0.
!
          K = K + 1  
          X0(K) = 0.D0  
          K = K + 1  
          X0(K) = -DELTA  
!
!---- red wing
!
          FXLOG = LOG10(FMIN)  
          YY = (FXLOG+4.D0)/DBLE(MFIRST-1)  
          DO I = 1,MFIRST  
               K = K + 1  
               X0(K) = -1.D1** (-4.D0+DBLE(I-1)*YY)  
               K = K + 1  
               X0(K) = -1.D1** (-4.D0+DBLE(I-1)*YY) - DELTA  
          END DO

          IF(K+MBLUE+MRED .NE. NFOBS-1) STOP' SOMETHING WRONG IN MBLUE/MRED PHILOSOPHY' 
          MRED=MRED+1

          ENDIF ! ONE OR TWO COMPONENT TREATMENT
!
!     additional points
!
          XX = (VMP-.5D0)/DBLE(MBLUE-1)  
          DO I = 1,MBLUE  
               K = K + 1  
               X0(K) = VMP - (I-1)*XX  
          END DO

          XX = (VMM-.5D0)/DBLE(MRED-1)  
          DO I = 1,MRED  
               K = K + 1  
               X0(K) = -0.5D0 - (I-1)*XX  
          END DO

          IF (K.NE.NFOBS) STOP 'ERROR IN OBSERV. FREQUENCY GRID, K.NE.NF(2)'

          DO I = 1,NFOBS  
               X0(I) = -X0(I)  
          END DO

          CALL SORT(NFOBS,X0)  

          DO I = 1,NFOBS  
               X0(I) = -X0(I)  
          END DO
     END IF  

ELSE  ! ELSCAT TREATMENT

     IF (XCMFE(1)-XCMFE(2).LT.1.) STOP ' DELTA X(1) < 1'  

     IEB = 0  

     DO K = 1,NFCMF  
          IF (XCMFE(K).GT.FMAX) IEB = IEB + 1  
     END DO

     IER = 0  

     DO K = 1,NFCMF  
          IF (XCMFE(K).LT.-FMIN) IER = IER + 1  
     END DO

     IE = IEB + IER  
!
!-----now, ie is the number of freq. points for el. scattering wings
!
     NFOBS1 = NFOBS - IE  

     M = NFOBS1/3  
!
!---- blue wing
!
     M1 = 2*M + 1  
     MBLUE = NFOBS1 - M1  
     MBLUE = MBLUE/2  
     MRED = NFOBS1 - (M1+MBLUE)  

     IF (VPLUS.LT.VMP) THEN  
!
!     1/3 of points from vmp to .5 and from -.5 to -vmm linear
!     2/3 of points from .5 to -.5 logarithmic
!
          XX = (FMAX-.5D0)/DBLE(MBLUE)  
          DO I = 1,MBLUE  
               X0(I) = FMAX - (I-1)*XX  
          END DO

          K = MBLUE  
          FXLOG = LOG10(0.5D0)  
          XX = (FXLOG+4.D0)/DBLE(M-1)  

          DO I = 1,M  
               K = K + 1  
               X0(K) = 1.D1** (FXLOG-DBLE(I-1)*XX)  
          END DO
!
!-----one point at x=0.
!
          K = K + 1  
          X0(K) = 0.D0  
!
!---- red wing
!
          FXLOG = LOG10(0.5D0)  
          YY = (FXLOG+4.D0)/DBLE(M-1)  
          DO I = 1,M  
               K = K + 1  
               X0(K) = -1.D1** (-4.D0+DBLE(I-1)*YY)  
          END DO

          XX = (FMIN-.5D0)/DBLE(MRED)  
          DO I = 1,MRED  
               K = K + 1  
               X0(K) = -0.5D0 - I*XX  
          END DO

     ELSE  
!
!     2/3 of points from vplus to -vplus logarithmic
!     1/3 of points from vmp to .5 and from -.5 to -vmm
!     linear additionally
!
          FXLOG = LOG10(FMAX)  
          XX = (FXLOG+4.D0)/DBLE(M-1)  
          K = 0  
          DO I = 1,M  
               K = K + 1  
               X0(K) = 1.D1** (FXLOG-DBLE(I-1)*XX)  
          END DO
!
!-----one point at x=0.
!
          K = K + 1  
          X0(K) = 0.D0  
!
!---- red wing
!
          FXLOG = LOG10(FMIN)  
          YY = (FXLOG+4.D0)/DBLE(M-1)  
          DO I = 1,M  
               K = K + 1  
               X0(K) = -1.D1** (-4.D0+DBLE(I-1)*YY)  
          END DO
!
!     additional points
!
          XX = (VMP-.5D0)/DBLE(MBLUE-1)  
          DO I = 1,MBLUE  
               K = K + 1  
               X0(K) = VMP - (I-1)*XX  
          END DO

          XX = (VMM-.5D0)/DBLE(MRED-1)  
          DO I = 1,MRED  
               K = K + 1  
               X0(K) = -0.5D0 - (I-1)*XX  
          END DO

     END IF  
!
!     additional cmf-points
!
     K = K + 1  
     X0(K) = XCMFE(1) - 1.D0  
     DO I = 2,IEB  
          K = K + 1  
          X0(K) = XCMFE(I)  
     END DO
!
     DO I = 1,IER - 1  
          K = K + 1  
          X0(K) = XCMFE(NFCMF-IER+I)  
     END DO

     K = K + 1  
     X0(K) = XCMFE(NFCMF) + 1.D0  

     DO I = 1,NFOBS  
          X0(I) = -X0(I)  
     END DO

     CALL SORT(NFOBS,X0)  

     DO I = 1,NFOBS  
          X0(I) = -X0(I)  
     END DO

     IF (K.NE.NFOBS) STOP 'ERROR IN OBSERV. FREQUENCY GRID, K.NE.NF(3)'

END IF  


GOTO 210

! path for three and more components
! so far, equidistant grid, resolved at line center
200 NPERLINE=32
    M1=MAX(60,NFOBS-NB*NPERLINE)

    IF (M1.EQ.60) THEN
      NPERLINE=(NFOBS-60)/NB
      IF (MOD(NPERLINE,2).NE.0) NPERLINE=NPERLINE-1
      M1=NFOBS-NB*NPERLINE
      PRINT*
      PRINT*,' WARNING!!!! WARNING!!!! WARNING!!!!'
      PRINT*,' ONLY ',NPERLINE,' HIGHLY RESOLVED FREQUENCY POINTS PER LINE' 
      PRINT*,' WARNING!!!! WARNING!!!! WARNING!!!!'
      PRINT*
    ENDIF
    M1=M1-NB
    
    DO I=1,NB
      XLAM1(I) = 1.D8/XREST(I)  
      XLAM1(I) = (XLAM/XLAM1(I)-1.D0)*CLIGHT/VMAX  ! nu/nu0 corresponds to lam0/lam
    ENDDO

! equidistant grid with M1 points
    XX = (FMAX+FMIN)/DBLE(M1-1)  
    DO I = 1,M1  
      X0(I) = FMAX - (I-1)*XX
    END DO
    
! higher resolution around line cores, logarithmically distributed
    M=M1+1
    DO K=1,NB
      DXS1=ALOG10(3./0.05)/(NPERLINE/2) !FROM 0.05 TO 3 DOPPLERUNITS 
      X0(M)=XLAM1(K)
      M=M+1
      DO I=1,NPERLINE/2
        DX=0.05*XMAXDOP(K)*10.**(I*DXS1)
        X0(M)=XLAM1(K)+DX
        M=M+1
        X0(M)=XLAM1(K)-DX
        M=M+1
      ENDDO
    ENDDO  
    
    X0=-X0
    CALL SORT(NFOBS,X0)  
    X0=-X0

210 IF (XSAT.EQ.0.D0) GO TO 270  

!     set in satelite component

DO K = 1,NFOBS - 1  
     IF (XSAT.LT.X0(K) .AND. XSAT.GE.X0(K+1)) GO TO 260  
END DO  

STOP ' SATELLITE COMPONENT NOT FOUND'  

  260 CONTINUE  

DXS1 = X0(K) - XSAT  
DXS2 = XSAT - X0(K+1)  

IF (DXS2.LT.DXS1) THEN  
     X0(K+1) = XSAT  
ELSE  
     X0(K) = XSAT  
END IF  
!
!     loop for every observers-frame-frequency
!
  270 CONTINUE  

DO I = 1,NFOBS  
!
!---- i-core calculated under difussion appr. with limb darkening
!
!         opa=opacon(nd,1)+(opacon(nd,2)-opacon(nd,1))/(xcblue-
!     *         xcred)*(x0(i)-xcred)
!
!     changed to be consistent with constant opacity approx. used
!     in elscat
!
     OPA = OPACON(ND,1) + (OPACON(ND,2)-OPACON(ND,1))/ (XCBLUE- &
      XCRED)* (0.D0-XCRED)
     XX0 = X0(I)*VMAX*XNUE0/CLIGHT + XNUE0  

     AIC(I,1) = BPLANC(XX0,TEM)  
     AIC(I,2) = AIC(I,1)*C2*XX0*TGRAD/TEM/OPA/ (1.D0-EXP(-C2*XX0))  

END DO  

RETURN  

END
!
!***********************************************************************
!
! subroutines: broadening functions
!
!***********************************************************************
!
FUNCTION PERFIL1(I,XI,TEM,XNE,VTURBL,FLAG)  
!
! no action concerning clumping required, since we solve inside clumps,
! and n_e is the increased value
!  
! new version, allowing for depth dependent vturb (from header)
USE nlte_type
USE nlte_dim
USE fund_const, ONLY: AKB, XMH=>AMH,CLIGHT,RAPI=>SQPI
USE formalsol_var, ONLY: GAMMAL,GAMMAC,VMAX,XNEMIN,WEIG,XNUE, &
& NSTARK,NLEVL,NLEVU,NWS,NTS,NES,QHALF,DWS,TS,ES,PS

IMPLICIT NONE
!
!----it calculates doppler and stark profiles (if neccesary) for the 'i'
!----component, at frequency 'x', with temperature and electron density
!----'tem' and 'xne' resp.
!    In this version, vturb = vturbmin = const has been folded (PREFORMAL) into all
!    tabulated profiles (Stark, Iso and Griem) 
!
!     .. parameters ..
INTEGER(I4B), PARAMETER :: MAXW=ID_MAXWW,MAXT=ID_MAXTT,MAXNE=ID_MAXNE, &
&          MAXPS=ID_MAXPS
!     ..
!     .. scalar arguments ..
REAL(DP) ::  TEM,XI,XNE,PERFIL1,VTURBL
INTEGER(I4B) ::  I  
LOGICAL FLAG  
!     ..
!     .. local scalars ..
REAL(DP) ::  A,AKTM,AVOIGT,DELDOP,DW,ELOG,PF,PFF,PFL,PL,PLF, &
&                 PLL,TLOG,VTH,WEF,WTF,WWF,XD,XLAM,XLAMB0
INTEGER(I4B) ::  IFI,J,JEB,JTB,JWB,L1,L2,L3,L4
!     ..
!     .. external functions ..
REAL(DP) ::  VOIGT  
EXTERNAL VOIGT  
!     ..
!     .. intrinsic functions ..
INTRINSIC ABS,EXP,LOG10,MAX,MIN,SQRT  
!     ..

FLAG = .FALSE.  
XLAMB0 = 1.D0/XNUE(I)  

IF (NSTARK(I).EQ.0 .OR. XNE.LT.XNEMIN) THEN  
!
!--- pure doppler profile (normalized in frequencies) with depth-dependent vturb
!
     AKTM = 2.D0*AKB*TEM/XMH/WEIG(I)+VTURBL*VTURBL  
     A = XI*XI*VMAX*VMAX/AKTM  

     IF (A.GT.25) THEN  
          PERFIL1 = 0.D0  
          FLAG = .TRUE.  
     ELSE  
          PERFIL1 = EXP(-A)/SQRT(AKTM)*XLAMB0/RAPI  
     END IF  

ELSE IF (NSTARK(I).EQ.2.or.NSTARK(I).EQ.3) THEN  
!---- with VTURBL
!
!------- voigt profile
!
     VTH = SQRT(2.D0*AKB*TEM/XMH/WEIG(I)+VTURBL*VTURBL)  
     DELDOP = VTH*XNUE(I)  
!
!--- note: gammal, gammac include 1/(4pi)
!
     AVOIGT = (GAMMAL(I)+XNE*GAMMAC(I))/DELDOP  
     XD = XI*VMAX/VTH  
     PERFIL1 = VOIGT(AVOIGT,XD)*XLAMB0/ (VTH*RAPI)  
!         p1=exp(-xd*xd)/vth*xlamb0/rapi
!         print*,log10(xne),' ',xd,' ',avoigt,' ',perfil,' ',p1
     RETURN  

ELSE IF (NSTARK(I).EQ.1) THEN  
!---- with VTURB=VTURBMIN (implicit included in tables)
!
!--- stark profile (doppler broadening -- temp+Stark -- is included)
!
     XLAM = XLAMB0/ (1.D0+XI*VMAX/CLIGHT)  
!
!--- weights for wavelength
!
     DW = (XLAM-XLAMB0)*1.D8  
     IF (QHALF(I)) DW = ABS(DW)  
!
!--- out of freq. range in table (assuming then phi = 0.)
!
     IF (DW.LT.DWS(1,I) .OR. DW.GT.DWS(NWS(I),I)) THEN  
          FLAG = .TRUE.  
          PERFIL1 = 0.D0  
          RETURN  
     END IF  

     DO J = 2,NWS(I)  
          IFI = J  
          IF (DWS(J,I).GE.DW) GO TO 20  
     END DO

     STOP ' ERROR IN TABLE FREQ. GRID'  

   20      CONTINUE  

     JWB = IFI - 1  
     WWF = (DW-DWS(IFI-1,I))/ (DWS(IFI,I)-DWS(IFI-1,I))  
!
!--- weights for t
!
     TLOG = LOG10(TEM)  
     TLOG = MAX(TS(1,I),MIN(TS(NTS(I),I),TLOG))  
     DO J = 2,NTS(I)  
          IFI = J  
          IF (TS(J,I).GE.TLOG) EXIT  
     END DO

     JTB = IFI - 1  
     WTF = (TLOG-TS(IFI-1,I))/ (TS(IFI,I)-TS(IFI-1,I))  
!
     ELOG = LOG10(XNE)  
     IF (ELOG.LT.ES(1,I)) THEN
       IF(XNEMIN.NE.0.) then
         print*,elog,es(1,i)
         sTOP ' ERROR IN XNEMIN OR STARK PROFILE TABLES'
       endif 
!
!     perfil called by CALCXMAX (xnemin not calculated yet) and
!      very thin atmosphere (SNe)
!
      ELOG=ES(1,I)  !approximate treatment (just to calculate xmax)
     ENDIF
    
     DO J = 2,NES(I)  
          IFI = J  
          IF (ES(J,I).GE.ELOG) EXIT  
     END DO

     JEB = IFI - 1  
     WEF = (ELOG-ES(IFI-1,I))/ (ES(IFI,I)-ES(IFI-1,I))  
!
!--- interpolation
!
     L1 = JWB + NWS(I)* (JTB+NTS(I)*JEB)  
     PFF = WWF* (PS(L1+1,I)-PS(L1,I)) + PS(L1,I)  
     L2 = L1 - NWS(I)  
     PLF = WWF* (PS(L2+1,I)-PS(L2,I)) + PS(L2,I)  
     L3 = L1 - NWS(I)*NTS(I)  
     PFL = WWF* (PS(L3+1,I)-PS(L3,I)) + PS(L3,I)  
     L4 = L3 - NWS(I)  
     PLL = WWF* (PS(L4+1,I)-PS(L4,I)) + PS(L4,I)  
     PF = WTF* (PFF-PLF) + PLF  
     PL = WTF* (PFL-PLL) + PLL  

     PERFIL1 = 10.D0** (WEF* (PF-PL)+PL)  

ELSE  

    STOP 'ERROR IN NSTARK  -  PERFIL1'  

END IF  

RETURN  
END
!
!-----------------------------------------------------------------------
!
SUBROUTINE RCONV(X,Y0,Y1,N,VROT)  
!
USE nlte_type
USE nlte_dim
IMPLICIT NONE
!
!
!-----unrotated profile y0
!-----  rotated profile y1
!
!-----rotation convolution result returned in y0 y1 is scratch
!-----limb-darkening coefficient 1.5 used
!     .. scalar arguments ..
REAL(DP) ::  VROT  
INTEGER(I4B) ::  N  
!     ..
!     .. array arguments ..
REAL(DP) ::  X(N),Y0(N),Y1(N)  
!     ..
!     .. local scalars ..
REAL(DP) ::  CON,DIV,PB,PBX,PF,PFX,PI1,QAD,SUMM,X0,XB,XF  
INTEGER(I4B) ::  I,J,M0,N0  
!     ..
!     .. intrinsic functions ..
INTRINSIC MAX,MIN,SQRT  
!     ..

IF (VROT.LT.0.1) THEN  
     DO I = 1,N  
          Y1(I) = Y0(I)  
     END DO
     RETURN  
END IF  

CON = 2.997925D5/VROT  
PI1 = 0.31830D0  
N0 = 2  

DO I = 1,N  
     SUMM = 0.D0  
     QAD = 0.D0  
     M0 = N0  
     X0 = X(I)  
     DIV = CON/X0  
     DO J = M0,N  
          XB = (X(J-1)-X0)*DIV  
          XF = (X(J)-X0)*DIV  
          IF (XB.GT.1.0) GO TO 40  
          IF (XF.LT.-1.0) GO TO 20  
          XB = MAX(-1.0D0,XB)  
          XF = MIN(1.0D0,XF)  
          PBX = 1.0D0 - XB*XB  
          PFX = 1.0D0 - XF*XF  
          PB = PI1*SQRT(PBX) + .375D0*PBX  
          PF = PI1*SQRT(PFX) + .375D0*PFX  
          SUMM = SUMM + (PB*Y0(J-1)+PF*Y0(J))* (XF-XB)  
          QAD = QAD + (PB+PF)* (XF-XB)  
          EXIT
   20     CONTINUE  
          N0 = J  
     END DO

   40 CONTINUE  

     Y1(I) = SUMM/QAD  
END DO  

RETURN  
END
!
!-----------------------------------------------------------------------
!
FUNCTION VOIGT(AA,VV)  
!
USE nlte_type
USE nlte_dim
IMPLICIT NONE
!
!-----computes unnormalized voigt function--i.e. normalized to root pi
!     .. scalar arguments ..
REAL(DP) ::  AA,VV,VOIGT
!     ..
!     .. local scalars ..
REAL(DP) ::  A,A2,A4,A6,C1,C2,V,V2,V4,V6,W,X,Z,Z2  
INTEGER(I4B) ::  I,J  
!     ..
!     .. local arrays ..
REAL(DP) ::  H(25)  
!     ..
!     .. external functions ..
REAL(DP) ::  DAWSON  
EXTERNAL DAWSON  
!     ..
!     .. intrinsic functions ..
INTRINSIC ABS,EXP  
!     ..
!     .. save statement ..
SAVE  
!     ..
!     .. data statements ..
DATA C1,C2/1.128379167095512D0,5.64189583547756D-1/  
!     ..
V = ABS(VV)  
A = AA  
V2 = V*V  
A2 = A*A  
Z = A2 + V2  
IF (A.LE.0.5) GO TO 20  
IF (Z.LT.10.) GO TO 50  
!-----asymptotic expansion for large modulus
   10 CONTINUE  
Z2 = Z*Z  
V4 = V2*V2  
V6 = V4*V2  
A4 = A2*A2  
A6 = A4*A2  
VOIGT = C2*A* (1.D0+ ((1.875D0* (7.D0*V6-35.D0*A2*V4+21.D0*A4*V2- &
&        A6)/Z2+0.75D0* (5.D0*V4-10.D0*A2*V2+A4))/Z2+1.5D0*V2- &
&        0.5D0*A2)/Z2)/Z
RETURN  
!-----harris expansion
   20 CONTINUE  
IF (V.GT.5.) GO TO 10  
W = DAWSON(V)  
H(1) = EXP(-V2)  
H(2) = -C1* (1.D0-2.D0*V*W)  
H(3) = (1.D0-2.D0*V2)*H(1)  
H(4) = -C1* (2.D0* (1.D0-V2)/3.D0-2.D0*V*W* (1.D0-2.D0*V2/3.D0))  
!-----higher terms by recursion
DO 30 I = 5,11  
     X = I - 1  
     H(I) = (2.D0* (2.D0*X-3.D0-2.D0*V2)*H(I-2)-4.D0*H(I-4))/ &
      (X* (X-1.D0))
   30 END DO  
VOIGT = H(11)  
DO 40 I = 1,10  
     J = 11 - I  
     VOIGT = H(J) + A*VOIGT  
   40 END DO  
RETURN  
!-----gronwall expansion
   50 CONTINUE  
X = 1.D0/ (1.D0+3.275911D-1*A)  
H(1) = ((((1.061405429D0*X-1.453152027D0)*X+1.421413741D0)*X- &
&       2.84496736D-1)*X+2.54829592D-1)*X
DO 60 I = 2,25  
     X = I - 1  
     H(I) = 2.D0*A* (C2-A*H(I-1))/ (2.D0*X-1.D0)  
   60 END DO  
VOIGT = 0.D0  
DO 70 I = 1,24  
     J = 26 - I  
     X = J - 1  
     VOIGT = (VOIGT+H(J))*V2/X  
   70 END DO  
VOIGT = EXP(-V2)* (VOIGT+H(1))  

RETURN  
END
!
!-----------------------------------------------------------------------
!
FUNCTION DAWSON(XX)  
!
USE nlte_type
USE nlte_dim
IMPLICIT NONE
!
!-----dawson*s integral using anl algorithm, math comp,1970,171
!     .. scalar arguments ..
REAL(DP) ::  XX,DAWSON
!     ..
!     .. local scalars ..
REAL(DP) ::  DOWN,U,UP,X  
!     ..
!     .. save statement ..
SAVE  
!     ..
X = XX  
U = X*X  
IF (X.LT.5.0) GO TO 10  
!-----x greater than 5
DAWSON = ((5.0000000167450D-1+7.4999919056701D-1/ &
&         (-2.5001711668562D0+U-2.4878765880441D0/ &
&         (-4.6731202214124D0+U-4.1254406560831D0/ &
&         (-1.1195216423662D1+U))))/U+1.0D0)/ (2.0D0*X)
RETURN  
!-----x on (3.5,5.0)
   10 CONTINUE  
IF (X.LT.3.5) GO TO 20  
DAWSON = (5.00001538408193D-1+2.49811162845499D-1/ &
&         (-1.53672069271915D0+U-6.53419359860764D-1/ &
&         (-1.77068693717670D1+U+2.04866410976332D2/ &
&         (7.49584016278357D0+U-2.298758419286D0/ &
&         (4.02187490205698D1+U+2.53388006963558D3/ &
&         (-5.9391591850032D1+U))))))/X
RETURN  
!-----x on (2.5,3.5)
   20 CONTINUE  
IF (X.LT.2.5) GO TO 30  
DAWSON = (5.0140106611704D-1+1.8897553014354D-1/ &
&         (-7.4499050579364D0+U+7.0204980729194D1/ &
&         (7.5077816490106D0+U+4.1821806337830D1/ &
&         (-2.6629001073842D1+U+3.7343084728334D1/ &
&         (3.0984087863402D1+U+1.2599323546764D3/ &
&         (-4.0847391212716D1+U))))))/X
RETURN  
!-----x less than 2.5
   30 CONTINUE  
UP = (((((U*2.0846835103886D-2-8.5410681195954D-1)*U+ &
&     5.4616122556699D1)*U-4.3501160207595D2)*U+9.6696398191665D3)* &
&     U-2.9179464300780D4)*U + 2.3156975201341D5
DOWN = (((((U+2.9391995612556D1)*U+4.668490654511D2)*U+ &
&       4.7447098440662D3)*U+3.1384620138163D4)*U+ &
&       1.2520037031851D5)*U + 2.3156975201425D5
DAWSON = X* (UP/DOWN)  

RETURN  
END
!
!***********************************************************************
!
! subroutines: miscellaneous
!
!***********************************************************************
!
FUNCTION BPLANC(X,T)  
!
USE nlte_type
USE nlte_dim
IMPLICIT NONE
!
!     .. scalar arguments ..
REAL(DP) ::  T,X,BPLANC
!     ..
REAL(DP), PARAMETER :: C1=3.972970127D-16, C21=1.4388354967334D0  
!     .. local scalars ..
REAL(DP) ::  C2  
!     ..
!     .. intrinsic functions ..
INTRINSIC EXP  
!     ..
!C1 = 3.9728D-16  
!C2 = 1.439426D0/T  
C2=C21/T
! changed by JO July 2015
BPLANC = C1* (X**3)/ (EXP(C2*X)-1.D0)  

RETURN  
END
!
!-----------------------------------------------------------------------
!
FUNCTION EXPUNO(DT)  
!
USE nlte_type
USE nlte_dim
IMPLICIT NONE
!
!---- calculation of (1-exp(-dt)), expanding the exp for small dt
!
!     .. scalar arguments ..
REAL(DP) ::  DT,EXPUNO
!     ..
!     .. intrinsic functions ..
INTRINSIC EXP  
!     ..

IF (ABS(DT).LT.1.D-8) THEN  
     EXPUNO = DT - 5.D-1*DT*DT  
ELSE  
     EXPUNO = 1.D0 - EXP(-DT)  
END IF  

RETURN  
END
!
!-----------------------------------------------------------------------
!
SUBROUTINE SORT(N,RA)  
!
USE nlte_type
USE nlte_dim
IMPLICIT NONE
!
!     .. scalar arguments ..
INTEGER(I4B) ::  N  
!     ..
!     .. array arguments ..
REAL(DP) ::  RA(N)  
!     ..
!     .. local scalars ..
REAL(DP) ::  RRA  
INTEGER(I4B) ::  I,IR,J,L  
!     ..

L = N/2 + 1  
IR = N  
   10 CONTINUE  

IF (L.GT.1) THEN  
     L = L - 1  
     RRA = RA(L)  
ELSE  
     RRA = RA(IR)  
     RA(IR) = RA(1)  
     IR = IR - 1  
     IF (IR.EQ.1) THEN  
          RA(1) = RRA  
          RETURN  
     END IF  
END IF  

I = L  
J = L + L  

   20 CONTINUE  

IF (J.LE.IR) THEN  
     IF (J.LT.IR) THEN  
          IF (RA(J).LT.RA(J+1)) J = J + 1  
     END IF  

     IF (RRA.LT.RA(J)) THEN  
          RA(I) = RA(J)  
          I = J  
          J = J + J  
     ELSE  
          J = IR + 1  
     END IF  

     GO TO 20  

END IF  

RA(I) = RRA  

GO TO 10  

END
!
!***********************************************************************
!
! subroutines: algebraic ones
!
!***********************************************************************
!
SUBROUTINE GMALU(GA,U,LMAX)  
!
USE nlte_type
USE nlte_dim
IMPLICIT NONE
!
!***  algebraic routine called from cmfray
!      result overwrites ga
!
!
!     .. scalar arguments ..
INTEGER(I4B) ::  LMAX  
!     ..
!     .. array arguments ..
REAL(DP) ::  GA(LMAX),U(LMAX)  
!     ..
!     .. local scalars ..
REAL(DP) ::  A,B  
INTEGER(I4B) ::  L,LZ  
!     ..

LZ = LMAX - 1  
B = U(1)  

DO L = 1,LZ  
     A = B  
     B = U(L+1)  
     GA(L) = GA(L)* (A-B)
END DO  

RETURN  
END
!
!-----------------------------------------------------------------------
!
SUBROUTINE INV(N,NDIM,A)  
!
USE nlte_type
USE nlte_dim
IMPLICIT NONE
!
!     scaled matrix inversion
!
!     .. scalar arguments ..
INTEGER(I4B) ::  N,NDIM  
!     ..
!     .. array arguments ..
REAL(DP) ::  A(NDIM,NDIM)  
!     ..
!     .. local scalars ..
REAL(DP) ::  QI  
INTEGER(I4B) ::  I,II  
!     ..
!     .. local arrays ..
REAL(DP) ::  Q(200)  
!     ..
!     .. external subroutines ..
EXTERNAL MATINV  
!     ..
!     .. intrinsic functions ..
INTRINSIC ABS,MAX  
!     ..

DO I = 1,N  
     QI = 0.D0  

     DO II = 1,N  
          QI = MAX(QI,ABS(A(II,I)))  
     END DO

     Q(I) = QI  

     DO II = 1,N  
          A(II,I) = A(II,I)/QI  
     END DO

END DO  

CALL MATINV(A,N,NDIM)  

DO II = 1,N  
     QI = Q(II)  
     DO I = 1,N  
          A(II,I) = A(II,I)/QI  
     END DO
END DO  

RETURN  
END
!
!-----------------------------------------------------------------------
!
SUBROUTINE MATINV(A,N,NO)  
!
USE nlte_type
USE nlte_dim
IMPLICIT NONE
!
!
!-----matinv executes matrix inversion by lu decomposition
!-----inversion is accomplished in place,
!-----and the original matrix is replaced by its inverse
!-----note that n must be smaller than no
!
!     .. scalar arguments ..
INTEGER(I4B) ::  N,NO  
!     ..
!     .. array arguments ..
REAL(DP) ::  A(NO,NO)  
!     ..
!     .. local scalars ..
REAL(DP) ::  DIV,SUMM  
INTEGER(I4B) ::  I,II,IM1,IP1,J,JJ,JM1,JP1,K,KO,L  
!     ..
!     .. intrinsic functions ..
INTRINSIC MAX0  
!     ..

ILOOP1: DO I = 2,N  

     IM1 = I - 1  
     DO J = 1,IM1  
          JM1 = J - 1  
          IF (A(J,J).EQ.0.0D0) A(J,J) = 1.0D-20  
          DIV = A(J,J)  
          SUMM = 0.0D+00  
          IF (JM1.LT.1) GO TO 20  

          DO L = 1,JM1  
               SUMM = SUMM + A(I,L)*A(L,J)  
          END DO

   20     CONTINUE  
          A(I,J) = (A(I,J)-SUMM)/DIV  
     END DO

     DO J = I,N  
          SUMM = 0.0D+00  
          DO L = 1,IM1  
               SUMM = SUMM + A(I,L)*A(L,J)  
          END DO
          A(I,J) = A(I,J) - SUMM  
     END DO
END DO ILOOP1 

IILOOP1: DO II = 2,N  
     I = N + 2 - II  
     IM1 = I - 1  
     IF (IM1.LT.1) CYCLE  
     DO JJ = 1,IM1  
          J = I - JJ  
          JP1 = J + 1  
          SUMM = 0.0D+00  
          IF (JP1.GT.IM1) GO TO 80  
          DO K = JP1,IM1  
               SUMM = SUMM + A(I,K)*A(K,J)  
          END DO
   80     CONTINUE  
          A(I,J) = -A(I,J) - SUMM  
     END DO
END DO IILOOP1  

IILOOP2: DO II = 1,N  
     I = N + 1 - II  
     IF (A(I,I).EQ.0.0D0) A(I,I) = 1.0D-20  
     DIV = A(I,I)  
     IP1 = I + 1  
     IF (IP1.GT.N) GO TO 130  
     DO JJ = IP1,N  
          J = N + IP1 - JJ  
          SUMM = 0.0D+00  
          DO K = IP1,J  
               SUMM = SUMM + A(I,K)*A(K,J)  
          END DO
          A(I,J) = -SUMM/DIV  
     END DO
  130 CONTINUE  
     A(I,I) = 1.D0/A(I,I)  
END DO IILOOP2  

ILOOP2: DO I = 1,N  
     DO J = 1,N  
          KO = MAX0(I,J)  
          IF (KO.EQ.J) GO TO 170  
          SUMM = 0.0D+00  

  150     CONTINUE  

          DO K = KO,N  
               SUMM = SUMM + A(I,K)*A(K,J)  
          END DO
          GO TO 180  

  170     CONTINUE  

          SUMM = A(I,KO)  
          IF (KO.EQ.N) GO TO 180  
          KO = KO + 1  
          GO TO 150  

  180     CONTINUE  
          A(I,J) = SUMM  

     END DO
END DO ILOOP2  

RETURN  
END
!
!-----------------------------------------------------------------------
!
SUBROUTINE INVTRI(A,B,C,Q,N)  
!
USE nlte_type
USE nlte_dim
IMPLICIT NONE
!
!     solution of tridiagonal equation system with
!     tridiag. matrix  -a(l),b(l),-c(l)  ,
!     right side: vector q(l),solution overwrites q
!
!
!     .. scalar arguments ..
INTEGER(I4B) ::  N  
!     ..
!     .. array arguments ..
REAL(DP) ::  A(N),B(N),C(N),Q(N)  
!     ..
!     .. local scalars ..
REAL(DP) ::  H  
INTEGER(I4B) ::  I  
!     ..

C(1) = C(1)/B(1)  
Q(1) = Q(1)/B(1)  

IF (N.EQ.1) RETURN  
IF (N.EQ.2) GO TO 20  

DO I = 2,N - 1  
     H = B(I) - A(I)*C(I-1)  
     C(I) = C(I)/H  
     Q(I) = (Q(I)+Q(I-1)*A(I))/H  
END DO  

   20 CONTINUE  

Q(N) = (Q(N)+Q(N-1)*A(N))/ (B(N)-C(N-1)*A(N))  

DO I = 1,N - 1  
     Q(N-I) = Q(N-I) + C(N-I)*Q(N-I+1)  
END DO  

RETURN  
END
!
!-----------------------------------------------------------------------
!
SUBROUTINE INVTRI3(TA,TB,TC,A,K,N)  
!
USE nlte_type
USE nlte_dim
IMPLICIT NONE
!
!
!     .. scalar arguments ..
INTEGER(I4B) ::  K,N  
!     ..
!     .. array arguments ..
REAL(DP) ::  A(ID_NDEPT,ID_NDEPT),TA(ID_NDEPT),TB(ID_NDEPT), &
&                 TC(ID_NDEPT)
!     ..
!     .. local scalars ..
INTEGER(I4B) ::  I,J,L  
!     ..
!     .. local arrays ..
REAL(DP) ::  D(0:100),DIA(100),E(101)  
!     ..

IF (N.GT.100) STOP 'TOO MANY GRID POINTS IN DIAG'  

D(0) = 0.D0  

DO L = 1,K  
     D(L) = -TC(L)/ (TB(L)+TA(L)*D(L-1))  
END DO
E(K+1) = 0.D0  

DO L = K,1,-1  
     E(L) = -TA(L)/ (TB(L)+TC(L)*E(L+1))  
END DO  

DO L = 1,K  
     DIA(L) = 1.D0/ ((1.D0-D(L)*E(L+1))* (TB(L)+TA(L)*D(L-1)))  
     A(L,L) = DIA(L)  
END DO  

DO J = 2,K  
     DO I = J - 1,1,-1  
          A(I,J) = A(I+1,J)*D(I)  
     END DO
END DO  

DO J = 1,K - 1  
     DO I = J + 1,K  
          A(I,J) = A(I-1,J)*E(I)  
     END DO
END DO  

RETURN  
END
!
!-----------------------------------------------------------------------
!
SUBROUTINE MADD(A,B,N,NDIM)  
!
USE nlte_type
USE nlte_dim
IMPLICIT NONE
!
!     a(i,j)=a(i,j)+b(i,j)
!
!
!     .. scalar arguments ..
INTEGER(I4B) ::  N,NDIM  
!     ..
!     .. array arguments ..
REAL(DP) ::  A(NDIM,NDIM),B(NDIM,NDIM)  
!     ..
!     .. local scalars ..
INTEGER(I4B) ::  I,J  
!     ..

A(1:N,1:N)=A(1:N,1:N)+B(1:N,1:N)

RETURN  
END
!
!-----------------------------------------------------------------------
!
SUBROUTINE MDMV(A,B,N,NDIM)  
!
USE nlte_type
USE nlte_dim
IMPLICIT NONE
!
!     matrix a(voll)=: matrix b(diag) * matrix a(voll)
!
!
!     .. scalar arguments ..
INTEGER(I4B) ::  N,NDIM  
!     ..
!     .. array arguments ..
REAL(DP) ::  A(NDIM,NDIM),B(NDIM)  
!     ..
!     .. local scalars ..
INTEGER(I4B) ::  I,J  
!     ..

DO I = 1,N  
     DO J = 1,N  
          A(I,J) = B(I)*A(I,J)  
     END DO
END DO  

RETURN  
END
!
!-----------------------------------------------------------------------
!
SUBROUTINE MDV(A,V,N)  
!
USE nlte_type
USE nlte_dim
IMPLICIT NONE
!
!     matrix(diagonal) * vector v
!     result overwrites v
!
!
!     .. scalar arguments ..
INTEGER(I4B) ::  N  
!     ..
!     .. array arguments ..
REAL(DP) ::  A(N),V(N)  
!     ..
!     .. local scalars ..
INTEGER(I4B) ::  I  
!     ..

V(1:N)=A(1:N)*V(1:N)

RETURN  
END
!
!-----------------------------------------------------------------------
!
SUBROUTINE MVMD(A,B,N,NDIM)  
!
USE nlte_type
USE nlte_dim
IMPLICIT NONE
!
!     matrix a(voll)=: matrix a(voll) * matrix b(diag)
!
!
!     .. scalar arguments ..
INTEGER(I4B) ::  N,NDIM  
!     ..
!     .. array arguments ..
REAL(DP) ::  A(NDIM,NDIM),B(NDIM)  
!     ..
!     .. local scalars ..
INTEGER(I4B) ::  I,J  
!     ..

DO J = 1,N  
     DO I = 1,N  
          A(I,J) = A(I,J)*B(J)  
     END DO
END DO  

RETURN  
END
!
!-----------------------------------------------------------------------
!
SUBROUTINE MVV(WX,B,W,JMAX,JMM,JP)  
!
USE nlte_type
USE nlte_dim
IMPLICIT NONE
!
!     matrix(voll) b *vektor w= vektor wx
!     format: wx(jmax)=b(jmax,jmm)*w(jmm)
!
!     .. scalar arguments ..
INTEGER(I4B) ::  JMAX,JMM,JP  
!     ..
!     .. array arguments ..
REAL(DP) ::  B(JP,JP),W(JP),WX(JP)  
!     ..
!     .. local scalars ..
REAL(DP) ::  WXI  
INTEGER(I4B) ::  I,K  
!     ..

DO I = 1,JMAX  
     WXI = 0.D0  
     DO K = 1,JMM  
          WXI = WXI + B(I,K)*W(K)  
     END DO
     WX(I) = WXI  
END DO  

RETURN  
END
!
!-----------------------------------------------------------------------
!
SUBROUTINE VADD(A,B,N)  
!
USE nlte_type
USE nlte_dim
IMPLICIT NONE
!
!     vector addition a=a+b
!
!
!     .. scalar arguments ..
INTEGER(I4B) ::  N  
!     ..
!     .. array arguments ..
REAL(DP) ::  A(N),B(N)  
!     ..
!     .. local scalars ..
INTEGER(I4B) ::  I  
!     ..

A(1:N)=A(1:N)+B(1:N)

RETURN  
END
!
!-----------------------------------------------------------------------
!
SUBROUTINE VMALV(VA,VB,V,Q,LMAX)  
!
USE nlte_type
USE nlte_dim
IMPLICIT NONE
!
!***  algebraic routine called from cmfray
!
!
!     .. scalar arguments ..
INTEGER(I4B) ::  LMAX  
!     ..
!     .. array arguments ..
REAL(DP) ::  Q(LMAX),V(LMAX-1),VA(LMAX),VB(LMAX)  
!     ..
!     .. local scalars ..
REAL(DP) ::  A,B  
INTEGER(I4B) ::  L,LZ  
!     ..

LZ = LMAX - 1  
B = V(1)  
Q(1) = VB(1)*B  

DO L = 2,LZ  
     A = B  
     B = V(L)  
     Q(L) = VA(L)*A + VB(L)*B  
END DO  

Q(LMAX) = VA(LMAX)*B  

RETURN  
END
!
!-----------------------------------------------------------------------
!
SUBROUTINE VSUB(A,B,N)  
!
USE nlte_type
USE nlte_dim
IMPLICIT NONE
!
!     a(i)=a(i)-b(i)
!
!
!     .. scalar arguments ..
INTEGER(I4B) ::  N  
!     ..
!     .. array arguments ..
REAL(DP) ::  A(N),B(N)  
!     ..
!     .. local scalars ..
INTEGER(I4B) ::  I  
!     ..

A(1:N)=A(1:N)-B(1:N)

RETURN  
END
!
!-----------------------------------------------------------------------
!
SUBROUTINE OCCUP_AT(NL,NU,XNE,TEMP,XNHII,XNL,XNU)
USE nlte_type
USE nlte_dim
USE fund_const, ONLY: CLIGHT,SAHA,HK
USE preformal_var, ONLY: GL,FL
IMPLICIT NONE
!
!---- calculates approximate occup. for NL and NU, assuming dep=1.
!
!     ..
!     .. scalar arguments ..
REAL(DP) ::  XNE,TEMP,XNHII,XNL,XNU,CONST,GHII=1.D0  
INTEGER(I4B) ::  NL,NU

CONST=0.5D0*SAHA/GHII*XNE*XNHII/TEMP**1.5
XNL=CONST*GL(NL)*EXP(HK*FL(NL)/TEMP)
XNU=CONST*GL(NU)*EXP(HK*FL(NU)/TEMP)

RETURN
END
