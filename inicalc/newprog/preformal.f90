MODULE preformal_var
!
USE nlte_type
USE nlte_dim
IMPLICIT NONE
!
!for STAFIL and LINETAB 
!
character*(*), parameter :: fpath='../inicalc/DATA/'
!----------------------------------------------------------------------
!comcha
!
CHARACTER*6, DIMENSION(ID_ATOMS) :: LABAT 
CHARACTER*6, DIMENSION(ID_LLEVS) :: LABL 
!----------------------------------------------------------------------
!comnui
!
INTEGER(I4B) ::  NALIN,NAT,NL
INTEGER(I4B), DIMENSION(ID_LLEVS) :: LE  
!----------------------------------------------------------------------
!comnur
!
REAL(DP), DIMENSION(ID_ATOMS) :: WEIGHT,ZEFF
REAL(DP), DIMENSION(ID_LLEVS) :: FL,GL,ZL
!----------------------------------------------------------------------
!charge,griem
!
INTEGER(I4B) ::  NS  
REAL(DP) ::  AS,ODOP,PS,ZZ  
REAL(DP), DIMENSION(69) :: SS,SX
!----------------------------------------------------------------------
!inout
!
INTEGER(I4B) ::  IU,IOU

END MODULE preformal_var
!
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccc                                                                cccc
!ccc      preparation of formal solucion                            cccc
!ccc                                                                cccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!
SUBROUTINE PREFORMAL(NCOMP,LEVEL,LEVEU,LINENO,NSTARK,VTURB,YHE)
!
USE nlte_type
USE nlte_dim
USE fund_const, ONLY: PI
USE preformal_var, ONLY: FPATH,NAT,NL,FL,GL,ZL,WEIGHT,ZEFF,IU,IOU
USE ffr_error
IMPLICIT NONE
!
!------ program run before the formal solution is performed
!------ it uses two files as input:
!
!            i) ATOM_FILE: contains the files of atomic data and stark
!                          profiles
!
!------ and two files as output:
!
!            i) IX.DAT: it will be used by the formal solution as input
!           ii) STARK.BIN: binary file containing the stark profile
!                          of the transition(s) considered
!
!  stark broadening options:
!  0: pure doppler
!  1: stark (from tables or griem)
!  2: voigt, with gammal and gammac from LINES.dat
!  3: voigt, with gammal and gammac approximated
!    *works only for ncomp=1*: 
!      lineno   =0: gammal and gammac calculated
!      lineno ne 0: gammal calculated, gammac=0.

!NOTE: if GammaL and GammaC not available, can be approximated quite well
!by using one line of the multiplet with lineno=0 and stark=3
!(checked, e.g., by comparing Si line values with given data)
!Works particularly good for lines with low lying lower level.

!
!------ enrique santolaya, november 1993, version 1.0
!
!       version 1.1 april 30th 1997 (j.puls)
!                  to be used as subroutine in 'formalsol'
!
!
!       version 1.2 july 4th 1997
!                  ix.dat contains now gf-value,
!                  broadening option "2" for voigt profiles incorporated
!
!
!       version 3.0.0 jan 21st 1999
!                  changed to f90, inclusion of modification by
!                  christian wiethaus: wavelenghts now for air!
!
!       version 3.1 march 11th 1999
!                   more f90 changes, obsolete features removed
!
!
!       version 4.0 april 16th 1999
!                   ISO-lines can now be treated, 
!                   vturb included by additional folding of tabulated data 
!
!       version 4.1 may 20th 1999
!                   GAMMAL and GAMMAC approximated for lines were no other
!                   information given (corresponds to NSTARK = 3)
!
!       version 4.1.1 june 16th 1999 
!                   error corrected (treatment of lines with two components
!		    with the same parent levels). This error was corrected
!		    by J. Puls, but new versions (4.2 and 5.0) have this 
!		    error. I have corrected it again for Carrie's version
!		    (Miguel A. Urbaneja, Bremen, january 3rd 2002)
!
!       version 4.2 june 28th 2000 inconsistency in griem (range = 6, &
!                   xmax in err = 5) removed 
!
!       version 5.0 feb 7th 2001 file paths to STAFIL and LINETAB
!                   changed via parameter fpath
!
!----lineno  = 0 : line from detail input, assuming that levels are
!                   not packed
!----lineno ne 0 : line for lines.dat, levels are packed
!
!       version 5.1 april 2004 new broadening routine for "iso1" levels
!                   according to the data by Dimitrijevic & Sahal-Brechot.
!                   So far, proton/HeII contribution only approximate
!                   from YHe and HeI input values (assumed to be constant
!                   throughout the atmosphere)
!
!       version 5.2 may 2006: stop statements in transic1 removed; allows
!                   to have lines in LINES.DAT from levels which are NOT
!                   present in the model
!
!       version 6.0 july 2009: generalization to arbitrary NCOMP 
!                   old bug removed: calculation with pure Doppler-broadening
!                   and more than one component possible

!       initializing data
!     .. scalar arguments ..
INTEGER(I4B) ::  NCOMP  
REAL(DP) :: VTURB,YHE
!     ..
!     .. array arguments ..
INTEGER(I4B), DIMENSION(NCOMP) ::  LINENO, NSTARK  
CHARACTER, DIMENSION(NCOMP) :: LEVEL*6,LEVEU*6  
!     ..
!     .. local scalars ..
REAL(DP) ::  AUX  
INTEGER(I4B) ::  I,IAUX,NS,NSUM
LOGICAL RTABLE  
CHARACTER DC*6,KEY*6,FICHERO*32,LINETAB*32,STAFIL*32,RET*4
!     ..
!     .. local arrays ..
REAL(DP), ALLOCATABLE, DIMENSION(:) ::  FLU,GAMMAC,GAMMAL,XLAMB,SUMLO,SUMUP  
INTEGER(I4B), ALLOCATABLE, DIMENSION(:) ::  NATOM,NLEVL,NLEVU,INDEX  
LOGICAL, ALLOCATABLE, DIMENSION(:) :: LOW  
!     ..
!     .. external subroutines ..
EXTERNAL FFRACC,PRESTARK,RDATOM,TRANSIC1,TRANSIC2  
!     ..
!     .. intrinsic functions ..
INTRINSIC MIN  
!     ..

ALLOCATE(FLU(NCOMP),GAMMAC(NCOMP),GAMMAL(NCOMP),XLAMB(NCOMP),SUMLO(NCOMP),SUMUP(NCOMP))
ALLOCATE(NATOM(NCOMP),NLEVL(NCOMP),NLEVU(NCOMP),INDEX(NCOMP))
ALLOCATE(LOW(NCOMP))

RTABLE = .FALSE.  
LOW = .FALSE.  

NL=0
NAT=0
WEIGHT=0.
ZEFF=0.
FL=0.
GL=0.
ZL=0.

NATOM=0
NLEVL=0
NLEVU=0
XLAMB=0.
FLU=0.
GAMMAL=0.
GAMMAC=0.
SUMLO=0.
SUMUP=0.

DO I =1,NCOMP
     IF (NSTARK(I).EQ.2 .OR. LINENO(I).NE.0) RTABLE = .TRUE.  
     IF (NSTARK(I).EQ.3 .AND. LINENO(I).NE.0) STOP' NSTARK = 3 AND LINENO NE 0'
END DO  

IU = 37  
IOU = 38  
DC = ':T'  

OPEN (1,FILE='ATOM_FILE',STATUS='OLD')  
REWIND 1  
READ (1,FMT='(A)') FICHERO  
READ (1,FMT='(A)') STAFIL  
IF (RTABLE) READ (1,FMT='(A)') LINETAB  
CLOSE (1)  

OPEN (UNIT=IU,FILE=FICHERO,FORM='FORMATTED',STATUS='OLD')  
OPEN (UNIT=IOU,FILE='control.dat',STATUS='UNKNOWN')  
!
!       rewind!!!
!
IU = -37  

CALL FFRACC(IU,RET)
IF (ON_ERROR(RET))  GOTO 60

RLOOP: DO  

CALL RDATOM(RET)  
IF (RET.NE.'RET0'.AND.RET.NE.'RET3')  GOTO 70
IF (RET.EQ.'RET3') GOTO 50

CALL TRANSIC1(LEVEL,LEVEU,NCOMP,LINENO,NLEVL,NLEVU,NATOM,XLAMB,FLU,LOW, &
&             SUMLO,SUMUP,NSTARK,GAMMAC,RET)
IF (ON_ERROR(RET))  GOTO 70

END DO RLOOP 
!
!       normal exit: end of file
!
   50 CONTINUE  

WRITE (*,FMT='(A)') ' DETAIL FILE IS OVER. SUCCESSFUL!! '  
CLOSE (IU)  


!  now, all contributions to SUMLO, SUMUP are calculated, &
!  hence we can calculate GAMMAL.
!  the constant below corresponds to 8 pi^2 e^2/(m_e c), if lambda in A

DO I=1,NCOMP
  IF(NSTARK(I).EQ.3) THEN
    GAMMAL(I)=6.6702082D15*(SUMLO(I)+SUMUP(I))/(4.*PI)
    PRINT*,GAMMAL(I),GAMMAC(I)
  ENDIF
END DO  

IF (RTABLE) THEN  
     OPEN (UNIT=IU,FILE=fpath//LINETAB,FORM='FORMATTED',STATUS='OLD')  
!
!       rewind!!!
!
     IU = -37  
     CALL FFRACC(IU,RET)  
     IF (ON_ERROR(RET))  GOTO 60
!
!       note here: flu corresponds to gf/sum(g), see above
!
     CALL TRANSIC2(LEVEL,LEVEU,NCOMP,NSTARK,LINENO,NLEVL,NLEVU, &
&                  NATOM,XLAMB,FLU,GAMMAL,GAMMAC,RET)
     IF (ON_ERROR(RET))  GOTO 70

     WRITE (*,FMT='(A)') ' LINE FILE IS OVER. SUCCESSFUL!! '  
     CLOSE (IU)  
END IF  

GO TO 90  
!
!       error in input unit exit
!
   60 CONTINUE  
WRITE (*,FMT='(A)') ' ERROR IN INPUT UNIT NUMBER '  
STOP

!       error conditions
70 SELECT CASE(RET)
CASE ('RET1') !       end of file "no esperado" exit  
  WRITE (*,FMT='(A)') ' EOF NOT EXPECTED. THERE IS SOMETHING WRONG! '  
  STOP
CASE ('RET2') !       error in ffr subroutines exit  
  WRITE (*,FMT='(A)') ' ERROR IN FFR SUBROUTINES '  
  STOP
CASE DEFAULT
  STOP ' WRONG ERROR CONDITION IN PREFORMAL'
END SELECT

   90 CONTINUE  
CLOSE (IOU)  

DO I = 1,NCOMP  
    IF (NLEVL(I).EQ.0 .OR. NLEVU(I).EQ.0) STOP 'LEVELS NOT FOUND'  
    IF (XLAMB(I).LE.0.D0) STOP 'ERROR IN LAMBDA'  
    IF (FLU(I).LE.0.D0) STOP 'ERROR IN FLU'  
    IF (NATOM(I).EQ.0) STOP 'ERROR IN ATOM'  
END DO  
!
!---    sorting for wavelengths (blue component at first)
!
CALL INDEXX(NCOMP,XLAMB,INDEX)
!
!---    interchange of components
!
XLAMB=XLAMB(INDEX)
FLU=FLU(INDEX)
GAMMAL=GAMMAL(INDEX)
GAMMAC=GAMMAC(INDEX)
NLEVL=NLEVL(INDEX)
NLEVU=NLEVU(INDEX)
NATOM=NATOM(INDEX)
NSTARK=NSTARK(INDEX)
!
!---    from here on, comp 1 = blue component
!
!
!------ nstark=0 : doppler
!------ nstark=1 : stark
!------ nstark=2 : voigt with data from LINETAB (LINES.DAT)
!------ nstark=3 : voigt with approx. for GAMMAL/GAMMAC (see above comments)
!

NSUM=0
DO I = 1,NCOMP  
     IF (NSTARK(I).EQ.1) NSUM = NSUM + 1  
END DO  
!
!------ nsum=0 : no stark;
!------ nsum=1 : only one comp. stark (nsum, precs.)
!------ nsum=n : n comps. stark
PRINT *,'NUMBER OF STARK-COMPONENTS',NSUM  

IF (NSUM.NE.0) &
& CALL PRESTARK(NSUM,STAFIL,NCOMP,NLEVL,NLEVU,XLAMB,FLU,NSTARK,VTURB,YHE)
!
!------ output file ix.dat
!
OPEN (1,FILE='IX.DAT',STATUS='UNKNOWN',FORM='FORMATTED')  
REWIND 1  

WRITE (1,FMT=*) NCOMP  
DO I = 1,NCOMP  
     WRITE (1,FMT=*) XLAMB(I)  
     WRITE (1,FMT=*) GL(NLEVL(I)),GL(NLEVU(I))  
     WRITE (1,FMT=*) GL(NLEVL(I))*FLU(I)  
     WRITE (1,FMT=*) NLEVL(I),NLEVU(I)  
     WRITE (1,FMT=*) WEIGHT(NATOM(I))  
     NS = NSTARK(I)  
     WRITE (1,FMT=*) NS  
     IF (NS.EQ.2.OR.NS.EQ.3) WRITE (1,FMT=*) GAMMAL(I),GAMMAC(I)  
END DO  
CLOSE (1)  

DEALLOCATE(FLU,GAMMAC,GAMMAL,XLAMB,SUMLO,SUMUP)
DEALLOCATE(NATOM,NLEVL,NLEVU,INDEX,LOW)

RETURN  

END
!
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!
SUBROUTINE RDATOM(RETCHAR)  
!
USE nlte_type
USE nlte_dim
USE preformal_var, ONLY: LABAT,LABL,NAT,NL,LE,FL,GL,ZL,WEIGHT,ZEFF
USE ffr_error
IMPLICIT NONE
!
!     .. local scalars ..
REAL(DP) ::  REALO,ZEF  
INTEGER(I4B) ::  INTEG,NC  
CHARACTER LABEL*4,KEY*6,RETCHAR*4,RET*4  
!     ..
!     .. external subroutines ..
EXTERNAL FFRKEY,FFRLOC,FFRNUM  
!     ..

LABEL = 'ATOM'  
CALL FFRLOC(LABEL,RET)  
SELECT CASE(RET)
CASE('RET1')
  GOTO 50
CASE('RET2')
  GOTO 60
CASE('RET0')
  CONTINUE
CASE DEFAULT
  STOP ' WRONG RETURN IN RDATOM'
END SELECT

NAT = NAT + 1  

CALL FFRKEY(KEY,NC,RET)  
IF (ON_ERROR(RET)) GOTO 60
LABAT(NAT) = KEY  

CALL FFRNUM(REALO,INTEG,RET)  
IF (ON_ERROR(RET)) GOTO 60
ZEFF(NAT)=REALO

CALL FFRNUM(REALO,INTEG,RET)  
IF (ON_ERROR(RET)) GOTO 60
WEIGHT(NAT) = REALO  

CALL FFRNUM(REALO,INTEG,RET)  
IF (ON_ERROR(RET)) GOTO 60

ZEF = ZEFF(NAT) - 1.D0  

   10 CONTINUE  

CALL FFRKEY(KEY,NC,RET)  
IF (ON_ERROR(RET)) GOTO 60

!------------- l  levels
IF (KEY.EQ.'L') THEN  
     ZEF=ZEF+1.D0
   20      CONTINUE  
     CALL FFRKEY(KEY,NC,RET)  
     IF (ON_ERROR(RET)) GOTO 60
     IF (KEY.EQ.'0') GO TO 10  
     NL = NL + 1  
     LABL(NL) = KEY  
     ZL(NL)=ZEF
     
     CALL FFRNUM(REALO,INTEG,RET)  
     IF (ON_ERROR(RET)) GOTO 60
     GL(NL) = REALO
     
     CALL FFRNUM(REALO,INTEG,RET)  
     IF (ON_ERROR(RET)) GOTO 60
     FL(NL) = REALO  
     LE(NL) = NAT  
     CALL FFRKEY(KEY,NC,RET)  
     IF (ON_ERROR(RET)) GOTO 60

     GO TO 20  

!----------------- x  levels
ELSE IF (KEY.EQ.'X') THEN  
   30      CONTINUE  
     CALL FFRKEY(KEY,NC,RET)  
     IF (ON_ERROR(RET)) GOTO 60
     
     IF (KEY.EQ.'0') GO TO 10  

     CALL FFRNUM(REALO,INTEG,RET)  
     IF (ON_ERROR(RET)) GOTO 60

     CALL FFRNUM(REALO,INTEG,RET)  
     IF (ON_ERROR(RET)) GOTO 60

     CALL FFRKEY(KEY,NC,RET)  
     IF (ON_ERROR(RET)) GOTO 60

     GO TO 30  

!-------------- s  levels
ELSE IF (KEY.EQ.'S') THEN  
   40      CONTINUE  
     CALL FFRKEY(KEY,NC,RET)  
     IF (ON_ERROR(RET)) GOTO 60

     IF (KEY.EQ.'0') GO TO 10  

     CALL FFRNUM(REALO,INTEG,RET)  
     IF (ON_ERROR(RET)) GOTO 60

     CALL FFRNUM(REALO,INTEG,RET)  
     IF (ON_ERROR(RET)) GOTO 60

     CALL FFRNUM(REALO,INTEG,RET)  
     IF (ON_ERROR(RET)) GOTO 60

     CALL FFRNUM(REALO,INTEG,RET)  
     IF (ON_ERROR(RET)) GOTO 60

     CALL FFRNUM(REALO,INTEG,RET)  
     IF (ON_ERROR(RET)) GOTO 60

     CALL FFRNUM(REALO,INTEG,RET)  
     IF (ON_ERROR(RET)) GOTO 60

     CALL FFRKEY(KEY,NC,RET)  
     IF (ON_ERROR(RET)) GOTO 60

     GO TO 40  

!--------------------  k  levels
ELSE IF (KEY.EQ.'K') THEN  
     NL = NL + 1  
     ZEF=ZEF+1
     CALL FFRKEY(KEY,NC,RET)  
     IF (ON_ERROR(RET)) GOTO 60
     LABL(NL) = KEY  

     CALL FFRNUM(REALO,INTEG,RET)  
     IF (ON_ERROR(RET)) GOTO 60
     GL(NL) = REALO  

     CALL FFRNUM(REALO,INTEG,RET)  
     FL(NL) = REALO  
     ZL(NL)=ZEF

     CALL FFRKEY(KEY,NC,RET)  
     IF (ON_ERROR(RET)) GOTO 60

END IF  

!       the atom is over.
RETCHAR='RET0'
RETURN  
!       no hay mas atomos que leer. ya se ha leido todo el fichero. es
!       cuando acaba el programa principal (prince)
   50 CONTINUE  

RETCHAR='RET3'
RETURN 

! error handling
60 SELECT CASE(RET)
CASE('RET1') !       end of file exit 
  WRITE (*,FMT='(A)') ' END OF FILE '  
  RETCHAR='RET1'
  RETURN
CASE('RET2') !       error exit 
  WRITE (*,FMT='(A)') 'ERROR IN FFR SUBROUTINES '  
  RETCHAR='RET2'
  RETURN
CASE DEFAULT
  STOP ' WRONG ERROR CONDITION IN RDATOM'
END SELECT

END
!
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!
SUBROUTINE TRANSIC1(LEVEL,LEVEU,NCOMP,LINENO,NLEVL,NLEVU,NATOM, &
&   XLAMB,FLU,LOW,SUMLO,SUMUP,NSTARK,GAMMAC,RETCHAR)
!
USE nlte_type
USE nlte_dim
USE fund_const, ONLY: CLIGHT
USE preformal_var, ONLY: GL,LABL,NL,LE,IU
USE ffr_error
IMPLICIT NONE
!
!       subrutina que lee los datos referentes a las transiciones, los
!       almacena y escribe (para control) en un fichero de salida.
!       espero poner suficientes explicaciones a lo largo de la
!       subrutina, de modo que este todo lo mas claro posible.
!
!     .. scalar arguments ..
INTEGER(I4B) ::  NCOMP  
!     ..
!     .. array arguments ..
REAL(DP) ::  FLU(NCOMP),XLAMB(NCOMP),SUMLO(NCOMP),SUMUP(NCOMP),GAMMAC(NCOMP) 
INTEGER(I4B) ::  LINENO(NCOMP),NATOM(NCOMP),NLEVL(NCOMP),NLEVU(NCOMP), &
&                NSTARK(NCOMP)
LOGICAL LOW(NCOMP)  
CHARACTER LEVEL(NCOMP)*6,LEVEU(NCOMP)*6  
!     ..
!     .. local scalars ..
REAL(DP) ::  REALO,TL,WLC  
INTEGER(I4B) ::  I,IFORM,INTEG,IQI,LABL1,LABU1,M,NC, &
&        NDATM,NDATOS,NLONG,NN,NNU
LOGICAL QCL,QRT  
CHARACTER KEY*6,RETCHAR*4,RET*4  
!     ..
!     .. local arrays ..
CHARACTER TABLA(8)*6  
!     ..
!     .. external functions ..
REAL(DP) ::  HALLCL,CALCGAM  
EXTERNAL HALLCL,CALCGAM  
!     ..
!     .. external subroutines ..
EXTERNAL FFRKEY,FFRNUM,IDENT  
!     ..
!     .. data statements ..
DATA TABLA/'RBB','CBB','RBX','CBX','CBS','CBF','RBF','RFF'/  
!     ..

   10 CONTINUE  

CALL FFRKEY(KEY,NC,RET)  

IF (ON_ERROR(RET))  GOTO 250

IF (KEY.EQ.'TY') THEN  
     CALL FFRKEY(KEY,NC,RET)  
     IF (ON_ERROR(RET))  GOTO 250

     CALL IDENT(KEY,TABLA,8,M)  
     CALL FFRNUM(REALO,IFORM,RET)  
     IF (ON_ERROR(RET))  GOTO 250

     CALL FFRNUM(REALO,NDATOS,RET)  
     IF (ON_ERROR(RET))  GOTO 250
!          son los numeros de formula y de datos, respectivamente
     NDATM = NDATOS  

     GO TO 10  

!       muestreo en anchuras doppler. el muestreo queda en dl.
ELSE IF (KEY.EQ.'DB') THEN  
     CALL FFRNUM(REALO,NLONG,RET)  
     IF (ON_ERROR(RET))  GOTO 250
     DO I = 1,NLONG  
          CALL FFRNUM(REALO,INTEG,RET)  
          IF (ON_ERROR(RET))  GOTO 250
     END DO

     GO TO 10  

!       muestreo en angstroms
ELSE IF (KEY.EQ.'DL') THEN  
     CALL FFRNUM(REALO,NLONG,RET)  
     IF (ON_ERROR(RET))  GOTO 250
     DO I = 1,NLONG  
          CALL FFRNUM(REALO,INTEG,RET)  
          IF (ON_ERROR(RET))  GOTO 250
     END DO

     GO TO 10  

!       temperatura de linea
ELSE IF (KEY.EQ.'TL') THEN  
     CALL FFRNUM(TL,INTEG,RET)  
     IF (ON_ERROR(RET))  GOTO 250

     GO TO 10  

!       longitud central de la linea
ELSE IF (KEY.EQ.'CL') THEN  
     QCL = .TRUE.  
     GO TO 10  

!       tipo de transicion
ELSE  
     CALL IDENT(KEY,TABLA,8,M)  
     IF (M.EQ.0) THEN  
          GO TO 270  
     ELSE  
          QRT = .FALSE.  
          IF ((M.EQ.1) .OR. (M.EQ.3)) QRT = .TRUE.  
          GO TO (40,40,130,130,160,160,190,220) M  
!  goto calculado, para que haga una cosa u otra segun el tipo de trans.


     END IF  

END IF  

!-------------transiciones rbb o cbb ------------
   40 CONTINUE  

CALL FFRKEY(KEY,NC,RET)  
IF (ON_ERROR(RET))  GOTO 250

   50 CONTINUE  
DO I = 1,NCOMP  
     LOW(I) = .FALSE.  
     IF (KEY.EQ.LEVEL(I) .AND. LINENO(I).EQ.0) LOW(I) = .TRUE.  
END DO  

DO IQI = 1,NL  
     IF (LABL(IQI).EQ.KEY) THEN  
          NN = IQI  
          GO TO 80  
     END IF  
END DO  

STOP 'ERROR IN TRANSIC1 - LABEL L NOT FOUND, BB'  
   80 CONTINUE  

LABL1 = NN  
CALL FFRKEY(KEY,NC,RET)  
IF (ON_ERROR(RET))  GOTO 250

DO IQI = 1,NL
     IF (LABL(IQI).EQ.KEY) THEN  
          NNU = IQI  
          GO TO 100  
     END IF  
END DO  
STOP 'ERROR IN TRANSIC1 - LABEL U NOT FOUND, BB'  
  100 CONTINUE  

LABU1 = NNU  

!       transicion radiativa
IF (QRT) THEN  
     IF (QCL) THEN  
          CALL FFRNUM(WLC,INTEG,RET)  
          IF (ON_ERROR(RET))  GOTO 250
     ELSE  
          WLC = HALLCL(LABL1,LABU1)  
     END IF  

     DO I = 1,NCOMP  

!  sumlo and sumup contain sum of (gi fij/gj/lambda^2) from all transitions
!  downwards from lower and upper level. Needed to calculate Sum(Aji) for
!  GAMMAL!

       IF(NSTARK(I).NE.2 .AND. LINENO(I).EQ.0) THEN     
          IF (LABL(NNU).EQ.LEVEL(I)) THEN
            CALL FFRNUM(REALO,INTEG,RET)  
            IF (ON_ERROR(RET))  GOTO 250
            NDATM = NDATOS - 1  
            SUMLO(I)=SUMLO(I)+GL(NN)*REALO/(GL(NNU)*WLC*WLC)
!            PRINT*,'LO ',I,' ',LABL(NN),LABL(NNU),GL(NN),GL(NNU),WLC,REALO
          ELSE IF(LABL(NNU).EQ.LEVEU(I)) THEN
            CALL FFRNUM(REALO,INTEG,RET)  
            IF (ON_ERROR(RET))  GOTO 250
            NDATM = NDATOS - 1  
            SUMUP(I)=SUMUP(I)+GL(NN)*REALO/(GL(NNU)*WLC*WLC)
!            PRINT*,'UP ',I,' ',LABL(NN),LABL(NNU),GL(NN),GL(NNU),WLC,REALO
          ENDIF   
       ENDIF

       IF (KEY.EQ.LEVEU(I) .AND. LOW(I)) THEN  
               XLAMB(I) = WLC  
!  osc. strength already read 
               FLU(I) = REALO
               NLEVL(I) = LABL1  
               NLEVU(I) = LABU1  
               NATOM(I) = LE(LABL1)  
               IF(NSTARK(I).EQ.3) GAMMAC(I)=CALCGAM(LABL1,LABU1,NATOM(I))
       END IF  
     END DO

END IF  

!       lectura de los datos
DO I = 1,NDATM  
     CALL FFRNUM(REALO,INTEG,RET)  
     IF (ON_ERROR(RET))  GOTO 250
END DO  

NDATM = NDATOS  

!       leer nueva keyword
CALL FFRKEY(KEY,NC,RET)  
IF (ON_ERROR(RET))  GOTO 250

IF (KEY.EQ.'0') THEN  
     QCL = .FALSE.  
     GO TO 10  
END IF  

GO TO 50  

!--------------- transiciones rbx o cbx ------------
  130 CONTINUE  
CALL FFRKEY(KEY,NC,RET)  
IF (ON_ERROR(RET))  GOTO 250

CALL FFRKEY(KEY,NC,RET)
IF (ON_ERROR(RET))  GOTO 250
  140 CONTINUE  

CALL FFRKEY(KEY,NC,RET)  
IF (ON_ERROR(RET))  GOTO 250

!       transicion radiativa
IF (QRT) THEN  
     IF (QCL) THEN  
          CALL FFRNUM(WLC,INTEG,RET)  
          IF (ON_ERROR(RET))  GOTO 250
     END IF  
END IF  

!       lectura de datos
DO I = 1,NDATOS  
     CALL FFRNUM(REALO,INTEG,RET)  
     IF (ON_ERROR(RET))  GOTO 250
END DO  

!       leer nueva keyword
CALL FFRKEY(KEY,NC,RET)  
IF (ON_ERROR(RET))  GOTO 250

IF (KEY.EQ.'0') THEN  
     QCL = .FALSE.  
     GO TO 10  
END IF  

GO TO 140  

!------------- transiciones  cbs o cbf ------------------
  160 CONTINUE  
CALL FFRKEY(KEY,NC,RET)  
IF (ON_ERROR(RET))  GOTO 250

CALL FFRKEY(KEY,NC,RET)  
IF (ON_ERROR(RET))  GOTO 250

  170 CONTINUE  

!       lectura de datos
DO I = 1,NDATOS  
     CALL FFRNUM(REALO,INTEG,RET)  
     IF (ON_ERROR(RET))  GOTO 250
END DO  

!       leer nueva keyword
CALL FFRKEY(KEY,NC,RET)  
IF (ON_ERROR(RET))  GOTO 250

IF (KEY.EQ.'0') GO TO 10  

GO TO 170  

!------------------ transicion  rbf -------------------
  190 CONTINUE  
CALL FFRKEY(KEY,NC,RET)  
IF (ON_ERROR(RET))  GOTO 250

CALL FFRKEY(KEY,NC,RET)  
IF (ON_ERROR(RET))  GOTO 250

  200 CONTINUE  
CALL FFRKEY(KEY,NC,RET)  
IF (ON_ERROR(RET))  GOTO 250

CALL FFRKEY(KEY,NC,RET)  
IF (ON_ERROR(RET))  GOTO 250

!       leer datos
DO I = 1,NDATOS  
     CALL FFRNUM(REALO,INTEG,RET)  
     IF (ON_ERROR(RET))  GOTO 250
END DO  

!       leer nueva keyword
CALL FFRKEY(KEY,NC,RET)  
IF (ON_ERROR(RET))  GOTO 250

IF (KEY.EQ.'0') GO TO 10  

GO TO 200  

!----------------- transiciones rff ------------------------
  220 CONTINUE  

CALL FFRKEY(KEY,NC,RET)  
IF (ON_ERROR(RET))  GOTO 250

  230 CONTINUE  

!       lectura de datos
DO I = 1,NDATOS  
     CALL FFRNUM(REALO,INTEG,RET)  
     IF (ON_ERROR(RET))  GOTO 250
END DO  

!       leer nueva keyword
CALL FFRKEY(KEY,NC,RET)
IF (ON_ERROR(RET))  GOTO 250

IF (KEY.EQ.'0') GO TO 10  

GO TO 230  

! error handling
250 SELECT CASE(RET)
CASE('RET1') !       end of file exit 
  WRITE (*,FMT='(A)') ' END OF FILE '  
  RETCHAR='RET1'
  RETURN
CASE('RET2') !       error exit 
  WRITE (*,FMT='(A)') 'ERROR IN FFR SUBROUTINES '  
  RETCHAR='RET2'
  RETURN
CASE DEFAULT
  STOP ' WRONG ERROR CONDITION IN TRANSIC1'
END SELECT

  270 CONTINUE  
BACKSPACE IU  

RETURN  

END
!
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!
SUBROUTINE TRANSIC2(LEVEL,LEVEU,NCOMP,NSTARK,LINENO,NLEVL,NLEVU, &
&                    NATOM,XLAMB,FLU,GAMMAL,GAMMAC,RETCHAR)
!
USE nlte_type
USE nlte_dim
USE fund_const, ONLY: CLIGHT
USE preformal_var, ONLY: LABL,NL,LE,GL
USE ffr_error
IMPLICIT NONE
!
!       finds line-transitions from file lines.dat
!
!     .. scalar arguments ..
INTEGER(I4B) ::  NCOMP  
!     ..
!     .. array arguments ..
REAL(DP) ::  FLU(NCOMP),GAMMAC(NCOMP),GAMMAL(NCOMP),XLAMB(NCOMP)
INTEGER(I4B) ::  LINENO(NCOMP),NATOM(NCOMP),NLEVL(NCOMP),NLEVU(NCOMP), &
&        NSTARK(NCOMP)
CHARACTER LEVEL(NCOMP)*6,LEVEU(NCOMP)*6,RETCHAR*4,RET*4  
!     ..
!     .. local scalars ..
REAL(DP) ::  REALO,SUMGLO  
INTEGER(I4B) ::  I,INTEG,IQI,LABL1,LABU1,NC,NDATOS,NLINE, &
&        NN,NNU
CHARACTER KEY*6  
!     ..
!     .. local arrays ..
LOGICAL LOW(NCOMP)  
!     ..
!     .. external subroutines ..
EXTERNAL FFRKEY,FFRNUM  
!     ..

   10 CONTINUE  
CALL FFRKEY(KEY,NC,RET)  
IF (ON_ERROR(RET))  GOTO 110
IF (KEY.NE.'CL') GO TO 120  

CALL FFRKEY(KEY,NC,RET)  
IF (ON_ERROR(RET))  GOTO 110
IF (KEY.NE.'TY') GO TO 120  

CALL FFRKEY(KEY,NC,RET)  
IF (ON_ERROR(RET))  GOTO 110
IF (KEY.NE.'RBB') GO TO 120  

CALL FFRNUM(REALO,INTEG,RET)  
IF (ON_ERROR(RET))  GOTO 110
IF (INTEG.NE.2) GO TO 120

CALL FFRNUM(REALO,INTEG,RET)  
IF (ON_ERROR(RET))  GOTO 110
NDATOS = INTEG  
CALL FFRKEY(KEY,NC,RET)  
IF (ON_ERROR(RET))  GOTO 110
IF (KEY.NE.'RBB') GO TO 120  

CALL FFRKEY(KEY,NC,RET)  
IF (ON_ERROR(RET))  GOTO 110

   20 CONTINUE  
DO I = 1,NCOMP  
     LOW(I) = .FALSE.  
     IF (KEY.EQ.LEVEL(I) .AND. LINENO(I).NE.0) LOW(I) = .TRUE.  
END DO  

DO IQI = 1,NL  
     IF (LABL(IQI).EQ.KEY) THEN  
          NN = IQI  
          GO TO 50  
     END IF  
END DO  
! stop statement removed: not ALL lines in LINES.dat must be present in level-list
!STOP 'ERROR IN TRANSIC2 - LABEL L NOT FOUND, BB'  
   50 CONTINUE  


LABL1 = NN  

CALL FFRKEY(KEY,NC,RET)  
IF (ON_ERROR(RET))  GOTO 110

DO IQI = 1,NL  
     IF (LABL(IQI).EQ.KEY) THEN  
          NNU = IQI  
          GO TO 70  
     END IF  
END DO  
! stop statement removed: not ALL lines in LINES.dat must be present in level-list
!STOP 'ERROR IN TRANSIC2 - LABEL U NOT FOUND, BB'  

   70 CONTINUE  
LABU1 = NNU  

CALL FFRNUM(REALO,INTEG,RET)  
IF (ON_ERROR(RET))  GOTO 110

NLINE = INTEG  

DO I = 1,NCOMP  
     IF (KEY.EQ.LEVEU(I) .AND. LOW(I) .AND. NLINE.EQ.LINENO(I)) THEN
          CALL FFRNUM(REALO,INTEG,RET)  
          IF (ON_ERROR(RET))  GOTO 110
          XLAMB(I) = REALO  

          CALL FFRNUM(REALO,INTEG,RET)  
          IF (ON_ERROR(RET))  GOTO 110
          FLU(I) = REALO  

          CALL FFRNUM(REALO,INTEG,RET)  
          IF (ON_ERROR(RET))  GOTO 110
          GAMMAL(I) = REALO  

          CALL FFRNUM(REALO,INTEG,RET)  
          IF (ON_ERROR(RET))  GOTO 110

          CALL FFRNUM(REALO,INTEG,RET)  
          IF (ON_ERROR(RET))  GOTO 110

          CALL FFRNUM(REALO,INTEG,RET)  
          IF (ON_ERROR(RET))  GOTO 110
          GAMMAC(I) = REALO  

          CALL FFRNUM(REALO,INTEG,RET)  
          IF (ON_ERROR(RET))  GOTO 110
          SUMGLO = REALO  
          IF (SUMGLO.NE.GL(LABL1)) STOP ' GLO NOT CONSISTENT!!!!'  
          NLEVL(I) = LABL1  
          NLEVU(I) = LABU1  
          NATOM(I) = LE(LABL1)  

          GO TO 100  

     END IF  

END DO  

!       lectura de los datos
DO I = 1,NDATOS  
     CALL FFRNUM(REALO,INTEG,RET)  
     IF (ON_ERROR(RET))  GOTO 110
END DO  

!       leer nueva keyword
  100 CONTINUE  
CALL FFRKEY(KEY,NC,RET)  
IF (ON_ERROR(RET))  GOTO 110

IF (KEY.EQ.'0') GO TO 10  

GO TO 20  
!
! error handling
!
110 SELECT CASE(RET)
CASE('RET1') !       end of file exit 
  WRITE (*,FMT='(A)') ' END OF FILE '  
  RETCHAR='RET1'
  RETURN
CASE('RET2') !       error exit 
  WRITE (*,FMT='(A)') 'ERROR IN FFR SUBROUTINES '  
  RETCHAR='RET2'
  RETURN
CASE DEFAULT
  STOP ' WRONG ERROR CONDITION IN TRANSIC2'
END SELECT

120  IF (KEY.EQ.'THEEND') THEN
    RETCHAR='RET0'
    RETURN
ELSE
    STOP ' WRONG END CONDITION IN TRANSIC2'
ENDIF

END
!-----------------------------------------------------------------------
!
FUNCTION CALCGAM(II1,II2,NATOM)  
!
USE nlte_type
USE nlte_dim
USE preformal_var, ONLY: FL,ZL,WEIGHT
USE fund_const, ONLY: CLIGHT,PI
IMPLICIT NONE
!
!  calculated GAMMAC in approximate weigh, using the relation between
!  neff and energy
!
!     ..
REAL(DP) :: CALCGAM
!
!     .. scalar arguments ..
INTEGER(I4B) ::  II1,II2,NATOM  
!     ..
REAL(DP) RYD,Z

Z=ZL(II1)
IF(Z.NE.ZL(II2)) STOP ' SOMETHING WRONG WITH NET CHARGE'
Z=Z+1.

RYD=109737.312*(1.+1./(1836.*WEIGHT(NATOM)))
CALCGAM=4.335D-7/(4.*PI)*Z*Z*(RYD*CLIGHT)**2 &
&       *(1./(FL(II2)*FL(II2))+1./(FL(II1)*FL(II1)))

RETURN  

END
!
!
!-----------------------------------------------------------------------
!
FUNCTION HALLCL(II1,II2)  
!
USE nlte_type
USE nlte_dim
USE preformal_var, ONLY: FL
IMPLICIT NONE
!
!        calcula la longitud de onda central de una transicion rbb,
!        a partir de los datos atomicos de los niveles involucrados
!
!     ..
REAL(DP) :: HALLCL
!
!     .. scalar arguments ..
INTEGER(I4B) ::  II1,II2  
!     ..
!     .. local scalars ..
REAL(DP) ::  CLIGHT,FC,FF1,FF2,XKW,XN  
!     ..
!     .. intrinsic functions ..
INTRINSIC ABS  

!     ..
!     .. data statements ..
DATA CLIGHT/2.997925E18/  
!     ..
FF1 = FL(II1)  
FF2 = FL(II2)  
FC = ABS(FF1-FF2)  
IF (FC.EQ.0.D0) FC = 1.D0  
HALLCL = CLIGHT/FC  
!---    primitive way to change hei 4026 line (to avoid
!---    false wavelength and order of components)
IF (ABS(HALLCL-4026.72472212120d0).LT.1.D-10) HALLCL = 4027.3d0  
!--- same for hei 4388
IF (ABS(HALLCL-4388.82518552703d0).LT.1.D-10) HALLCL = 4389.16d0  
!
!------CONVERSION TO WAVELENGTH IN AIR
!
XKW=1.D4/HALLCL      
XN=1.D0+1.D-7*(643.28D0+294981.D0/(146.D0-XKW**2)+2554.D0/(41.D0-XKW**2))
HALLCL=HALLCL/XN
RETURN  

END
!
!-----------------------------------------------------------------------
!
SUBROUTINE IDENT(KEY,TABLE,NMAX,J)  
!
USE nlte_type
USE nlte_dim
IMPLICIT NONE
!
!        compara el caracter key con los de la tabla 'table', y devuelve
!        en j el indice que corresponda a key en table, si esta, o 0 en
!        otro caso.
!     .. scalar arguments ..
INTEGER(I4B) ::  J,NMAX  
CHARACTER KEY*6  
!     ..
!     .. array arguments ..
CHARACTER TABLE(*)*6  
!     ..
!     .. local scalars ..
INTEGER(I4B) ::  I  
!     ..
I = 1  
   10 CONTINUE  

IF (KEY.EQ.TABLE(I)) THEN  
     J = I  
     RETURN  
END IF  

I = I + 1  
IF (I.LE.NMAX) THEN  
     GO TO 10  
END IF  

J = 0  

RETURN  

END
!
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!
SUBROUTINE PRESTARK(NSUM,STAFIL,NB,NLEVL,NLEVU,XLAMB,FLU,NSTARK,VTURB,YHE)  
!
USE nlte_type
USE nlte_dim
USE preformal_var, ONLY: FPATH,LABL,LE,WEIGHT,IU
USE ffr_error
IMPLICIT NONE
!
!---- preparation of stark solution. stark profiles are read and
!---- the desired transition(s) is(are) stored in file 'stark.bin'
!
!     .. parameters ..
INTEGER(I4B), PARAMETER :: MAXW=ID_MAXWW,MAXT=ID_MAXTT,MAXNE=ID_MAXNE, &
& MAXPS=ID_MAXPS,NDATAMAX=50
!     ..
!     .. scalar arguments ..
INTEGER(I4B) ::  NB,NSUM  
REAL(DP) :: VTURB,YHE
CHARACTER STAFIL*32  
!     ..
!     .. array arguments ..
REAL(DP) ::  XLAMB(NB),FLU(NB)  
INTEGER(I4B) ::  NLEVL(NB),NLEVU(NB),NSTARK(NB)  
!     ..
!     .. local scalars ..
REAL(DP) ::  AA,BB,EE,FAC,OSC,PESO,REALO,TT,XL0,WAVEAIR  
INTEGER(I4B) ::  I,IJK,INTEG,ISTARK,J,JE,JT,MFULL, &
&        NC,NDATA,NIN,NN,NN1,NN2,NNAT,NNN,NPS,NTSISO,NFSISO
LOGICAL QLOG
CHARACTER KEY*6,KEY2*6,RET*4
!     ..
!     .. local arrays ..
REAL(DP) ::  DATA(NDATAMAX),DDWS(35),SQ1(35),EEES(11), &
&            POLD(MAXPS),SQ(MAXW),TTTS(6),PROF(MAXW)
INTEGER(I4B) ::  NWSN 
LOGICAL Q  

REAL(DP), ALLOCATABLE, DIMENSION(:,:) :: DWS, ES, P, TS
INTEGER(I4B), ALLOCATABLE, DIMENSION(:) :: NES, NTS, NWS
LOGICAL, ALLOCATABLE, DIMENSION(:) :: DONE, QHALF

!     ..
!     .. external functions ..
INTEGER(I4B) ::  NPPAL  
EXTERNAL NPPAL  
!     ..
!     .. external subroutines ..
EXTERNAL FFRACC,FFRKEY,FFRLOC1,FFRNUM,GRIEM  
!     ..
!     .. intrinsic functions ..
INTRINSIC DBLE,LOG10  
!     ..
!     .. data statements ..

DATA TTTS,EEES/4.,4.301,4.602,4.903,5.204,5.505,10.5,11.5,12., &
&     12.5,13.,13.5,14.,14.5,15.,16.,17./
!     ..

IF(NSUM.NE.1.AND.NSUM.NE.2) STOP ' MORE THAN TWO STARK-COMPONENTS, CHECK!'

ALLOCATE(DWS(MAXW,NB),ES(MAXNE,NB),P(MAXPS,NB),TS(MAXT,NB))
ALLOCATE(NES(NB),NTS(NB),NWS(NB))
ALLOCATE(DONE(NB),QHALF(NB))

DONE=.TRUE.

OPEN (1,FILE=FPATH//STAFIL,STATUS='OLD',FORM='FORMATTED')  

! NEEDS TO BE CHANGED FOR MORE THAN TWO STARK LINES
DO I = 1,NB  
     IF(NSTARK(I).EQ.1) DONE(I) = .FALSE.  
END DO  

!BIG_LOOP

BIG_LOOP: DO NNN=1,NB

IF (NSTARK(NNN).NE.1) CYCLE

!
!     rewind!!!
!
IU = -1  
CALL FFRACC(IU,RET)  
SELECT CASE(RET)
CASE('RET1')
  GOTO 220
CASE('RET0')
  CONTINUE
CASE DEFAULT
  STOP ' WRONG RETURN IN PRESTARK'
END SELECT

!---- read stafil

   20      CONTINUE  
!
!        note! 'line' has to be situated within first five characters of
!        input-line
!
     CALL FFRLOC1('LINE',RET)  
     SELECT CASE(RET)
     CASE('RET1')
       GOTO 150
     CASE('RET2')
       GOTO 230
     CASE('RET0')
       CONTINUE
     CASE DEFAULT
       STOP ' WRONG RETURN IN PRESTARK'
     END SELECT
       
     CALL FFRKEY(KEY,NC,RET)  
     SELECT CASE(RET)
     CASE('RET1')
       GOTO 230
     CASE('RET2')
       GOTO 230
     CASE('RET0')
       CONTINUE
     CASE DEFAULT
       STOP ' WRONG RETURN IN PRESTARK'
     END SELECT
!   
!----------------------------------------------------------------------
!
!  frequency grid for ISO lines
!     
     IF(KEY.EQ.'DL') THEN

     CALL FFRNUM(REALO,NC,RET)  
     IF (ON_ERROR(RET))  GOTO 230
     IF(NC.GT.MAXW) STOP ' NWS(ISO) > MAXW'
     NWS(NNN)=NC
     DO I = 1,NC
          CALL FFRNUM(REALO,INTEG,RET)  
          IF (ON_ERROR(RET))  GOTO 230
          DWS(I,NNN) = REALO
     END DO
     GOTO 20

     ENDIF
!   
!----------------------------------------------------------------------
!
!
!  treat ISO lines
!     
     IF (KEY.EQ.'ISO') THEN

     CALL FFRKEY(KEY,NC,RET)  
     IF (ON_ERROR(RET))  GOTO 230
 
     CALL FFRKEY(KEY2,NC,RET)  
     IF (ON_ERROR(RET))  GOTO 230
     Q = (LABL(NLEVL(NNN)).EQ.KEY .AND. LABL(NLEVU(NNN)).EQ.KEY2)
     
     IF (.NOT. Q)  GOTO 20
     CALL FFRNUM(REALO,INTEG,RET)  
     WAVEAIR=REALO
     IF (ABS(WAVEAIR-XLAMB(NNN)).GT. 0.1) &
&     PRINT*,' CHECK WAVELENGTHS (ISO)! ',WAVEAIR,' ',XLAMB(NNN) 
!
!    take value provided
!     
     XLAMB(NNN)=WAVEAIR
     CALL FFRNUM(REALO,INTEG,RET)  
     OSC=REALO
     IF (ABS(LOG10(OSC/FLU(NNN))).GT. 0.1)&
&     PRINT*,' CHECK OSCILLATOR STRENGTH (ISO)! ',OSC,' ',FLU(NNN) 
!
!    take value provided
!     
     FLU(NNN)=OSC
!
!    read and write data in following format     
!----OSC,DLP,DENS,NTS,NFS,
!----TS(NTS),WS(NTS),DS(NTS),AS(NTS),
!----(DLF,FORB(NTS)) FOR EACH NFS
!     
     J=1
     DATA(J)=OSC
     J=J+1 
     CALL FFRNUM(REALO,INTEG,RET)  
     DATA(J)=REALO  !DLP
     J=J+1 
     CALL FFRNUM(REALO,INTEG,RET)  
     DATA(J)=REALO  !DENS
     J=J+1 
     CALL FFRNUM(REALO,INTEG,RET)  
     NTSISO=INTEG 
     DATA(J)=INTEG  !NTS
     J=J+1 
     CALL FFRNUM(REALO,INTEG,RET)  
     NFSISO=INTEG
     DATA(J)=INTEG  !NFS
!
!----NDATA=5+4*NTS+NFS*(1+NTS)
!   
     NDATA=5+4*NTSISO+NFSISO*(1+NTSISO)
     IF(NDATA.GT.NDATAMAX) STOP ' NDATA > NDATAMAX'

     DO I=1,4*NTSISO
        J=J+1 
        CALL FFRNUM(REALO,INTEG,RET)  
        DATA(J)=REALO  !TS,WS,DS,AS
     END DO

     DO I=1,NFSISO*(1+NTSISO)
        J=J+1 
        CALL FFRNUM(REALO,INTEG,RET)  
        DATA(J)=REALO  !(DLF,FORB) for each forbidden component
     END DO

     NWSN=NWS(NNN)
     IF(DWS(1,NNN)*DWS(NWSN,NNN).GE.0.D0) STOP ' ERROR IN ISO-FREQ (QHALF)'
     QHALF(NNN)=.FALSE.
     NNAT = LE(NLEVL(NNN))  
     PESO = WEIGHT(NNAT)  
     XL0 = XLAMB(NNN)  
     NTS(NNN) = 6  
     NES(NNN) = 11  
     NPS = NWS(NNN)*NTS(NNN)*NES(NNN)  
     IF (NPS.GT.MAXPS) STOP 'TOO MANY POINTS IN PROFILE'  
     DO IJK = 1,6  
       TS(IJK,NNN) = TTTS(IJK)  
     END DO

     DO IJK = 1,11  
       ES(IJK,NNN) = EEES(IJK)  
     END DO

     NIN=0
     JELOP: DO JE = 1,11  
       DO JT = 1,6  
         TT = 10.D0**TS(JT,NNN)  
         EE = 10.D0**ES(JE,NNN)  
         CALL ISO(DATA,NDATA,TT,EE,PESO,VTURB,XL0,DWS(1:NWSN,NNN),NWSN,SQ)
         DO IJK = 1,NWSN  
           NIN=NIN+1
           P(NIN,NNN) = LOG10(SQ(IJK))  
         END DO 
       END DO 
     END DO JELOP
     IF(NIN.NE.NPS) STOP ' ERROR IN NPS(ISO)'

     DONE(NNN) = .TRUE.  
     IF (NNN.NE.NB) THEN
        CYCLE
     ELSE  
        EXIT
     ENDIF

     ENDIF
!   
!----------------------------------------------------------------------
!
!
!  treat ISO1 lines
!     
     IF (KEY.EQ.'ISO1') THEN

     CALL FFRKEY(KEY,NC,RET)  
     IF (ON_ERROR(RET))  GOTO 230
 
     CALL FFRKEY(KEY2,NC,RET)  
     IF (ON_ERROR(RET))  GOTO 230
     Q = (LABL(NLEVL(NNN)).EQ.KEY .AND. LABL(NLEVU(NNN)).EQ.KEY2)
     
     IF (.NOT. Q)  GOTO 20
     CALL FFRNUM(REALO,INTEG,RET)  
     WAVEAIR=REALO
     IF (ABS(WAVEAIR-XLAMB(NNN)).GT. 0.1) &
&     PRINT*,' CHECK WAVELENGTHS (ISO1)! ',WAVEAIR,' ',XLAMB(NNN) 
!
!    take value provided
!     
     XLAMB(NNN)=WAVEAIR
     CALL FFRNUM(REALO,INTEG,RET)  
     OSC=REALO
     IF (ABS(LOG10(OSC/FLU(NNN))).GT. 0.1)&
&     PRINT*,' CHECK OSCILLATOR STRENGTH (ISO1)! ',OSC,' ',FLU(NNN) 
!
!    take value provided
!     
     FLU(NNN)=OSC
!
!    read and write data in following format     
!----OSC,DLP,DENS,NTS, &
!----TS(NTS),WS(NTS),DS(NTS),   electrons
!----WS1(NTS),DS1(NTS),WS2(NTS),DS2(NTS) protons, HeII
!
!    NOTE: tables contain FULL half-widths, in contrast to ISO tables
!          provided by Griem
!     
     J=1
     DATA(J)=OSC
     J=J+1 
     CALL FFRNUM(REALO,INTEG,RET)  
     DATA(J)=REALO  !DLP
     J=J+1 
     CALL FFRNUM(REALO,INTEG,RET)  
     DATA(J)=REALO  !DENS
     J=J+1 
     CALL FFRNUM(REALO,INTEG,RET)  
     NTSISO=INTEG 
     DATA(J)=INTEG  !NTS
     J=J+1
     CALL FFRNUM(REALO,INTEG,RET)  
     DATA(J)=REALO  !CRIT

!
!----NDATA=5+NTS+6*NTS
!   
     NDATA=5+7*NTSISO
     IF(NDATA.GT.NDATAMAX) STOP ' NDATA > NDATAMAX'

     DO I=1,7*NTSISO
        J=J+1 
        CALL FFRNUM(REALO,INTEG,RET)  
        DATA(J)=REALO  !TS,WS,DS,WS1,DS1,WS2,DS2
     END DO

     NWSN=NWS(NNN)
     IF(DWS(1,NNN)*DWS(NWSN,NNN).GE.0.D0) STOP ' ERROR IN ISO-FREQ (QHALF)'
     QHALF(NNN)=.FALSE.
     NNAT = LE(NLEVL(NNN))  
     PESO = WEIGHT(NNAT)  
     XL0 = XLAMB(NNN)  
     NTS(NNN) = 6  
     NES(NNN) = 11  
     NPS = NWS(NNN)*NTS(NNN)*NES(NNN)  
     IF (NPS.GT.MAXPS) STOP 'TOO MANY POINTS IN PROFILE'  
     DO IJK = 1,6  
       TS(IJK,NNN) = TTTS(IJK)  
     END DO

     DO IJK = 1,11  
       ES(IJK,NNN) = EEES(IJK)  
     END DO

     NIN=0
     JELOP1: DO JE = 1,11  
       DO JT = 1,6  
         TT = 10.D0**TS(JT,NNN)  
         EE = 10.D0**ES(JE,NNN)  
         CALL ISO1(DATA,NDATA,TT,EE,PESO,VTURB,XL0,DWS(1:NWSN,NNN),NWSN,SQ,YHE)
         DO IJK = 1,NWSN  
           NIN=NIN+1
           P(NIN,NNN) = LOG10(SQ(IJK))  
         END DO 
       END DO 
     END DO JELOP1
     IF(NIN.NE.NPS) STOP ' ERROR IN NPS(ISO)'

     DONE(NNN) = .TRUE.  
     IF (NNN.NE.NB) THEN
        CYCLE
     ELSE  
        EXIT
     ENDIF

     ENDIF
!   
!----------------------------------------------------------------------
!
!
!   "normal" lines
!     
     CALL FFRKEY(KEY,NC,RET)  
     IF (ON_ERROR(RET))  GOTO 230
 
     CALL FFRKEY(KEY2,NC,RET)  
     IF (ON_ERROR(RET))  GOTO 230
     Q = (LABL(NLEVL(NNN)).EQ.KEY .AND. LABL(NLEVU(NNN)).EQ.KEY2)

     IF (.NOT. Q)  GOTO 20
     
     CALL FFRNUM(REALO,INTEG,RET)  
     IF (ON_ERROR(RET))  GOTO 230
     MFULL = INTEG  
     QHALF(NNN) = MFULL .EQ. 0  
     CALL FFRNUM(REALO,INTEG,RET)  
     IF (ON_ERROR(RET))  GOTO 230
!---------- number of wavelengths
     IF (INTEG.GT.MAXW) STOP 'TOO MANY LAMBDAS'  
     NWS(NNN) = INTEG  
     DO I = 1,INTEG  
        CALL FFRNUM(REALO,INTEG,RET)  
        IF (ON_ERROR(RET))  GOTO 230
        DWS(I,NNN) = REALO  
     END DO 
     CALL FFRNUM(REALO,INTEG,RET)  
     IF (ON_ERROR(RET))  GOTO 230
!---------- number of temperatures
     IF (INTEG.GT.MAXT) STOP 'TOO MANY TEMPS'  
     NTS(NNN) = INTEG  
     DO I = 1,INTEG  
        CALL FFRNUM(REALO,INTEG,RET)
        IF (ON_ERROR(RET))  GOTO 230
        TS(I,NNN) = REALO  
     END DO
     CALL FFRNUM(REALO,INTEG,RET)  
     IF (ON_ERROR(RET))  GOTO 230
!---------- number of electron densities
     IF (INTEG.GT.MAXNE) STOP 'TOO MANY ELECTRON DENS.'  
     NES(NNN) = INTEG  
     DO I = 1,INTEG  
        CALL FFRNUM(REALO,INTEG,RET)  
        IF (ON_ERROR(RET))  GOTO 230
        ES(I,NNN) = REALO  
     END DO
     CALL FFRNUM(REALO,INTEG,RET)
     IF (ON_ERROR(RET))  GOTO 230
     FAC = REALO  
     CALL FFRNUM(REALO,INTEG,RET)  
     IF (ON_ERROR(RET))  GOTO 230
     QLOG = INTEG .EQ. 1  
!---------- number of profile points
     NPS = NWS(NNN)*NTS(NNN)*NES(NNN)  
     IF (NPS.GT.MAXPS) STOP 'TOO MANY POINTS IN PROFILE'  

     IF(VTURB.EQ.0.) THEN
        DO I = 1,NPS
           CALL FFRNUM(REALO,INTEG,RET)  
           IF (ON_ERROR(RET))  GOTO 230
           P(I,NNN) = REALO*FAC  
           IF (QLOG) P(I,NNN) = LOG10(P(I,NNN))  
        END DO
     ELSE 
        NWSN=NWS(NNN)
        NIN=0
        DO I = 1,NTS(NNN)*NES(NNN)
          DO J=1,NWSN
            CALL FFRNUM(REALO,INTEG,RET)  
            IF (ON_ERROR(RET))  GOTO 230
            PROF(J)=REALO*FAC
          END DO
          CALL CONVVTURB(PROF(1:NWSN),DWS(1:NWSN,NNN),NWSN,QLOG, &
&           QHALF(NNN),I,VTURB,XLAMB(NNN))
          IF(QLOG) THEN
            DO J=1,NWSN  
              PROF(J)=LOG10(PROF(J))
            END DO  
          ENDIF
          DO J=1,NWSN
            NIN=NIN+1
            P(NIN,NNN) = PROF(J)  
          END DO
        END DO
        IF(NIN.NE.NPS) STOP ' ERROR IN NPS(CONVVTURB)'
     ENDIF
!
     DONE(NNN) = .TRUE.  
     IF (NNN.NE.NB) THEN
        CYCLE
     ELSE  
        EXIT
     ENDIF

STOP ' ERROR IN PATH'

!   
!----------------------------------------------------------------------
!
!---- stark profile(s) not in tables. we calculate them in Griem-approx,
!     if hydrogen or HeII

  150 CONTINUE  
!
IF (DONE(NNN)) STOP ' WRONG PATH'  
!
     PRINT *,'STARK PROFILES NOT PRESENT; COMPONENT:',NNN  
     KEY = LABL(NLEVL(NNN))  
     IF (KEY(:3).EQ.'HE2' .OR. KEY(:2).EQ.'H1') THEN  
          QHALF(NNN) = .TRUE.  
          NNAT = LE(NLEVL(NNN))  
          PESO = WEIGHT(NNAT)  
          XL0 = XLAMB(NNN)  
          NN1 = NPPAL(KEY)  
          KEY = LABL(NLEVU(NNN))  
          NN2 = NPPAL(KEY)  
          AA = DBLE(NN1)  
          BB = DBLE(NN2)  
          NTS(NNN) = 6  
          NES(NNN) = 11  
          NWS(NNN) = 35  
          NIN = 0  
          DO IJK = 1,6  
               TS(IJK,NNN) = TTTS(IJK)  
          END DO

          DO IJK = 1,11  
               ES(IJK,NNN) = EEES(IJK)  
          END DO

          JELOOP: DO JE = 1,11  
               DO JT = 1,6  
                    TT = 10.D0**TS(JT,NNN)  
                    EE = 10.D0**ES(JE,NNN)  
                    CALL GRIEM(TT,EE,SQ1,PESO,XL0,AA,BB,DDWS,VTURB)  
                    DO IJK = 1,35  
                         SQ(IJK)=SQ1(IJK)
                         IF(SQ(IJK).LT.0.) STOP' NEG. PROFILE IN GRIEM'
                         NIN = NIN + 1  
                         DWS(IJK,NNN) = DDWS(IJK)  
                         P(NIN,NNN) = LOG10(SQ(IJK))  
                    END DO 
                END DO 
          END DO JELOOP
     ELSE  
          PRINT *,'STARK CALCULATION NOT IMPLEMENTED'  
          PRINT *,'LINE:',LABL(NLEVL(NNN)),'-',LABL(NLEVU(NNN))  
          STOP  
     END IF  

DONE(NNN) = .TRUE.  
IF (NNN.NE.NB) THEN
  CYCLE
ELSE  
  EXIT
ENDIF

END DO BIG_LOOP

CLOSE(1)

!---- output file is written

OPEN (1,FILE='STARK.BIN',STATUS='UNKNOWN',FORM='UNFORMATTED')  
REWIND 1  

DO I = 1,NB 
IF(NSTARK(I).NE.1) CYCLE
IF(.NOT.DONE(I))  STOP ' ERROR IN PRESTARK, BEFORE WRITING'
     NN = NWS(I)*NTS(I)*NES(I)  
     WRITE (1) NLEVL(I),NLEVU(I),QHALF(I),NWS(I), (DWS(J,I),J=1, &
      NWS(I)),NTS(I), (TS(J,I),J=1,NTS(I)),NES(I), (ES(J,I),J=1, &
      NES(I)),NN, (P(J,I),J=1,NN)
END DO  
CLOSE (1)  

DEALLOCATE(DWS,ES,P,TS)
DEALLOCATE(NES,NTS,NWS)
DEALLOCATE(DONE,QHALF)

RETURN  

!---- error in input unit
  220 CONTINUE  

STOP 'ERROR IN INPUT UNIT'  
!
! error handling
!
230 SELECT CASE(RET)
CASE('RET1') !       end of file exit 
  WRITE (*,FMT='(A)') ' END OF FILE '  
  STOP 
CASE('RET2') !       error exit 
  WRITE (*,FMT='(A)') 'ERROR IN FFR SUBROUTINES '  
  STOP
CASE DEFAULT
  STOP ' WRONG ERROR CONDITION IN PRESTARK'
END SELECT

END
!
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!
SUBROUTINE GRIEM(TT,EE,SQ,WEIGHT,WAVC,AA,BB,DWS,VTURB)  
!
USE nlte_type
USE nlte_dim
USE fund_const, ONLY: XMH=>AMH,AKB,PI
USE preformal_var, ONLY: NS,AS,ODOP,PS,ZZ,SS,SX

IMPLICIT NONE
!
!-----create a profile table using the griem theory as used by a and m
!-----in their ap j s24 paper
!     ..
!     .. scalar arguments ..
REAL(DP) ::  AA,BB,EE,TT,WAVC,WEIGHT,VTURB  
!     ..
!     .. array arguments ..
REAL(DP) ::  DWS(35),SQ(35)  
!     ..
!     .. local scalars ..
REAL(DP) ::  ASQ,BETA,BETAP,BETAW,BSQ,CLIGHT,CONKB,CUE,DIVKAB, &
&                 DOPKAB,DW,F0,FAC,FPW,GAM0,GAMA,GAML,GAMW,OBA, &
&                 RFT,SRE,SRT,SSS,SUMM,VMOT,WSQ,XKABF0
INTEGER(I4B) ::  I,JM,JP,NBET  
!     ..
!     .. local arrays ..
REAL(DP) ::  BET(35),DDWS(35)  
!     ..
!     .. external functions ..
REAL(DP) ::  DCONV,TBG,TH  
EXTERNAL DCONV,TBG,TH  
!     ..
!     .. intrinsic functions ..
INTRINSIC LOG,LOG10,MIN,SQRT  
!     ..
!     .. save statement ..
SAVE  
!     ..
!     .. data statements ..
DATA DDWS/0.D0,1.5849D-3,2.5119D-3,3.9811D-3,6.3096D-3,1.D-2, &
&     1.5849D-2,2.5119D-2,3.9811D-2,6.3096D-2,1.D-1,1.585D-1, &
&     1.995D-1,2.512D-1,3.162D-1,3.981D-1,5.012D-1,6.310D-1, &
&     7.943D-1,1.00D0,1.259D0,1.585D0,1.995D0,2.512D0,3.981D0, &
&     6.310D0,1.0D1,1.585D1,2.512D1,3.891D1,6.310D1,1.0D2,1.585D2, &
&     2.512D2,3.981D2/


DATA CLIGHT/2.997925D18/
DATA NBET/35/  
!     ..

SRT = SQRT(TT)  

!---- calculation of clight/v_thermal
VMOT = CLIGHT*1.D-8/SQRT(2.D0*AKB*TT/XMH/WEIGHT+VTURB**2)  

!-----doppler width (a)
RFT = WAVC/CLIGHT  
ODOP = VMOT/WAVC  

!-----upper and lower level parameters
ASQ = AA*AA  
BSQ = BB*BB  
WSQ = WAVC*WAVC  
OBA = 1.0d0/ (BSQ-ASQ)  

!-----normal field strength
SRE = SQRT(EE)  
CUE = EE**0.3333333333d0  
F0 = 1.25d-9*CUE*CUE  

!-----ratio stark/doppler width
DOPKAB = 5.5d-5* (ASQ*BSQ)**2*OBA*F0*ODOP/ZZ**5  

!-----1/stark width     (a)**-1
DIVKAB = ODOP/DOPKAB  

!---- calculation  of beta grid
XKABF0 = DOPKAB/ODOP  

DO I = 1,NBET  
  BET(I) = DDWS(I)/XKABF0
END DO  

BETAW = 6.951d-9*WSQ*ZZ*TT*DIVKAB/BSQ  
BETAP = 2.995d-15*WSQ*SRE*DIVKAB  
BETAP = MIN(BETAP,.99d0*BETAW)  
GAM0 = 3.78d-5* (BSQ-3.D0*AA)*BSQ*OBA*CUE*LOG10(4.d6*ZZ*TT/BSQ/SRE)/SRT/ZZ
GAML = LOG(BETAW/BETAP)  
GAMW = 4.712389d0/SQRT(BETAW)  
FPW = GAM0/GAML  

!-----initialize gama for zero,betap
GAMA = GAMW + GAM0  

!-----generate s(beta) on a fixed basis
JM = NBET  
JP = JM  

DO I = 1,NBET  
     BETA = BET(I)  
     IF (BETA.LT.BETAP) GO TO 20  
     IF (BETA.GT.BETAW) GO TO 40  

!-----   assign gama for betap,betaw
     GAMA = GAMW + FPW*LOG(BETAW/BETA)  

!-----   look for regions of the gama,beta plane where fast methods are
!-----   this is not just to save time as the t(beta,gama) analysis
!-----   is numerically unstable in these same regions
   20      CONTINUE  
     IF (GAMA.GT.25.) GO TO 50  
     IF (GAMA.LT..01D0*BETA) GO TO 30  
     IF (GAMA.LT..01) GO TO 30  

!-----   full case
     SSS = TBG(BETA,GAMA)  
     GO TO 60  

!-----   beta gt 100*gama for beta lt 1
   30      CONTINUE  
     SSS = TH(BETA)  
     IF (BETA.LT.1.0d0) GO TO 60  

!-----   gama lt  .01  or beta gt 100*gama for beta gt 1
     SSS = SSS + GAMA/ (PI*BETA*BETA)  
     GO TO 60  

!-----   beta gt betaw
   40      CONTINUE  
     SSS = 2.0D0*TH(BETA)  
     GO TO 60  

!-----   gama gt 25.
   50      CONTINUE  
     SSS = GAMA/ (PI* (GAMA*GAMA+BETA*BETA))  

!-----   fill the symmetric ss,sx vectors
   60      CONTINUE  
     SS(JM) = SSS  
     SX(JM) = -BETA*DOPKAB  
     SS(JP) = SSS  
     SX(JP) = BETA*DOPKAB  
     JM = JM - 1  
     JP = JP + 1  
END DO  


!-----make the asymtotic power law constants
NS = 2*NBET - 1  
PS = LOG(SS(NS)/SS(NS-1))/LOG(SX(NS)/SX(NS-1))  
CONKB = -2.0D0  
PS = MIN(PS,CONKB)  
AS = SS(NS)/SX(NS)**PS  

!-----normalize to unit frequency integral
SUMM = 0.0D0  
DO I = 2,NS  
     SUMM = SUMM + (SS(I)+SS(I-1))* (SX(I)-SX(I-1))  
END DO  

FAC = 2.0D0*VMOT*RFT/SUMM  

!-----convolve with doppler function
DO I = 1,35  
     DW = DDWS(I)  
     SQ(I) = FAC*DCONV(DW)  
     DWS(I) = DDWS(I)
END DO  

RETURN  


END
!
!-----------------------------------------------------------------------
!
FUNCTION TBG(BET,GAM)  
!
USE nlte_type
USE nlte_dim
USE fund_const, ONLY: PI
IMPLICIT NONE
!
!     .. scalar arguments ..
REAL(DP) ::  BET,GAM,TBG
!     ..
!     .. local scalars ..
REAL(DP) ::  T  
!     ..
!     .. external functions ..
REAL(DP) ::  TF,TG,TH  
EXTERNAL TF,TG,TH  
!     ..
!     .. save statement ..
SAVE  
!     ..

IF (GAM.GT.1.d-2) GO TO 10  

!     small gamma
TBG = TH(BET)  
IF (BET.GT.1.) TBG = TBG + GAM/PI/BET**2  
RETURN  

!     normal case
   10 CONTINUE  
IF (BET/GAM.GT.1.d2) GO TO 20  
T = GAM* (TF(BET,GAM)+TF(-BET,GAM)+TG(BET,GAM)+TG(-BET,GAM))/PI  
TBG = T  
RETURN  

   20 CONTINUE  
TBG = TH(BET) + GAM/PI/BET**2  

RETURN  

END
!
!-----------------------------------------------------------------------
!
FUNCTION TG(BET,GAM)  
!
USE nlte_type
USE nlte_dim
IMPLICIT NONE
!
!     .. scalar arguments ..
REAL(DP) ::  BET,GAM,TG
!     ..
!     .. local scalars ..
REAL(DP) ::  B,BET2,C,C4,C5,C6,CC,COSA2,D1,D2,D3,D4,D5,D6,D7, &
&                 D8,D9,G2,P,P2,PI2,Q,Q2,R,SINA,SINA2,SUM1,SUM2, &
&                 SUM3,SUM4,X1,X2,X3,X4,X5,Y1,Y2
!     ..
!     .. intrinsic functions ..
INTRINSIC ATAN,LOG,SQRT  
!     ..
!     .. save statement ..
SAVE  
!     ..
!     .. data statements ..
DATA C4,C5,C6/1.5,3.4636008d1,-1.3253986d2/  
DATA PI2/1.57079632679489D0/  
!     ..

G2 = GAM*GAM  
B = 2.0D0*BET  
C = BET*BET + G2  
BET2 = BET*BET  
CC = C*C  
R = SQRT(C)  
Q = SQRT(R)  
Q2 = Q*Q  
P = 1.D0/Q  
P2 = P*P  
SINA = GAM/R  
COSA2 = SQRT(0.5D0* (1.D0-BET/R))  
SINA2 = SQRT(0.5D0* (1.D0+BET/R))  
X5 = BET + 4.D0  
Y2 = LOG((X5*X5+G2)/16.D0)  
IF (X5/GAM.GT.1.0) GO TO 10  
Y1 = PI2 - ATAN(X5/GAM)  

GO TO 20  

   10 CONTINUE  
Y1 = ATAN(GAM/X5)  

   20 CONTINUE  
D2 = C/192.D0  
D3 = -B/32.D0  
D4 = 0.25D0* (3.D0*BET2-G2)/C  
D5 = B* (G2-BET2)/CC  
D6 = (BET2* (BET2-6.0D0*G2)+G2*G2)/CC  
SUM1 = ((D6*Y1)/GAM+ (D4+D3)) + D2  
SUM1 = (SUM1+D5*Y2)*C5/CC  
D1 = C/1024.D0  
D2 = -B/192.D0  
D3 = (3.0D0*BET2-G2)/ (32.D0*C)  
D4 = BET* (G2-BET2)/CC  
D5 = 0.5D0* (G2*G2+5.D0*BET2* (BET2-2.0D0*G2))/ (C*CC)  
D6 = -BET* (BET2*BET2+5.0D0*G2* (G2-2.0D0*BET2))/ (CC*C)  
SUM2 = (D5*Y2+D3) + D1  
SUM2 = (SUM2+ (D2+D4+ (D6*Y1)/GAM))*C6/CC  
D7 = C4/C  
D8 = D7* (B*B/C-1.D0)/ (2.D0*Q*Q2*SINA)  
D9 = D7* (B/ (C*C))/ (2.D0*P*P2*SINA)  
X1 = (4.0D0-Q2)/ (4.0D0*Q*SINA2)  
X2 = (Q* (Q+4.D0*COSA2)+4.D0)/ (Q* (Q-4.D0*COSA2)+4.D0)  
X3 = (0.25D0-P2)/ (P*SINA2)  
X4 = (P* (P+COSA2)+0.25D0)/ (P* (P-COSA2)+0.25D0)  
IF (X1.GT.1.) GO TO 30  
Y1 = PI2 - ATAN(X1)  

GO TO 40  

   30 CONTINUE  
Y1 = ATAN(1.D0/X1)  

   40 CONTINUE  
IF (X3.GT.-1.) GO TO 50  
Y2 = -ATAN(1.D0/X3)  

GO TO 60  

   50 CONTINUE  
Y2 = PI2 + ATAN(X3)  

   60 CONTINUE  
SUM3 = D8* (2.D0*COSA2*Y1-SINA2*LOG(X2))  
SUM4 = D9* (2.D0*COSA2*Y2+SINA2*LOG(X4))  
TG = (SUM4+D7* (1.D0/12.D0-B/C)) + SUM3  
TG = TG + SUM1 + SUM2  

RETURN  

END
!
!-----------------------------------------------------------------------
!
FUNCTION TF(B,G)  
!
USE nlte_type
USE nlte_dim
IMPLICIT NONE
!
!     .. scalar arguments ..
REAL(DP) ::  B,G,TF  
!     ..
!     .. local scalars ..
REAL(DP) ::  C0,C1,C2,C3,D,D1,D2,D3,G2,X1  
!     ..
!     .. intrinsic functions ..
INTRINSIC ATAN,LOG  
!     ..
!     .. save statement ..
SAVE  
!     ..
!     .. data statements ..
DATA C0,C1,C2,C3/1.0007744d-1,4.93208719d-3,-7.09873526d-3, &
&     7.11559325d-4/
!     ..
D3 = C3  
D2 = C2 - 3.0D0*B*C3  
D1 = (3.0D0*C3*B-2.0D0*C2)*B + C1  
D = ((-C3*B+C2)*B-C1)*B + C0  
G2 = G*G  
X1 = B + 4.D0  
TF = 4.0D0*D2 + 4.0D0*D3* (B+2.0D0) + &
&     0.5D0* (D1-G2*D3)*LOG((X1*X1+G2)/ (B*B+G2)) + &
&     (D-G2*D2)* (ATAN(X1/G)-ATAN(B/G))/G

RETURN  

END
!
!-----------------------------------------------------------------------
!
FUNCTION DCONV(DLAM)  
!
USE nlte_type
USE nlte_dim
USE preformal_var, ONLY: NS,ODOP,SS,SX
IMPLICIT NONE
!
!-----
!-----convolution of gaussian profile with s function  6 aug 77
!-----
!     .. scalar arguments ..
REAL(DP) ::  DLAM,DCONV  
!     ..
!     .. local scalars ..
REAL(DP) ::  CON,CR,DB,DX,GR,HALF,RANGE,SRTPI,TERX,TEX,X1,XKB, &
&                 XN,ZERO
INTEGER(I4B) ::  I,J,JM,N0  
!     ..
!     .. local arrays ..
REAL(DP) ::  ERX(69),EX(69),X(69)  
!     ..
!     .. external functions ..
REAL(DP) ::  ASINT,ERR  
EXTERNAL ASINT,ERR  
!     ..
!     .. intrinsic functions ..
INTRINSIC ABS,EXP  
!     ..
!     .. save statement ..
SAVE  
!     ..
!     .. data statements ..
DATA HALF/0.5d0/,ZERO/0.0d0/,SRTPI/5.6418958d-1/  
DATA RANGE/6.0d0/  
!     ..

DB = ABS(DLAM)*ODOP  
X1 = SX(1) + DB  
IF (X1.LT.RANGE) GO TO 10  

!-----asint -range,range
DCONV = ASINT(-RANGE,RANGE,DB)  
RETURN  

   10 CONTINUE  
DCONV = ZERO  

!-----asint -range,x1
IF (X1.GT.-RANGE) DCONV = DCONV + ASINT(-RANGE,X1,DB)  

!-----asint xn,range
XN = SX(NS) + DB  
IF (XN.LT.RANGE) DCONV = DCONV + ASINT(XN,RANGE,DB)  

!-----set up x in the gaussian frame
!-----store all exponential and error functions once
N0 = 2  
CON = HALF*SRTPI  

DO I = 1,NS  
     XKB = SX(I) + DB  
     X(I) = XKB  
     IF (XKB.LT.-RANGE) N0 = I + 1  
     EX(I) = EXP(-XKB*XKB)  
     ERX(I) = ERR(XKB,EX(I))  
END DO  

!-----by s segment
TERX = ZERO  
TEX = ZERO  

DO J = N0,NS  
     JM = J - 1  
     DX = X(J) - X(JM)  
     IF (DX.LT.0.1) GO TO 30  
     GR = (SS(J)-SS(JM))/DX  
     CR = SS(J) - GR*X(J)  
     TERX = TERX + CR* (ERX(J)-ERX(JM))  
     TEX = TEX + GR* (EX(J)-EX(JM))  

     GO TO 40  
   30      CONTINUE  
     DCONV = DCONV + CON* (SS(J)*EX(J)+SS(JM)*EX(JM))*DX  
   40      CONTINUE  
     IF (X(J).GT.RANGE) EXIT  
END DO  

DCONV = DCONV + HALF* (TERX-SRTPI*TEX)  

RETURN  

END
!
!-----------------------------------------------------------------------
!
FUNCTION ASINT(X0,X1,DB)  
!
USE nlte_type
USE nlte_dim
USE preformal_var, ONLY: AS,PS
IMPLICIT NONE
!
!-----
!-----integral over asymtotic region by simpsons rule
!-----
!     .. scalar arguments ..
REAL(DP) ::  DB,X0,X1,ASINT
!     ..
!     .. local scalars ..
REAL(DP) ::  C23,DX,H,HALF,ONE,SRTPI,STEP,TWO,X  
INTEGER(I4B) ::  I,N  
!     ..
!     .. intrinsic functions ..
INTRINSIC ABS,DBLE,EXP,INT  
!     ..
!     .. statement functions ..
REAL(DP) ::  F  
!     ..
!     .. save statement ..
SAVE  
!     ..
!     .. data statements ..
DATA SRTPI/5.6418958d-1/,C23/6.666666667d-1/  
DATA HALF,ONE,TWO/0.5d0,1.0d0,2.0d0/  
DATA STEP/0.2d0/  
!     ..
!     .. statement function definitions ..
F(X) = EXP(-X*X)*ABS(X-DB)**PS  
!     ..

!-----chose h to be sept
N = INT((X1-X0)/STEP+ONE)  
DX = (X1-X0)/DBLE(N)  
H = HALF*DX  
X = X0 - H  
ASINT = F(X0)  

DO I = 1,N  
     X = X + DX  
     ASINT = ASINT + TWO*F(X) + F(X+H)  
END DO  

ASINT = ASINT*AS*SRTPI*H*C23  

RETURN  

END
!
!-----------------------------------------------------------------------
!
FUNCTION ERR(X,EX)  
!
USE nlte_type
USE nlte_dim
IMPLICIT NONE
!
!-----error function
!     .. scalar arguments ..
REAL(DP) ::  EX,X,ERR
!     ..
!     .. local scalars ..
REAL(DP) ::  ONE,T  
!     ..
!     .. intrinsic functions ..
INTRINSIC ABS,SIGN  
!     ..
!     .. save statement ..
SAVE  
!     ..

ONE = 1.0D0  
T = ABS(X)  
IF (T.GT.6.) GO TO 10  
T = 1.D0/ (1.D0+0.3275911D0*T)  
ERR = ((((1.061405429D0*T-1.453152027D0)*T+1.421413741D0)*T- &
&      0.284496736D0)*T+0.254829592D0)*T
ERR = SIGN(ONE-EX*ERR,X)  

RETURN  

   10 CONTINUE  
ERR = SIGN(ONE,X)  

RETURN  

END
!
!-----------------------------------------------------------------------
!
FUNCTION TH(X)  
!
USE nlte_type
USE nlte_dim
IMPLICIT NONE
!
!-----griem microfield fudge - normalized to unit b integral
!     .. scalar arguments ..
REAL(DP) ::  X,TH
!     ..
!     .. local scalars ..
REAL(DP) ::  B,B2,C0,C1,C2,C3,C4,C5,C6,SRT  
!     ..
!     .. intrinsic functions ..
INTRINSIC ABS,SQRT  
!     ..
!     .. save statement ..
SAVE  
!     ..
!     .. data statements ..
DATA C0,C1,C2,C3/1.0007744d-1,4.93208719d-3,-7.09873526d-3, &
&     7.11559325d-4/
DATA C4,C5,C6/1.5,3.4636008d1,-1.3253986d2/  
!     ..

B = ABS(X)  
IF (B.GT.4.) GO TO 10  
TH = ((C3*B+C2)*B+C1)*B + C0  

RETURN  

   10 CONTINUE  
SRT = SQRT(B)  
B2 = B*B  
TH = ((C6/B+C5)/B2+C4/SRT)/B2  

RETURN  

END
!
!-----------------------------------------------------------------------
!
FUNCTION NPPAL(LAB)  
!
USE nlte_type
USE nlte_dim
USE preformal_var, ONLY: ZZ
IMPLICIT NONE
!
!---- this function determines the principal quantum number, n,
!---- of a certain level of an hydrogenic ion, if the level's label
!---- is written in the "normal detail way"
!
!      ..
INTEGER(I4B) :: NPPAL
!
!     .. scalar arguments ..
CHARACTER LAB*6  
!     ..
!     .. local scalars ..
INTEGER(I4B) ::  IJ,J  
CHARACTER CAR*1,AT*2,CARA*2  
!     ..
!     .. save statement ..
SAVE  
!     ..

J = 1  

   10 CONTINUE  
CAR = LAB(J:J)  
IF (CAR.EQ.'0' .OR. CAR.EQ.'1' .OR. CAR.EQ.'2' .OR. &
&    CAR.EQ.'3' .OR. CAR.EQ.'4' .OR. CAR.EQ.'5' .OR. &
&    CAR.EQ.'6' .OR. CAR.EQ.'7' .OR. CAR.EQ.'8' .OR. &
&    CAR.EQ.'9') THEN
!----------- atom determination for atomic charge
     AT = LAB(1:J-1)  

     IF (AT.EQ.'H') THEN  
          ZZ = 1.D0  
     ELSE IF (AT.EQ.'HE') THEN  
          ZZ = 2.D0  
     ELSE  
          PRINT *,'ATOM ',AT,' NOT IMPLEMENTED - NPPAL'  
          STOP  
     END IF  

     J = J + 1  
     IJ = J  

   20      CONTINUE  
     CAR = LAB(J:J)  
     IF (CAR.EQ.'0' .OR. CAR.EQ.'1' .OR. CAR.EQ.'2' .OR. &
&        CAR.EQ.'3' .OR. CAR.EQ.'4' .OR. CAR.EQ.'5' .OR. &
&        CAR.EQ.'6' .OR. CAR.EQ.'7' .OR. CAR.EQ.'8' .OR. &
&        CAR.EQ.'9') THEN
          J = J + 1  
          GO TO 20  
     ELSE  
          J = J - 1  
          CARA = LAB(IJ:J)  
          READ (CARA,FMT='(I2)') NPPAL  
          IF (NPPAL.EQ.0) NPPAL = 10  
     END IF  

ELSE  
     J = J + 1  
     GO TO 10  
END IF  

RETURN  
END
!
!-----------------------------------------------------------------------
!
SUBROUTINE CONVVTURB(PROF,DDWS,NS,QLOG,QHALF,ISTART,VTURB,LAMBDA0)
!
!convolution with vturb, assuming profile is linear between grid points.
!for istart=1, convolution matrix will be calculated, for istart > 1
!"only" applied  
!
!
USE nlte_type
USE nlte_dim
USE fund_const, ONLY: CLIGHT
IMPLICIT NONE

!Input data
INTEGER(I4B) :: NS,ISTART
REAL(DP) :: PROF(NS),DDWS(NS),VTURB,LAMBDA0
LOGICAL :: QLOG, QHALF

INTEGER(I4B), PARAMETER :: NSMAX=ID_MAXWW,NSMAX2=2*NSMAX-1
REAL(DP), PARAMETER     :: SRTPI1=0.5641895835D0
!
INTEGER(I4B) :: I,J,JMIN,JMAX,J1,J2,NS2
REAL(DP) :: CONST,ERR,EX,X0,W
REAL(DP) :: Q,Q1,DX,DX1,DX2,DELTAEXP,DELTAERF,X1,X2,EXP1,EXP2,ERFC1,ERFC2
REAL(DP) :: DW(NSMAX2),X(0:NSMAX2+1),P(0:NSMAX2+1), &
&           A(NSMAX,0:NSMAX2+1),PNEW(NSMAX) 

SAVE A

!check whether input from blue to red as it is assumed
IF(DDWS(1).GT.DDWS(NS)) STOP ' INPUT FROM RED TO BLUE IN CONVVTURB'

!change order from red to blue and complete variables if QHALF

NS2=NS

IF (QHALF) THEN
   NS2=2*NS-1
   DO I=1,NS
     J=I+NS-1
     DW(I)=DDWS(NS+1-I)
     P(I)=PROF(NS+1-I)
     DW(J)=-DDWS(I)
     P(J)=PROF(I)
   END DO
ELSE
   DO I=1,NS2
     DW(I)=DDWS(NS2+1-I)
     P(I)=PROF(NS2+1-I)
   END DO
ENDIF

IF (.NOT.QLOG) THEN
  DO I=1,NS2
    P(I)=10.**P(I)
  END DO
ENDIF

!calculate matrix once

IF(ISTART.EQ.1) THEN

A=0.
!conversion to Doppler units (with respect to vturb)

CONST=CLIGHT/(VTURB*LAMBDA0)
DO I=1,NS2
  X(I)=-DW(I)*CONST
END DO
X(0)=X(1)-5.01D0
X(NS2+1)=X(NS2)+5.1D0

!append first and last element assuming constant profiles
P(0)=P(1)
P(NS2+1)=P(NS2)

JMIN=0
JMAX=0

!set up matrix, calculate full (NS2=NS) or only half (NS2 NE NS) range
BIG: DO I=1,NS
X0=X(I)

!find range
DO J=JMIN,NS2
  IF(X(J)-X0.GT.-5.D0) EXIT
ENDDO  
JMIN=J-1

DO J=JMAX,NS2+1
  IF(X(J)-X0.GE.5.D0) EXIT
ENDDO  
JMAX=J

!calculate weights
!first element
J1=JMIN
J2=J1+1
X1=X(J1)
X2=X(J2)
DX=X2-X1
DX1=X1-X0
DX2=X2-X0
Q=-DX1/DX
Q1=1.D0-Q
EXP1=EXP(-DX1*DX1)
EXP2=EXP(-DX2*DX2)
DELTAEXP=EXP1-EXP2
ERFC1=1.D0-ERR(DX1,EXP1)
ERFC2=1.D0-ERR(DX2,EXP2)
DELTAERF=ERFC1-ERFC2
W=Q1*DELTAERF-SRTPI1*DELTAEXP/DX
A(I,J1)=.5D0*W

!middle elements
DO J=JMIN+1,JMAX-1

!old values
W=Q*DELTAERF+SRTPI1*DELTAEXP/DX

!new values
J1=J
J2=J1+1
X1=X(J1)
X2=X(J2)
DX=X2-X1
DX1=X1-X0
DX2=X2-X0
Q=-DX1/DX
Q1=1.D0-Q
EXP1=EXP(-DX1*DX1)
EXP2=EXP(-DX2*DX2)
DELTAEXP=EXP1-EXP2
ERFC1=1.D0-ERR(DX1,EXP1)
ERFC2=1.D0-ERR(DX2,EXP2)
DELTAERF=ERFC1-ERFC2
W=W+Q1*DELTAERF-SRTPI1*DELTAEXP/DX
A(I,J1)=.5D0*W
END DO

!last element
W=Q*DELTAERF+SRTPI1*DELTAEXP/DX
A(I,JMAX)=.5D0*W

END DO BIG

ENDIF

!convolution
PNEW(1:NS)=MATMUL(A(1:NS,0:NS2+1),P(0:NS2+1))

!test
W=0.D0
Q=0.D0
DO I=1,NS-1
DX=.5*(DW(I+1)-DW(I))
W=W+(P(I)+P(I+1))*DX
Q=Q+(PNEW(I)+PNEW(I+1))*DX
END DO

!error in equivalent width
DX=1.-Q/W
IF(ABS(DX).GT.0.1D0) PRINT*,' WARNING!!! ERROR IN CONVOLUTION LARGE: ',DX

!renormalization
DO I=1,NS
  PNEW(I)=PNEW(I)*W/Q
END DO

!reversal of order: either pnew is ordered from -xmax to 0 (symmetric case)
! or from -xmax to +xmax (asymmetric case)

DO I=1,NS
  PROF(I)=PNEW(NS+1-I)
END DO

IF(.NOT.QLOG) THEN
  DO I=1,NS
    PROF(I)=LOG10(PROF(I))
  END DO
ENDIF

RETURN
END
!
!-----------------------------------------------------------------------
!
SUBROUTINE ISO(DATA,NDATA,TT,EE,WEIGHT,VTURB,LAMBDA0,DWS,NWS,SQ)
!
USE nlte_type
USE nlte_dim
IMPLICIT NONE


!-----ISOLATED AND MULTI COMPONENT HE I LINE PROFILES
!-----INPUT DATA ARE - OSC,DLP,DENS,NTS,NFS,
!-----                 TS(NTS),WS(NTS),DS(NTS),AS(NTS),
!-----                 (DLF,FORB(NTS)) FOR EACH NFS
!      
!-----Updated/corrected by j. puls 13/04/99
!     depends majorly on treatment by Barnard, Cooper, Smith, 1974,
!     J.Q.S.R.T. 14, 1025
!
!      
!-----NDATA=5+4*NTS+NFS*(1+NTS)
!
INTEGER(I4B), PARAMETER :: MAXWW=ID_MAXWW
REAL(DP), PARAMETER :: CLIGHT=2.997925D18
!
!input values
!
INTEGER(I4B) :: NDATA,NWS
REAL(DP) :: DATA(NDATA),TT,EE,WEIGHT,VTURB,LAMBDA0,DWS(NWS),SQ(NWS)

!local values
INTEGER(I4B) :: I,IB,IF,J,K,L,NFS,NTS
REAL(DP) :: A,ALF,CON,D,DB,DENS,DLF,DLP,FT,FQ(MAXWW),P,RBA,RBHZ,RFT,RWT, &
&           RHOM,SIGMA,SRT,VA,VB,VTH,VMOT,VP,W,WF,WQ,WT,X,Y

REAL(DP) :: TS(4),WS(4),DS(4),AS(4),DLS(3),FB(6,3),FOS(3) 

!     .. external functions ..
REAL(DP) ::  VOIGT  
EXTERNAL VOIGT  

!-----frequential quantities

DO I=1,NWS
  WQ=LAMBDA0+DWS(I)
  FQ(I)=CLIGHT/WQ
END DO

FT=CLIGHT/LAMBDA0
RFT=1.D0/FT

SRT=SQRT(TT)

VTH=SQRT(1.6504D8*TT/WEIGHT+VTURB**2)
VMOT=CLIGHT*1.D-8/VTH

WT=CLIGHT*RFT
RWT=FT/CLIGHT

!     move DATA to local

I = 1
DLP=DATA(I+1)
DENS=DATA(I+2)
NTS=DATA(I+3)+0.5D0
NFS=DATA(I+4)+1.5D0
K=I+4

DO J=1,NTS
  K=K+1
  TS(J)=DATA(K)
ENDDO

DO J=1,NTS
  K=K+1
  WS(J)=DATA(K)
ENDDO

DO J=1,NTS
  K=K+1
  DS(J)=DATA(K)
ENDDO

DO J=1,NTS
  K=K+1
  AS(J)=DATA(K)
ENDDO

IF(NFS.GT.1) THEN
  DO L=2,NFS
    K=K+1
    DLS(L)=DATA(K)
    DO J=1,NTS
      K=K+1
      FB(J,L)=DATA(K)
    ENDDO
  ENDDO
ENDIF

!
!     RBHZ = 1/deltanu-dop
!     RBA  = nu0/vth  (for transformation dlambda -> dnue   
!
RBHZ=VMOT*RFT
RBA=VMOT*RWT

DLP=DLP*RBA
IF(NFS.GT.1) THEN
  DO L=2,NFS
    DLS(L)=DLS(L)*RBA
  ENDDO
ENDIF

!-----SET UP INTERPOLATION IN T
DO J=2,NTS
  IF=J
  IF(TS(J).GE.TT) EXIT
ENDDO

IB=IF-1
WF=(TT-TS(IB))/(TS(IF)-TS(IB))

IF(TT.LT.TS(IB)) WF = 0.0D0
IF(TT.GT.TS(IF)) WF = 1.0D0
   
!-----PERTURBER QUANTITIES
Y=EE/DENS
!
!       VP=8.78D0/SRT !factor cannot be understood, should be
!
!       factor = [(1./A_perturber)+(1./A_radiator)]^(-1/2), A atomic weight
!       A_perturber = hydrogen = 1, A_radiatior = helium = 4    
!
!
VP=1.D0/(SQRT(1.+1./WEIGHT)*SRT)  !inverse of mean vel., without factors
!
RHOM=0.62035D0/EE**0.333333D0     ! mean distance, from 4pi/3 Ne rhom^3=1

!-----IMPACT WIDTH WIDTH (A)
W=(WF*(WS(IF)-WS(IB))+WS(IB))*Y

!-----RATIO IMPACT SHIFT/WIDTH
D=WF*(DS(IF)-DS(IB))+DS(IB)

!-----ION BROADENING PARAMETERS
ALF=(WF*(AS(IF)-AS(IB))+AS(IB)) *Y**0.25D0
!
!      factor accounts for transformation dlambda -> dnu and appropriate units
!      factor = sqrt(pi*m_hyd/(8 k_botz)) * clight * 1.e8 
!        
SIGMA=2.06D14*W*RHOM*VP*RWT*RWT      ! sig=w*rhom/v (*c/lam0^2)
X=ALF**0.888889D0/SIGMA**0.333333D0  ! alf^(8/9) sig^(-1/3) propto w=wi/we

!-----TOTAL WIDTH IN DOPPLER UNITS
A=W*(1.0D0+1.36D0*X)*RBA                ! we*(1+wi/we) at line-center
DLS(1)=W*D*(1.0D0+2.36D0*X/ABS(D))*RBA  ! de*(1+di/de) at line-center, no Debye shielding
                                        ! and di/de=(di/we)/(de/we), BCS 6.6
FOS(1)=1.0D0

!-----SATELLITE COMPONENTS
X=FOS(1)
IF(NFS.GT.1) THEN
   DO L=2,NFS
     FOS(L)=WF*(FB(IF,L)-FB(IB,L))+FB(IB,L)
     X=X+FOS(L)
   ENDDO
ENDIF
!
!      con includes transformation to Doppler units via RBHZ,
!      factor 1/sqrt(pi) from normalization of Voigt profile and
!      factor 1/9 from using (8:1) weighted Voigt profiles to account
!      for fine-structure of lower component, level 2P3.         
!      Works also for other cases if separation is set to zero
!        
CON=6.268773D-2*RBHZ/X

!-----COMPUTE PROFILE
DO J=1,NWS                                                             
  DB=(FT-FQ(J))*RBHZ                                                     
  P=0.D0                                                                  
  DO L=1,NFS                                                         
    VA=DB-DLS(L)                                                        
    VB=VA-DLP                                                           
    P=P+FOS(L)*(8.0D0*VOIGT(A,VA)+VOIGT(A,VB))                            
  END DO                                                               
  SQ(J)=CON*P                                                            
END DO                                                                  

RETURN
END
!
!-----------------------------------------------------------------------
!
SUBROUTINE ISO1(DATA,NDATA,TT,EE,WEIGHT,VTURB,LAMBDA0,DWS,NWS,SQ,YHE)
!
USE nlte_type
USE nlte_dim
IMPLICIT NONE


!-----ISOLATE HE I LINE PROFILES according to data from
!     Dimitrijevic & Sahal-Brechot.
!     So far, proton/HeII contribution only approximate
!     from YHe input values and HeI (assumed to be constant
!     throughout the atmosphere)
!     If HeIII major component (corresp. to HeI = 2), same
!     values as for HeII used. 


!-----INPUT DATA ARE - OSC,DLP,DENS,NTS,
!-----                 TS(NTS),WS(NTS),DS(NTS),  for electrons
!-----                 WS1(NTS),DS1(NTS),WS2(NTS),DS2(NTS) for protons and HeII
!      
!-----programmed by j.p. 04/04 in analogy to subroutine iso from above
!
!      
!-----NDATA=4+7*NTS
!
INTEGER(I4B), PARAMETER :: MAXWW=ID_MAXWW
REAL(DP), PARAMETER :: CLIGHT=2.997925D18
!
!input values
!
INTEGER(I4B) :: NDATA, NWS
REAL(DP) :: DATA(NDATA),TT,EE,WEIGHT,VTURB,LAMBDA0,DWS(NWS),SQ(NWS),YHE

!local values
INTEGER(I4B) :: I,IB,IF,J,K,L,NTS
REAL(DP) :: A,CON,CRIT,D,D1,D2,DB,DENS,DLP,DLS,FT,FQ(MAXWW),P,RBA,RBHZ,RFT,RWT, &
& SRT,VA,VB,VTH,VMOT,W,W1,W2,WF,WQ,WT,Y, &
& NP,NHE,HEI

REAL(DP) :: TS(6),WS(6),DS(6),WS1(6),DS1(6),WS2(6),DS2(6) 

!     .. external functions ..
REAL(DP) ::  VOIGT  
EXTERNAL VOIGT  

!-----frequential quantities

DO I=1,NWS
  WQ=LAMBDA0+DWS(I)
  FQ(I)=CLIGHT/WQ
END DO

FT=CLIGHT/LAMBDA0
RFT=1.D0/FT

SRT=SQRT(TT)

VTH=SQRT(1.6504D8*TT/WEIGHT+VTURB**2)
VMOT=CLIGHT*1.D-8/VTH

WT=CLIGHT*RFT
RWT=FT/CLIGHT

!     move DATA to local

I = 1
DLP=DATA(I+1)
DENS=DATA(I+2)
NTS=DATA(I+3)+0.5D0
CRIT=DATA(I+4)
K=I+4

DO J=1,NTS
  K=K+1
  TS(J)=DATA(K)
ENDDO

DO J=1,NTS
  K=K+1
  WS(J)=DATA(K)
ENDDO

DO J=1,NTS
  K=K+1
  DS(J)=DATA(K)
ENDDO

DO J=1,NTS
  K=K+1
  WS1(J)=DATA(K)
ENDDO

DO J=1,NTS
  K=K+1
  DS1(J)=DATA(K)
ENDDO
DO J=1,NTS
  K=K+1
  WS2(J)=DATA(K)
ENDDO

DO J=1,NTS
  K=K+1
  DS2(J)=DATA(K)
ENDDO

!
!     RBHZ = 1/deltanu-dop
!     RBA  = nu0/vth  (for transformation dlambda -> dnue   
!
RBHZ=VMOT*RFT
RBA=VMOT*RWT

DLP=DLP*RBA

!-----SET UP INTERPOLATION IN T
DO J=2,NTS
  IF=J
  IF(TS(J).GE.TT) EXIT
ENDDO

IB=IF-1
WF=(TT-TS(IB))/(TS(IF)-TS(IB))

IF(TT.LT.TS(IB)) WF = 0.0D0
IF(TT.GT.TS(IF)) WF = 1.0D0
   
!-----PERTURBER QUANTITIES
Y=EE/DENS
!
!-----IMPACT WIDTH WIDTH (A)
W=(WF*(WS(IF)-WS(IB))+WS(IB))*Y
W=.5D0*W !tables give full halfwidths 

!-----SHIFT (A)
D=(WF*(DS(IF)-DS(IB))+DS(IB))*Y

! the same for protons and helium ions (note, that heiii approximated by heii!)
! test for crit

! rough estimate
IF(TT.GE.25000.) THEN
  HEI=2.
ELSE IF (TT.GE.10000.) THEN
  HEI=1.
ELSE
  HEI=0.
ENDIF  

NP=EE/(1.+HEI*YHE) !valid also for hei=0
Y=NP/DENS

W1=(WF*(WS1(IF)-WS1(IB))+WS1(IB))
IF(W1.EQ.0.) THEN
  D1=0.
ELSE
  IF(CRIT/W1.LE.NP) THEN
    W1=.5D0*W1*Y !tables give full halfwidths 
!-----SHIFT (A)
    D1=(WF*(DS1(IF)-DS1(IB))+DS1(IB))*Y
  ELSE ! NOT RELIABLE
    W1=0.
    D1=0.
  ENDIF
ENDIF


IF(HEI.EQ.0.) THEN
!no contribution from heii/heiii
  W2=0.
ELSE
  NHE=YHE*NP !(heii/heiii)  
  Y=NHE/DENS
  W2=(WF*(WS2(IF)-WS2(IB))+WS2(IB))
ENDIF

IF(W2.EQ.0.) THEN
  D2=0.
ELSE
  IF(W2.NE.0.AND.CRIT/W2.LE.NHE) THEN
    W2=.5D0*W2*Y !tables give full halfwidths 
!-----SHIFT (A)
    D2=(WF*(DS2(IF)-DS2(IB))+DS2(IB))*Y
  ELSE ! NOT RELIABLE
    W2=0.
    D2=0.
  ENDIF
ENDIF

!-----TOTAL WIDTH IN DOPPLER UNITS
A=(W+W1+W2)*RBA
DLS=(D+D1+D2)*RBA
!
!      con includes transformation to Doppler units via RBHZ,
!      factor 1/sqrt(pi) from normalization of Voigt profile and
!      factor 1/9 from using (8:1) weighted Voigt profiles to account
!      for fine-structure of lower component, level 2P3.         
!      Works also for other cases if separation is set to zero
!        
CON=6.268773D-2*RBHZ

!-----COMPUTE PROFILE

DO J=1,NWS                                                             
  DB=(FT-FQ(J))*RBHZ
  VA=DB-DLS                                                        
  VB=VA-DLP                                                           
  P=8.0D0*VOIGT(A,VA)+VOIGT(A,VB)
  SQ(J)=CON*P                                                            
END DO                                                                  

RETURN
END
!
!-----------------------------------------------------------------------
!
SUBROUTINE INDEXX(N,ARR,INDX)  

USE nlte_type
USE nlte_dim
IMPLICIT NONE
!
!     .. parameters ..
INTEGER(I4B), PARAMETER :: M=7,NSTACK=50  
!     ..
!     .. scalar arguments ..
INTEGER(I4B) ::  N  
!     ..
!     .. array arguments ..
REAL(DP) ::  ARR(N)  
INTEGER(I4B) ::  INDX(N)  
!     ..
!     .. local scalars ..
REAL(DP) ::  A  
INTEGER(I4B) ::  I,INDXT,IR,ITEMP,J,JSTACK,K,L  
!     ..
!     .. local arrays ..
INTEGER(I4B) ::  ISTACK(NSTACK)  
!     ..

DO J = 1,N  
     INDX(J) = J  
END DO  

JSTACK = 0  
L = 1  
IR = N  

   20 CONTINUE  

IF (IR-L.LT.M) THEN  

JLOOP: DO J = L + 1,IR  
          INDXT = INDX(J)  
          A = ARR(INDXT)  
          DO I = J - 1,1,-1  
               IF (ARR(INDX(I)).LE.A) GO TO 40  
               INDX(I+1) = INDX(I)  
          END DO
          I = 0  

   40           CONTINUE  

          INDX(I+1) = INDXT  
     END DO JLOOP

     IF (JSTACK.EQ.0) RETURN  
     IR = ISTACK(JSTACK)  
     L = ISTACK(JSTACK-1)  
     JSTACK = JSTACK - 2  

ELSE  

     K = (L+IR)/2  
     ITEMP = INDX(K)  
     INDX(K) = INDX(L+1)  
     INDX(L+1) = ITEMP  

     IF (ARR(INDX(L+1)).GT.ARR(INDX(IR))) THEN  
          ITEMP = INDX(L+1)  
          INDX(L+1) = INDX(IR)  
          INDX(IR) = ITEMP  
     END IF  

     IF (ARR(INDX(L)).GT.ARR(INDX(IR))) THEN  
          ITEMP = INDX(L)  
          INDX(L) = INDX(IR)  
          INDX(IR) = ITEMP  
     END IF  

     IF (ARR(INDX(L+1)).GT.ARR(INDX(L))) THEN  
          ITEMP = INDX(L+1)  
          INDX(L+1) = INDX(L)  
          INDX(L) = ITEMP  
     END IF  

     I = L + 1  
     J = IR  
     INDXT = INDX(L)  
     A = ARR(INDXT)  

   60      CONTINUE  

     I = I + 1  
     IF (ARR(INDX(I)).LT.A) GO TO 60  

   70      CONTINUE  

     J = J - 1  
     IF (ARR(INDX(J)).GT.A) GO TO 70  
     IF (J.LT.I) GO TO 80  
     ITEMP = INDX(I)  
     INDX(I) = INDX(J)  
     INDX(J) = ITEMP  
     GO TO 60  

   80      CONTINUE  

     INDX(L) = INDX(J)  
     INDX(J) = INDXT  
     JSTACK = JSTACK + 2  
     IF (JSTACK.GT.NSTACK) STOP 'NSTACK TOO SMALL IN INDEXX'  
     IF (IR-I+1.GE.J-L) THEN  
          ISTACK(JSTACK) = IR  
          ISTACK(JSTACK-1) = I  
          IR = J - 1  
     ELSE  
          ISTACK(JSTACK) = J - 1  
          ISTACK(JSTACK-1) = L  
          L = I  
     END IF  

END IF  

GO TO 20  

END
