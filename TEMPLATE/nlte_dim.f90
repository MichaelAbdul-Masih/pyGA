Module nlte_dim
 
Use nlte_type
IMPLICIT NONE
 
!  DIMENSIONS FROM FILE  A10HHeNCOPSi.dat                
 
 
!    NUMBER OF ATOMS
INTEGER(I4B), PARAMETER :: ID_ATOMS =  7
 
!    NUMBER OF FREQUENCIES (coarse mesh, modify in case)
INTEGER(I4B), PARAMETER :: ID_FREC1 =  2300
 
!    NUMBER OF FREQUENCIES (fine mesh, input)
INTEGER(I4B), PARAMETER :: ID_FREC2 =  4000
 
!    LEVMAX (see frescal)
INTEGER(I4B), PARAMETER :: ID_LEVMA =     4
 
!    LEVMIN1 (see frescal and fresfin)
INTEGER(I4B), PARAMETER :: ID_LEVM1 =     3
 
!    LEVMIN2(see frescal)
INTEGER(I4B), PARAMETER :: ID_LEVM2 =     2
 
!    NUMBER OF IONS
INTEGER(I4B), PARAMETER :: ID_IONES =  28
 
!    NUMBER OF L + K LEVELS
INTEGER(I4B), PARAMETER :: ID_LLEVS =   702
 
!    NUMBER OF X LEVELS 
INTEGER(I4B), PARAMETER :: ID_XLEVS =     1
 
!    NUMBER OF S LEVELS
INTEGER(I4B), PARAMETER :: ID_SLEVS =     9
 
!    NUMBER OF RBB + CBB TRANSITIONS
INTEGER(I4B), PARAMETER :: ID_RCBBT = 17847
 
!    NUMBER OF RBX + CBX TRANSITIONS
INTEGER(I4B), PARAMETER :: ID_RCBXT =     1
 
!    NUMBER OF CBS + CBF TRANSITIONS
INTEGER(I4B), PARAMETER :: ID_CBSFT =   739
 
!    NUMBER OF RBF TRANSITIONS
INTEGER(I4B), PARAMETER :: ID_RBFTR =   708
 
!    NUMBER OF RFF TRANSITIONS
INTEGER(I4B), PARAMETER :: ID_RFFTR =    18
 
!    NUMBER OF TOTAL TRANSITIONS
INTEGER(I4B), PARAMETER :: ID_NTRAN = 19312
 
!   NUMBER OF RBB + RBX TRANSITIONS
INTEGER(I4B), PARAMETER :: ID_NTTRD =  4823
 
!    NUMBER OF DATA
INTEGER(I4B), PARAMETER :: ID_NDATA = 56562
 
!    NUMBER OF FREQ. PER LINE (from input)
INTEGER(I4B), PARAMETER :: ID_NLONG =    19
 
!    MAX. NUMBER OF IONS PER ATOM EXCL. LAST ONE (set)
INTEGER(I4B), PARAMETER :: ID_KISAT =  4
 
!    NUMBER OF DEPTH POINTS (ND, ND1)
INTEGER(I4B), PARAMETER :: ID_NDEPT =  51
 
!    NUMBER OF P-RAYS
INTEGER(I4B), PARAMETER :: ID_NPOIN = 56
 
!    NUMBER OF CMF-FREQUENCIES
INTEGER(I4B), PARAMETER :: ID_NFCMF = 21
 
 
!   DIMENSIONS FOR FORMAL SOLUTION
 
!    NUMBER OF DEPTH POINTS (FINE MESH)
INTEGER(I4B), PARAMETER :: ID_DEPFI = 3000
 
!    NUMBER OF CORE RAYS
INTEGER(I4B), PARAMETER :: ID_CORES = 10
 
!    NUMBER OF NON-CORE RAYS MODULO 4 (+1)
INTEGER(I4B), PARAMETER :: ID_NOCOR = 13
 
!    NUMBER OF OBS. FRAME FREQUENCIES
 INTEGER(I4B), PARAMETER :: ID_NFOBS = 161
 
!    NUMBER OF CMF FREQUENCIES (EL. SCAT.)
 INTEGER(I4B), PARAMETER :: ID_NFESC = 500
 
 
!    DIMENSIONS FOR STARK EFFECT TREATMENT
 
!    MAX. NUMBER OF WAVELENGTHS
INTEGER(I4B), PARAMETER :: ID_MAXWW = 80
 
!    MAX. NUMBER OF TEMPERATURES
INTEGER(I4B), PARAMETER :: ID_MAXTT = 50
 
!    MAX. NUMBER OF ELECTRON DENSITIES
INTEGER(I4B), PARAMETER :: ID_MAXNE = 50
 
!    MAX. NUMBER IN PERFIL
INTEGER(I4B), PARAMETER :: ID_MAXPS = 10000
 
END MODULE nlte_dim
