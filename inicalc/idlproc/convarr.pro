; 
; --------------------------------------------------------------------------
; >>>
; NAME        : CONVARR
; FILENAME    : CONVARR.PRO
; DIRECTORY   : [FLORIAN.PROFIL]
; PURPOSE     : CONVOLUTION OF A LIST OF LINES (ARBITRARY RESAMPLING-DISTANCE 
;               WITH ARBITRARY KERNEL)
; MODUL       : IDL - Modul
; CALLING SEQ.: CONVARR,MODE,X,Y,CY,P1,NP
; INPUTS      : X : WAVELENGTH-VECTOR 
;               Y : FLUX-VECTOR
;               MODE: = 1 -> CONVOLUTION WITH ROT.-PROFIL
;                     = 2 -> CONVOLUTION WITH GAUSS-PROFIL
;               P1    = ROT.-VELOCITY in km/s,      IF MODE = 1
;               P1    = FWHM OF GAUSS in Angstroem, IF MODE = 2
;               NP    = NUMBER OF FREQUENCY POINTS OF EACH LINE
; OUTPUTS     : CY: FLUX-RESULT OF CONVOLUTION 
; AUTHOR      : Johannes Reetz      
; DATE        : 13-MAR-1991 19:31:05.23
; MODIFIED    : 17-AUG-1992 by Florian Sellmaier
; <<<
; --------------------------------------------------------------------------
; 


function rot,delta,v,lambda0
c=299792.5
normf=c/(v*lambda0)
xi=normf*delta
one=where(abs(xi) gt 1)
if (n_elements(one) ge 2) then xi(one)=1.
return,(sqrt(1-xi^2)/!pi+(1-xi^2)*3/8)*normf
end
;---------------------------------------------------------------------------

PRO CONV_SPEC,SX,SY,PX,PY,CONVX,CONVY 

N=N_ELEMENTS(SX)
IF N NE N_ELEMENTS(PX) THEN PRINT,'KERNEL HAS DIFFERENT RESAMPLING-DISTANCE'
                                   
SHFT=FIX(N/2)

; MM    = (SY(N-1)-SY(0))/(N-1)
; TTY   = MM*FINDGEN(N) + SY(0)

TTY   = FLTARR(N) + (SY(0) + SY(N-1))/2. 

SY_T  = SY - TTY

CONVX = SX
CONVY = SHIFT(FFT(FFT(SY_T,-1)*FFT(PY,-1),1),SHFT)

CONVY = (CONVX(N-1)-CONVX(0))*CONVY
CONVY = ABS(CONVY + TTY)

RETURN
END
;-----------------------------------------------------------------------------

PRO CONVARR,MODE,X,Y,CY,P1,NP

     XX = FLTARR(NP)       
     YY = XX
     CY = X
     NL = N_ELEMENTS(X)/NP

     FOR IL=0,NL-1 DO BEGIN


         FOR IP=0,NP-1 DO BEGIN
            XX(IP) = X(IL*NP+IP) 
            YY(IP) = Y(IL*NP+IP)
         ENDFOR
         SXX = DOUBLE(XX)
         SYY = DOUBLE(YY)

         N   = N_ELEMENTS(SXX) 
 
         NN = N
         RD = (SXX(N-1)-SXX(0))/(N-1)    ;RESAMPLING-DISTANCE
         RDD= RD         
         POW  = FIX(ALOG(N)/ALOG(2.) + 1.)  

         INTP = 0

         ;HANDLE NOT EQUIDISTANT WAVELENGTH-VECTORS
         NOT_EQDIST = 0
         DSX     = SXX(1:*) - SXX(0:NP-2)
         RD_MIN  = MIN(DSX)
         TMP     = WHERE(DSX NE RD_MIN,COUNT)
         IF COUNT GT 0 THEN NOT_EQDIST = 1

         IF NOT_EQDIST THEN BEGIN
             RDD = RD_MIN * 0.75 
             NN  = FIX((SXX(N-1) - SXX(0))/RDD) + 1
         ENDIF          
         POW  = FIX(ALOG(NN)/ALOG(2.) + 1.)  
                         
         IF POW LE 16 AND POW GT 2 OR NOT_EQDIST THEN BEGIN
          NN  = FIX(2^POW)
          RDD = (SXX(N-1) - SXX(0))/(NN-1)
          SXX = DINDGEN(NN)*RDD + XX(0)
          SYY = INTERPOL(YY,XX,SXX)
          INTP= 1
         ENDIF
         

         PRINT,'RESAMPL.DIST.:',RDD,' (RESAMPLED) SPECTRUM HAS ',NN,' POINTS'            

          ;GENERATE CONVOLUTION-PROFILE               
              PX      = DOUBLE((FINDGEN(NN) - FIX(NN/2))*RDD)  
              
         CASE MODE OF
           1:BEGIN ;ROTATION-PROFILE
              LAMBDA0 = (SXX(0)+SXX(N_ELEMENTS(SXX)-1))/2.
              PY = ROT(PX,P1,LAMBDA0)
             END
           2:BEGIN ;GAUSS
              SIGMA   = P1/2.
              SQRT_PI = DOUBLE(SQRT(!PI))
              PY  = EXP(-(PX/SIGMA)^2)/(SQRT_PI*SIGMA)
             END
           ELSE:
         ENDCASE

          ;EXECUTE CONVOLUTION
          CONV_SPEC,SXX,SYY,PX,PY,CXX,CYY                  


         IF INTP THEN BEGIN
            CYY = INTERPOL(CYY,CXX,XX)
;            CXX = XX
         ENDIF

         FOR IP=0,NP-1 DO BEGIN
            CY(IL*NP+IP) = CYY(IP)
         ENDFOR

      ENDFOR   

RETURN
END 
