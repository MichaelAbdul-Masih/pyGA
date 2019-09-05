PRO CONVOL,sx,sy,cx,cy,VDOP=vdop,AINST=ain,RESOL=resol,VSINI=vrot,BETA=beta, $
	   vmacro=vmacro, $
           MESSAGE=message, ORIGINAL=original
max_resol=60000
  
; ALL UNITS IN ANGSTROM OR km/s!
  
; original: remap finally on original wavelength grid
; rt-macroturbulence according to Gray, with vmac_t = vmac_r


; keywords vdop, ain, resol, vmacro mutually exclusive.
; rot. folding if present done at first, of course  
 
 
; ain = FWHM
; resolution = lambda/delta lambda
  
;modified version of older routines by Reetz, Fuhrmann and Haser  
;small changes applied and checked for performance. 
  
IF NOT KEYWORD_SET(beta) THEN beta = 1.5 
IF NOT KEYWORD_SET(message) THEN message = 0
IF NOT KEYWORD_SET(original) THEN original = 0 

clight    = 299792.5
rsmpl_fac = 0.7

sxx       = sx
syy       = sy
n         = N_ELEMENTS(sxx) 
nn        = n
rdst      = (sxx(n-1)-sxx(0))/(n-1)             ; resampling distance
intp      = 0

;
; handle not equidistant wavelength vectors
;

dsx        = ABS(sxx(1:*)-sxx(0:N_ELEMENTS(sxx)-2))
rdst_min   = MIN(dsx,MAX=rdst_max)
meanw      = 0.5*(sxx(0)+sxx(n-1))
eps        = meanw/max_resol
diff       = ABS(rdst_max-rdst_min) 
not_eqdist = diff GT eps

IF message THEN PRINT,'Max(dlam): ', diff
IF not_eqdist THEN BEGIN
   IF message THEN $
     PRINT,'Spectrum is not equidistant: RDMIN= ',rdst_min,', RDMAX= ',rdst_max
   rdst = MAX([(rdst_min * rsmpl_fac),eps]) 
   intp = 1
   nn = (sxx(n-1)-sxx(0))/rdst
 ENDIF          

xpow = ALOG(nn)/ALOG(2.)
pow = LONG(xpow)
IF ((pow LE 14 AND pow GT 2) OR intp) AND ((xpow-pow) NE 0.) THEN BEGIN
   pow = pow + 1
   nn_x = LONG(2.^pow)
   IF (FLOAT(nn)/FLOAT(nn_x) GT rsmpl_fac) AND (NOT not_eqdist) THEN BEGIN
      nn = nn_x * 2L
   ENDIF ELSE BEGIN
      nn = nn_x
   ENDELSE

   rdst = (sxx(n-1)-sxx(0))/(nn-1)
   sxx = LINDGEN(nn)*rdst + sx(0)
   syy = INTERPOL(sy,sx,sxx)
   res=finite(syy)
   bres=where(res eq 0, countb)
   if countb ne 0 then begin

   for i=0,nn-1 do begin
   b=where(sx eq sxx(i),count)
     if count gt 1 then begin
       sym=mean(sy(b))
       syy(i)=sym
     endif
   endfor
   endif

   IF message THEN BEGIN
   PRINT,'Resampled grid with resampling distance: ',rdst
   PRINT, nn, FORMAT= "('Resampled spectrum has ',I6,' points')"
ENDIF

ENDIF

px = (FINDGEN(nn)-LONG(nn/2))*rdst


; at first, rotational profile, to allow for final Gauss-folding

IF KEYWORD_SET(vrot) and vrot ge 0.1 THEN BEGIN 
   lambda0 = (sxx(nn-1)+sxx(0))/2.
   dlam_max = vrot*(lambda0/clight)
   xx = px / dlam_max
   one = WHERE(ABS(xx) GT 1)
   xx(one) = 1.
   xx_2 = xx*xx
   unsoeld =  (2.*     SQRT(1-xx_2)/!PI + (1-xx_2)*beta/2.) / (1 + 2*beta/3.)
;  gray = (2.*(1-beta)*SQRT(1-xx_2)/!PI + (1-xx_2)*beta/2.) / (1 -   beta/3.) 
   py = unsoeld
   py = py / dlam_max
   CONV_SPEC,sxx,syy,px,py,cx,cy       
   sxx = cx
   syy = cy
ENDIF


; *********************************************************
; ------- radial-tangential macroturbulence profile -------
; *********************************************************
; assuming vt=vr
; remember: then only vr necessary, gives same result as with separate calculations
; (otherwise, see routine by Sergio S., don't forget to modify corresponding statements)

IF KEYWORD_SET(vmacro) THEN BEGIN

    AAR=1.

;old    PX2 = PX(NN/2:NN-1)
;new formulation; use all freq. points to avoid asymmetries 

    spi=sqrt(!dpi)
    
    LAMBDA0  = (SXX(NN-1) + SXX(0))/2.

    MR=vmacro*lambda0/clight
    CCR=2.*AAR/spi/MR
    
    PXMR=abs(PX)/MR
; from resubstitution y=1/u and partial integration
;    PY2T=CCT*PXMT*[-spi+exp(-PXMT^2)/PXMT+spi*errorf(PXMT)]
;    PY2R=CCR*PXMR*[-spi+exp(-PXMR^2)/PXMR+spi*errorf(PXMR)]
; new
         
;old PY2 = PY2R + PY2T 
;old    PY = [REVERSE(PY2),PY2(0),PY2(0),PY2]
    PY=CCR*[exp(-PXMR^2)+spi*pxmr*(errorf(PXMR)-1.)]
    
    CONV_SPEC,SXX,SYY,PX,PY,CX,CY                  

    SXX = CX
    SYY = CY 
endif   
    
;    
; Gauss profile
;


IF KEYWORD_SET(vdop) or KEYWORD_SET(ain) or KEYWORD_SET(resol) THEN BEGIN

  IF(resol gt max_resol) then begin
  IF message THEN print,' HiRes anyway, not convolution necessary'
  goto, no_resol
  endif

  fac     = 2.*sqrt(alog(2.))
  lambda0 = (sxx(nn-1)+sxx(0))/2.
  
  if KEYWORD_SET(ain) then begin
    dlam_dop = ain/fac
    vdop     = dlam_dop*clight/lambda0

  endif else if KEYWORD_SET(resol) then begin
    vdop=clight/(resol*fac)
    dlam_dop = vdop*(lambda0/clight)
 
  endif else begin  
    dlam_dop = vdop*(lambda0/clight)
  endelse

  min_cnr = 5                  
  max_cnr = LONG(nn*0.8)

   IF message THEN BEGIN 
      PRINT,    vdop,    dlam_dop, FORMAT="('Vdop=',F7.3,'km/s  <=>',F7.3,'A')"
      PRINT,fac*vdop,fac*dlam_dop, FORMAT="('FWHM=',F7.3,'km/s  <=>',F7.3,'A')"
   ENDIF
   cnr = LONG(4.*dlam_dop/rdst) + 1       
   IF max_cnr LE cnr THEN BEGIN
      percent = cnr/FLOAT(nn)*100.
      PRINT,'Gauss profile is very large:' + STRING(percent,'(F6.2)') $
                               + '% of spectrum points are forming the profile'
    ENDIF
   IF min_cnr GE cnr THEN BEGIN
   PRINT,'Gauss profile is very small: only ' + STRING(cnr,'(I4)') $  
                                       + ' data points are forming the profile'
   ENDIF
   xx = px / dlam_dop
   six = WHERE(ABS(xx) lt 6.)
   py = fltarr(nn)
   py(six) = EXP(-(xx(six)^2))/(SQRT(!PI)*dlam_dop)
   CONV_SPEC,sxx,syy,px,py,cx,cy       
   sxx = cx
   syy = cy
ENDIF

no_resol:

if original then begin
   cy = INTERPOL(syy,sxx,sx)
   cx = sx
endif

return
end

;
;------------------------------------------------------------------------
;

PRO CONV_SPEC,sx,sy,px,py,cx,cy

; note:
; no zero padding applied, however sufficient precision also at end points
; note also, that response function centered, not wrapped arround, 
; since referred to central frequency of signal.    
  
n = N_ELEMENTS(sx)
IF n NE N_ELEMENTS(px) THEN PRINT,'ERROR: Different array dimensions'
shft = LONG(n/2)
tty = FLTARR(n) + (sy(0)+sy(n-1))/2. 
sy_t = sy - tty
cx = sx       
cy = SHIFT(FFT(FFT(sy_t,-1)*FFT(py,-1),1),shft)
cy = ((cx(n-1)-cx(0))/(n-1)) * n * cy         ; renorming result of convolution
cy = cy + tty
END
