PRO COMPUTE_DISTORTION, x, y, xpolynom, ypolynom, xd, yd,invert=invert
;+
; NAME: 
;    COMPUTE_DISTORTION
; PURPOSE:
;  it computes xd and yd, coordinates with distortion
;   the coefficients of the polynome:
;     xd  = polynomes(0,0) + polynomes(1,0)*x +
;             polynomes(2,0)*y + polynomes(3,0)* x*x +
;             polynomes(4,0)*x*y + polynomes(5,0)* y*y +
;             polynomes(6,0)*x*x*x + polynomes(7,0)*x*x*y +
;             polynomes(8,0)*x*y*y + polynomes(9,0)*y*y*y 
;
;     yd  = polynomes(0,1) + polynomes(1,1)*x +
;             polynomes(2,1)*y + polynomes(3,1)* x*x +
;             polynomes(4,1)*x*y + polynomes(5,1)* y*y +
;             polynomes(6,1)*x*x*x + polynomes(7,1)*x*x*y +
;             polynomes(8,1)*x*y*y + polynomes(9,1)*y*y*y 
;
;
; CATEGORY: 
;    II-6 internal
; CALLING SEQUENCE:
;    COMPUTE_DISTORTION, x, y, xpolynom, ypolynom, xd, yd
; INPUTS:
;   x -- float  : the x coordinate
;   y -- float  : the x coordinate
;   XPOLYNOM -- fltarr(10) : the coefficient of the x polynome
;   YPOLYNOM -- fltarr(10) : the coefficient of the x polynome
;
; OUTPUTS:
;   xd -- float  : the x coordinate
;   yd -- float  : the x coordinate
;
; SEE ALSO
;    ISO_astr.cc, xy2ad
; MODIFICATION HISTORY:
;     13-AUG-1997 R Gastaud  from H Aussel created 
;                  COMPUTE_DISTORSION.PRO
;     06-Oct-1997 D Landriu  renamed it 
;      6-Aug-1998 Seb Oliver,  computation speeded up by calculating powers at begining
;
; COPYRIGHT:
;
; This routine belongs to the ISOCAM Interactive Analysis Package CIA.
;
; CIA is a joint development by the ESA Astrophysics Division and the ISOCAM
; Consortium led by the ISOCAM PI, C. Cesarsky, Direction des Sciences de la
; Matiere, C.E.A., France.
; Contributing ISOCAM Consortium institutes are Service d'Astrophysique (SAp,
; Saclay, France) and Institut d'Astrophysique Spatiale (IAS, Orsay, France)
;
; When publishing ISOCAM Data reduced with this analysis package, please
; mention this in the acknowledgement the following way:
;   "The ISOCAM data presented in this paper was analysed using "CIA",
;    a joint development by the ESA Astrophysics Division and the ISOCAM
;    Consortium led by the ISOCAM PI, C. Cesarsky, Direction des Sciences de la
;    Matiere, C.E.A., France. "
;
; Its use and distribution to authorised sites are free, as long as this header
; text is not removed, or changed.
;
; No warranties for installation/ support/ maintenance are given.
;-

polynomes=0
 IF N_PARAMS() LT 6 THEN BEGIN
   PRINT, 'CALLING SEQUENCE: ', $
    ' COMPUTE_DISTORTION, x, y, xpolynom, ypolynom, xd, yd'
   return
 ENDIF

x=double(x)
y=double(y)

x2=x*x
y2=y*y
x3=x*x2
y3=y*y2
xy=x*y
xxy=x2*y
xyy=x*y2

if not keyword_set(invert) then begin

xd  = XPOLYNOM(0) + XPOLYNOM(1)*x + $
        XPOLYNOM(2)*y + XPOLYNOM(3)* x2 + $
        XPOLYNOM(4)*xy + XPOLYNOM(5)* y2 + $
        XPOLYNOM(6)*x3 + XPOLYNOM(7)*xxy + $
        XPOLYNOM(8)*xyy + XPOLYNOM(9)*y3 
;
yd  = YPOLYNOM(0) + YPOLYNOM(1)*x + $
        YPOLYNOM(2)*y + YPOLYNOM(3)* x2 + $
        YPOLYNOM(4)*xy + YPOLYNOM(5)* y2 + $
        YPOLYNOM(6)*x3 + YPOLYNOM(7)*xxy + $
        YPOLYNOM(8)*xyy + YPOLYNOM(9)*y3 

endif else begin

; first have to mock up 34X34 array and its distorted positions
  make_2d,dindgen(17)*2-1.d0,dindgen(17)*2-1.d0,xraster,yraster
  COMPUTE_DISTORTION,xraster,yraster,xpolynom, ypolynom,xsky,ysky
; then calculate the polynomial coefficients 
; that invert the transformation to go from xsky-> xraster
  polywarp, xraster, yraster, xsky, ysky,  3, kx, ky  


;  polywarp, xsky, ysky,xraster, yraster,  3, kx_test, ky_test
;  stop
;
; calculate extra polynomial terms necessary for the polywarp function
;
;x=double(xsky)
;y=double(ysky)
;
;x2=x*x
;y2=y*y
;x3=x*x2
;y3=y*y2
;xy=x*y
;xxy=x2*y
;xyy=x*y2


 xxxy=x3*y
 xxyy=x2*y2
 xyyy=x*y3
 xxxyy=x3*y2
 xxyyy=x2*y3
 xxxyyy=x3*y3


 xd= kx(0,0)    + kx(0,1)*x    + kx(0,2)*x2    + kx(0,3)*x3     + $
     kx(1,0)*y  + kx(1,1)*xy   + kx(1,2)*xxy   + kx(1,3)*xxxy   + $
     kx(2,0)*y2 + kx(2,1)*xyy  + kx(2,2)*xxyy  + kx(2,3)*xxxyy  + $
     kx(3,0)*y3 + kx(3,1)*xyyy + kx(3,2)*xxyyy + kx(3,3)*xxxyyy 

 yd= ky(0,0)    + ky(0,1)*x    + ky(0,2)*x2    + ky(0,3)*x3     + $
     ky(1,0)*y  + ky(1,1)*xy   + ky(1,2)*xxy   + ky(1,3)*xxxy   + $
     ky(2,0)*y2 + ky(2,1)*xyy  + ky(2,2)*xxyy  + ky(2,3)*xxxyy  + $
     ky(3,0)*y3 + ky(3,1)*xyyy + ky(3,2)*xxyyy + ky(3,3)*xxxyyy 

;imax=n_elements(x)-1
;
;
;print,kx(0,0),kx(0,1)*x(imax),kx(0,2)*x2(imax),kx(0,3)*x3(imax)
;print,kx(1,0)*y(imax),kx(1,1)*xy(imax),kx(1,2)*xxy(imax),kx(1,3)*xxxy(imax)
;print,kx(2,0)*y2(imax),kx(2,1)*xyy(imax),kx(2,2)*xxyy(imax),kx(2,3)*xxxyy(imax)
;print,kx(3,0)*y3(imax),kx(3,1)*xyyy(imax),kx(3,2)*xxyyy(imax),kx(3,3)*xxxyyy(imax)
;
;
;
;xt=x(imax)
;yt=y(imax)
;
;xpol=dblarr(4)
;xpol(0)=1
;for i=1,3 do xpol(i)=xpol(i-1)*xt

;ypol=dblarr(4)
;ypol(0)=1
;for i=1,3 do ypol(i)=ypol(i-1)*yt

;xyarray=ypol#xpol
;print,transpose(xyarray*kx)
;xdt=total(xyarray*kx)



;stop
;plot,x,xd-xraster,psym=1
;icplot,xd-xraster
;plot,y2,xd-xraster,psym=1

endelse


return
END

