;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
function icstarast_residuals, cd_column_vector
common data, xpos, ypos, ra, dec, crvalc, crpixc

cd = dblarr(2,2)
cd(0,0)=cd_column_vector(0)
cd(1,0)=cd_column_vector(1)
cd(0,1)=cd_column_vector(2)
cd(1,1)=cd_column_vector(3)

header = ['EQUINOX =                 2000 /EQUINOX OF REF. COORD.                          ',$
' END                                                                            ']
crval=crvalc
crpix=crpixc
putast,header,cd,crpix,crval

xpos2=xpos
ypos2=ypos
icxy2ad,xpos2,ypos2,header,ra_model,dec_model


sumofsquares=total((ra-ra_model)^2 +(dec-dec_model)^2)

return,sumofsquares
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro icstarast, x, y, ra, dec, cd, crval, crpix, header, equinox=equinox,$
               naxis1=naxis1,naxis2=naxis2
;+
; NAME:
;       ICSTARAST
;
; PURPOSE:
;       Finds a least squares astrometric solution for an 
;       array of x,y and ra,dec positions. Generalises the
;       astrolib routine starast to >3 objects.
;
; CALLING SEQUENCE:
;       icstarast, x, y, ra, dec, cd, crval, crpix
;
; INPUTS:
;       x, y: arrays of star positions in pixel coords
;       ra, dec: right ascension and declination of stars
;
; OPTIONAL INPUTS:
;       equinox: equinox for header
;       naxis1: dimension 1 of image for header
;       naxis2: dimension 2 of image for header
;
; KEYWORD PARAMETERS:
;       none
;
; OUTPUTS:
;       cd, crval, crpix: fits header parameters
;       
; OPTIONAL OUTPUTS:
;       header: fits header
;       
; EXAMPLE:
;       icstarast, x, y, ra, dec, cd, crval, crpix
;
; MODIFICATION HISTORY:
;       Written by:     Stephen Serjeant  11 Mar 1997
;          Any modifications get described here
;	version 2: naxis1 and naxis2 added, calling sequence retur, etc - SS
;-

common data, xc, yc, rac, decc, crvalc, crpixc

 IF N_PARAMS() LT 7 THEN BEGIN
   PRINT, 'CALLING SEQUENCE: ', $ 
    'ICSTARAST, x, y, ra, dec, cd, crval, crpix [,header] ,equinox=equinox,$'
    PRINT,'           naxis1=naxis1,naxis2=naxis2
   GOTO, CLOSING
 ENDIF

crvalc=1.0d*[1.0,1.0]
crpixc=1.0d*[1.0,1.0]
xc=x
yc=y
rac=ra
decc=dec


; Make up an initial guess
ra_test=ra(0:2) & dec_test=dec(0:2) & x_test=x(0:2) & y_test=y(0:2)
delta_ra=median(ra_test)
delta_dec=median(dec_test)
ra_test=ra_test-delta_ra
dec_test=dec_test-delta_dec
starast,ra_test,dec_test,x_test,y_test,cd
crvalc = [ra_test(0)+delta_ra,dec_test(0)+delta_dec]
crpixc = [x_test(0),y_test(0)] +1
;print,'Initial cd = ',cd
;print,'Initial crval = ',crvalc
;print,'Initial crpix = ',crpixc


crval=crvalc
crpix=crpixc

cd_column_vector=[cd(*)]
directions=fltarr(4,4)
directions(*) = 0.05*min(cd)
for i=0,3 do directions(i,i)=0.1*cd_column_vector(i)

tolerance = 1.0e-5

powell, cd_column_vector, directions, tolerance, $
              minimum_value, 'icstarast_residuals'

cd(*)=cd_column_vector(*)
;print,cd
;print,'Minimum value=',minimum_value

if(keyword_set(header)) then begin
;   print,'Making header'
   if(not keyword_set(equinox)) then begin
     equinox=2000
   endif
   header=[' ',' ']
   putast,header,cd,crpix,crval,equinox=equinox
   sxaddpar,header,'NAXIS',2
   date=bin_date(systime(0))
   sxaddhist,' NAXIS added '+systime(0),header

   if(keyword_set(naxis1)) then begin
      sxaddpar,header,'NAXIS1',naxis1
      date=bin_date(systime(0))
      sxaddhist,' NAXIS1 added '+systime(0),header
   endif
   if(keyword_set(naxis2)) then begin
      sxaddpar,header,'NAXIS2',naxis2
      date=bin_date(systime(0))
      sxaddhist,' NAXIS2 added '+systime(0),header
   endif

endif

CLOSING:
return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

