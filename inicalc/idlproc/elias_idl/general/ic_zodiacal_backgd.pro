;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
function cam_zodiacal_spectrum, lambda
 
; Written by Stephen Serjeant
; This routine is called by ic_zodiacal_backgd function
 
if(lambda gt 17.0 or lambda lt 2.5) then $
  print,'WARNING: wavelength outside CAM range: ',lambda
 
value = 1.21e-4*lambda^4 - 5.91e-4*lambda^3 + 7.71e-4*lambda^2 + $
    1.221e-3*lambda
 
return, value
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

function ic_zodiacal_spectrum, lambda, temperature

; Written by MRR in fortran and translated to IDL by Stephen Serjeant
; Assumes a blackbody spectrum
; This routine is called by ic_zodiacal_backgd function

c2=14388.3
x=c2/(lambda*temperature)
value=1./(lambda*lambda*lambda*(exp(x)-1.))
return,value

end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

function ic_zodiacal_backgd, ecliptic_latitude, solar_elongation, lambda,$
         camman=camman
;+
; NAME:
;       IC_ZODIACAL_BACKGD
;
; PURPOSE:
;
;       Calculates zodiacal background in mid-ir for given 
;       ecliptic latitude, solar elongation angle and wavelength. 
;       The routine linearly interpolates between entries in table 8 
;       of the ISOCAM Observer's Manual, but assumes a blackbody 
;       zodiacal spectrum rather than the alternative in the manual. 
;
;       Units are MJy/sr.  Divide by 42.5 to get mJy/sq arcsec
;
;
; CALLING SEQUENCE:
;       backgd = ic_zodiacal_backdg(ecliptic_latitude, solar_elongation,$
;                                   lambda)
;
; INPUTS:
;       ecliptic_latitude: as is says
;       solar_elongation : as is says
;       lambda: wavelength 
;
; OPTIONAL INPUTS:
;       none
;
; KEYWORD PARAMETERS:
;       camman: uses the cam manual zodiacal spectrum instead of
;               Hausman et al
;
; EXAMPLE:
;       print, ic_zodiacal_backgd(10.0, 120.0, 10.9)
;
; MODIFICATION HISTORY:
; 
; original version MRR
; changed 16/7/94 from program to subroutine, and zodi.dat included
;                 as data statment rather than read in from file
;                 by Seb Oliver
; Translated from FORTRAN subroutine to IDL function, and modified to
; accept solar elongation as an input parameter. Zodi.dat data also 
; incorporated within code. S Serjeant 9 Aug 1996
; uses absolute eclliptica latitude Seb 13 th Dec 1996
; camman keyword added S Serjeant 30 May 1997
;
;-

zod60 = [78.3,56.9,38.1,27.2,21.4,18.1,14.0,13.6,13.3,13.0]
zod120= [24.4,22.6,20.5,18.4,16.4,14.9,13.0,13.0,13.0,13.0]


;   zod60 and zod120 are tables of intensity at 10.9 mu as function 
;   of ecliptic latitude given
;   in ISOCAM cookbook, for solar elongations 60 and 120

if(keyword_set(camman)) then begin
   b1=cam_zodiacal_spectrum(10.9)
   b2=cam_zodiacal_spectrum(lambda)
endif else begin
   b1=ic_zodiacal_spectrum(10.9,275.)
   b2=ic_zodiacal_spectrum(lambda,275.)
endelse

i=fix(0.1*abs(ecliptic_latitude))
del=0.1*abs(ecliptic_latitude)-i

z60=zod60(i)+del*(zod60(i+1)-zod60(i))
z120=zod120(i)+del*(zod120(i+1)-zod120(i))

fac=b2/b1

;   Intensity calculated at wavelength lambda assuming a blackbody of 
;   temperature 275 K (Hauser et al 1984 ApJ 278, L15)
;
;
;   The contribution of Galactic emission needs to be found from 
;   Jones et al I(100) maps and then scaled to al using cirrus model 
;   of RR 1992, MN 258,787 (his Fig 3a).  The total flux then needs to 
;   be reduced by a factor of 1.7 at al > 40 mu to bring intensities 
;   into line with those seen by COBE.
;
;   The formula for calculating intensity at walength al given in 
;   ISOCAM cookbook is:
;        fac=1.21e-4*(lambda**4)-5.91e-4*(lambda**3)+ $
;                  7.71e-4*(lambda*lambda)+1.221e-3*lambda
;

z60=z60*fac
z120=z120*fac

return, z60 + (z120 - z60)*(solar_elongation - 60.0)/(120.0-60.0)

end

