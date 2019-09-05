pro sbgs_pfov, i, j, iprime, jprime
;+
; NAME:
;       SBGS_PFOV
;
; PURPOSE:
;       This program takes pixel position arrays as input
;       and outputs "true" positions once corrected for the field
;       distortion. The CAM detector array is trapezoidal
;       with the j=31 line longer than the j=0 line. 
;       (WARNING: I've still to check that I have that the right way round!
;       The columns
;       are all assumed to have the same length. Thus, the pixel 
;       (1,2) has a true position of about (1.2,2.0), and pixel(30,2) 
;       has a true position of about (29.8,2.0). 
;       The correction itself is from ISOCAM DATA CALIBRATION
;       VERSION 1.0, Starck et al, March 13 1996
;       PFOV = 6'' assumed. 
;
; CALLING SEQUENCE:
;       sbgs_pfov, i, j, iprime, jprime
;
; INPUTS:
;       i,j: arrays of pixel positions (intarr or fltarr)
;
; OPTIONAL INPUTS:
;       none
;       
; OUTPUTS:
;       iprime,jprime: fltarrs of same size as i,j containing
;                      corrected positions
;
; KEYWORD PARAMETERS:
;       none
;
; EXAMPLE:
;       sbgs_pfov, i, j, iprime, jprime
;
; MODIFICATION HISTORY:
;       Written by:     Stephen Serjeant  Jan 1997
;       Any additional modifications to be described here
;-

info_i=size(i)
info_j=size(j)

if(info_i(1) ne info_j(1)) then begin
   print,'i and j must have same size'
   return
endif

delta_i = ((1.0*i-15.95)/31.9) * ((1.0*j-15.95)/31.90)

iprime = 1.0*i + delta_i
jprime = 1.0*j

return
end


