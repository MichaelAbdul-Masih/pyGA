;***********************************************************************
pro readbc95,file,ks,iw,t,w,f

;+
; NAME:
;	readbc95
;
; PURPOSE:
;	reads in a Bruzual and Charlot model file
;
; CATEGORY:
;	analysis
;
; CALLING SEQUENCE:
;	readbc95,file,ks,iw,t,w,f
;
; INPUTS:
;	file: file name
;
; OPTIONAL INPUTS:
;	ks: number of models
;       iw: number of wavelength bins
;        t: time after start of model
;        w: wavelength in A
;        f: spectra 
;	
; KEYWORD PARAMETERS:
;	
;
; OUTPUTS:
;	bc95_sed:  
;
; OPTIONAL OUTPUTS:
;	
;
; COMMON BLOCKS:
;	
;
; SIDE EFFECTS:
;	
;
; RESTRICTIONS:
;	
;
; PROCEDURE:
;	
;
; EXAMPLE:
;	
;
; MODIFICATION HISTORY:
; 	Written by:	Seb Oliver 20 th August 1996
;	July, 1996	
;-


ks=0L
iw=0l

get_lun,unit
openr,unit,file,/f77_unformatted
readu,unit,ks
readu,unit,iw
free_lun,unit
print,ks,iw


t=fltarr(ks)
w=fltarr(iw)
temp=fltarr(iw)
f=fltarr(iw,ks)

openr,unit,file,/f77_unformatted

readu,unit,ks,t
readu,unit,iw,w
for j=0,ks-1 do begin
  readu,unit,temp
  f(*,j)=temp
endfor

free_lun,unit


end
