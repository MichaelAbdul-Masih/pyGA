;***********************************************************************

PRO ZHMPNT,ra,dec,i100_out,j2000=j2000

;+
; NAME:
;	zhmpnt
;
; PURPOSE:
;	reads the i100 intensity at a given ra,dec from Mark Jones's Map
;
; CATEGORY:
;	Put a category (or categories) here.  For example: Widgets.
;
; CALLING SEQUENCE:
;	 ZHMPNT,ra,dec,i100_out,/j2000
;
; INPUTS:
;	ra: decimal degrees (1950?) can be scalers, vector or matrix
;       dec: decimal degrees (1950?)
;
; OPTIONAL INPUTS:
;	
;	
; KEYWORD PARAMETERS:
;	
;      j2000: If set then input coordinates are in j2000  
;             if not set then input coordinates are in B1950
;
; OUTPUTS:
;	i100_out:  Intensity at 100 micron in MJy/sr
;
; OPTIONAL OUTPUTS:
;	Describe optional outputs here.
;
; COMMON BLOCKS:
;	BLOCK1:	Describe any common blocks here.
;
; SIDE EFFECTS:
;	Describe "side effects" here.
;
; RESTRICTIONS:
;	Describe any "restrictions" here.
;
; PROCEDURE:
;	You can describe the foobar superfloatation method being used here.
;
; EXAMPLE:
;	Making a plot of i100 in Galactic Coordinates
;
; IDL> gallon=findgen(360)*0.5-90.
; IDL> gallon=replicate(1,360)#gallon
; IDL> gallat=findgen(360)
; IDL> gallat=gallat#replicate(1,360)
; IDL> euler,gallon,gallat,ra,dec,2
; IDL> zhmpnt,ra,dec,i100_out
; IDL> implot,i100_out
;
;
; MODIFICATION HISTORY:
; 	Written by:	Seb Oliver August 1996 Adapted from MHJ's fortan
;	July, 1994	Any additional mods get described here.
;-

on_error,2

if n_params() lt 3 then message,$
    'CALLING SEQUENCE: ZHMPNT,ra,dec,i100_out,/j2000'




;----------------------------------------------------------------------
; READING in Mark Jones' Unformated file
;----------------------------------------------------------------------

; m and n are size of array (N.B. m index counts idl rows not columns)


m=361
n=721

i100=fltarr(n,m) 

get_lun,unit
openr,unit,'/arch/maps/iras/equ_b_361.dat'
readu,unit,i100
free_lun,unit


;----------------------------------------------------------------------
; widths of pixels in ra and dec
;----------------------------------------------------------------------

; delta_dec is width of dec rows
delta_dec=!pi/(m-1)

; dec_arr is dec of centre of pixels in row i
dec_arr=!pi/2.-findgen(m)*delta_dec

; n_in_line is number of valid pixels in a dec row
n_in_line=fix(2.*!pi*cos(dec_arr)/delta_dec)+1
; delta_ra is ra pixel width at each dec row
delta_ra=2.*!pi/n_in_line

;----------------------------------------------------------------------
; ra_arr and dec_arr are arrays corresponding to the coordinates of
; the centres of the pixels in i100 array
;----------------------------------------------------------------------

j=indgen(n)+0.5
ra_arr=j#delta_ra

j=replicate(1,n)
dec_arr=j#dec_arr


;----------------------------------------------------------------------
; converting input coordinates into B1950 (if required)
; thence into radians and finally into x and y coordinates
;----------------------------------------------------------------------

if keyword_set(j2000) then begin
  bprecess,ra,dec,ra_rad,dec_rad
  dec_rad=dec_rad*!dtor
  ra_rad=ra_rad*!dtor
endif else begin
  dec_rad=dec*!dtor
  ra_rad=ra*!dtor
endelse


y=(!pi/2.-dec_rad)/delta_dec
x=ra_rad/delta_ra(round(y)) - 0.5

;----------------------------------------------------------------------
; interpolating I100 flux
;----------------------------------------------------------------------

i100_out=interpolate(i100,x,y)


END
