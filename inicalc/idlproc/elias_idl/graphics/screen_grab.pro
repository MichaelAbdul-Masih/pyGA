;***********************************************************************

PRO screen_grab,file,format=format,gif=gif,bmp=bmp,jpeg=jpeg,tiff=tiff,$
back=back

;+
; NAME:
;	screen_grab
;
; PURPOSE:
;	gives the background from the IC models at and given positions
;       and dates.
;
; CATEGORY:
;	analysis
;
; CALLING SEQUENCE:
;	screen_grab,[file ,format=format,gif=gif,bmp=bmp,jpeg=jpeg,tiff=tiff]
;
; INPUTS:
;
;
; OPTIONAL INPUTS:
;	
;	file: file name defaults to pickpile
;	
; KEYWORD PARAMETERS:
;	format: format for image file gif, tiff, bmp, (jpeg)
;       gif: specfies gif format
;      tiff: specfies tiff format
;       bmp: specfies Microsoft Bit map format
;      jpeg: not implimented 
;      back: change default black background pixels to white
;
; OUTPUTS:
;	
;
; OPTIONAL OUTPUTS:
;	
;
; COMMON BLOCKS:
;	colors: colour look-up table
;
; SIDE EFFECTS:
;	writes a file to disk
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
; 	Written by:	Seb Oliver 1st May 1997 (election day)
;	July, 1996	
;-
common colors, r_orig,g_orig,b_orig,r_cur,g_cur,b_cur
if keyword_set(tiff) then format='tiff'
if keyword_set(gif) then format='gif'
if keyword_set(jpeg) then format='jpeg'
if keyword_set(bmp) then format='bmp'

if not  keyword_set(format) then begin
   print,'No file type specified using gif'
   format='gif'
endif

if n_params() lt 1 then file=pickfile(filter='*.'+format)
im0=tvrd()

if keyword_set(back) then begin

	towhite=where(im0 eq 0B)
	toblack=where(im0 eq 176B)

	if (towhite(0) ne -1) then im0(towhite)=255B
	if (toblack(0) ne -1) then im0(toblack)=0B

endif

case strupcase(strmid(format,0,3)) of
 'TIF': tiff_write,file,im0,red=r_cur,green=g_cur,blue=b_cur
 'GIF': write_gif,file,im0,r_cur,g_cur,b_cur
 'JPE': message,'JPEG not supported or recommended'
 'BMP': write_bmp,file,im0,r_cur,g_cur,b_cur
  else: message,'Format: '+format+' not recognised'
endcase

END

