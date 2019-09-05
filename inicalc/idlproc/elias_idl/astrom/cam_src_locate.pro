;***********************************************************************

PRO cam_src_locate, ra, dec, raster, i, j, scd,disto=disto

;+
; NAME:
;	cam_src_locate
;
; PURPOSE:
;       Takes the RA and DEC coordinates of a "source"
;       and returns the i, j and scd positions for any
;       frames in which that source might appear.
;
;
; CATEGORY:
;	
;
; CALLING SEQUENCE:
;	cam_src_locate,  ra, dec, raster, i, j, scd
;
; INPUTS:
;	ra:    RA(2000) degrees
;       dec:   DEC(2000) degrees
;       raster:       raster structure in which sources detected
;
; OPTIONAL INPUTS:
;	
;	
; KEYWORD PARAMETERS:
;	
;
; OUTPUTS:
;
;   i:  vector of REAL i coordinates
;   j:  vector of REAL j coordinates
;  scd: vector of scd numbers
;
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
; slow and inefficient especially for many sources
;
; PROCEDURE:
;	
;
; EXAMPLE:
;	
; ICSTM CIA> ra=249.381
; ICSTM CIA> dec=40.8669
; ICSTM CIA> cam_src_locate, ra, dec, raster, i, j, scd
; ICSTM CIA> !p.multi=[0,1,2]
; ICSTM CIA> cam_src_plot,round(i(0)),round(j(0)),scd(0),raster
; ICSTM CIA> cam_src_plot,round(i(1)),round(j(1)),scd(1),raster
;
; MODIFICATION HISTORY:
; 	Written by:	Seb Oliver, 19th September 1996
;	Added return where no matches found S Serjeant 3 Aug 1998
;   06 aug 1998: disto keyword added to allow for field distortions Seb Oliver
;	
;-

; ------------------------------------------------------------
;  on error conditions
; ------------------------------------------------------------
 ON_ERROR, 2
 
 
; ------------------------------------------------------------
;  parameters check
; ------------------------------------------------------------
 IF N_PARAMS() LT 6 THEN BEGIN
   MESSAGE, 'CALLING SEQUENCE:  cam_src_locate, ra, dec, raster, i, j, scd'
 ENDIF

 if not keyword_set(disto) then disto=0


nsource=n_elements(ra)
if nsource gt 1 then message,'Can only work on single sources'

nscd=raster.nscd

naxis1=32
naxis2=32
cdelt1=raster.pfov/60./60.
cdelt2=raster.pfov/60./60.

;crude cut
search_rad=cdelt1*naxis1*60.*60
gcirc,1,ra/15.,dec,raster.info.ra/15.,raster.info.dec,dis
pos=where(dis lt search_rad,count)
if count le 0 then begin
	oq = !quiet
	!quiet = 0
	message, 'No matches in this raster',/info
	!quiet=oq
	i=-1
	j=-1
	scd=-1
	return
endif
nscd=count


test_i=fltarr(nscd)
test_j=fltarr(nscd)
test_scd=intarr(nscd)

for ipos=0,nscd-1 do begin
   iscd=pos(ipos)
   icmkhdr, naxis1, naxis2, cdelt1,cdelt2,$
             raster.info.ra(iscd),$
             raster.info.dec(iscd),$
             raster.info.roll(iscd), /detector,$
              hd

   
   icad2xy,ra,dec,hd,x,y,disto=disto

   test_i(ipos)=x
   test_j(ipos)=y
   test_scd(ipos)=iscd


endfor

; checking if within limits

good=where(test_i ge -0.5 and test_i le naxis1-0.5 $
      and  test_j ge -0.5 and test_j le naxis1-0.5 ,count)

if count le 0 then begin
	oq = !quiet
	!quiet = 0
	message, 'No matches in this raster',/info
	!quiet=oq
	i=-1
	j=-1
	scd=-1
	return
endif


i=test_i(good)
j=test_j(good)
scd=test_scd(good)



CLOSING:

END


