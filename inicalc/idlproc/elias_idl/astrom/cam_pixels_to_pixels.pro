PRO cam_pixels_to_pixels, x, y, iscd, raster, x2, y2, iscd2, ra, dec, ra2, dec2

;+
; NAME:
;	 cam_pixels_to_pixels
;
; PURPOSE:
;       takes cam "source" positions defined as x,y,iscd 
;       and returns all pixels that are see these sources
;
;
; CATEGORY:
;	
;
; CALLING SEQUENCE:
;	cam_pixels_to_pixels, x, y, iscd, raster, x2, y2, iscd2[, ra, dec, ra2, dec2]

;
; INPUTS:
;	x: vector of x positions in a detector pointing
;       y: vector of y positions in a detector pointing
;    iscd: vector of scd numbers for each "source"
;  raster: raster structure
;
; OPTIONAL INPUTS:
;	
;	
; KEYWORD PARAMETERS:
;	
;
; OUTPUTS:
;
;   x2:  vector of all pixels corresponding to these sources
;   y2:  vector of  all pixels corresponding to these sources
;  iscd2: vector of scd numbers corresponding to these sources
;
;
; OPTIONAL OUTPUTS:
;	ra: ra of sources
;      dec: dec of sources
;       ra2: ras of corresponding pixels
;     dec2:  decs of corresponding pixels
; COMMON BLOCKS:
;	
;
; SIDE EFFECTS:
;	
;
; RESTRICTIONS:
;	
; slow and inefficient especially for many sources
; needs the following lines added to your ~/bin/user_init file
;
; .r /arch/scm/soft/local/lib/astrom/ad2xy
; .r /arch/scm/soft/local/lib/astrom/xy2ad
; .r /arch/scm/soft/local/lib/astrom/compute_distortion
;
;
; PROCEDURE:
;	
;
; EXAMPLE:
;
; CIA> y = [0., 13.5, 10, 0.5]
; CIA> iscd = [10, 0, 2, 3]
; CIA> 
; CIA> restore,'rasdark796001760_99062414231600.cub'
; CIA> 
; CIA>  cam_pixels_to_pixels, x, y, iscd, raster, x2, y2, iscd2
; CIA> 
; CIA> plot,x2,y2,xrange=[-1, 32], yrange=[-1, 32],psym=1
; CIA> oplot,x,y,psym=4,color=3
;
;
; MODIFICATION HISTORY:
; 	Written by:	Seb Oliver, 15th July 1999
;	
;-



;----------------------------------------------------------------------
; generating a dummy source structure

ra = dblarr(n_elements(x))
dec = dblarr(n_elements(y))

src = { x:0.0, y:0.0, iscd:0, ra: 0.0d0, dec: 0.0d0}

sources = replicate(src, n_elements(x))
sources.x = x
sources.y = y
sources.iscd = iscd

; --------------------------------------------------------------------------
;     Reading in field distortion file

	dist_file='/soft/lib/cia/OCT98/tables/'
	if raster.channel ne 'LW' then message,'Can not use SW detector!
	dist_file=dist_file+'lw'
	case raster.pfov OF  
	  6: dist_file=dist_file+'6asr' 
	  3: dist_file=dist_file+'3asr' 
	else: message,'PFOV not acceptable' 
	endcase

	case round(raster.wavelength) of
	  7: dist_file=dist_file+'2.dis'
	  15: dist_file=dist_file+'3.dis'
	else: message,'Wavelength not acceptable'
	ENDCASE


	disto=read_distortion_file(dist_file)
        print, 'Distortion File:', dist_file


; --------------------------------------------------------------------------
; calculating ra and dec for each "source"
; --------------------------------------------------------------------------

cam_src_astrom, raster, sources,disto=disto

ra = sources.ra
dec = sources.dec

; --------------------------------------------------------------------------
; calculating x,y and scds for each ra, dec
; --------------------------------------------------------------------------

x2 = 0.
y2 = 0.
iscd2 = 0


FOR isrc=0, n_elements(sources)-1 DO BEGIN 

   cam_src_locate, sources[isrc].ra, sources[isrc].dec, raster, x0, y0, iscd0,disto=disto

   x2 = [x2, x0]
   y2 = [y2, y0]
   iscd2 = [iscd2, iscd0]

ENDFOR

x2 = x2[1:*]
y2 = y2[1:*]
iscd2 = iscd2[1:*]

;----------------------------------------------------------------------
; this last section is only a test but doesn't waste much time
;print, systime()

sources2 = replicate(src, n_elements(x2))
sources2.x = x2
sources2.y = y2
sources2.iscd = iscd2

cam_src_astrom, raster, sources2,disto=disto
ra2 = sources2.ra
dec2 = sources2.dec

;print, systime()

END
