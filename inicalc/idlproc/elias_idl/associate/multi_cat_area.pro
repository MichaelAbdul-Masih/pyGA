;***********************************************************************

PRO multi_cat_area,catalogue,nsizes,minbinsize,maxbinsize,binsize,area,$
    ramin,ramax,decmin,decmax,allsky=allsky,plot=plot,silent=silent,$
    hardcopy=hardcopy	
;+
; NAME:
;	multi_cat_area
;
; PURPOSE:
;	runs cat_area.pro for a range of bin sizes, to see how the estimate
;	of the area covered by a given catalogue varies with bin size, and,
;	hence, determine the appropriate bin size to use for a given
;	catalogue
;
; CATEGORY:
;	DBASE routines
;
; CALLING SEQUENCE:
;	multi_cat_area,catalogue,nsizes,minbinsize,maxbinsize[,binsize,area,$
;	ramin,ramax,decmin,decmax,/allsky,/plot,/silent,/hardcopy]
;
; INPUTS:
;	catalogue: the catalogue whose area is to be estimated
;	nsizes: the number of different binsizes to try
;	minbinsize: the minimum size for bins 
;	maxbinsize; the maximum size for bins 
;
; OPTIONAL INPUTS:
;	ramin: if allsky isn't set, then this is minimum RA considered
;	ramax: if allsky isn't set, then this is maximum RA considered
;	decmin: if allsky isn't set, then this is minimum dec considered
;	decmax: if allsky isn't set, then this is minimum dec considered
;	
;	
; KEYWORD PARAMETERS:
;	plot: if plot is set, then plots are produced by cat_area.pro for
;	      each bin size
;	silent: if silent is set, then various messages aren't printed during
;		the running of cat_area.pro
;
;	hardcopy: if hardcopy is set, then a postscript version of the plot
;		  of catalogue area against bin size is produced
;
;	allsky: if set, then bin up the whole sky, ignoring ramin,ramax,
;	        decmin and decmax
;
; OUTPUTS:
;	no outputs as such -- just a plot of catalogue area against binsize
;
; OPTIONAL OUTPUTS:
;	binsize: a list of nsizes values of bin size for which the area of
;	         the survey was estimated
;	area: a list of nsizes estimates for the area of the survey, estimated
;	      using the binsizes given in binsizes
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
;	just runs cat_area.pro for a variety of bin sizes, ensuring that
;	square bins in (x,y) space are used
;
; EXAMPLE:
;	IDL> multi_cat_area,'abell',30,1.0,10.0,/plot
;
;	run cat_area for 30 different bin sizes, where binsize varies
;	between 1.0 and 10.0, producing plots for each bin size,
;	but not returning the values of the binsizes or the areas
;
; MODIFICATION HISTORY:
; 	Written by:	Bob Mann, July 1996
;-

;	on error, return to calling procedure

	on_error,2

;	check that parameters correctly specified

	if n_params() lt 4 then message,$
	'Calling sequence: multi_cat_area,catalogue,nsizes,minbinsize,'+$
	'maxbinsize[,binsize,area,ramin,ramax,decmin,decmax,/allsky,/plot'+$
	',/silent,/hardcopy]'

;	set up arrays to hold results of multiple calls to cat_area.pro

	binsize=fltarr(nsizes)

	area=fltarr(nsizes)

;	set allsky if ramin=ramax=decmin=decmax=0

	if (keyword_set(allsky) eq 0) then begin

		if ((ramin eq 0) and (ramax eq 0) and (decmin eq 0) $
		     and (decmax eq 0)) then allsky=1

	endif

;	loop over each of the binsizes

	for i=0L,nsizes-1L do begin

		binsize(i)=minbinsize+i*(maxbinsize-minbinsize)/$
				float(nsizes-1L)

;		various choices, depending on whether plots and printed 
;		messages are wanted in cat_area.pro		

		if (keyword_set(plot)) then begin

			if (keyword_set(silent)) then begin

				if (keyword_set(allsky)) then begin

					cat_area,catalogue,binsize(i),blah,$
					/plot,/silent,/allsky

				endif else begin

					cat_area,catalogue,binsize(i),blah,$
					ramin,ramax,decmin,decmax,/plot,$
					/silent
					
				endelse


			endif else begin

				if (keyword_set(allsky)) then begin	

					cat_area,catalogue,binsize(i),$
					blah,/plot,/allsky

				endif else begin

					cat_area,catalogue,binsize(i),$
					blah,ramin,ramax,decmin,decmax,/plot

				endelse
			endelse

		endif else begin

			if (keyword_set(silent)) then begin

				if keyword_set(allsky) then begin

					cat_area,catalogue,binsize(i),blah,$
					/silent,/allsky

				endif else begin

					cat_area,catalogue,binsize(i),blah,$
					/silent

				endelse	

			endif else begin

				if keyword_set(allsky) then begin

					cat_area,catalogue,binsize(i),blah,$
					/allsky

				endif else begin

					cat_area,catalogue,binsize(i),blah
				
				endelse
		
			endelse


		endelse

		area(i)=blah

	endfor

;	plot the results: the estimated survey area vs binsize

	plot,binsize,area,xtitle='side of bin',$
	ytitle='survey area [steradians]',title=catalogue
	
;	make a hardcopy of that plot, if required

	print,'keyword_set(hardcopy) = ',keyword_set(hardcopy)

	if keyword_set(hardcopy) then begin
		
		set_plot,'ps'

		device,file='multi_cat_area.ps'

		plot,binsize,area,xtitle='side of bin',$
		ytitle='survey area [steradians]',title=catalogue

		device,/close

		set_plot,'x'

	endif

END









