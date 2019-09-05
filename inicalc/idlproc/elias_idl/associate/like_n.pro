;***********************************************************************

PRO like_n,objects,objarea,fluxname,nbins,minflux,maxflux,histname

;+
; NAME:
;	like_n
;
; PURPOSE:
;	this procedure computes the background flux distribution for
;	objects in a given catalogue
;
; CATEGORY:
;	DBASE routines
;
; CALLING SEQUENCE:
;	like_n,objects,objarea,fluxname,nbins,minflux,maxflux,histname
;
; INPUTS:
;	objects: object catalogue
;	objarea: area of catalogue in square degrees
;	fluxname: name for the flux field in the catalogue
;	nbins: number of bins to be used in creation of histogram
;	minflux: minimum flux for the histogram, in native units
;	maxflux: maximum flux for the histogram, in native units
;	histname: name for the flux histogram file created here
;	
; OPTIONAL INPUTS:
;	
;	
; KEYWORD PARAMETERS:
;	
;
; OUTPUTS:
;	the output histogram is written to a file, rather than returned as
;	an IDL structure
;
; OPTIONAL OUTPUTS:
;	Describe optional outputs here.
;
; COMMON BLOCKS:
;	BLOCK1:	Describe any common blocks here.
;
; SIDE EFFECTS:
;	
;
; RESTRICTIONS:
;	sensible values for minflux and maxflux must be found externally
;
; PROCEDURE:
;	Makes a histogram of the flux distribution of objects in a catalogue,
;	using the native flux units of the object catalogue 
;
; EXAMPLE:
;	IDL> like_n,'first',1477.0,,inte_int,1000,0.0,50.0,'first.bhist'
;
;	this computes a flux distribution for objects in the FIRST
;	catalogue, using 500 bins between 0.0 and 50.0 (in native units), 
;	and writing the output to the file first.bhist 	
;
; MODIFICATION HISTORY:
; 	Written by:	Bob Mann, July 1996
;
;-

;	on error, return to the calling routine

	on_error,2

;	check that the correct number of parameters were given

	if n_params() lt 7 then message,$
	'Calling sequence: like_n,objects,objarea,fluxname,nbins,minflux,'+$
	'maxflux,histname'

;	open the object catalogue

	dbopen,objects

;	find out the number of objects in the catalogue

	nobjects=db_info('entries')

;	extract the fluxes from the catalogue

	dbext,-1,fluxname,fluxlist 

;	obtain flux histogram

	binsize=(maxflux-minflux)/float(nbins)

	bhist=histogram(fluxlist,max=maxflux,min=minflux,binsize=binsize)

;	divide by the survey area, to get the surface  number density of 
;	objects as a function of flux

	bhist=bhist/objarea

	ymax=max(bhist,min=ymin)

	fluxes=fltarr(nbins)

	fluxes=[minflux+binsize/2.0+findgen(nbins)*binsize]

;	plot the histogram, just to check that all is OK

	erase

	plot,fluxes,bhist,title='flux histogram',$
	xtitle='flux [native units]',ytitle='n(flux)'

;	write the normalised histogram to a file

	openw,unit,histname,/get_lun

	printf,unit,nbins,minflux,maxflux

	printf,unit,fluxes

	printf,unit,bhist

	close,unit

	free_lun,unit

END





