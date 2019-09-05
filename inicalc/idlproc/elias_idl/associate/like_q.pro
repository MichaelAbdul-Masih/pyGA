;***********************************************************************

PRO like_q,sources,objects,objarea,omask,fluxname,shist,nrandom,sourcelist,$
    allcat=allcat

;+
; NAME:
;	like_q
;
; PURPOSE:
;	this routine estimates the flux distribution, q(f), used
;	in the likelihood ratio association procedure like_associate
;
; CATEGORY:
;	DBASE
;
; CALLING SEQUENCE:
;	like_q,sources,objects,objarea,omask,fluxname,shist,nrandom,$
;	sourcelist,allcat=allcat
;
; INPUTS:
;	sources: the catalogue of sources to be associated
;	objects: the catalogue of objects with which to associate them
;	objarea: the area covered by the object catalogue
;	omask: the name for the object mask file
;	fluxname: the name for the flux field in the object catalogue
;	nrandom: the no. of random circles to be used per source 
;
; OPTIONAL INPUTS:
;	sourcelist: a list of entries in the source catalogue to be used,
;		    if the whole catalogue is not to be considered
;	
; KEYWORD PARAMETERS:
;	allcat: if allcat is set, then the whole source catalogue is used;
;		if not set, then only the sources in sourcelist are used
;	
;
; OUTPUTS:
;	shist: the name for the q(f) histogram to be estimated here
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
;	For each source in the source catalogue, compute the flux 
;	distribution of the objects falling within the circle circumscribing
;	its three sigma error ellipse and subtract from that the flux
;	distributions of a series of similarly-sized, randomly-placed 
;	circles. The flux distribution build up by this procedure is
;	an estimate for the quantity q(m) defined by Sutherland & Saunders
;	in the likelihood ratio source association paper.
;
; EXAMPLE:
;	
;
; MODIFICATION HISTORY:
; 	Written by:	Bob Mann, July 1996
;
;-

;	on error, return to the calling routine

	on_error,2

;	check that the correct number of parameters were given

	if n_params() lt 5 then message,'Calling sequence:like_q,sources,'+$
	'objects,objarea,omask,fluxname,shist,nrandom,sourcelist,/allcat'

;	open the source catalogue 

	dbopen,sources

;	run zdbcats to find out the name for the error ellipse major axis
;	field

	zdbcats,sources,seq,smajaxname,sminaxname,sscalem,spaname,sscalea

;	convert scalem to refer to scaling to arcminutes

	sscalem=sscalem*60	

;	extract the positions and error ellipse major axes for the sources

	if keyword_set(allcat) then begin

		dbext,-1,'ra,dec',sra,sdec

		dbext,-1,smajaxname,smajax

	endif else begin

		dbext,sourcelist,'ra,dec',sra,sdec

		dbext,sourcelist,smajaxname,smajax

	endelse

;	find out the number of sources to be used

	nsources=n_elements(sra)

	print,'nsources = ',nsources

;	close the source catalogue

	dbclose,sources

;	open the object catalogue

	dbopen,objects

;	use zdbcats to check the equinox of the object catalogue
			
	zdbcats,objects,oeq

;	if the equinox of the source catalogue is not the same as that of the
;	object catalogue, then precess the source ra and dec to the equinox
;	of the object catalogue: the equinox system used depends on the
;	equinox of the object catalogue

	if (seq ne oeq) then begin

		if (oeq eq 1950.0) then begin

			sra=sra*15.0

			precess,sra,sdec,seq,oeq,/fk4

		endif else begin

			sra=sra*15.0

			precess,sra,sdec,seq,oeq

		endelse

		sra=sra/15.0

	endif 

;	extract the object fluxes and find the max and min values thereof

	dbext,-1,fluxname,fluxlist

	max=max(fluxlist,min=min)

	print,'min, max flux:',min,max

;	set up bins  for the flux distribution histogram

	nbins=500

	binsize=(max-min)/float(nbins)

	q=fltarr(nbins)

;	for each source in the source catalogue, do the following:
;
;	(i) run dbcircle to find objects in search circle and find their
;	    fluxes
;	
;	(ii) compute the fraction of the circle lying within the object
;	     catalogue area and multiply the flux distribution of
;	     objects within the search circle accordingly, then add this
;	     to the source flux distribution
;
;	(iii) place a number of similarly sized circles at random within
;	      the object catalogue area, compute the corrected flux 
;	      distribution in each of them and then subtract that from the
;	      source flux distribution
	
	for i=0L,nsources-1L do begin

;		set the search radius

		radius=smajax(i)*3.0

		list=dbcircle(sra(i),sdec(i),radius,dis,/silent)

;		if no objects found then go on to the next source

		if (list(0) ne -1) then print,i,list(0)

		if (list(0) eq -1) then goto, jump1 			

;	        compute the fraction of the search circle that is within 
;		the object catalogue

		frac_in,omask,sra(i),sdec(i),radius,fraction	

;		extract the positions and fluxes for the objects in list

		dbext,list,'ra,dec',ora,odec

		dbext,list,fluxname,fluxlist

;		for each object, add 1/fraction into the appropriate flux bin

		for j=0L,n_elements(list)-1L do begin

			bin=fix((fluxlist(j)-min)/binsize)

			if ((bin lt 0) or (bin gt nbins-1)) then begin

				print,'flux = ',fluxlist(j)

				print,'bin = ',bin

			endif

			q(bin)=q(bin)+1.0/fraction			

		endfor


;	        for nrandom randomly-placed circles of the same size,
;		compute the contribution to qrand similarly

		for j=0L,nrandom-1L do begin

jump2:			randra=randomu(a)
			randdec=randomu(b)

;			set RA and dec of random position

			rra=randra*24.0

			rdec=asin(randdec*2.0-1.0)*180.0/!dpi

;			compute fraction of circle centred on (rra,rdec) that
;			is in the catalogue area

			frac_in,omask,rra,rdec,radius,horse,/silent

			fraction=horse	

;			reject this position if fraction=0.5

			if (fraction le 0.5) then goto, jump2

;			if fraction is greater than half, then find objects
;			within this circle and extract their fluxes
								
			list=dbcircle(rra,rdec,radius,dis,/silent)

			dbext,list,fluxname,fluxlist

;			for each object, subtract 1/fraction/nrandom from the 
;			appropriate flux bin

			for k=0L,n_elements(list)-1L do begin

				bin=fix((fluxlist(k)-min)/binsize)

				if ((bin lt 0) or (bin gt nbins-1)) then begin

					print,'flux = ',fluxlist(k)

					print,'bin = ',bin

				endif

;				print,'fraction,nrandom:',fraction,nrandom

				q(bin)=q(bin)-1.0/fraction/float(nrandom)
	
			endfor
		endfor

jump1:	
	endfor

	print, 'end of loop over sources'

;	divide by the area of the object catalogue, to normalise q

	q=q/objarea

	fluxes=fltarr(nbins)

	fluxes=[min+binsize/2.0+findgen(nbins)*binsize]

	qmax=max(q,min=qmin)

	print,'qmin,qmax:',qmin,qmax

;	plot the histogram, just to check that all is OK

	erase

	plot,fluxes,q,psym=3,xrange=[min,max],yrange=[qmin,qmax],$
	xtitle='flux [native units]',ytitle='q(f)',$
	title='source flux distribution'

;	write out q to a file

	openw,unit,shist,/get_lun

	printf,unit,nbins,min,max

	printf,unit,fluxes

	printf,unit,q

	close,unit

	free_lun,unit

END
