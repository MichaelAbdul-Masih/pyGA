;***********************************************************************

PRO mask,catalogue,binsize,occupied,ramin,ramax,decmin,decmax,allsky=allsky,$
plot=plot,silent=silent,to_file=to_file,filename

;+
; NAME:
;	mask
;
; PURPOSE:
;	constructs a mask for a given catalogue: a list of bins, of a given
;	size, defined in a cylindrical equal area projection, within
;	which are to be found objects in a given catalogue
;
; CATEGORY:
;	DBASE
;
; CALLING SEQUENCE:
;	mask,catalogue,binsize,occupied[,/plot,/silent,/to_file,filename]
;
; INPUTS:
;	catalogue: the catalogue for which the mask is to be created
;	binsize: the width of the bins in the (x,y) projection -- i.e.
;	there are 360/binsize bins in RA
;
; OPTIONAL INPUTS:
;	ramin: min RA to be considered   \
;	ramax: max RA to be considered    |	these limits are only used
;	decmin: min dec to be considered  |     if /allsky isn't set
;	decmax: max dec to be considered /
;	filename: name of file to which the mask should be written
;	
; KEYWORD PARAMETERS:
;	allsky: if allsky is set, then the whole sky is binned and, if not
;		set, then only the area ramin<RA<ramax, decmin<dec<decmax
;		is considered
;
;	plot: if plot is set, then plot both the original catalogue and the
;	      resulting mask
;	silent: if silent is set, then don't print out various messages
;		indicating the progress of the procedure 	
;	to_file: if to_file is set, then write mask to a file
;
; OUTPUTS:
;	occupied: a list of the bin numbers which are occupied
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
;	a sensible value of binsize must be chosen externally
;
;	the rows and columns of bins are numbered from zero, not one - 
;	i.e. the bottom left bin, which is the first bin, is bin zero
;
;	be careful when setting ramin and ramax to take account of the fact
;	that RA wraps around x: i.e. x increase with RA to 12 hours, where
;	x=180  and then flips to x=-180 -- it may be safest to consider the
;	whole RA range, setting ramin=0.0 and ramax=24.0
;
;	if using multi_cat_area.pro to choose an appropriate bin size,
;	be sure to use the same ra and dec limits.
;
; PROCEDURE:
;	a cylindrical equal area projection is made of the catalogue, which
;	is then binned up in (x,y) and a list made of the bins which are
;	occupied
;
; EXAMPLE:
;	IDL>mask,'abell',1.0,list,/plot,/silent
;
;	this produces a list of bins of size 1.0  which contain
;	Abell clusters, plots the original catalogue and the mask, but
;	doesn't print out various messages along the way, nor write the mask
;	to a file
;
; MODIFICATION HISTORY:
; 	Written by:	Bob Mann, July 1996
;-

;	on error, return to the calling routine

	on_error,2

;	check that the correct number of parameters have been supplied

	if n_params() lt 3 then message,$
	'Calling sequence: mask,catalogue,binsize,occupied'+$
	'[ramin,ramax,decmin,decmax,/allsky,/plot,/silent,/to_file,filename]'

;	open the catalogue

	dbopen,catalogue

;	find out the number of objects in the catalogue

	nobjects=db_info('entries')

;	extract the RAs and decs of the objects in the catalogue: RAs are
;	in hours and decs in degrees

	dbext,-1,'ra,dec',ra,dec
	
;	convert to (x,y) using a cylindrical equal area projection: this gives
;	-180 < x < 180 and -180/pi < y < 180/pi

	wcssph2xy,ra*15,dec,x,y,12,projp1=1

;	find out the max and min x and y values -- depends on whether /allsky
;	is set or not

;	set allsky if ramin=ramax=decmin=decmax=0

	if (keyword_set(allsky) eq 0) then begin

		ramin=ramin(0)
		ramax=ramax(0)
		decmin=decmin(0)
		decmax=decmax(0)

		if ((ramin eq 0) and (ramax eq 0) and (decmin eq 0) $
		and (decmax eq 0)) then allsky=1

	endif

	if (keyword_set(allsky) eq 0)   then begin

		wcssph2xy,ramax*15,decmax,xmax,ymax,12,projp1=1

		wcssph2xy,ramin*15,decmin,xmin,ymin,12,projp1=1	

		xmax=xmax(0)

		xmin=xmin(0)

		ymax=ymax(0)

		ymin=ymin(0)

		if ((ramin eq 0.0) and (ramax eq 24.0)) then begin

			xmin=-180.0

			xmax=180.0

		endif

	endif else begin

		xmin=-180.0

		xmax=180.0

		ymin=-180.0/!dpi

		ymax=180.0/!dpi

	endelse	

;	plot the catalogue in the (x,y) plane, if desired

	if keyword_set(plot) then begin

		!p.multi=[0,1,1]

		erase

		plot,x,y,psym=3,title='original catalogue',$
		xtitle='x',ytitle='y',xrange=[xmin,xmax],yrange=[ymin,ymax]

	endif

;	modify binsize to fit the x and y intervals an integer no. of times

	totnxbins=long(360.0/binsize)+1

	totnybins=long(360.0/!dpi/binsize)+1

	if ((totnxbins-1)*binsize eq 360.0) then totnxbins=totnxbins-1

	if ((totnybins-1)*binsize eq 360.0/!dpi) then totnybins=totnybins-1

	oldxbinsize=binsize

	oldybinsize=binsize

	xbinsize=360.0/totnxbins

	ybinsize=360.0/!dpi/totnybins

	if ((keyword_set(silent) eq 0) and $
	    	(oldxbinsize ne xbinsize)) then begin

	    	print,'old x bin size = ',oldxbinsize

		print,'new x bin size = ',xbinsize

	endif

	if ((keyword_set(silent) eq 0) and $
	    	(oldybinsize ne ybinsize)) then begin

	    	print,'old y bin size = ',oldybinsize

		print,'new y bin size = ',ybinsize

	endif

;	find the columns and rows correponding to xmin,xmax and ymin,ymax
;	respectively, and set up the bin array

	xbinmin=long((xmin+180.0)/xbinsize)

	xbinmax=long((xmax+180.0)/xbinsize)

	if (xmax eq 180.0) then xbinmax=xbinmax-1	

	ybinmin=long((ymin+180.0/!dpi)/ybinsize)

	ybinmax=long((ymax+180.0/!dpi)/ybinsize)

	if (ymax eq 180.0/!dpi) then ybinmax=ybinmax-1

	nxbins=xbinmax-xbinmin+1

	nybins=ybinmax-ybinmin+1

	nbins=long(float(nxbins)*float(nybins))

	if (keyword_set(silent) eq 0) then begin

		print,'no. of bins = ',nbins

		print,'xbinmin,xbinmax,nxbins:',xbinmin,xbinmax,nxbins

		print,'ybinmin,ybinmax,nybins:',ybinmin,ybinmax,nybins

	endif

	full=intarr(nxbins,nybins)

	occupied=lonarr(nbins)

;	loop over each particle in the catalogue, see which bin it is in
;	and set the value of full for that bin to 1 if the bin is empty

	count=-1L

	for i=0L,nobjects(0)-1L do begin

		xbin=long((x(i)+180.0)/xbinsize)

		ybin=long((y(i)+180.0/!dpi)/ybinsize)

;		these two traps beat rounding errors

		if ((xbin gt xbinmax) and (xbin-xbinmax eq 1)) then $
			xbin=xbinmax

		if ((ybin gt ybinmax) and (ybin-ybinmax eq 1)) then $
			ybin=ybinmax

		if (full(xbin-xbinmin,ybin-ybinmin) eq 0) then begin

			full(xbin-xbinmin,ybin-ybinmin)= 1

			count=count+1L

			occupied(count)=ybin*nxbins+xbin

		endif

	endfor

;	remove the unwanted entries from occupied and sort the entries into
;	numerical order

	occupied=occupied(0:count)

	occupied=occupied(sort(occupied))

	if (keyword_set(silent) eq 0) then begin

		print,'no. of occupied bins = ',count+1

	endif

;	plot mask, if desired

	if keyword_set(plot) then begin

		ct=-1L

		x2=fltarr(count+1)

		y2=fltarr(count+1)

		for i=0L,nxbins-1L do begin

			for j=0L,nybins-1L do begin

				if (full(i,j) eq 1) then begin
	
					ct=ct+1L		

					if (((ct lt 0) or (ct gt count))$
					and (keyword_set(silent) eq 0))$
				 	then print,'ct = ',ct

					x2(ct)=xmin+$
					((xmax-xmin)*float(i)/float(nxbins))

					y2(ct)=ymin+$
					((ymax-ymin)*float(j)/float(nybins))

				endif

			endfor

		endfor
	
		plot,x2,y2,psym=3,xtitle='x',ytitle='y',$
		title='occupied bins',subtitle='no. of bins = '+string(nbins),$
		xrange=[xmin,xmax],yrange=[ymin,ymax]

	endif

 
;	write the mask to a file, if desired

	if keyword_set(to_file) then begin

		openw,unit,filename,/get_lun

		printf,unit,binsize,count+1

		printf,unit,occupied

		close,unit

		free_lun,unit

	endif
END





