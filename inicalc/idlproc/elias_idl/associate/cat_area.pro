;***********************************************************************

PRO cat_area,catalogue,binsize,area,ramin,ramax,decmin,decmax,$
allsky=allsky,plot=plot,silent=silent

;+
; NAME:
;	cat_area
;
; PURPOSE:
;	estimates the area covered by an object catalogue
;
; CATEGORY:
;	DBASE routines
;
; CALLING SEQUENCE:
;	cat_area,catalogue,binsize,area[ramin,ramax,decmin,decmax,/allsky,
;	,/plot,/silent]
;
; INPUTS:
;	catalogue: the catalogue whose area is to be estimated
;	binsize: the size of bin to be used - defined in (x,y) coordinates
;
; OPTIONAL INPUTS:
;	ramin: min RA to be considered   \
;	ramax: max RA to be considered    |	these limits are only used
;	decmin: min dec to be considered  |     if /allsky isn't set
;	decmax: max dec to be considered /
;	
; KEYWORD PARAMETERS:
;	allsky: if allsky is set, then the whole sky is used and, if not
;	        set, then only the area ramin<RA<ramax, decmin<dec<decmax
;		is considered
;
;	plot: if plot is set, then plot out the following things:
;		(a) the original catalogue, in (x,y)
;		(b) the bin centres, for the (x,y) binning
;		(c) the centres of the occupied bins
;
;	silent: if silent is set, then don't print various messages which
;		indicate the progress of the procedure
;		
; OUTPUTS:
;	area: the value of area of the catalogue in square degrees
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
;	it is necessary to find a sensible value for the bin size 
;	(e.g. by running multi_cat_area.pro for a number of bin sizes),
;	since the estimate of the area deduced by this procedure varies
;	with the size of bin used.
;
;	be careful when setting ramin and ramax to take account of the fact
;	that RA wraps around x: i.e. x increase with RA to 12 hours, where
;	x=180  and then flips to x=-180 -- it may be safest to consider the
;	whole RA range, setting ramin=0.0 and ramax=24.0
;
; PROCEDURE:
;	bin the objects in the catalogue into bins of a certain size, using
;	a cylindrical equal area projection, then count what fraction of bins 
;	are occupied, to yield a measure of the area covered by the catalogue
;	
;
; EXAMPLE:
;	IDL>cat_area,abell,1.0,area
;	
;	this computes the area of the sky covered by the Abell catalogue,
;	using bins that are 1 unit on a side, not plotting, but printing
;	intermediate messages
;
; MODIFICATION HISTORY:
; 	Written by:	Bob Mann, July 1996
;-

;	check that parameters have been set properly

	if n_params() lt 3 then message,$

	'Calling sequence: cat_area,catalogue,binsize,area'+$
	'[ramin,ramax,decmin,decmax,/allsky,/plot,/silent]'

;	if plots are required then allow multiple plots

	if keyword_set(plot) then begin

		!p.multi=[0,3,2]

	endif

;	open the catalogue

	dbopen,catalogue

;	find out number of objects in catalogue

	nobjects=db_info('entries')

	if (keyword_set(silent) eq 0) then print,'nobjects = ',nobjects

;	extract the RAs and decs of the objects in the catalogue: RAs are
;	in hours and decs are in degrees

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

		if ((ramin eq 0) and (ramax eq 0) and (decmin eq 0)$
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

	if keyword_set(plot)  then begin

;		plot the original catalogue in the (x,y) plane: plotting the
;	        whole (x,y) plane, unless the max and min RA and dec values
;		have been set

		erase

		plot,x,y,psym=3,title='original catalogue',$
		xtitle='x',ytitle='y',subtitle='no. of objects = '+$
		string(nobjects(0)),xrange=[xmin,xmax],yrange=[ymin,ymax]

	endif

;	modify binsize to fit the x and y plane exactly an integer no. of times

	totnxbins=long(360.0/binsize)+1

	if ((totnxbins-1)*binsize eq 360.0) then totnxbins=totnxbins-1

	oldxbinsize=binsize

	xbinsize=360.0/totnxbins

	if ((keyword_set(silent) eq 0) and $
	    	(oldxbinsize ne xbinsize)) then begin

	    	print,'old x bin size = ',oldxbinsize

		print,'new x bin size = ',xbinsize

	endif

	totnybins=long(360.0/!dpi/binsize)+1

	if ((totnybins-1)*binsize eq 360.0/!dpi) then totnybins=totnybins-1

	oldybinsize=binsize

	ybinsize=360.0/!dpi/totnybins

	if ((keyword_set(silent) eq 0) and $
	    	(oldybinsize ne ybinsize)) then begin

	    	print,'old y bin size = ',oldybinsize

		print,'new y bin size = ',ybinsize

	endif

;	find the columns and rows corresponding to xmin,xmax and ymin,ymax
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

		print,'xmin,xmax:',xmin,xmax
	
		print,'ymin,ymax:',ymin,ymax

		print,'no. of bins = ',nbins

		print,'xbinmin,xbinmax,nxbins:',xbinmin,xbinmax,nxbins

		print,'ybinmin,ybinmax,nybins:',ybinmin,ybinmax,nybins

	endif

	full=intarr(nxbins,nybins)

;	loop over each object in the catalogue, see which bin it is in
;	and set the value of full for that bin to 1 if the bin is empty

	count=0L

	x2=fltarr(nbins)

	y2=fltarr(nbins)

	if keyword_set(plot) then begin

;		plot the centres of the bins to be used

		for i=0L,nxbins-1L do begin

			col=xbinmin+i

			for j=0L,nybins-1L do begin

				row=ybinmin+j

				entry=long(float(j)*float(nxbins)+float(i))
			
				x2(entry)=-180.0+(0.5+col)*binsize

				y2(entry)=-180.0/!dpi+(0.5+row)*binsize

			endfor

		endfor

		plot,x2,y2,psym=3,xtitle='x',ytitle='y',$
		title='bin centres',subtitle='no. of bins = '+$
		string(nbins),xrange=[xmin,xmax],yrange=[ymin,ymax]

	endif

;	count the number of bins which are occupied

	for i=0L,nobjects(0)-1L do begin

		xbin=long((x(i)+180.0)/xbinsize)

		ybin=long((y(i)+180.0/!dpi)/ybinsize)

;		these two traps beats a rounding error 

		if ((xbin gt xbinmax) and (xbin-xbinmax eq 1)) then $
			xbin=xbinmax

		if ((ybin gt ybinmax) and (ybin-ybinmax eq 1)) then $
			ybin=ybinmax


		if (full(xbin-xbinmin,ybin-ybinmin) eq 0) then begin

			full(xbin-xbinmin,ybin-ybinmin)= 1

			count=count+1L

		endif

	endfor

;	compute the area of the catalogue

	bins_in_sky=(360.0/xbinsize)*(360.0/!dpi/ybinsize)

	occupied_fraction=float(count)/bins_in_sky

	area=4.0*!dpi*180.0*180.0/!dpi/!dpi*occupied_fraction

	if (keyword_set(silent) eq 0) then begin

		print,'no. of occupied bins  = ',count

		print,'area of catalogue = ',area,' square degrees

	endif

	if keyword_set(plot) then begin

;		plot the occupied bins

		ct=-1L

		x2=fltarr(count+1)

		y2=fltarr(count+1)

		for i=0L,nxbins-1 do begin

			for j=0L,nybins-1 do begin

				if (full(i,j) eq 1) then begin
	
					ct=ct+1L		

					if ((ct lt 0) or (ct gt count-1)) $
						then print,'ct = ',ct

					x2(ct)=xmin+$
					((xmax-xmin)*float(i)/float(nxbins))

					y2(ct)=ymin+$
					((ymax-ymin)*float(j)/float(nybins))

				endif

			endfor

		endfor

		plot,x2,y2,psym=3,xtitle='x',ytitle='y',$
		title='full bins',subtitle='no. of bins = '+$
		string(nbins),xrange=[xmin,xmax],yrange=[ymin,ymax]

	endif

END





