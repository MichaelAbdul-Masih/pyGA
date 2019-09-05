;***********************************************************************

pro like_associate,seq,ra,dec,objects,objlist,dis,like,bhistname,shistname,$
    smajax,sminax,spa,radius=radius,n_err,nerr=nerr,raname=raname,$
    decname=decname,fluxname=fluxname,majaxname=majaxname,$
    minaxname=minaxname,paname=paname,single=single,stband=stband

;+
; NAME:
;	LIKE_ASSOCIATE
;
; PURPOSE:
;	associate a source with objects in another catalogue,
;	using the likelihood ratio method of Sutherland & Saunders
;
; CATEGORY:
;	DBASE routines
;
; CALLING SEQUENCE:
;	like_associate,seq,ra,dec,objects,objlist,dis,like,bhistname,
;	shistname,smajax,sminax,spa,radius=radius,n_err,/nerr,raname=raname,
;	decname=decname,fluxname=fluxname,majaxname=majaxname,
;	minaxname=minaxname,paname=paname,/single,stband=stband
;	
;
; INPUTS:
;	seq: equinox of source position
;	ra: ra of source (in hours)
;	dec: dec of source (in degrees)
;	objects: name of catalogue of objects
;
;
; OPTIONAL INPUTS:
;	none
;	
; KEYWORD PARAMETERS:
;	radius: preliminary search radius 
;	n_err: no. of times bigger than the semi-major axis of source error 
;	ellipse the radius of the (circular) preliminary search area should be
;	smajax: major axis of source error ellipse
;	sminax: minor axis of source error ellipse
;	spa: position angle of source error ellipse
;	shistname: name of histogram giving source flux distribution
;	bhistname: name of histogram giving background flux distribution
;	majaxname: name of major axis field in object catalogue
;	minaxname: name of minor axis field in object catalogue
;	paname: name of position angle field in object catalogue
;	fluxname: name of flux field in object catalogue
;	single: if set, then ignore smaller error ellipse
;	stband: STScI photometric band in which to run test: V606 is default
;
; OUTPUTS:
;	objlist: list of entry numbers in object catalogue of associated 
;	         objects: objects are sorted by likelihood if likelihoods
;		 evaluated, or by distance from the source, if not: if no 
;		 objects detected in preliminary search  then list(0)=-1
;
; OPTIONAL OUTPUTS:
;	dis: list of distances of objects listed in list from source: objects
;	     listed in order of increasing dis if likelihood ratios not 
;	     evaluated, or in decreasing likelihood ratio if they were
;	like: list of likelihood ratios of objects found in preliminary search
;	      area, listed in order of decreasing likelihood ratio
;	
;
; COMMON BLOCKS:
;	BLOCK1:	Describe any common blocks here.
;
; SIDE EFFECTS:
;	Describe "side effects" here.
;
; RESTRICTIONS:
;	degrees are used throughout the routine, so need to convert from 
;	units used in catalogues: assume that RAs are in hours and decs
;	in degrees, and use zdbcats to find the units used for the axes
;	of the error ellipses
;
;	at the moment, can only use the /single option
;
; PROCEDURE:
;	(for background see Sutherland & Saunders, 1992, MNRAS, 259, 413)
;	
;      	a preliminary search is made for objects in a circle around the
;	source, whose radius can be set by the user, or expressed in terms
;	of the major axis of the source's error ellipse
;	
;	if more than one object is found in that area, then sources are
;	ranked either by increasing distance from the source, if no flux
;	information about the source population is given, or by decreasing
;	likelihood ratio, if that information is available.
;
;
; EXAMPLE:
;
;	IDL>like_associate,seq1,ra1,dec1,'first',output,dist,smajax=1.0/60.0
;
;	this searches for objects in the FIRST radio catalogue around an 
;	object with position (ra1,dec1), where the names for the RA and dec
;	fields in the catalogue are ra and dec, respectively. Candidates
;	are returned in the list output, in order of decreasing distance from
;	the source (as no source population flux magnitude was given with
;	which to compute the likelihood ratios) and these distances are
;	returned, in order, in distances. Since the value of radius is not set,
;	nor is the value of n_err, the preliminary search is performed in a 
;	circle whose radius is five times the semi-major axis of the error 
;	ellipse of the source, which is given as 1.0 arcmins. 

; MODIFICATION HISTORY:
; 	Written by:	Bob Mann, July 1996
;        		16.xii.96 modified to take account of multiple
;				  counting of bright objects in HDF_STSCI
;			8.ii.97   modified to remove the last addition -
;				  see note below, by commented-out text
;			8.ii.97   modified to allow likelihood ratio test
;				  be run for any of the four STScI
;				  photometric bands
;			5.viii.99 modified, to return like=-1.0,dis=9999.0,
;				  obj=0 if no object in initial search radius
;-

;	on error, return to calling procedure

	on_error,2

;	check that required parameters have been set correctly
	
	if n_params() lt 5 then message,'Calling sequence:like_associate,'+$
	'seq,ra,dec,objects,objlist,dis,like,bhistname,shistname,smajax,'+$
	'sminax,spa,radius=radius,n_err,/nerr,/single'

;	open the objects catalogue

	dbopen,objects 

;stop
;	use  zdbcats procedure to set names of various fields in the
;	object catalogue, plus units

	zdbcats,objects,equinox,majaxname,minaxname,scalem,paname,scalea

	scalem=scalem

;	if the equinox of the source catalogue is not the same as that of the
;	object catalogue, then precess the object ra and dec to the equinox
;	of the object catalogue: the equinox system used depends on the
;	equinox of the object catalogue

	if (seq ne equinox) then begin

		ra=ra*15.0

		if (equinox eq 1950.0) then begin

			precess,ra,dec,seq,equinox,/fk4

		endif else begin

			precess,ra,dec,seq,equinox

		endelse

		ra=ra/15.0

	endif 

	ra=ra(0)

	dec=dec(0)

;	need to tranform source position to HST reference frame if object
;       catalogue is HDF_IFA -- will need to transform back after dbcircle

	if (objects eq 'hdf_ifa') then begin

		ra=ra-0.089/60.0/60.0

		dec=dec+1.03/60.0/60.0

	endif

;	perform preliminary search for objects in a circle around the source:
;	if the radius keyword isn't set then it uses a radius n_err times the
;	major axis of the error ellipse of the source, with a default of 
;	n_err=5 if set isn't specified.

;	list contains the entry nos. of objects found and dis is a vector 
;	listing their distances from the source  (in arc minutes)

	if (radius eq 0)  then begin

		if (keyword_set(nerr) eq 0 ) then n_err=5		

		radius=n_err*scalem*smajax

	endif	

	radius=radius(0)

	list=dbcircle(ra,dec,radius,dis,/silent)	

;	transform source position back to MERLIN reference frame, in the
;	case that objects='HDF_IFA', when it's been transformed the other
;	way above. 

	if (objects eq 'hdf_ifa') then begin

		ra=ra+0.089/60.0/60.0

		dec=dec-1.03/60.0/60.0

	endif

;	if zero objects were found, then return

	if (list(0) le 0 ) then begin

		objlist=0

		dis=9999.0

		like=-1.0
	
		return

	endif




;	THIS NEXT BIT HAS BEEN COMMENTED OUT, BECAUSE IT'S CLEAR THAT SOME
;	OF THE DAUGHTER OBJECTS IN THE HDF_STSCI DATABASE REALLY ARE
;	DISTINCT GALAXIES, AND NOT ALL OUR ASSOCIATIONS SHOULD BE WITH
;	OBJECTS WITH NAMES ENDING ".0"

;	more than one object found

;	if (objects eq 'hdf_stsci') then begin


;		need to take account of multiple objects in HDF_STSCI database:
;		get names of objects in list

;		dbext,list,'name',names

;		find which names do contain the string ".0" - 
;		i.e. are parent objects

;		namelist=strpos(names,'.0')

;		parents=where(namelist ne -1)

;		return if (parents eq -1)

;		if (n_elements(parents) lt 2) then begin

;			objlist=list

;			return

;		endif

;		list=list(parents)

;		dis=dis(parents)

;	endif

;	check that the keywords for the histograms of source and background
;	flux estimations have been set: if not, then association is only
;	by distance, so sort the candidates on distance and return

	if (keyword_set(shistname) eq 0) then begin


		order=sort(dis)
                dis=dis(order)
		objlist=list(order)

		return

	endif

;	proceed with likelihood estimation

;	first, extract information about the candidates, so that we don't
;	have to keep using the full database from now onwards

	if (keyword_set(raname) eq 0) then message,'raname not set'

	if (keyword_set(decname) eq 0) then message,'decname not set'

	if (keyword_set(fluxname) eq 0) then message,'fluxname not set'

	if (keyword_set(majaxname) eq 0) then message,'majaxname not set'

	if (keyword_set(minaxname) eq 0) then message,'minaxname not set'

	if (keyword_set(paname) eq 0) then message,'paname not set'

	items=[raname,decname,fluxname,majaxname,minaxname,paname]
	
;stop

	dbext,list,items,objra,objdec,objflux,omajax,ominax,opa

;	extra work to be done if using HDF_STScI catalogue in a band
;	other than V_606

	if ((objects eq 'hdf_stsci') and (keyword_set(stband) ne 0)) then begin

		if (stband eq 'u') then begin
	
			dbext,list,'vmagt,b_v,u_b',vmagt,b_v,u_b

			objflux=vmagt+b_v+u_b

		endif

		if (stband eq 'b') then begin

			dbext,list,'vmagt,b_v',vmagt,b_v

			objflux=vmagt+b_v

		endif

		if (stband eq 'v') then begin

			dbext,list,'vmagt',vmagt

			objflux=vmagt

		endif

		if (stband eq 'i') then begin

			dbext,list,'vmagt,v_i',vmagt,v_i

			objflux=vmagt-v_i

			neg=where(objflux lt 0.0,count)

			if (count gt 0) then begin

				objflux(neg)=99.0		
	
			endif		
	
		endif

	endif


;	extra stuff to be done if objects is the FIRST catalogue

	if (objects eq 'first') then begin

		dbext,list,'peak_int,rms',peak_int,rms

		snr=(peak_int-0.25)/rms

		omajax=sqrt(5.4^2 + omajax^2)*(1.0/snr+1/20.0)

		ominax=sqrt(5.4^2 + ominax^2)*(1.0/snr+1/20.0)

	endif

;	shift IfA objects to MERLIN reference frame

	if (objects eq 'hdf_ifa') then begin

		objra=objra+0.089/60.0/60.0

		objdec=objdec-1.03/60.0/60.0

	endif

;	convert omajax and ominax to arcmins

	omajax=omajax*scalem

	ominax=ominax*scalem

;	read in the flux histograms

	openr,u,shistname,/get_lun

	readf,u,snbins,sminflux,smaxflux

	sflux=fltarr(snbins)

	shist=fltarr(snbins)

	readf,u,sflux

	readf,u,shist

	close,u

	free_lun,u

	openr,u,bhistname,/get_lun

	readf,u,bnbins,bminflux,bmaxflux

	bflux=fltarr(bnbins)

	bhist=fltarr(bnbins)

	readf,u,bflux

	readf,u,bhist

	close,u

	free_lun,u

	sminflux=sminflux(0)

	smaxflux=smaxflux(0)

	bminflux=bminflux(0)

	bmaxflux=bmaxflux(0)

;	compute the values of s and b from the histograms shist and
;	bhist by interpolation: set svalues to zero if objflux is outside
;	the range of the flux histogram		

	svalues=interpol(shist,sflux,objflux)

	index=where(svalues lt 0.0,count)

	if count ne 0 then svalues(index)=0.0

	bvalues=interpol(bhist,bflux,objflux)	

;	compute the values of f

	pi=!dpi

	rdn=180/pi

	ncands=n_elements(list)

	factor=fltarr(ncands)

	majax=fltarr(ncands)

	minax=fltarr(ncands)

	pa=fltarr(ncands)

	if keyword_set(single) then begin

		for i=0L,ncands-1L do begin

			sarea=smajax*sminax

			oarea=omajax(i)*ominax(i)

			if (sarea gt oarea) then begin

				majax(i)=smajax

				minax(i)=sminax

				pa(i)=spa

			endif else begin

				majax(i)=omajax(i)

				minax(i)=ominax(i)

				pa(i)=opa(i)

			endelse

		endfor

		factor=1.0/2.0/pi/majax/minax

	endif else begin

		factor=abs(1.0/4.0/pi/pi/smajax/sminax/omajax/ominax)

	endelse

	x=(ra-objra)*15*cos(dec/rdn)

	y=(dec-objdec)

	nx1=fltarr(ncands)

	ny1=fltarr(ncands)

	nx2=fltarr(ncands)

	ny2=fltarr(ncands)

	if keyword_set(single) then begin

		nx1=(x*cos(pa/rdn)-y*sin(pa/rdn))/majax

		ny1=(x*sin(pa/rdn)+y*cos(pa/rdn))/minax

		nx2(*)=0.0

		ny2(*)=0.0

	endif else begin

		nx1=(x*cos(spa/rdn)-y*sin(spa/rdn))/smajax

		ny1=(x*sin(spa/rdn)+y*cos(spa/rdn))/sminax

		nx2=(x*cos(opa/rdn)-y*sin(opa/rdn))/omajax

		ny2=(x*sin(opa/rdn)+y*cos(opa/rdn))/ominax

	endelse
		
	f=fltarr(ncands)

	normdissq=nx1*nx1+ny1*ny1+nx2*nx2+ny2*ny2
 
	index=where(normdissq lt 175.0,count)

	if count ne 0 then f(index)=exp(-1.0*normdissq(index)/2.0)

	f=factor*f

;	compute the likelihood ratios: setting to -2 those where the
;	object flux falls outside the object flux histogram

	index=where(bvalues gt 0.0,count)

	like=fltarr(ncands)

	like(*)=-2.0

	if count ne 0 then like(index)=f(index)*svalues(index)/bvalues(index)

;       Also account for the few IfA sources with H+K band 
;	magnitudes less than the nominal magnitude limit of the 
;	sample, by greatly decreasing its likelihood

	if (objects eq 'hdf_ifa') then begin

		index=where(list ge 181,count)
	
		if count ne 0 then like(index)=0.0

	endif

;	sort the outputs by likelihood ratio and return, unless the highest
;	likelihood is zero, in which case order by increasing distance

	order=lonarr(ncands)

	order=reverse(sort(like))

	like=like(order)

	if (like(0) gt 0.0) then begin

		objlist=list(order)

		dis=dis(order)

	endif else begin

		order=sort(dis)

		objlist=list(order)

		dis=dis(order)

	endelse


END










