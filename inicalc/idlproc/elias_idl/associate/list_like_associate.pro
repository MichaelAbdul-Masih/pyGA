;***********************************************************************

PRO list_like_associate,seq,ra,dec,objects,like,bhistname,shistname,$
smajax,sminax,spa,radius=radius,raname=raname,decname=decname,$
fluxname=fluxname,majaxname=majaxname,minaxname=minaxname,paname=paname,$
stband=stband

;+
; NAME:
;	list_like_associate
;
; PURPOSE:
;	runs like_associate for a list of (RA,dec) positions, rather than
;	a database of 
;
; CATEGORY:
;	DBASE
;
; CALLING SEQUENCE:
;	list_like_associate,seq,ra,dec,objects,like,bhistname,shistname,$
;	smajax,sminax,spa,radius=radius
;
; INPUTS:
;	seq: equinox of source position
;	ra: list of ras of sources (in hours)
;	dec: list of decs of source (in degrees)
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
;	stband: if set, gives STScI band (not V606) to be used
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
; MODIFICATION HISTORY:
; 	Written by:	Bob Mann, 27 November 1996
;			8.ii.97 stband keyword added
;-

;	on error, return to calling procedure

	on_error,2

;	check that required parameters have been set correctly
	
	if n_params() lt 5 then message,'Calling sequence:'+$
	'list_like_associate,seq,ra,dec,objects,objlist,dis,like,'+$
	'bhistname,shistname,smajax,sminax,spa,radius=radius,n_err,'+$
	'/nerr'

	no=n_elements(ra)

	like=fltarr(no)

	xlike=fltarr(10000)
	xdis=fltarr(10000)
	xobj=intarr(10000)
	

	for i=0l,no-1l do begin

		like_associate,seq,ra(i),dec(i),objects,xobj,xdis,xlike,$
		bhistname,shistname,smajax,sminax,spa,radius=radius,$
		raname=raname,decname=decname,fluxname=fluxname,$
		majaxname=majaxname,minaxname=minaxname,paname=paname,$
		/single,stband=stband

		like(i)=xlike(0)

	endfor
END



