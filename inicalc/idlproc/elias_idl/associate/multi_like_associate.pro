;***********************************************************************

PRO multi_like_associate,objects,sources,objects1,objects2,shist,bhist,$
    likes1,likes2,distances1,distances2,sourcelist,larger=larger,$
    allcat=allcat,radius=radius,raname=raname,decname=decname,$
    fluxname=fluxname,majaxname=majaxname,minaxname=minaxname,paname=paname,$
    single=single,to_file=to_file,stband=stband,$
    equinox=equinox,smajaxname=smajaxname,sminaxname=sminaxname,$
    scalem=scalem,spaname=spaname,scalea=scalea
   
 
;+
; NAME:
;	multi_like_associate
;
; PURPOSE:
;	runs like_associate for a list of sources
;
; CATEGORY:
;	DBASE routines
;
; CALLING SEQUENCE:
;	multi_like_associate,objects,sources,objects1,objects2,shist,bhist'+$
;	[,likes1,likes2,distances1,distances2,sourcelist,/larger,/allcat,
;	/single,/to_file]
;
; INPUTS:
;	objects: catalogue of objects
;	sources: catalogue of sources in database format
;	shist: histogram giving source flux distribution
;	bhist: histogram giving background flux distribution
;	radius: value of preliminary search radius (arcmins)
;	fluxname: name for flux field in object catalogue
;	
; OPTIONAL INPUTS:
;	sourcelist: list of entry numbers (in sources) of sources for
;	which we want to find associations - to be given if associations
;	for full catalogue are not required
;
; KEYWORD PARAMETERS:
;	larger: if set, then run like_associate for increasingly large
;	        preliminary search radii, if don't find any associations
;	        for first value
;	allcat: if set, then run like_associate for all sources in the source
;	        catalogue
;	single: if set, then only need to consider larger of two error ellipses
;	to_file: if set, then the output is written to a file
;	stband: if set, gives STScI band other than V to be used
; OUTPUTS:
;	objects1: list of first-ranked object for each source
;	objects2: list of second-ranked object for each source
;
; OPTIONAL OUTPUTS:
;	likes1: likelihood ratios for first-ranked object for each source
;	likes2: likelihood ratios for second-ranked object for each source
;	distances1: distances from sources to first-ranked objects
;	distances2: distances from sources to second-ranked objects
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
;	runs like_associate for a list of entries in a given catalogue or
;	for the whole catalogue
;
; EXAMPLE:
;	IDL> multi_like_associate,iras_psc,abell,list,nearest,second,/larger
;
;	associate Abell clusters with IRAS PSC sources in list, solely 
;	on the basis of distance, recording the entry nos. in the Abell 
;	catalogue of the first and second nearest cluster to each source, 
;	no matter how far away it is
;
; MODIFICATION HISTORY:
; 	Written by:	Bob Mann, July 1996.
;			8.ii.97 stband keyword introduced
;     "radius not being passed if sourcelist set" bug fixed Seb Oliver 22/7/97
;     also quick fix for sourcelist=-1  Seb Oliver 22/7/97
;
;-
	
;	on error, return to calling procedure

	on_error,2

;	check that required parameters have been set correctly

	if n_params () lt 4 then message,'Calling sequence: '+$
	'multi_like_associate,objects,sources,objects1,objects2,shist,bhist'+$
	'[,likes1,likes2,distances1,distances2,radius,sourcelist,'+$
	'/larger,/allcat,/single,/to_file]'

;	open the source catalogue

	dbopen, sources

;	use zdbcats to get names for various fields in the source catalogue,
;	plus units if they are not defined as keywords
        if not( keyword_set(equinox) and  keyword_set(smajaxname) and $
                keyword_set(sminaxname) and keyword_set(spaname) ) then begin
	zdbcats,sources,equinox,smajaxname,sminaxname,scalem,spaname,scalea
        endif else begin
          if not  keyword_set(scalem) then scalem=1.
          if not  keyword_set(scalea) then scalea=1.
        endelse

;	two options here, depending on whether all the catalogue is to be
;	associated, or just a subsample from it

;       sourcelist being set to -1 is same as allcat being set (Seb 22/7/97)
        if keyword_set(sourcelist) then if sourcelist(0) eq -1 then allcat=1 

	if (keyword_set(allcat) eq 0) then begin

;		associate only a subset of the source catalogue

;		set size of output lists

		objects1=lonarr(n_elements(sourcelist))

		objects2=lonarr(n_elements(sourcelist))

		associates=lonarr(n_elements(sourcelist))

		likes=fltarr(n_elements(sourcelist))

		likes1=fltarr(n_elements(sourcelist))

		likes2=fltarr(n_elements(sourcelist))

		distances=fltarr(n_elements(sourcelist))

		distances1=fltarr(n_elements(sourcelist))

		distances2=fltarr(n_elements(sourcelist))

;		extract information for sources in sourcelist

		dbext,sourcelist,'ra,dec',ralist,declist

		dbext,sourcelist,smajaxname,smajaxlist

		dbext,sourcelist,sminaxname,sminaxlist

		dbext,sourcelist,spaname,spalist

;		multiply axes of error ellipses by scalem

		smajaxlist=smajaxlist*scalem

		sminaxlist=sminaxlist*scalem

		for i=0L,n_elements(sourcelist)-1L do begin		

			dbopen,sources

jump1:			like_associate,equinox,ralist(i),declist(i),objects,$
			associates,distances,likes,bhist,shist,smajaxlist(i),$
			sminaxlist(i),spalist(i),radius=radius,raname=raname,$
			decname=decname,fluxname=fluxname,majaxname=majaxname,$
			minaxname=minaxname,paname=paname,single=single,$
			stband=stband

;			if /larger is set, and no objects have been found,
;			then try again with a preliminary search radius 
;			twice as big as before

			if (keyword_set(larger) and (associates(0) eq -1)) $
				then begin

				radius=radius*2.0

				goto, jump1

			endif	

;			record the desired information about the associates

			objects1(i)=associates(0)

			if (n_elements(associates) gt 1) then $ 
					objects2(i)=associates(1)	

			distances1(i)=distances(0)

			if (n_elements(distances) gt 1) then $
					distances2(i)=distances(1)	

			likes1(i)=likes(0)

			if (n_elements(likes) gt 1) then likes2(i)=likes(1)
			
		endfor

	endif else begin

;		associate all sources in source catalogue

;		set size of output lists

		nsources=db_info('entries')

		nsources=nsources(0)

		associates=lonarr(nsources)

		objects1=lonarr(nsources)

		objects2=lonarr(nsources)

		likes2=fltarr(nsources)

		likes1=fltarr(nsources)

		likes2=fltarr(nsources)

		distances=fltarr(nsources)

		distances1=fltarr(nsources)

		distances2=fltarr(nsources)

;		dbext,-1,'ra,dec',ralist,declist

;		dbext,-1,smajaxname,smajaxlist

;		dbext,-1,sminaxname,sminaxlist

;		dbext,-1,spaname

		for i=1L,nsources do begin

			dbopen,sources

			dbext,i,'ra,dec,',ra,dec

			dbext,i,smajaxname,smajax

			dbext,i,sminaxname,sminax

;			multiply axes of error ellipses by scalem

			smajax=smajax(0)*scalem

			sminax=sminax(0)*scalem

			dbext,i,spaname,spa

jump2:			like_associate,equinox,ra,dec,objects,associates,$
			distances,likes,bhist,shist,smajax,sminax,spa,$
			radius=radius,raname=raname,decname=decname,$
    			fluxname=fluxname,majaxname=majaxname,$
			minaxname=minaxname,paname=paname,single=single,$
			stband=stband

;			if /larger is set, and no objects have been found,
;			then try again with a preliminary search radius 
;			twice as big as before

			if (keyword_set(larger) and (associates(0) eq -1)) $
			then begin

				radius=radius*2.0

				goto, jump2

			endif	

;			record the desired information about the associates

			objects1(i-1L)=associates(0)

			if (n_elements(associates) gt 1) then begin $ 
					

				objects2(i-1L)=associates(1)	

				distances1(i-1L)=distances(0)

				if (n_elements(distances) gt 1) then $
					
					distances2(i-1L)=distances(1)	

				likes1(i-1L)=likes(0)

				if (likes(0) gt -1) then likes2(i-1L)=likes(1)
	
			endif

		endfor

	endelse

;	write out all the data to a file, if required

	if keyword_set(to_file) then begin
			
		openw,u,'/disk/2/rmann/hdf/multi_like_associate.dat',$
		/get_lun

		printf,u,keyword_set(allcat)

		if (keyword_set(allcat) eq 0) then begin

			printf,u,n_elements(sourcelist)

			printf,u,sourcelist

		endif else begin


			printf,u,nsources

		endelse

		printf,u,objects1

		printf,u,objects2

		printf,u,likes1

		printf,u,likes2

		printf,u,distances1

		printf,u,distances2

		close,u

		free_lun,u

	endif
END








