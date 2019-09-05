;***********************************************************************

PRO ASSOCIATE, RA, DEC, RADIUS, LIST, SEP, LIST2, SEP2, _EXTRA= _EXTRA,$
    list_in=list_in,silent=silent, sort_field=sort_field,rev=rev,$
    element=element, quick=quick

;+
; NAME:
;	Associate
;
; PURPOSE:
;	Associates a source list with an already opened  database 
;
; CATEGORY:
;	DBASE routines
;
; CALLING SEQUENCE:
;	ASSOCIATE, RA, DEC, RADIUS, LIST, SEP [, LIST2, SEP2]
; 
;
; INPUTS:
;	RA:     Array of RAs in decimal hours
;       DEC:    Array of DECs in decimal degrees
;       RADIUS: Maximum association radius/degrees
;
; OPTIONAL INPUTS:
;	
;	
; KEYWORD PARAMETERS:
;
;       list_in: list of entries to be checked
;       Takes any keywords to DBCIRCLE routine
;       silent: no printed output
;       sort_field: field(s) on which to sort matches, largest is best match
;                   by default the best match is nearest
;                   if you want the smallest to be the best match 
;                   (e.g. magnitude) set the rev keyword
;       rev:  if set then best match is has the smallest sort_field value
;
;
; OUTPUTS:
;
;       LIST:   Array of length RA with index of closest match (-1) = no match
;       SEP:    Array Seperation of closest match /degrees
;       LIST2:  Array of length RA with index of 2nd closest match 
;       SEP2:   Array Seperation of next closest match/degrees
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
;	Pretty rudimentary.
;
; PROCEDURE:
;	
;
; EXAMPLE:
;
;  ra and dec are arrays containing source coordinates in decimal degrees
;
;  open the "FIRST" database
;
;       IDL> dbopen,'first'
;       
;  associating sources with nearest "first" within a 0.48' search radius
;
;       IDL> associate,ra/15.,dec,0.008,list,sep
;
; MODIFICATION HISTORY:
; 	Written by:	Seb Oliver July 1996
;       16th April 1997 list_in keyword added
;       10th July 1997 silent
;       8th APril 1999 rev keyword changed to reverse, since I 
;	July, 1994	Any additional mods get described here.
;        
;-

; On error return to calling procedure
  on_error,2

; Paramter checking
  if n_params() lt 5 then  message, $
     'Calling Sequence: ASSOCIATE, RA(HR), DEC(DEG), RADIUS(DEG), LIST, SEP [, LIST2, SEP2,list_in=list_in]'

  if db_info('open') ne 1 then message,'Association Data base must be open'
  if not keyword_set(list_in) then list_in=-1


   list=lonarr(n_elements(ra)) & list2=list
   sep=float(list) & sep2=sep
   ncount=0L
   ucount=0L
   dcount=0L


   IF NOT keyword_set(quick) THEN BEGIN
      
; going through each source

  for i=0L,n_elements(ra)-1L do begin

; listing all matches

     list_temp=dbcircle(ra(i),dec(i),radius*60.,dis,list_in,_extra=_extra,/silent)

; if match found
     if(list_temp(0) ne -1) then begin

      ucount=ucount+1
; if more than one match found
       if(n_elements(list_temp) gt 1)then begin
          dcount=dcount+1

; sorting the matches by radius or search field
; not using dbsort as it has trouble with vectors

          if not keyword_set(sort_field) then begin
             list_sort=sort(dis) 
          endif else begin
             dbext,list_temp,sort_field,sf
             if keyword_set(element) then sf=sf(element-1,*)
             list_sort=sort(sf)
             if not keyword_set(rev) then list_sort=reverse(list_sort)

          endelse

             list_temp=list_temp(list_sort)
             dis=dis(list_sort)


          list(i)=list_temp(0)
          list2(i)=list_temp(1)
          sep(i)=dis(0)/60.
          sep2(i)=dis(1)/60.

        endif else begin
          list(i)=list_temp(0)
          list2(i)=-1
	  sep(i)=dis(0)/60.
	  sep2(i)=-1
        endelse
     endif else begin
          ncount=ncount+1
          list(i)=-1
          list2(i)=-1
	  sep(i)=-1
	  sep2(i)=-1
     endelse
   endfor

; speedy option using memory rather than loops
   ENDIF ELSE BEGIN 
      
      dbext, list_in, 'entry,ra,dec', entry, ra2, dec2
      n1 = n_elements(ra )
      n2 = n_elements(ra2)

      ra1_mat = ra#replicate(1, n2)
      ra2_mat = replicate(1, n1)#(ra2)

      dec1_mat = dec #replicate(1, n2)
      dec2_mat = replicate(1, n1)#dec2
      icgcirc, 1, ra1_mat, dec1_mat, ra2_mat, dec2_mat, dis
      maxdis = max(dis)
      FOR i=0, n_elements(ra)-1 DO BEGIN
         ord = sort(dis[i, *])
         list[i] = entry[ord[0]]
         sep[i] = dis[i, ord[0]]
         list2[i] = entry[ord[1]]
         sep2[i] = dis[i, ord[1]]
      ENDFOR
      sep = sep/3600.
      sep2 = sep2/3600.
      bad = where(sep gt radius, ncount)
      bad2 = where(sep2 gT radius, n2)

      IF ncount GT 0 THEN list[bad] = -1
      IF n2 GT 0 THEN list2[bad2] = -1

      good = where(sep le radius, ucount)
      good2 = where(sep2 le radius, dcount)

   ENDELSE


   if not keyword_set(silent) then begin
     print,'No matches:',ncount
     print,'At least one match:',ucount
     print,'At least two matches:',dcount
   endif

END

