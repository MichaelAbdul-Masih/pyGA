;***********************************************************************

PRO xassociate,db1,db2,radius,list1to2,list2to1,both1,both2,only1,only2,$
    list_in1=list_in1, list_in2=list_in2

;+
; NAME:
;	xassociate
;
; PURPOSE:
;	
;       cross associates two databases 
;
; CATEGORY:
;	analysis
;
; CALLING SEQUENCE:
;	xassociate,db1,db2,radius,list1to2,list2to1,both1,both2,only1,only2
;
; INPUTS:
;	db1: name of database 1 
;       db2: name of database 2
;    radius: search radius in degrees
;   list1to2: entries numbers of unique matches from 1--->2 
;   list2to1: entries numbers of unique matches from 2--->1 
;   both1: the entrynumbers in database one of the links
;   both2: ditto 
;   only1: the entry number in database one of those unique to one
;   only2: ditto
;
; OPTIONAL INPUTS:
;	
;	
; KEYWORD PARAMETERS:
;	
;
; OUTPUTS:
;	
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
;     Cannot yet handle list of entries within databases
;     Does not know what to do with databases with containg multple sources
;          with similar positions
;
; PROCEDURE:
;	
;
; EXAMPLE:
;	
;
; MODIFICATION HISTORY:
; 	Written by:	Seb Oliver 13th Dec 1996
;       BOTH2 vector changed so that it is ordered so that matched
;       entries have the same index in BOTH1 and BOTH2 7th September 1999
;	July, 1996	
;-

if n_params() lt 4 then message,'CALLING SEQUENCE:'+$
	'xassociate,db1,db2,radius,list1to2 [,list2to1,both1,both2,only1,only2]'

if not keyword_set(list_in1) then list_in1=-1
if not keyword_set(list_in2) then list_in2=-1
;stop

; reading in ras and decs
unavail=1
dbopen,db1,unavail=unavail
if unavail then message,db1+' Unavailable'
dbext,list_in1,'entry,ra,dec',en1,ra1,dec1


unavail=1
dbopen,db2,unavail=unavail
if unavail then message,db1+' Unavailable'
dbext,list_in2,'entry,ra,dec',en2,ra2,dec2

;----------------------------------------------------------------------
; checking for uniqueness in source lists 
;----------------------------------------------------------------------

dbopen,db1
associate,ra1,dec1,radius,lista,sepa,listb,sepb,list_in=list_in1
dup = where ( listb ne -1,count)
if count ne 0 then print,'WARNING there are possible duplications in ',db1,'Severity ',count

dbopen,db2
associate,ra2,dec2,radius,lista,sepa,listb,sepb,list_in=list_in2
dup = where ( listb ne -1,count)
if count ne 0 then print,'WARNING there are possible duplications in ',db2,'Severity ',count

;----------------------------------------------------------------------
; Associating 1 with 2 and 2 with 1
;----------------------------------------------------------------------
dbopen,db1
associate,ra2,dec2,radius,list1a,sep1a,list1b,sep1b,list_in=list_in1
dbopen,db2
associate,ra1,dec1,radius,list2a,sep2a,list2b,sep2b,list_in=list_in2

;
; list1a are the entry numbers in database1 for all elements in database 2
;

;----------------------------------------------------------------------
; checking for duplicate links in first list
;----------------------------------------------------------------------
for i=0L, n_elements(list1a)-1L do begin
  checklist= list1a(i)
  if checklist gt 0 then begin
    dups = where(list1a eq checklist,count)

    if count gt 1 then begin
; found a duplicate

; minimum separation to establish best link
      minsep=min(sep1a(dups),bestlink)
      bestlink=dups(bestlink)
      bestlinklist=list1a(bestlink)
; breaking all links
      list1a(dups)=-2
; replacing best link
      list1a(bestlink)=checklist
    endif
  endif
endfor



;----------------------------------------------------------------------
; checking for duplicate links in second list
;----------------------------------------------------------------------
for i=0L, n_elements(list2a)-1L do begin
  checklist= list2a(i)
  if checklist gt 0 then begin
    dups = where(list2a eq checklist,count)

    if count gt 1 then begin
; found a duplicate

; minimum separation to establish best link
      minsep=min(sep2a(dups),bestlink)
      bestlink=dups(bestlink)
      bestlinklist=list2a(bestlink)
; breaking all links
      list2a(dups)=-2
; replacing best link
      list2a(bestlink)=checklist
    endif
  endif
endfor



;-------------------------------------------------------------------
; going through broken links in 1 and replacing with crosslink
;----------------------------------------------------------------------

brokenlinks=where(list1a eq -2,count)
if count gt 0 then begin
;  print, count, 'Broken links in list1'
  for i=0L, count-1 do begin
    entryno=en2(brokenlinks(i))
    crosslink=where(list2a eq entryno,crosscount)
    case crosscount of
         0: list1a(brokenlinks(i))=-1
         1: list1a(brokenlinks(i))=en1(crosslink)
         else: print,'Shouldnot be here'
    endcase
  endfor
endif

;----------------------------------------------------------------------
; going through broken links in 2 and replacing with crosslink
;----------------------------------------------------------------------

brokenlinks=where(list2a eq -2,count)
if count gt 0 then begin
  print, count, 'Broken links in list1'
  for i=0L, count-1 do begin
    entryno=en1(brokenlinks(i))
    crosslink=where(list1a eq entryno,crosscount)
    case crosscount of
         0: list2a(brokenlinks(i))=-1
         1: list2a(brokenlinks(i))=en2(crosslink)
         else: print,'Shouldnot be here'
    endcase
  endfor
endif


;----------------------------------------------------------------------
; Quick check all links match
;----------------------------------------------------------------------

good1=where(list1a gt 0,count1)
good2=where(list2a gt 0,count2)

if count1 ne count2 then print,'Screw up somewhere'
if min([count1,count2]) gt 0 then begin 
  both1=list1a(good1)
  both2=list2a(good2)
  both1=both1(sort(both1))
  both2=both2(sort(both2))

; sort this bit out *****
  both1check=en2(good1)
  both2check=en1(good2)
  both1check=both1check(sort(both1check))
  both2check=both2check(sort(both2check))

  ok1=where(both1check ne both2, okcount)
  if okcount gt 0 then print, 'Screw up here 1'
  ok2=where(both2check ne both1, okcount)
  if okcount gt 0 then print, 'Screw up here 2'

endif else begin
  both1=-1
  both2=-1
endelse


bad1=where(list1a le 0,count1)

if count1 eq 0 then only2=-1 else only2=en2(bad1)

bad2=where(list2a le 0,count2)
if count2 eq 0 then only1=-1 else only1=en1(bad2)





list1to2=list2a
list2to1=list1a

IF both2[0] GT 0 THEN both2 = list1to2[where(list1to2 gt 0)]


END

