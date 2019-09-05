;***********************************************************************

PRO IC_MEDIAN, TIME, DATA, FLAG, TSCALE, MED, NUSE, skip=skip, mean=mean

;+
; NAME:
;	IC_MEDIAN
;
; PURPOSE:
;	Median filter a time-sequence allowing for unequal time steps
;
; CATEGORY:
;	Statistics
;
; CALLING SEQUENCE:
;	IC_MEDIAN, TIME, DATA, FLAG, TSCALE, MED, NUSE
;
; INPUTS:
;	time:  time-axis of data
;       data:  data values
;       flag:  for bad values (0. is good) 
;       tscale: time-scale for median filter
;
; OPTIONAL INPUTS:
;	Parm2:	Describe optional inputs here.
;	
; KEYWORD PARAMETERS:
;	skip: skip is a time scale in the centre to be skipped over
;
; OUTPUTS:
;	MED:  Median filter at each data point
;       NUSE:    Number of points used to calculate median at each point
;
; OPTIONAL OUTPUTS:
;	Describe optional outputs here.
;
; COMMON BLOCKS:
;	BLOCK1:	Describe any common blocks here.
;
; SIDE EFFECTS:
;	Describe "side effects" here.
;
; RESTRICTIONS:
;	Describe any "restrictions" here.
;
; PROCEDURE:
;	You can describe the foobar superfloatation method being used here.
;
; EXAMPLE:
;	Please provide a simple example here.
;
; MODIFICATION HISTORY:
; 	Written by:	Your name here, Date.
;	July, 1994	Any additional mods get described here.
;-

on_error, 2


if not keyword_set(skip) then skip =0

ndata=n_elements(data)
tby2=tscale/2.
skipby2=skip/2.

; setting up output vectors
  nuse=intarr(ndata)
  med=fltarr(ndata)
for i=0L,ndata-1L do begin

  ts=time(i)-tby2
  te=time(i)+tby2

  sks=time(i)-skipby2
  ske=time(i)+skipby2

; selecting only good data within specified time range and 
; Not excluding the data point itself

  use=where( (time ge ts and time le sks) or $
             (time ge ske and time le te) $
            and flag eq 0. ,count)
  nuse(i)=count

  if count gt 0 then begin
     if keyword_set(mean)then begin
        med(i)=total(data(use))/nuse(i)
     endif else begin
        med(i)=median(data(use))
     endelse
  endif
endfor
    

END

