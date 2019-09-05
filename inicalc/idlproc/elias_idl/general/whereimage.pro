;***********************************************************************

PRO whereimage,imsize,list,isub,jsub

;+
; NAME:
;	whereimage
;
; PURPOSE:
;	takes a list of array indicies and converts it into a
;       list of multi-D subscripts indicies for >=2-D arrays
;
; CATEGORY:
;	analysis
;
; CALLING SEQUENCE: whereimage,imsize,list,isub,jsub
;	
;
; INPUTS:
;	imsize: ouput of SIZE function applied to image
;       list:   list of 1-D subscripts
;     
;
; OPTIONAL INPUTS:
;	
;	
; KEYWORD PARAMETERS:
;	
;
; OUTPUTS:
;	isub:  i subscript
;       jsub:  j subscript
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
;       only works on 2-D arrays so far
;
; PROCEDURE:
;	
;
; EXAMPLE:
;       IDL> modelpsf = findgen(100,100)/1000.
;	IDL> br=where(modelpsf gt 0.001)
;	IDL> sz=size(modelpsf)
;	IDL> whereimage,sz,br,i,j
;	IDL> print,i,j

;
; MODIFICATION HISTORY:
; 	Written by:	Seb Oliver 26th July 1996
;	July, 1996	
;-

on_error,2

if n_params() lt 4 then message,'CALLING SEQUENCE: whereimage,imsize,list,isub,jsub'

if imsize(0) gt 2 then message,'Sorry only works on 2-D arrays'
if n_elements(list) le 0 then message,'List undfined'

subscripts=lonarr(imsize(0),n_elements(list))

subscripts(imsize(0)-1,*)=list/imsize(1)
subscripts(0,*)=list-imsize(1)*subscripts(imsize(0)-1,*)

isub=subscripts(0,*)
jsub=subscripts(1,*)

IF n_elements(isub) EQ 0 THEN isub = isub[0]
IF n_elements(jsub) EQ 0 THEN jsub = jsub[0]


END

