
function logfmt,axis,index,value

;+
; NAME:
;	logfmt
;
; PURPOSE:
;	For use in labeling logarithmic axis
;
; CATEGORY:
;	graphics
;
; CALLING SEQUENCE:
;	called by axis procedure
;
; INPUTS:
;	axis:  See axis procedure
;       index: 
;       value:
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
;
; PROCEDURE:
;	
;
; EXAMPLE:
;	IDL> seed=10  
;	IDL> x=10^(randomu(seed,100)*10)+100
;	IDL> y=x^2*randomu(seed,100) 
;	IDL> plot,x,y,/xlog,/ylog,psym=1,xtickformat='logfmt'
;
;
; MODIFICATION HISTORY:
; 	Written by:	Seb Oliver 3rd Feb 1998
;	July, 1996	
;-


   print,'test'
   print,value
   stop
   expon=round(alog10(value))
   order=max(fix(alog10(expon))+1)
   expon=string(expon,format='(i'+string(order)+')')
   fmt='!17 10!u'+expon+'!d!17'


   return,fmt



END


