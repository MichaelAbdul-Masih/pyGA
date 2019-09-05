;***********************************************************************
       function seyfert_sed, lam


;+
; NAME:
;	seyfert_sed
;
; PURPOSE:
;	Returns a seyfert FIR SED
;
; CATEGORY:
;	analysis
;
; CALLING SEQUENCE:
;	sed=seyfert_sed(lam)
;
; INPUTS:
;	lam:  wavelength in Micron
;
; OPTIONAL INPUTS:
;	
;	
; KEYWORD PARAMETERS:
;	
;
; OUTPUTS:
;	seyfert_sed:   nuFnu of seyfert SED in arbitrary units
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
;	
;
; MODIFICATION HISTORY:
; 	Written by:	Seb Oliver 20 th August 1996
;	July, 1996	
;-

       lgnu0=[$
       11.2757  ,$  
       11.3757  ,$  
       11.4757  ,$  
       11.5757  ,$  
       11.6757  ,$  
       11.7757  ,$  
       11.8757  ,$  
       11.9757  ,$  
       12.0757  ,$  
       12.1757  ,$  
       12.2757  ,$  
       12.3757  ,$  
       12.4757  ,$  
       12.5757  ,$  
       12.6757  ,$  
       12.7757  ,$  
       12.8757  ,$  
       12.9257  ,$  
       12.9757  ,$  
       13.0257  ,$  
       13.0757  ,$  
       13.1257  ,$  
       13.1757  ,$  
       13.2257  ,$  
       13.2757  ,$  
       13.3257  ,$  
       13.3757  ,$  
       13.4257  ,$  
       13.4757  ,$  
       13.5257  ,$  
       13.5757  ,$  
       13.6757  ,$  
       13.7757  ,$  
       13.8757  ,$  
       13.9757  ,$  
       14.0757  ,$  
       14.1757  ,$  
       14.2757  ,$  
       14.3757  ,$  
       14.4757  ]
       lgnufnu0=[$ 
         -6.958,$
         -6.563,$
         -6.170,$
         -5.779,$
         -5.389,$
         -5.002,$
         -4.619,$
         -4.240,$
         -3.867,$
         -3.501,$
         -3.148,$
         -2.797,$
         -2.465,$
         -2.152,$
         -1.862,$
         -1.601,$
         -1.201,$
         -0.986,$
         -0.818,$
         -0.704,$
         -0.657,$
         -0.636,$
         -0.620,$
         -0.626,$
         -0.643,$
         -0.648,$
         -0.601,$
         -0.591,$
         -0.605,$
         -0.594,$
         -0.610,$
         -0.692,$
         -0.707,$
         -0.728,$
         -0.768,$
         -0.844,$
         -0.989,$
         -1.244,$
         -1.666,$
         -2.305]



       lgnu=14.48-alog10(lam)

       seyfert_sed=interpol(lgnufnu0,lgnu0,lgnu)

; change these to sensible numbers

       seyfert_sed=10.^seyfert_sed




       return,seyfert_sed

       end
