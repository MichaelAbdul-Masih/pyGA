;***********************************************************************

PRO PHTITK2UTK, ITK, UTK, ICS

;+
; NAME:
;	PHTITK2UTK
;
; PURPOSE:
;	Converts PHOT ITK to UTK
;
; CATEGORY:
;	
;
; CALLING SEQUENCE:
;	PHTITK2UTK, ITK, UTK, ICS
;
; INPUTS:
;	ITK: PHOT Instrument Time Key
;	ICS: Name of PHT ICS structure 
;
; OPTIONAL INPUTS:

;	
; KEYWORD PARAMETERS:
;	
;
; OUTPUTS:
;       UTK: Universal Time Key
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
;
;
; MODIFICATION HISTORY:
; 	Written by:	Seb Oliver 5th August 1996
;	July, 1994	Any additional mods get described here.
;-

on_error,2

if n_params() lt 3 then message,'Calling Sequence: PHTITK2UTK, ITK, UTK, ICS'

ditk=0.00006103515630d0
dutk=1.0d0/24.0d0

itk2utk=ditk/dutk

;--------------------------------------------------------------------------

utk0=ics.handle.ukst(0)
itk0=ics.handle.ikst(0)

utk=long(float(itk-itk0)*itk2utk)+utk0


END

