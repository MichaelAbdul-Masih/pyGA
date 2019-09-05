FUNCTION READ_DISTORTION_FILE, filename, error_message
;+
; NAME: 
; READ_DISTORTION_FILE
; PURPOSE:
;  it extracts from t the ascii distortion file the detector ID,
;   the coefficients of the polynome:
;     xdif  = polynomes(0,0) + polynomes(1,0)*x +
;             polynomes(2,0)*y + polynomes(3,0)* x*x +
;             polynomes(4,0)*x*y + polynomes(5,0)* y*y +
;             polynomes(6,0)*x*x*x + polynomes(7,0)*x*x*y +
;             polynomes(8,0)*x*y*y + polynomes(9,0)*y*y*y 
;
;     ydif  = polynomes(0,1) + polynomes(1,1)*x +
;             polynomes(2,1)*y + polynomes(3,1)* x*x +
;             polynomes(4,1)*x*y + polynomes(5,1)* y*y +
;             polynomes(6,1)*x*x*x + polynomes(7,1)*x*x*y +
;             polynomes(8,1)*x*y*y + polynomes(9,1)*y*y*y 
;
;
; CATEGORY: 
; II-6 internal
; CALLING SEQUENCE:
;    Polynomes = READ_DISTORTION_FILE( filename)
; INPUTS:
;   filename -- string : name of the ascii distortion file
; OUTPUTS:
;   Polynomes     -- fltarr(10,2) : the coefficients of the polynome
;
; SEE ALSO
;    ISO_astr.cc, compute_distortion
; MODIFICATION HISTORY:
;     13-AUG-1997 R Gastaud  created read_distorsion_file
;     06-Oct-1997 D Landriu renamed it
;
;
; COPYRIGHT:
;
; This routine belongs to the ISOCAM Interactive Analysis Package CIA.
;
; CIA is a joint development by the ESA Astrophysics Division and the ISOCAM
; Consortium led by the ISOCAM PI, C. Cesarsky, Direction des Sciences de la
; Matiere, C.E.A., France.
; Contributing ISOCAM Consortium institutes are Service d'Astrophysique (SAp,
; Saclay, France) and Institut d'Astrophysique Spatiale (IAS, Orsay, France)
;
; When publishing ISOCAM Data reduced with this analysis package, please
; mention this in the acknowledgement the following way:
;   "The ISOCAM data presented in this paper was analysed using "CIA",
;    a joint development by the ESA Astrophysics Division and the ISOCAM
;    Consortium led by the ISOCAM PI, C. Cesarsky, Direction des Sciences de la
;    Matiere, C.E.A., France. "
;
; Its use and distribution to authorised sites are free, as long as this header
; text is not removed, or changed.
;
; No warranties for installation/ support/ maintenance are given.
;-

polynomes=0
 IF N_PARAMS() LT 1 THEN BEGIN
   PRINT, 'CALLING SEQUENCE: ', $
    ' polynomes = READ_DISTORTION_FILE(filename, [error_message])'
   GOTO, CLOSING
 ENDIF

error_message = 'success'
pos = strpos(strupcase(filename), '.DIS')
if( pos le 0 ) then my_filename = filename+'.dis' else my_filename = filename

taille = size(findfile(my_filename))
if ( taille(0) eq 0) then begin
   error_message = 'Error file '+ my_filename +' not found '
   print, error_message 
   GOTO, CLOSING
ENDIF

openr, unit, my_filename, /get_lun
n = 0L
tmp = ''

readf, unit, tmp
while (strmid(tmp, 0, 1) eq '#') do begin
   readf,unit,tmp; Read next line.
endwhile
print, tmp

polynomes = fltarr(10,2)
readf, unit, polynomes

;print, ' I ll close unit', unit 
close, unit
free_lun, unit

CLOSING:
return, polynomes
END

