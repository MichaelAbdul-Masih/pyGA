pro absmag,mv,bv,bv0,d,RATIO=ratio
;  +
;   absolute magnitude
;
;   Calculates absolute magnitude of a star with known
;   distance, visual magnitude, observed B-V color index,
;   intrinsic B-V color index and ratio between
;   interstellar and stellar redening
;
;  Imput parameters
;
;  d - distance to the star  in pc
; mv - visual magnitude
; bv - observed color index
; bv0 - intrinsic color index
;
; Key words
; RATIO= ratio between interstellar and stellar redening
;
; History:
;       Created by N.Markova, NAO "Rozhen"
;   13 June 2001    

             exc=bv - bv0
   if NOT Keyword_Set(RATIO)    then ratio= 3.1  ;2.6
    av=ratio*exc
     abslmag=mv-av -5*(alog10(d)-1)
      print,abslmag
   end                                          
