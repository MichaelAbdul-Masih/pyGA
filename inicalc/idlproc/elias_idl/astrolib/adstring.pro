Function adstring,ra_dec,dec,precision,zero=zero
;+
; NAME:
;	ADSTRING
; PURPOSE:
;	Return RA and Dec as character string in sexigesimal format.
;	RA and Dec may be entered as either a 2 element vector or as
;	2 scalars.  One can also specify the precision of the declination
;	in digits after the decimal point.
;
; CALLING SEQUENCE
;	result = ADSTRING( ra_dec )	      
;		or
;	result = ADSTRING( ra,dec,[ precision ] )
;
; INPUTS:
;	RA_DEC - 2 element vector giving the right ascension and declination
;		in decimal degrees.
;                     or
;	RA     - Right ascension in decimal degrees, numeric scalar
;	DEC    - Declination in decimal degrees, numeric scalar
;
; OPTIONAL INPUT:
;	PRECISION  - The number of digits after the decimal of DEClination.
;		The RA is automatically 1 digit more.  This parameter may 
;		either be the third parameter after RA,DEC or the second 
;		parameter after [RA,DEC].  It is not available for just DEC.  
;		If no PRECISION parameter is passed, a precision of 1 for 
;		both RA and DEC is returned to maintain compatibility with 
;		past ADSTRING functions.  A precision of 0 will result in 
;		the below format, always claimed, but but never delivered by 
;		ADSTRING.
;  KEYWORD:  
;       zero:  if set then seconds are zero padded.
;
; OUTPUT:
;	RESULT - Character string containing HR,MIN,SEC,DEC,MIN,SEC formatted
;		as ( 2I3,F5.(p+1),2I3,F4.p ) where p is the PRECISION 
;		parameter.    If only a single scalar is supplied it is 
;		converted to a sexigesimal string (2I3,F5.1).
;
; EXAMPLE:
;	(1) Display CRVAL coordinates in a FITS header, H
;
;	IDL> crval = sxpar(h,'CRVAL*')  ;Extract 2 element CRVAL vector (degs)
;	IDL> print, adstring(crval)     ;Print CRVAL vector sexigesimal format
;
;	(2)  print,adstring(30.42,-1.23,1)  ==>  ' 02 01 40.80  -01 13 48.0'
;	     print,adstring(30.42,+0.23)    ==>  ' 02 01 40.8  +00 13 48.0'
;	     print,adstring(+0.23)          ==>  '+00 13 48.0'
;
; PROCEDURES CALLED:
;	RADEC, SIXTY()
;
; REVISION HISTORY:
;	Written   W. Landsman                      June 1988
;	Addition of variable precision and DEC seconds precision fix. 
;	ver.  Aug. 1990 [E. Deutsch]
;	Output formatting spiffed up       October 1991 [W. Landsman]
;	Remove ZPARCHECK call, accept 1 element vector  April 1992 [W. Landsman]
;	Call ROUND() instead of NINT()    February 1996  [W. Landsman]
; Appropriate roundoff added for RA (Seb Oliver 4th March 1997)
; ZERO keyword added to allow zero padding on seconds (Seb Oliver 4th March 1997)
; 
;-
  On_error,2

  Npar = N_params()

  case N_elements(ra_dec) of 

     1: if ( Npar EQ 1 ) then dec = ra_dec else ra = ra_dec
     2: begin
        ra = ra_dec(0) mod 360.
        if (Npar GT 1) then begin 
             precision = dec & Npar=3 & endif
        dec = ra_dec(1)
        end
   else: message, $
          'First parameter must be a scalar or 2 element ([RA,DEC]) vector'

   endcase

  if ( Npar GE 2 ) then $
	if N_elements(dec) NE 1 then message, $
      'ERROR - Declination (second parameter) must be a numeric scalar'

  if N_elements(ra) EQ 1 then begin

     if ( dec(0) LT -90. ) or ( dec(0) GT 90. ) then message, $
       'WARNING - Illegal declination value of '+strtrim(dec,2)+' degrees',/INF
     radec, ra(0), dec(0), ihr, imin, xsec, ideg, imn, xsc
     if (Npar LT 3) then precision = 0
     precision = precision > 0 < 4         ;No more than 4 decimal places



;Appropriate roundoff added for RA (Seb Oliver 4th March 1997)

      if fix( xsec + 0.5/10^(precision+1) ) GE 60  then begin   
          xsec = 0.
          imin = imin + 1
      endif

; bug fix seb oliver 1st Oct 1997
     roundxsec=round(xsec*10.^(precision+1))/10.^(precision+1)

     if keyword_set(zero) and roundxsec lt 10. then begin
        secfmt = '(" 0",F' + string( 2+precision+1,'(I1)' ) + '.' + $
                        string(   precision+1,'(I1)' ) + ')'
     endif else begin
        secfmt = '(F' + string( 4+precision+1,'(I1)' ) + '.' + $
                        string(   precision+1,'(I1)' ) + ')'
     endelse

     if (Npar LT 3) then precision = 1
     result = string([ihr,imin],'(2I3.2)') + string(xsec,secfmt) + '  ' 




  endif else begin

     x = sixty(dec)
     precision = 1
     ideg = fix(x(0)) & imn = fix(x(1)) & xsc = x(2)
     result = ''

  endelse
   if ( precision EQ 0 ) then begin 
           secfmt = '(I3.2)' 
           xsc = round(xsc)
           if xsc EQ 60 then begin
                  xsc = 0
                  imn = imn + 1
           endif

   endif else begin


     if fix( xsc + 0.5/10^precision ) GE 60  then begin   ;Appropriate roundoff
         xsc = 0.
         imn = imn + 1
     endif


; bug fix seb oliver 1st Oct 1997
     roundxsc=round(xsc*10.^(precision))/10.^(precision)

     if keyword_set(zero) and roundxsc lt 10. then begin
         secfmt = '(" 0",F' + string( 2+precision,'(I1)') + '.' + $
                         string(   precision,'(I1)') + ')'
     endif else begin 
         secfmt = '(F' + string( 4+precision,'(I1)') + '.' + $
                         string(   precision,'(I1)') + ')'
     endelse

  endelse

   if imn EQ 60 then begin
       if dec GT 0 then ideg = ideg + 1 else ideg = ideg -1
       imn = 0
   endif

   if ( ideg EQ 0 ) and ( min([imn,xsc]) LT 0. ) then begin
         deg = '-00' 
         imn = abs(imn) & xsc = abs(xsc)
   endif else deg =  string(ideg,'(I3.2)')

   if dec(0) GT 0 then strput, deg, '+', 0

   return, result + deg + string(imn,'(I3.2)') + string(xsc,secfmt)

   end
