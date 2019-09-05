pro icstringad,coords_input,ra,dec,iwarn=iwarn
;+ 
; NAME:
;	ICSTRINGAD
; PURPOSE:
;	Converts a string of sexigesimal coordinates into decimal degrees.
;
; CALLING SEQUENCE:
;	ICSTRINGAD, COORDS, RA, DEC [,IWARN=IWARN]
; INPUT:
;	COORDS    A string of coordinates (e.g. '17 00 45.2 25 4 32.4')
;		It should have six numbers delimited by spaces or colons
; OUTPUT:
;	RA        Right Ascension, decimal degrees, scalar
;	DEC       Declination, decimal degrees, scalar
;	IWARN     (Optional) Set to 1 if any mins or secs > 60
; PROCEDURES CALLED:
;	Getopt   Ten
; HISTORY:
;	09-AUG-90 Version 1 written by Kerry McQuade
;	20-AUG-90 Put code to account for '-0' back in after it was
;		removed by someone.  E. Deutsch
;	17-JUL-95 Added support for coordinates separated by colons, e.g.
;		17:00:45.2 25:4:32.4, which IRAF uses.  E. Deutsch
;	21 Aug 98: Renamed icstringad, added check for >60mins or >60 secs
;		and iwarn keyword. Also no longer modifies input
;		and supports vector arguments. 
;		Steve Serjeant ICSTM
;-
  On_error,2

  arg = N_params()
  if ( arg LT 1 ) then begin
    print,'Call: IDL> ICSTRINGAD,coord_string,ra,dec,iwarn=iwarn'
    print,"e.g.: IDL> ICSTRINGAD,'17 00 45.2  25 4 32.4',ra,dec"
    print," or : IDL> ICSTRINGAD,'17:00:45.2  25:4:32.4',ra,dec"
    return
  endif

; Check for vector argument - Added by S Serjeant 21 Aug 1998
  n = n_elements(coords_input)
  if(n gt 1) then begin
	if(arg gt 1) then begin
		ra=dblarr(n)
		dec=dblarr(n)
		iwarn=intarr(n)
		dum3=0
		for i=0,n-1 do begin
			icstringad,coords_input(i),dum1,dum2,iwarn=dum3
			ra(i)=dum1
			dec(i)=dum2
			iwarn(i)=dum3
		endfor
		return
	endif else begin
		for i=0,n-1 do icstringad,coords_input(i)
		return
	endelse
  endif


; Copy coords_input to coords to avoid modifying input
;  - Added by S Serjeant 21 Aug 1998
  coords = coords_input

; Initialise iwarn variable - Added by S Serjeant 21 Aug 1998
  iwarn=0


;                        Remove any gaps between '-' or '+' and numeral  

  I = strpos(coords,'+ ')
  if ( I GE 0 ) then strput,coords,'  +', I-1
  J = strpos(coords,'- ')
  if ( J GE 0 ) then strput,coords,'  -',J-1


; Replace colons with spaces - Added by Deutsch 7/17/95
  i=0
  while (i ne -1) do begin
    i=strpos(coords,':')
    if (i ne -1) then strput,coords,' ',i
    endwhile


  radec = getopt(coords,'F')
  if ( N_elements(radec) LT 6 ) then message, $
        'Coordinate format should be HR MIN SEC DEG DMIN DSEC'

; Warn if mins or secs > 60  - Added by S Serjeant 21 Aug 1998
  if(radec(1) gt 60 or radec(2) gt 60 or $
     radec(4) gt 60 or radec(5) gt 60) then begin
	message, $
	'Warning: Greater than 60 mins or secs: '+coords(0),/info
	iwarn=1
  endif

  ra = ten(radec(0:2)*15.)      ;Convert to decimal degrees
  dec = ten(radec(3:5))

; Some formats write this: '12 34 15.33 -0 12 45.3'  Make this convert properly
  if ((strpos(coords,'-') NE -1) and (dec gt 0)) then dec = -dec

  if (arg LT 2) then print,'Decimal coords:   ',ra,dec

  return
  end
