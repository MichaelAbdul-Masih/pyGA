PRO TVLASER,image, xstart=xstart,ystart=ystart,xdim=xdim,ydim=ydim, $
	noeight=noeight,portrait=portrait,header=header,reverse=reverse, $
        scale = scale, color = color, encap = encap, Noprint = npr, $
        Nodelete = nde, Title = title, filename = filename
;+
; NAME:
;	TVLASER
; PURPOSE:
;	Prints screen onto a Postscript laser printer.    TVLASER
;       uses the logical name or environment variable PSLASER to check which 
;       postscript printer to use.    Optionally, prints selected information
;       from a FITS header.   Users will want to tailor this program to
;       their particular computer environment.
;
; CALLING SEQUENCE:     
;       TVLASER, [ HEADER = ,YSTART = ,XSTART = ,XDIM = ,YDIM = , /NOEIGHT
;		   TITLE = ,	/PORTRAIT, /REVERSE, /SCALE, /COLOR, /ENCAP ]
;
; INPUTS: 
;	None 
;
; OPTIONAL KEYWORD INPUT PARAMETERS: 
;	HEADER         =  a FITS header string array to create labels
;	TITLE          = Scalar string to be printed above image as the 
;		 	 title of the picture.  
;	XSTART, YSTART = lower left corner (default of (0,0))
;	XDIM,YDIM    =   Number of pixels read (default taken from !d.x_size 
;	                 and !d.y_size)
;	NOEIGHT        = if set then only four bits sent to printer (saves 
;	                 space and time)
;	PORTRAIT       = if present and non-zero, the printer results will
;	                 be in portrait format; otherwise, they will be in
;	                 landscape format. If labels are requested, image 
;                        will be in portrait mode, regardless
;       REVERSE        = if present and non-zero, color table will be fliped,
;                        so black and white are reversed.
;       SCALE          = if present and non-zero, image will be bytscaled after
;                        being read off the screen. 
;	COLOR         = If present and non-zero, the IDL.PS file is written
;			using color postscript.
;       ENCAP	      = If present and non-zero, the IDL.PS file is written 
;                       in encapsulated postscript for import into LaTeX 
;                       documents.   Automatically sets /NOPRINT.
;	NOPRINT       = If present and non-zero, the output is sent into the
;			 file idl.ps, which is NOT deleted and is NOT sent to
;			 the printer.
;	NODELETE      = If present and non-zero, the idl.ps file is kept AND
;			 is also sent to the printer
;	FILENAME      = scalar string giving name of output postscript file.
;			Default is idl.ps.   Automatically sets /NODELETE
;
; OUTPUTS:
;	IMAGE = array  containing byte values of display
;
; SIDE EFFECTS: 
;	By default an idl.ps file created and deleted in directory.  User 
;	must have write privileges in the current directory.
;
; PROCEDURE:  
;	Read display and then redisplay into a postscript file.   If a FITS
;	header is supplied then the values of the OBJECT, TELESCOPE, EXPTIME,
;	and IMAGE keywords are printed, and astrometry info is displayed
;	if present
;
; EXAMPLE:
;	Send the image displayed in the current window to the color postscript 
;	printer.   Display annotation from the FITS header hdr, and save the 
;	postscript file with the name 'm33.ps' and give a title 'M33 Nucleus'
;
;	IDL> tvlaser,h=hdr,/COLOR, FILE = 'm33.ps', TIT = 'M33 Nucleus'
;
; MODIFICATION HISTORY:     
;	Major rewrite from UIT version   W. Landsman   Dec 94
;       Adapted to local (ICSTM) print commands Seb Oliver Apr 1996
;-
 On_error,2

 if !D.NAME EQ 'PS' then set_plot,'X'          ;Return to X terminal
 sv_device = !D.NAME

; Default values are to start at 0,0 and read entire window.
 
 if not keyword_set( XSTART ) then xstart = 0
 if not keyword_set( YSTART ) then ystart = 0
 if not keyword_set( XDIM ) then xdim = !d.x_size 
 if not keyword_set( YDIM ) then ydim = !d.y_size 
 if keyword_set( NOEIGHT ) then nbits = 4 else nbits = 8
 if keyword_set( PORTRAIT ) then landscape = 0 else landscape = 1

; Read image from the screen.  Rescale it to a full range of 0--255 (to 
; allow for translation table changes.

 message,'Reading image from window ' + strtrim(!D.WINDOW,2), /INF
 image = tvrd( xstart, ystart, xdim, ydim )

; If using B/W PostScript, use NTSC color -> B/W formula, J Brinkmann

 if not keyword_set( COLOR ) then begin
        tvlct, rr, gg, bb, /get
	image = 0.299 * rr(image) + 0.587 * gg(image) + 0.114 * bb(image)
 endif

 if keyword_set(scale) then image = bytscl(image)

; Get dimensions of current window

 if keyword_set(header) then landscape = 0

; Redirect output to Postscript printer file.  Send the results to the printer. 

 if landscape then begin
   inx = 10.0
   iny = float(ydim)/float(xdim)*float(inx)
   if (iny gt 7.5) then begin
     iny = 7.5
     inx = (float(xdim)/float(ydim))*float(iny)
   end

 endif else begin

   iny = 10.0
   inx = float(xdim)/float(ydim)*float(iny)
   if (inx gt 7.5) then begin
     inx = 7.5
     iny = (float(ydim)/float(xdim))*float(inx)
   end

 endelse

 message, 'Writing image to postscript file',/INF
 if not keyword_set(filename) then fname = 'idl.ps' else begin
	fdecomp,filename,disk,dir,name,ext
	if ext EQ '' then ext = 'ps'
	fname = disk + dir + name + '.' + ext
        nde = 1
 endelse 

 set_plot, 'ps' 

 if keyword_set( ENCAP ) then begin

    npr = 1
    if landscape then device, bits=nbits, /landscape,/inches,/helvetica, $
            xsize= inx,  ysize = iny, filename = fname, /encapsulated  $
    else  device,bits=nbits, /portrait,/inches,/helvetica, $
    xsize = inx, ysize = iny, filename= fname,yoffset=10-iny, /encapsulated

 endif else begin

    if landscape then device, bits=nbits, /landscape,/inches,/helvetica, $
            xsize= inx,  ysize = iny, filename = fname  $
    else  device,bits=nbits, /portrait,/inches,/helvetica, $
          xsize = inx, ysize = iny, filename= fname,yoffset=10-iny

 endelse

 if keyword_set( REVERSE ) then image = 255b-image
 if keyword_set( COLOR )  then device, /color

 tv, image				

; Reset output direction to X-windows.

 if keyword_set( HEADER ) then begin
   lands = 0
   h = header		
   space = float(xdim/ydim) > 1.

   object = sxpar( h ,'OBJECT', COUNT = N)
   if N EQ 0 then object = ' N/A ' else object = ' ' + object
   xyouts, 0.1, -0.10*space, 'OBJECT: ' + object,/NORMAL

  scop = sxpar( h, 'TELESCOP', COUNT = N)
  if N EQ 0 then scop = ' N/A'
  xyouts, 0.10, -0.05*space, 'TELESCOPE: ', /NORMAL
  xyouts, 0.30, -0.05*space, ' '+ scop, /NORMAL

  image = sxpar( h, 'IMAGE', COUNT = N)
  if N EQ 0 then image = ' N/A'  else IMAGE = ' ' + image
  xyouts,.10,-.075*space,'IMAGE: ', /NORMAL
  xyouts,.30,-.075*space, image, /NORMAL

  exptime = sxpar (h, 'EXPTIME', COUNT = N)
  if N EQ 0 then exptime = ' N/A' else $
                    exptime = string( exptime, f = '(F6.1)') + ' seconds'
  xyouts,.10,-.125*space,'EXPOSURE TIME:',SIZE=1.0,/NORMAL		
  xyouts,.30,-.125*space,EXPTIME, SIZE=1.0,/NORMAL

  siz = ' ' + strtrim(xdim,2) +' by ' + strtrim(ydim,2) + ' pixels'
  xyouts, 0.55,-0.05*space, 'SIZE: ',/NORMAL
  xyouts, 0.70,-0.05*space, siz, /NORMAL

;User name + Date/Time printout is made
    xyouts, 0.55,-.200*space, "DATE: ", /NORMAL
    xyouts, 0.7, -.200*space,' '+strmid(!STIME,0,17),/NORMAL
    xyouts, 0.55,-.225*space,'USER: ',/NORMAL
    xyouts, 0.7, -.225*space,' '+GetEnv('USER'),/NORMAL

  extast, h, astr, noastrom         ;Does image have astrometry?

  if noastrom LT 0 then begin
	equi = ' N/A'
        acen =  ' N/A'
        dcen = ' N/A'
	CDELT = [0.0,0.0]
	CDELT1 = '  N/A'
	CDELT2 = '  N/A'
	ROTATE = '  N/A'
   endif else begin

;EQUINOX
  equinox = sxpar( h, 'EQUINOX', COUNT = N)
  if N EQ 0 then equinox = ' N/A' else $
                    equinox = string( equinox, f = "(F7.1)")	
  xyouts, 0.10,-.175*space, 'EQUINOX: ', /NORMAL
  xyouts, 0.30,-.175*space, equinox, /NORMAL

  xcen = (xdim-xstart-1)/2.
  ycen = (ydim-ystart-1)/2.

  xyad,h,xcen,ycen,ra_cen,dec_cen
  str = adstring(ra_cen,dec_cen,1)
  acen = strmid( str, 0, 12)
  dcen = strmid( str, 14, 11)

  getrot, h, rotate, cdelt  
  cdelt = abs(cdelt)*60.*60.  
  rotate =' '+STRING(rotate, f='(f7.1)')+' deg (E of N)'
  cdelt1 = string( cdelt(0), f='(f7.2)')  + ' "/pixel'
  cdelt2 = string( cdelt(1), f='(f7.2)')   + ' "/pixel'

 endelse

  xyouts,0.10,-0.200*space,"CENTER RA: ",/NORMAL
  xyouts,0.30,-0.200*space, acen,/NORMAL
  xyouts,0.10,-0.225*space,"CENTER DEC: ",/NORMAL
  xyouts,0.30,-0.225*space, dcen,/NORMAL
  xyouts,0.55,-0.075*space,'ROTATION: ',/NORMAL
  xyouts,0.70, -0.075*space, rotate,/NORMAL
  xyouts,0.55,-0.100*space,'SCALE: ',/NORMAL
  xyouts,0.70,-0.100*space, cdelt1,/NORMAL

  if (cdelt(0) - cdelt(1) GT 0.01*cdelt(0)) then $
	xyouts, 0.6, -0.100*space, ',' + cdelt2, /NORMAL


 endif
 
 if keyword_set( TITLE ) then $
         xyouts,.50, +1.01, title, SIZE=2.0, /NORMAL, ALIGN=0.5

 Device,/close

 if not keyword_set( npr ) then begin

; changed couldn't get environment variables etc to work properly
; so just used the defauly lppost command

 if !VERSION.OS NE "vms" then begin            ;Unix
;     psprtr = getenv('LPDEST')
;    if psprtr EQ "" then begin
;	 read,'Enter name of postscript printer: ',psprtr
;         setenv,'LPDEST = ' + psprtr
;    endif
    spawn, 'lpost ' + fname
    if not keyword_set( NDE) then spawn, 'rm ' + fname

 endif else begin

	test = trnlog("PSLASER",pslaser)            ;VMS
	if not test then begin
		pslaser = ''
		read,'Enter name of postscript printer: ',pslaser
		setlog,"pslaser",pslaser 
        endif
 	if keyword_set( NDE) then spawn, 'print/queue=pslaser ' + fname $
                             else spawn, 'print/queue=pslaser/delete ' + fname

 endelse
 endif

;			Reset output direction to X-windows.

 set_plot, sv_device

 return
 end
