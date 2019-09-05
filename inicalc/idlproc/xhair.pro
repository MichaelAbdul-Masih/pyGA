pro xhair,x,y,buttonorkey,color=color,$                      
            data=data,device=device,normal=normal
;
;+XHAIR
;
;The XHAIR procedure is used to read the positon of
;the interactive crosshair from the current graphics device.
;XHAIR first waits untill all mouse buttons are
;released and returns when a button or key is pressed.
;
;Calling Sequence
;
;        XHAIR, X, Y [, Buttonorkey]
;
;Arguments
;
;    X
;         A named variable to receive the crosshair
;         column position.
;
;    Y
;         A named variable to receive the crosshair
;         row position.
;
;    Buttonorkey
;         The mouse button or key pressed (as character)
;         'ML','MM','MR' correspond to left, middle and right button 
;
;Keywords
;
;   DATA
;         Set this keyword to return X and Y in data
;         coordinates.
;
;   DEVICE
;         Set this keyword to return X and Y in device
;         coordinates.
;
;   NORMAL
;         Set this keyword to return X and Y in normalized
;         coordinates.
;                       
;   COLOR         
;        Set the color of the crosshair
;
;   Example
;Activate the crosshair cursor, select a point in the graphics
;window, return the position of the cursor in normal
;coordinates and the key pressed. Enter:
;
;XHAIR, X, Y, Key,/NORMAL
;
;Move the crosshair over the graphics window and press a key.
;The position of the cursor in normal coordinates is stored in
;the variables X and Y, the key pressed as character in Key. To label
;the location with the key pressed, enter:
;
;XYOUTS, X, Y, Key,/NORMAL,CHARSIZE=2
;
;CAUTION:
; Mouse buttons are non-printable charcters !!!
; If you interrupt this procedure, you must reset the graphic mode:
; DEVICE,SET_GRAPHICS_FUNCTION=3
;
; MODIFICATION HISTORY:
;        Written by A. Fiedler, June, 1993.
;        crosshair can now set to x,y if defiened, A. Fiedler, Jan. 1994
; Modified by J. Puls, Feb. 1997 to allow also for logarithmic axes
; Modified by J. Puls, Nov. 2006 with respect to buttonorkey
;-
 on_error,2
;(old version) 
; if not keyword_set(color) then color=!d.n_colors 
 if not keyword_set(color) then color=!d.n_colors-1 
 
 if not keyword_set(data) then data=0
 if not keyword_set(device) then device=0
 if not keyword_set(normal) then normal=0
 if data then data=1 else data=0
 if device then device=1 else device=0
 if normal then normal=1 else normal=0
 if (data+device+normal) gt 1 then begin
   print,'% CROSSHAIR: Incorrect number of keywords.'
   return
 endif
 if (device+normal) eq 0 then data=1
 if (data eq 1) and ((!x.s(0) eq !x.s(1)) or (!y.s(0) eq !y.s(1))) then begin
   print,'% CROSSHAIR: Data coordinate system not established.'
   return
 endif
 device,get_gra=oldgra,set_gra=6,cursor_image=intarr(16)
 if data then begin
  xr=!x.crange & dxr=.025*(xr(1)-xr(0)) & exr=[xr(0)-dxr,xr(1)+dxr]
  yr=!y.crange & dyr=.025*(yr(1)-yr(0)) & eyr=[yr(0)-dyr,yr(1)+dyr]
  if !x.type eq 1 then begin
    exr=10.^exr
  endif
  if !y.type eq 1 then begin
    eyr=10.^eyr
  endif
 endif
 if device then begin
  xr=[0,!d.x_vsize-1] & dxr=fix((xr(1)-xr(0))/40) & exr=xr
  yr=[0,!d.y_vsize-1] & dyr=fix((yr(1)-yr(0))/40) & eyr=yr
 endif
 if normal then begin
  xr=[0.,1.] & dxr=(xr(1)-xr(0))/40 & exr=xr
  yr=[0.,1.] & dyr=(yr(1)-yr(0))/40 & eyr=yr
 endif
 if (size(x))(1) eq 0 then begin
  x=total(xr)/2
 endif
 if (size(y))(1) eq 0 then begin
  y=total(yr)/2
 endif
 if (x lt xr(0)) or (x gt xr(1)) then begin
  x=total(xr)/2
 endif
 if (y lt yr(0)) or (y gt yr(1)) then begin
  y=total(yr)/2
 endif
 tvcrs,x,y,data=data,device=device,normal=normal
 repeat begin
  cursor,x,y,0,data=data,device=device,normal=normal
 endrep until !err eq 0
 pflg = -1
 repeat begin
  if (pflg) then begin
   plots,col=color,[x,x],eyr,data=data,device=device,normal=normal
   plots,col=color,exr,[y,y],data=data,device=device,normal=normal
 endif
  wait,.02
  cursor,cx,cy,0,data=data,device=device,normal=normal
  button = byte(!err)
  mouse=button
  if button eq 0 then begin
    key=get_kbrd(0)
    h = byte(key) 
    button = h(0)
  endif
  if (button ne 0) or (x ne cx) or (y ne cy) then begin
   plots,col=color,[x,x],eyr,data=data,device=device,normal=normal
   plots,col=color,exr,[y,y],data=data,device=device,normal=normal
   x=cx & y=cy
   pflg = -1
  endif else pflg = 0
 endrep until button ne 0
 device,set_gra=oldgra,/cursor_crosshair
 case mouse of
 0: buttonorkey=key
 1: buttonorkey='ML'
 2: buttonorkey='MM'
 4: buttonorkey='MR'
 else: buttonorkey=''
 endcase
end
