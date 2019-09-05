;***********************************************************************

PRO LONGPLOT,X,Y,slide_window=slide_window,xsize=zsize,ysize=ysize,_extra=e

;+
; NAME:
;	LONGPLOT
;
; PURPOSE:
;	Plots a long graph in a window with a scroll bar
;
; CATEGORY:
;	Graphics/ Widgets.
;
; CALLING SEQUENCE:
;       longplot, [x,y] , [slide_window=slide_window,xsize=zsize,ysize=ysize],
;                 [plot keywords]
;
; INPUTS:
;
; OPTIONAL INPUTS:
;	Y: Y-axis data array
;	X: X-axis data array
;	
; KEYWORD PARAMETERS:
;
;       slide_window: window number for slide window created
;       xsize:        xsize of graphics area
;       ysize:        ysize of graphics area
;	extra:        Takes all PLOT keywords
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
;	Produces widgit
;
; RESTRICTIONS:
;	Untested
;
; PROCEDURE:
;	
;
; EXAMPLE:
;	
; Set up variables
;
; IDL> x=findgen(10000)
; IDL> y=x^2
;
; Basic plot
;
; IDL> longplot,x,y
;
; Having set this up later plots go straight into this window
;
; IDL> !p.multi=[0,1,2]
; IDL> plot,x,y,yticklen=0.001,xticklen=0.01,xticks=29
; IDL> plot,x,X^3,yticklen=0.001,xticklen=0.01,xticks=29
;
; MODIFICATION HISTORY:
; 	Written by:	Seb Oliver (ICSTM) June 1996
;-

on_error,2

; Calculate xsize which is available size of graphics window

if not keyword_set(xsize) then begin
  case n_params() of
    2:  xsize=max([512L,n_elements(y)*2])
    1:  xsize=n_elements(x)*2
    0:  xsize=10000
   endcase
endif

; ysize of avaiable graphics window default 512
if not keyword_set(ysize) then ysize=512

; actual size of visible window (available window accessed by croll bars)
xvisible=750 & yvisible=500


; setting up widgity stuff
base=widget_base(/row)

scroll = widget_draw(base, retain=2, xsize=xsize, ysize=ysize, $
	/frame, /scroll, x_scroll_size=xvisible, y_scroll_size=yvisible)
WIDGET_CONTROL, /REAL, base
WIDGET_CONTROL, get_value=SLIDE_WINDOW, scroll
wset,slide_window

case n_params() of
2:  plot,x,y,_extra=e
1:  plot,x,_extra=e
endcase


END








