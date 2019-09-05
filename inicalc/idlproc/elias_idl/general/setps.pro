;==============================

pro setps,pub=pub,col=col,por=por,land=land,eps=eps

portrait=0
landscape=1
if keyword_set(por) then portrait=1
if keyword_set(land) then landscape=1


encaps=0
if keyword_set(eps) then encaps=1
set_plot, 'PS' 

device, filename='idl.ps', bit=8,landscape=landscape,portrait=portrait,encaps=encaps
if keyword_set(col) then device,filename='idl.ps', bit=8,$
   landscape=landscape,portrait=portrait,/color,encaps=encaps

if keyword_set(pub) then begin
; setting character sizes and line thickness

!p.thick=3*pub
!x.thick=3*pub
!y.thick=3*pub
!p.charsize=2.
!x.charsize=1.5
!y.charsize=1.5
endif

end
