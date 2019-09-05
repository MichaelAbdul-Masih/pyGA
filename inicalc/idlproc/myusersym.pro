pro myusersym,no,fill=fill

if no eq 11 then begin
; circle
 a = findgen(17) * (!pi*2/16.)
 if keyword_set(fill) then begin
  usersym, 2.*cos(a), 2.*sin(a),/fill
 endif else begin
  usersym, 2.*cos(a), 2.*sin(a)
 endelse
 return
endif

if no eq 14 then begin
; triangle
  x=[-2,0,2,0,-2]
  y=[ 0,2,0,-2,0]
 if keyword_set(fill) then begin
  usersym,x,y,/fill
 endif else begin
  usersym,x,y
 endelse
 return
endif

if no eq 15 then begin
; triangle
  x=[-2,0,2,-2]
  y=[-2,2,-2,-2]
 if keyword_set(fill) then begin
  usersym,x,y,/fill
 endif else begin
  usersym,x,y
 endelse
 return
endif

print,no,' no symbol defined'
return
end
