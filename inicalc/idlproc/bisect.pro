function bisect,x1,x2,xacc=xacc,itmax=itmax

; needs user-supplied function func(x)
;
; determines zero of given function func(x) via bi-section
; zero needs to be bracketed between x1 and x2
;

  xfalse=-9.9999d9 
  if not keyword_set(itmax) then itmax=100
  if not keyword_set(xacc) then xacc=1.d-5
  fmid=func(x2)
  f=func(x1)
  if fmid*f ge 0. then begin
    print,x1,x2,fmid,f
    print,'root must be bracketed in bisect'
    return,xfalse
  endif
  if f lt 0. then begin
    rtbis=x1
    dx=x2-x1
  endif else begin
    rtbis=x2
    dx=x1-x2
  endelse
  for i=1,itmax do begin
    dx=dx*0.5
    xmid=rtbis+dx
    fmid=func(xmid)
    if fmid le 0. then rtbis=xmid
    if abs(dx) lt xacc or fmid eq 0. then return, rtbis
  endfor

print,' too many bisections in bisect'
return,xfalse
end
