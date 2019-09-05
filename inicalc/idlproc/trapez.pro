function trapez, x, f

n=n_elements(x)


trapez=0.d0
for i=1L,n-1 do begin
  trapez=trapez+0.5d0*(f(i-1)+f(i))*(x(i-1)-x(i))
endfor

return, trapez
end  
