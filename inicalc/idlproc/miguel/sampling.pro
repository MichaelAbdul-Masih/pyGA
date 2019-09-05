function sampling,dt,x,y
;
;  
;

i=indgen(1)
j=indgen(1)
k=indgen(1)
l=indgen(1)
n=indgen(1)
si=indgen(1)
i=n_elements(x)
si=1
dto=x(1)-x(0)
if(si) then begin
	intervalo=x(i-1)-x(0)
	k=fix(intervalo/dt)+1
	z=fltarr(k,2)
	z(0,0)=x(0)
	z(0,1)=y(0)
	for j=1,k-1 do begin
		z(j,0)=x(0)+dt*j
		for l=0,i-2 do begin
			if (x(l) le z(j,0)) then n=l
		endfor	
		z(j,1)=(y(n+1)-y(n))/(x(n+1)-x(n))*(z(j,0)-x(n))+y(n)
	endfor	
endif			
return,z
end
