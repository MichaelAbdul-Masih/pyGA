function inthnue, lam, hnue
; lambda in A
clight = 2.997925e18
n=n_elements(lam)

nu=clight/lam

inthnue=0.
for i=1L,n-1 do begin
   inthnue=inthnue+0.5*(hnue(i-1)+hnue(i))*(nu(i-1)-nu(i))
endfor

return, abs(inthnue)
end  
