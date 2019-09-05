;;
;; para convertir un real a cadena, redondeando en la cifra indicada
;;
pro roundcad,numero,cadena,ipo=ipo

if not keyword_set(pos) then ipo=3

i=n_elements(numero)
cadena = strarr(i)
auxc=' '
auxc2=' '

for j=0,i-1 do begin
     x=numero(j)
     auxc = strtrim(string(x),2)
     ito = strpos(auxc,'.')
     inp = ito+1+ipo	     
     auxn = fix(strmid(auxc,inp,1))	     
     if ( auxn ge 5 ) then begin
     	 if x gt 0. then auxnew = auxc + 1.d-3 $
     	  else if x lt 0. then auxnew = auxc - 1.d-3
     endif else auxnew = auxc
     	  			  
;;;  	print,auxc,' ',auxnew
     auxc2 = strtrim(string(auxnew),2)
     ito2 = strpos(auxc2,'.')
     auxc2 = strmid(auxc2,0,ito2+1+ipo)
     cadena(j) = strtrim(auxc2,2)			     
endfor		

return
end
