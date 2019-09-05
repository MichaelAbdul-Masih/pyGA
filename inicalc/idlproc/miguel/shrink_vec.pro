PRO shrink_vec,x   ;elimination of equal entries
  x = x(sort(x))     
  k = 0L
  FOR i = 1L,N_ELEMENTS(x)-1L DO BEGIN  ; doppelte Indices rausschmeissen
    IF x(k) NE x(i) THEN BEGIN
      k = k + 1L 
      x(k) = x(i)
    ENDIF
  ENDFOR
  x=x(0:k)  
RETURN              
END
