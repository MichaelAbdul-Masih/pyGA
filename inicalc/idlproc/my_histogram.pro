pro my_histogram, bins, x, h, xx, mi=mi, ma=ma, plot=plot, norm=norm 

; x input array
; h histogram, xx midpoint locations  

; sorting in bins with xx_i le x lt xx_i+1
  
nx=n_elements(x)

; to avoid that mi=0 is interpreted as non-set
if n_elements(mi) eq 0 then mi=min(x) 

if n_elements(ma) eq 1 then begin
  h=histogram(x,bins=bins,min=mi,max=ma,location=xx)
endif else begin
  ma=max(x)
  h=histogram(x,bins=bins,min=mi,location=xx)
;  nbin=fix((ma-mi)/float(bins))+1 ; without +1
;  ma=mi+nbin*bins
endelse  

if keyword_set(norm) then h=h/float(nx*bins)

xx=xx+0.5*bins

if not keyword_set(plot) then return

nel=n_elements(xx)-1

plot,xx,h,psym=10,xrange=[mi,xx(nel)+0.5*bins]
oplot,[mi,xx(0)],[h(0),h(0)]
oplot,[xx(nel),xx(nel)+0.5*bins],[h(nel),h(nel)]

return
end
