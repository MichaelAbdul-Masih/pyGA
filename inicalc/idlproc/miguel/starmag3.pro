; Miguel A. Urbaneja, 2005
;
; Auxiliary procedure to be use by wr_fstwndspec.pro
;
; PURPOSE: synthetic magnitudes and colors to be stored with the flux file
;
;@star_color_mod3.pro
;@get_flt.pro

@/media/disk/tazon_07-01-09/norberto/MIGUEL_MODELOS/IDL_PRO/PHOTOMETRY/star_color_mod3.pro
@/media/disk/tazon_07-01-09/norberto/MIGUEL_MODELOS/IDL_PRO/PHOTOMETRY/get_flt.pro


pro starmag3,x,y,rstar,mag,filt=filt

rsun = 6.95d10 	; constant

magnitudes=['U','B','V','R','I','J','H','BC']	; filters to be used
ic1 = [0,1,2,2,3,4,5]
ic2 = [1,2,3,4,4,5,6]
inc = n_elements(ic1)

imag = n_elements(magnitudes)
sintemag = dblarr(imag)

mag={ v : dblarr(1), bc : dblarr(1), lumi : dblarr(1), mbol : dblarr(1), $
      color : dblarr(n_elements(ic1)), colorname : strarr(n_elements(ic1)) }
nwave = x
nflux = y

;## conversion factor to Jy/kpc^2

nflux = nflux*(1.d23/3.0739401d21^2)*(rstar*rsun)^2*4.d0*!pi

;## remove the extra flux close to the series' limit (Balmer and Paschen)

if not keyword_set(filt) then begin
    ix=where((nwave le 3646.26) or (nwave ge 3682.73 and nwave le 8204.6) or $
        (nwave ge 8388.61))
    xx = nwave(ix)  &  yy = nflux(ix)    
    nwave = xx  &  nflux = yy    
endif    	
lumino=0.d0
for j=0,imag-1 do begin
    sintemag(j)=star_color_mod3(magnitudes(j),nwave,nflux,lumino,/fnue,/jyzero, $
				/dist,/quiet,/ccdphot,lumi,l0,leff) 
endfor
iv = where(magnitudes eq 'V')
ibc = where(magnitudes eq 'BC')
if (iv(0) eq -1 or ibc(0) eq -1) then begin
    print,'ERROR, V mag or BC not found'
    return
endif   

;## star_color_mod computes magnitudes at a distance of 1Kpc 

mag.v    = sintemag(iv(0)) - 10.d0
mag.bc   = sintemag(ibc(0))
mag.mbol = sintemag(2)+sintemag(n_elements(magnitudes)-1)-10.d0
mag.lumi = lumino

for j=0,n_elements(ic1)-1 do begin
        cad = magnitudes(ic1(j))+'-'+magnitudes(ic2(j))
	mag.colorname(j) = strtrim(cad,2)
	mag.color(j) = sintemag(ic1(j))-sintemag(ic2(j))
endfor		

return
end	
	
