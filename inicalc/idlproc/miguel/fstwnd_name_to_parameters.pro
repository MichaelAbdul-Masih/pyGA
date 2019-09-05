pro fstwnd_name_to_parameters,name,dat

  il = strpos(name,'_f')
  if il ne -1 then sc = strtrim(strmid(name,0,il),2) $
      else sc = strtrim(name,2)

  il = strlen(sc)
  it = strpos(sc,'t')
  if it eq -1 then begin
    print,'model name without parameter-codification!!!'
    dat = 0
    return
  endif

  dat = {teff : 0.d0, logg : 0.d0, he : 0.d0, vf : 0.d0, $
       logq : 0.d0, beta : 0.d0, met : 0.d0, vt : 0.d0, $
       vinfty : 0.d0 }

  ig  = strpos(sc,'g')
  iaux = strpos(sc,'x')
  if n_elements(iaux ge 1) then ix = iaux(0) $
      else ix = iaux
  ihe = strpos(sc,'He')
  ixf = strpos(sc,'xf')
  iq  = strpos(sc,'q')
  ib  = strpos(sc,'b')
  ivinf = strpos(sc,'vt')
  
  iz  = ix + 3

  dat.teff = double(strmid(sc,it+1,ig-it-1))
  dat.logg = double(strmid(sc,ig+1,ix-ig-1))
  if dat.logg ge 10.d0 then dat.logg = dat.logg*1.d-2
  dat.vt   = double(strmid(sc,ix+1,iz-ix-1))

  if ivinf ne -1 then begin      
      dat.vinfty = double(strmid(sc,ivinf+2,il-ivinf-2))
      il = ivinf-1
  endif
  
  d = strmid(sc,iz,ihe-iz)
  verdad = strcmp(d,'__nomet__',/FOLD_CASE)
  if not verdad then begin
    a = strmid(d,0,1)
    dat.met = double(strmid(d,1,strlen(d)-1))
    if a eq 'm' then dat.met = dat.met*(-0.1d0)  
  endif else dat.met=0.d0
  
  dat.he = double(strmid(sc,ihe+2,ixf-ihe-2))  
  dat.vf = double(strmid(sc,ixf+2,iq-ixf-2))
  dat.logq = double(strmid(sc,iq+1,ib-iq-1))*(-1.d0)
  dat.beta = double(strmid(sc,ib+1,il-ib-1))

;  print,dat.teff,dat.logg,dat.vt
;  print,dat.met
;  print,dat.he,dat.logq,dat.vf,dat.beta
;  print,dat.vinfty
  
  return

end  
