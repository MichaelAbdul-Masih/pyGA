pro rd_fstwnd_indat,dir=dir,file=file,tab

  if not keyword_set(dir) then dir = './'
  if not keyword_set(file) then file = 'INDAT.DAT'
  fich = dir + file
  sc = ' '
  
  get_lun,u1  
  openr,u1,fich
  readf,u1,sc
  sc = strmid(sc,2)
  il = strpos(sc,"'")  ; ' 
  catname = strmid(sc,0,il)

  readf,u1,sc
  readf,u1,sc
  readf,u1,teff,logg,r
  readf,u1,sc
  readf,u1,mdot,dummy,vterm,beta,dummy
  readf,u1,nhe,dummy
  readf,u1,sc
  readf,u1,sc
  sc = strtrim(sc,2)
  il = strpos(sc,' ')
  sc2 = strtrim(strmid(sc,0,strlen(sc)-il-1),2)
  il = strpos(sc2,' ')
  vturb = double(strtrim(strmid(sc2,0,il-1),2))
  sc = strtrim(strmid(sc,il,strlen(sc)-il),2)
  il = strpos(sc,' ')
  met = double(strtrim(strmid(sc,0,il-1),2))*1.d0

  logq = alog10(mdot/(r*vterm)^1.5d0)
  sc = strmid(string(logq),2)
	    
  aux_beta = strtrim(string(beta),2)
  i_b = strpos(aux_beta,'.')
  i_l = strlen(aux_beta)
  sbeta = strtrim(strmid(aux_beta,0,i_b+3),2)
  aux_vterm = strtrim(string(vterm),2)
  i_b = strpos(aux_vterm,'.')
  svterm = strtrim(strmid(aux_vterm,0,i_b),2)
;
; block for clumping
;	    
  readf,u1,sc
  readf,u1,sc
  close,u1
  sc = strtrim(sc,1)
  clf_tab = dblarr(3) + 0.d0
  sclf_tab = strarr(3) + ''
  for j=0,1 do begin
      ib = strpos(sc,' ')
      il = strlen(sc)
      clf_tab(j) = double(strtrim(strmid(sc,0,ib-1),2))
      sc = strtrim(strmid(sc,ib+1,il-ib-1),1)
  endfor
  clf_tab(2) = double(strtrim(sc,2))
  sclf_tab = strmid(strtrim(string(clf_tab),2),0,4)	    
  clf = clf_tab(0)
  v0clf = clf_tab(1)
  v1clf = clf_tab(2)
;
;
  mdot = alog10(mdot) 
;
;
  tab = { teff : 0.d0, logg : 0.d0, r : 0.d0, logmdot : 0.d0, vterm : 0.d0, $
          beta : 0.d0, nhe : 0.d0, vturb : 0.d0, met : 0.d0, $
          clf : 0.d0, v0clf : 0.d0, v1clf : 0.d0, $
          id  : strarr(12), catname : strtrim(catname,2), $ 
	      steff : ' ', slog : ' ', sr : ' ', smdot : ' ', svterm : ' ', $
	      sbeta : ' ', snhe : ' ', svturb : ' ', smet : ' ', sclf : ' ', $
	      sv0clf : ' ', sv1clf : ' ' }

  tab.teff    = teff  & tab.id(0)  = 'Teff'
  tab.logg    = logg  & tab.id(1)  = 'logg'
  tab.r       = r     & tab.id(2)  = 'R'
  tab.logmdot = mdot  & tab.id(3)  = 'logMdot'
  tab.vterm   = vterm & tab.id(4)  = 'vterm'
  tab.beta    = beta  & tab.id(5)  = 'beta' 
  tab.nhe     = nhe   & tab.id(6)  = 'N(He)/N(H)' 
  tab.vturb   = vturb & tab.id(7)  = 'vturb' 
  tab.met     = met   & tab.id(8)  = 'Z/Z_sun' 
  tab.clf     = clf   & tab.id(9)  = 'clf_0' 
  tab.v0clf   = v0clf & tab.id(10) = 'clf_v1' 
  tab.v1clf   = v1clf & tab.id(11) = 'clf_v2' 
;
; strings
;
  steff=strtrim(string(teff),2)
  il=strpos(steff,'.')	      
  tab.steff =  strmid(steff,0,il)
  
  slog=strtrim(string(logg),2)  
  tab.slog = strtrim(strmid(slog,0,4),2)

  raux = strtrim(string(tab.r),2)
  il = strpos(raux,'.')
  tab.sr = strmid(raux,0,il+3)

  mdaux = strtrim(string(mdot),2)
  il = strpos(mdaux,'.')
  tab.smdot =  strmid(mdaux,0,il+3)
   	    
  tab.svterm = svterm 
  tab.snhe = strmid(strtrim(string(nhe),2),0,5)   
;;  tab.logq = strtrim(string(logq),2) 

  smet = strtrim(string(met),2)
  il = strpos(smet,'.')
  tab.sbeta = strmid(strtrim(string(beta),2),0,4)
  tab.smet = strmid(smet,0,il+3)

  sturb = strtrim(string(vturb),2)
  il=strpos(sturb,'.')	    	    
  tab.vterm = svterm+'.0'
  tab.svturb = strtrim(strmid(sturb,0,il),2)
;
  tab.sclf   = sclf_tab(0)
  tab.sv0clf = sclf_tab(1)
  tab.sv1clf = sclf_tab(2)
;	    
  if met gt 0.0d0 then lmet=double(alog10(met*1.d0)) $
   else lmet=100.d0	    
  smet=strtrim(string(lmet),2)
	    	    
  il=strlen(smet)
  ip=strpos(smet,'.')
  ii=strcmp(strmid(smet,ip-1,1),'0')
  if (ii) then ii=ip+1 $
   else ii=ip-1
  zcomp = fix(strtrim(strmid(smet,ip+2,1),2))
  if zcomp ge 5 then begin
     if lmet lt 0.d0 then lmet = lmet - 0.1d0 $
       else lmet = lmet + 0.1d0
  endif  
  smet = strtrim(string(lmet),2)
  ip=strpos(smet,'.')
  im=strpos(smet,'-')
  if im eq -1 then $
      sc=strtrim(strmid(smet,0,ip),2) + strtrim(strmid(smet,ip+1,1),2) $
  else $
      sc=strtrim(strmid(smet,im+1,ip-1),2) + strtrim(strmid(smet,ip+1,1),2)

  if (lmet lt 0.d0) then smet='m'+strtrim(sc,2) $
    else  smet='p'+strtrim(sc,2)
   if (lmet ge 100.d0) then smet='__nomet__'
   
  dhe=nhe/(1.d0+nhe) ; DETAIL way for He definition
  uno=fix(strmid(strtrim(string(dhe),2),4,1))
  if (uno ge 5) then dhe=dhe+0.01d0
  sdhe=strmid(strtrim(string(dhe),2),1,3)     
  ;
  sloq=strmid(strtrim(string(logq),2),1,5)
  uno=fix(strmid(strtrim(string(sloq),2),4,1))
  if (uno ge 5) then logq=logq-1.d-1
  slogq=strmid(strtrim(string(logq),2),1,4)
; 	     
;  filen=filen+sturb+smet+'He'+sdhe
;  filen=filen+'q'+slogq
;  filen=filen+'b'+sbeta
;  filen=filen+'vt'+svterm
;
;  if clf gt 1.d0 then begin
;     ip = strpos(sclf_tab(0),'.')	     
;     filen = filen+'clf'+strtrim(strmid(sclf_tab(0),0,ip),2)
;  endif

;  file=filen+'__fstwnd.spec'
  
return

end


