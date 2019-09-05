; Miguel A. Urbaneja 
; Current version: v9c  Jan 2008
; 
; [ history record abridged ]     
;
;  Version 9c   : several minor improvements with respect to version v9b
;
;
; PURPOSE: emulate a normalized spectrum, using FASTWIND's line profiles
;
; WARNINGS: overlaping lines are not properly treated by this program
;
; REQUIREMENTS: the following IDL programs are required
;               roundcad.pro
;               sampling.pro
;               starmag3.pro
;               rd_fstwnd_abundan.pro
;               fstwnd_version.pro
;               SHORTNAME.PRO (source: A. Herrero IDL library)
;
; COMMENTS: some features, present in previous versions, are no longer supported
;
;
@/scratch/norberto/MIGUEL_MODELOS/IDL_PRO/FASTWIND/shortname.pro
@/scratch/norberto/MIGUEL_MODELOS/IDL_PRO/FASTWIND/leep.pro
@/scratch/norberto/MIGUEL_MODELOS/IDL_PRO/FASTWIND/roundcad.pro
@/scratch/norberto/MIGUEL_MODELOS/IDL_PRO/FASTWIND/sampling.pro
@/scratch/norberto/MIGUEL_MODELOS/IDL_PRO/FASTWIND/starmag3.pro
@/scratch/norberto/MIGUEL_MODELOS/IDL_PRO/FASTWIND/rd_fstwnd_abundan.pro
@/scratch/norberto/MIGUEL_MODELOS/IDL_PRO/FASTWIND/fstwnd_version.pro
@/scratch/norberto/MIGUEL_MODELOS/IDL_PRO/FASTWIND/rd_fstwnd_line_profile.pro
@/scratch/norberto/MIGUEL_MODELOS/IDL_PRO/FASTWIND/fstwnd_compose_model_name.pro
@/scratch/norberto/MIGUEL_MODELOS/IDL_PRO/FASTWIND/shrink_vec.pro
@/scratch/norberto/MIGUEL_MODELOS/IDL_PRO/FASTWIND/rd_fstwnd_indat.pro
;@SHORTNAME.PRO 
;


  pro wr_fstwndspec_v9c,dir,help=help,sed=sed,coarse=coarse,plot=plot, $
	                quiet=quiet,nocompress=nocompress,vf_turb=vf_turb,  $
	                outname=outname 	     

        !EXCEPT = 0 
	        
	akb = 1.38062d-16
	amh = 1.67352d-24
	
	if (keyword_set(help)) then begin
		print,''
		print,' Procedure WR_FSFWNDSPEC to create an ASCII file '
		print,' with all the lines created with FORMALSOL/FASTWIND'
		print,' '
		print,' Miguel A. Urbaneja '
		print,' Version 0, 2002 (wspuma.pro) '
		print,' Version 9c, Jan 2008'
		print,' '
		print,'   default lambda range   : 920 - 45000 AE  '
		print,'   default lambda sampling:        0.01 AE  '
		print,' '
		print,' MISSING POINTS:'
		print,' correct treatment of the H series limits '
		print,' '
		print,' COMMENTS: '
		print,' some features included in previous versions are no longer available'
		print,' '
		print,' CALLING SEQUENCE: '
		print,' wr_fstwnd_v9c,modeladdress,[ help=help,coarse=coarse,vf_tur=vf_tur,sed=sed, '
		print,'                         outname=outname,quiet=quiet,nocompress=nocompress ] '
		print,' '
		print,' OPTIONAL KEYWORDS: '
		print,' help	   : print this information'
		print,' coarse	   : reduced files, coarse frequency grid (RECOMENDED) '
		print,' vf_tur     : string providing information about the microturbulence. This is'
		print,'                useful in those cases where different micros are present'
		print,' sed        : if enable, a second file with the spectral energy distribution is '
		print,'                created. Note that this contains important approximations, use it'
		print,'                wisely'  
		print,' outname    : contains the output file name'		
		print,' quiet      : messages supressed '
		print,' nocompress : output file(s) not compressed '
		print,' plot	   : only for debugging, not really useful'
		print,' '
		return
	endif

    detail = 1 & lineas = findfile(dir+'OUT.*',count = nlin) & dumm = findfile(dir+'OUT.H*',count = ndum) 
    nmet   = nlin - ndum   &  if (nmet eq 0) then detail = 0
		 
    aux_turb = ' '
    nfre=161       ;# No. of frequency points expected in a line profile. Kept for consistency with older versions
    if (not keyword_set(vf_turb)) then vf_turb='' $
       else aux_turb = 'VT*'+vf_turb	
    file='fstwnd-spec' & sc=' '
    busca = dir+'OUT.H*'+strtrim(aux_turb,2)+'*'
    lineah=findfile(busca,count=nlinh)
    if nlinh eq 0 then begin
       print,'files not found'
       return
    endif
    shortname,lineah,slineah
    ix=intarr(nlinh)+0
    for a=0,nlinh-1 do begin
       cad=string(strmid(strtrim(slineah[a],2),5,2))
       if (cad ne 'EI') then ix[a]=1
    endfor	
    indice=where(ix ne 0) & indhe=where(ix eq 0)
    if not keyword_set(quiet) then $
       print,'No. of Hidrogen lines :  ',n_elements(indice)		   
      	   
;## this ensures that VTURB is known 

     aux_turb_new = '' & auxvf_turb = 0
     if not keyword_set(vf_turb) then begin
	auxname = slineah[0]   
	il = strlen(auxname) & i_v = strpos(auxname,'_V')
        if i_v ne -1 then begin 
           auxvf_turb = strtrim(strmid(auxname,i_v+3,il-i_v-3),2)
           aux_turb_new = 'VT'+auxvf_turb 		   
	endif   
    endif else aux_turb_new = aux_turb  & auxvf_turb = vf_turb     ;A ver si esto funciona  

;## wavelength base

    delta = 1.d-2  &  if keyword_set(coarse) then delta=2.5d-2 
    lm = 920.d0    &  lx = 45000.d0
    colita = ''

    lm0 = lm  &  lx0 = lx  &  delta2=2.d0*delta	
    omega = lm+delta2*dindgen(long((lx-lm)/delta2)+1l)
    flujo = dblarr(n_elements(omega))+0.d0

    ixs = indgen(n_elements(indice)) 
    cenwave  = dblarr(n_elements(indice))
    cenwave2 = dblarr(n_elements(indice))
    for j=0,n_elements(indice)-1 do begin
       ind = indice[j]
       rd_fstwnd_line_profile,slineah[ind],dir=dir,d
       cenwave[j] = 0.5d0*(max(d.x)+min(d.x))
       dummy = min(d.y,ixm)
       cenwave2[j] = d.x[ixm]
    endfor
    a = sort(cenwave2)    ;## sorting with respect to cenvawe2
    cenwave = cenwave[a]  &  cenwave2 = cenwave2[a]
    indice  = indice[a]   ;## H lines sorted, from bluest to redder wavelenght 

    newtreat = ['NO_METALS']  ;# work in progress: treatment of H+metal blend

;#  first, take care of the hydrogen lines 	
    for j=0,n_elements(indice)-1 do begin	; only hydrogen
       lambda0 = cenwave2[j] 
       if ( lambda0 ge lm0 and lambda0 le lx0) then begin
	  tmpname = lineah[indice[j]]
	  shortname,tmpname,auxname 
	  auxname=strtrim(auxname,2)
	  il = strlen(auxname) & i_v = strpos(auxname,'_V')
          if i_v ne -1 then $
               tmpname = strmid(auxname,4,i_v-4) $
	  else tmpname = strmid(auxname,4,il-4)   
          if not keyword_set(quiet) then begin
	     print,'Hydrogen line ',j+1,' : '+tmpname 
          endif		
	  aux = strmatch(newtreat,tmpname)
	  if (total(aux) eq 0) then $		
	     rd_fstwnd_line_profile,slineah[indice[j]],dir=dir,output 

	  d = { wave : output.x, flux_nor : output.y }				   
  	  l1 = sampling(delta,d.wave,d.flux_nor)        ;# resample the line using the same delta_lambda
    	  dm = min(d.wave)  &  dx=max(d.wave)
    	  if (j eq 0) then begin                        ;# the 1st line is in isolation
    	     if (dm ge lm and dx le lx) then begin
                a1 = where(omega ge dm)
		a2 = where(omega le dx)
   		i1 = long(a2(n_elements(a2)-1)-a1(0)+1)
		omegin = dblarr(i1)
		omegin = omega(a1(0):a2(n_elements(a2)-1))		   
		s = n_elements(l1(*,0))
  		f1 = spline(l1(*,0),l1(*,1),omegin)-1.d0
                flujo(a1(0):a2(n_elements(a2)-1))=f1(0:n_elements(f1)-1)
		oflag=0  ; no overlapping
 		if not keyword_set(quiet) then print,'First H line treated as isolated'		      		
	     endif	
	  endif else begin          ;# other H lines could be affected by overlaping H lines 		
	       rd_fstwnd_line_profile,slineah(indice[j-1]),output2,dir=dir
               d2 = { wave : output2.x, flux_nor : output2.y } 		     		
	       l2 = samplig(delta,d2.wave,d2.flux_nor)		   
	       dm2 = min(d2.wave)  &  dx2=max(d2.wave)
	       xmed2 = 0.5d0*(cenwave2[j]+cenwave2[j-1])
		   
	       if (dx2 lt dm) then begin   ;# no other line affects it		   		   
    		  if (dm ge lm and dx le lx) then begin
                      a1 = where(omega ge dm)
		      a2 = where(omega le dx)
   		      i1 = long(a2(n_elements(a2)-1)-a1(0)+1)
		      omegin = dblarr(i1)
		      omegin = omega(a1(0):a2(n_elements(a2)-1))		   
		      s = n_elements(l1(*,0))
  		      f1 = spline(l1(*,0),l1(*,1),omegin)-1.d0
                      flujo(a1(0):a2(n_elements(a2)-1)) = f1(0:n_elements(f1)-1)		      
		      oflag = 0  ; no overlapping		      		
		  endif
	       endif else begin
		  if not keyword_set(quiet) then begin
		     shortname,slineah(indice(j-1)),dummy_string
   		     il = strlen(dummy_string) & i_v = strpos(dummy_string,'_V')
                     if i_v ne -1 then $
                        tmpname = strmid(dummy_string,4,i_v-4) $
		     else tmpname = strmid(dummy_string,4,il-4)    		          
		     print,' --- overlap with '+tmpname  
                  endif
                  a1 = where(omega ge dm)
	          a2 = where(omega le dx)
	          c1 = where (omega ge dm and omega le dx)
   	          i1 = long(n_elements(c1))
	          omegin = dblarr(i1)
	          omegin = omega(c1)		   
	          f1 = spline(l1(*,0),l1(*,1),omegin)-1.d0
                  b1 = where(omega ge dm2)
	          b2 = where(omega le dx2)
                  c2 = where (omega ge dm2 and omega le dx2)
   	          i2 = long(n_elements(c2))
	          omegin2 = dblarr(i2)
	          omegin2 = omega(c2)		   
	          f2 = spline(l2(*,0),l2(*,1),omegin)-1.d0		    
	          xc1 = 0.5d0*(max(l1(*,0))+min(l1(*,0)))
	          xc2 = 0.5d0*(max(l2(*,0))+min(l2(*,0)))
    	          xmed=0.5d0*(xc2+xc1)		    
    	          if keyword_set(plot) then begin
    		     print,'Central wavs ',xc2,' ',xc1,' ',xmed		    
    		     plot,omegin,f1+1.d0,xr=[min(omegin2),max(omegin)],yr=[0.,2]
	    	     oplot,omega,flujo+1.d0,color=5
		     oplot,xmed*[1.d0,1.d0],!y.crange, color=6
		     oplot,xmed2*[1.d0,1.d0],!y.crange, color=12,linestyle=4			 
		     stop
		  endif
    	          xmed = xmed2  &  a1n = where(omega(c1) ge xmed)
		  jj = where(omegin ge xmed)
                  flujo(c1(a1n(0)):a2(n_elements(a2)-1)) = f1(jj(0):n_elements(f1)-1)                    		    
	       endelse 
          endelse     
       endif else $
          if not keyword_set(quiet) then $
	    print,' skipping H line: '+lineah(indice(j))+' at '+strtrim(string(lambda0),2)+' AE'
    endfor	
;#
;# H lines done!!
;#
;# He lines     
;#
    nhe = nlinh - n_elements(indice)	
    if not keyword_set(quiet) then begin
       print,''
       print,'Number of He lines: ',nlinh-n_elements(indice)
       print,''
    endif       
    if nhe gt 0 then begin 	  
       busca  = dir+'OUT.HEI*'+strtrim(aux_turb_new,2) ;; +'*'
       lineahe = findfile(busca,count=nhe)         

       for j=0,nhe-1 do begin	     ; He lines
	  namelinea=' '
	  shortname,lineahe[j],namelinea
	  igz = strpos(namelinea,'.gz')
	  if igz eq -1 then begin		 
	     cad1=strtrim(strmid(strtrim(namelinea,2),4,1),2)	
	     if not keyword_set(quiet) then begin
   		il = strlen(namelinea) & i_v = strpos(namelinea,'_V')
                 if i_v ne -1 then $
                    tmpname = strmid(namelinea,4,i_v-4) $
		 else tmpname = strmid(namelinea,4,il-4)    		          		 
		 print,'Line '+tmpname
		 print,' '
	     endif  
	     rd_fstwnd_line_profile,namelinea,dir=dir,output
	     d = { wave : output.x, flux_nor : output.y }		 
	     dm = min(d.wave)  &  dx = max(d.wave)
 	     if (dm ge lm and dx le lx) then begin
		a1 = where(omega ge dm)
		a2 = where(omega le dx)
		i1 = long(a2(n_elements(a2)-1)-a1(0)+1)
		omegin = dblarr(i1)
		omegin = omega(a1(0):a2(n_elements(a2)-1))		
		l1 = samplig(delta,d.wave,d.flux_nor)
		f1 = spline(l1(*,0),l1(*,1),omegin)-1.d0
		flujo(a1(0):a2(n_elements(a2)-1)) = flujo(a1(0):a2(n_elements(a2)-1)) + $
		f1(0:n_elements(f1)-1)
	     endif
	  endif          
       endfor	   
    endif
;#
;# metal lines, if any         
;#
    lineas = findfile(dir+'OUT.*'+strtrim(aux_turb_new,2),count = nlin)
    dumm   = findfile(dir+'OUT.H*'+strtrim(aux_turb_new,2),count = ndum) 
    nmet   = nlin - ndum
    if nmet ne 0 then begin
       if (nmet gt 0.d0) then begin
	  cad1=' '
	  cad2=' '
          l0_met = dblarr(nmet) + 1.d20
	  metlines = strarr(nmet)
	 
	  k = -1
	  nmet_old = nmet
	  nmet = 0 
	  for j=0,nlin-1 do begin
	     namelinea = ' '  &  shortname,lineas(j),namelinea
	     namelinea = strtrim(namelinea,2)
	     cad1 = strtrim(strmid(namelinea,4,1),2)
	     cad2 = 'H'
	     verdad = strcmp(cad1,cad2,/FOLD_CASE)
 	     igz = strpos(namelinea,'.gz')		 
	     if (not verdad and igz eq -1) then begin
                k = k + 1
                leep,lineas(j),d,np=nfre	     
	        y_m = min(d.flux_nor,i_m)
	        l0_met(k) = d.wave(i_m)
	        metlines(k) = lineas(j)	     
	     endif
	  endfor  
	  nmet = k + 1  
          if not keyword_set(quiet) then begin
	     print,' '  &  print,'Metal lines: ',nmet
	     print,' '
          endif   	  
	  i_met = sort(l0_met)
	  l0_met = l0_met(i_met)
	  metlines = metlines(i_met) 
	  
	  get_lun,unit_1
	  openw,unit_1,dir+'ewdata.fstwnd.txt'
          printf,unit_1,' LINE     LAMBDA_0      EW(mA) '	 	      
          for k=0,nmet - 1 do begin
	     j = k
 	     namelinea=' '
	     shortname,metlines(j),namelinea
	     namelinea=strtrim(namelinea,2)		
	     igz = strpos(namelinea,'.gz')
	     if igz eq -1 then begin			
 	       il = strlen(namelinea)
	       i_v = strpos(namelinea,'_V')
               if i_v ne -1 then $
                  l_nam = strmid(namelinea,4,i_v-4) $
	        else l_nam = strmid(namelinea,4,il-4)
		  
	       if not keyword_set(quiet) then $
                     print,'Line ' + l_nam 	
		     leep,metlines(j),d,np=nfre
		     ew = d.ew*(-1.d3)
		     i_b = strpos(namelinea,'_')
		     if (i_b gt 0) then i_b = i_b-1 $
		       else i_b = il - 4
		       
		     ww = d.wave  &  shrink_vec,ww
		     ff = interpol(d.flux_nor,d.wave,ww) > 0
		     i = n_elements(ff)		    
		     nl0 = l0_met[j]
		     printf,unit_1,format='(1X,A10,4X,G12.5,2X,G12.5)', $
		        strtrim(l_nam,2),nl0,ew
		    
		     if keyword_set(plot) then begin
		        plot,d.wave,d.flux_nor,/nodata
		        oplot,d.wave,d.flux_nor,psym=2
		        oplot,ww,ff,psym=5,color=5
                     endif
		     d = { wave : dblarr(i), flux_nor : dblarr(i) }
		     d.wave=ww
		     d.flux_nor=ff
		     if not keyword_set(quiet) then print,' '
		     dm = min(d.wave)  &  dx = max(d.wave)
                     if (dm ge lm and dx le lx) then begin
		       a1 = where(omega ge dm)
		       a2 = where(omega le dx)
		       i1 = long(a2(n_elements(a2)-1)-a1(0)+1)
		       omegin = dblarr(i1)
		       omegin = omega(a1(0):a2(n_elements(a2)-1))	       
		       l1 = samplig(delta,d.wave,d.flux_nor)		     
		       f1 = spline(l1(*,0),l1(*,1),omegin)-1.d0
		       flujo(a1(0):a2(n_elements(a2)-1)) = flujo(a1(0):a2(n_elements(a2)-1)) + $
		               f1(0:n_elements(f1)-1)
		     endif
		 endif     
	    endfor	   
	   endif 
           close,unit_1
	   free_lun,unit_1
	endif	
	flujo = flujo + 1.d0
;#
;# normalized spectrum done!	
;#
;# read INDAT.DAT file
;#
   rd_fstwnd_indat,dir=dir,indata
   catname = indata.catname        ;# catalog's name
   
   logq = alog10(10^indata.logmdot/(indata.vterm * indata.r)^1.5d0) 
   slogq = strtrim(string(logq),2) & ip = strpos(slogq,'.') & slogq = strmid(slogq,0,ip+4)
      
   vfturb = double(auxvf_turb)
   epsilon = indata.nhe/(1.d0 + indata.nhe) 
   filen = fstwnd_compose_model_name(indata.teff,indata.logg,indata.vturb,epsilon,vfturb, $ 
                        logq,indata.beta,indata.vterm,indata.met) ;# filename from parameters
                      
   sc = 'Teff '+indata.steff+'   log g '+ indata.slog+'   N(H) = 1.0   N(He) = '
   il = strpos(indata.slog,'.')
   auxslog = strtrim(strmid(indata.slog,0,il),2) + strtrim(strmid(indata.slog,il+1,2),2)	    
   sc1 = sc + indata.snhe
   sc  = 'logQ '+ slogq + '    beta '

   il = strpos(indata.smet,'.')
   sc = sc + indata.sbeta + '   Z ' + indata.smet
   sturb = strtrim(string(indata.svturb),2) & il = strpos(sturb,'.')	   
   svf_turb = strtrim(string(auxvf_turb),2)  	    
   sc2 = sc + '    vterm  '+indata.svterm
   sc2 = sc2+ '    vturb  '+indata.svturb + '    vformal  '+strtrim(svf_turb,2)+'.0'
   clumping_info = 0 
      
   file = filen + '__fstwnd.spec'   ;#### output name ready
;#----
;#----   	SED, approximated treatment
;#----
    s = findfile(dir+'OUT_TOT',count=b)
	if ( b eq 1  and keyword_set(sed) ) then begin
	     hcan = [3647.019d0,8205.826d0,14588.166d0]
	     diff = 0.5d0
	     hm = hcan-diff  & hx = hcan+diff
	     hm = alog10(hm) & hx = alog10(hx)
	     lom = alog10(omega)
	     readmod,dir+'OUT_TOT',' ',out
	     ia = sort(out.lam)  &  ww = out.lam(ia)
	     ff = out.hnue(ia)*(-1.d0)	     
	     ix = where (ww ge lm and ww le lx)	     
	     ix = [ix,ix(0)-1,ix(n_elements(ix)-1)+1]
	     ixo = sort(ix)
	     w1 = alog10(ww[ix[ixo]]) & f1 = ff[ix[ixo]]
	     kk = dblarr(n_elements(w1))-99.d0
	     for j=1,n_elements(w1)-1 do begin
	     	 if (abs(w1(j)-w1(j-1)) le 2.d-5) then kk(j) = 1.d0
	     endfor
	     ix = where(kk lt 1.d0)
	     zx = w1[ix]  &  zf = f1[ix]

;# to avoid numerical problems, the interpolation is done by pieces
;# w < Balmer jump
	     ic1 = where(zx le hm[0]) & ib1 = where(lom le hm[0])
	     zy1 = spline(zx(ic1),zf(ic1),lom(ib1))
	     ib  = ib1
;# Balmer jump < w < Paschen jump	      
	     ic1 = where(zx gt hx(0) and zx le hm(1)) & ib1 = where(lom gt hx(0) and lom le hm(1))
	     zy2 = spline(zx(ic1),zf(ic1),lom(ib1))
	     ib = [ib,ib1]
;# Paschen jump < w < 1.4 mu	     
     	 ic1 = where(zx gt hx[1] and zx le hm[2])
	 ib1 = where(lom gt hx[1] and lom le hm[2])
	 zy3 = spline(zx[ic1],zf[ic1],lom[ib1])
     	 ib=[ib,ib1]
;# 1.45 mu < w     	 
	 ic1 = where(zx gt hx[2])
	 ib1 = where(lom gt hx[2])
	 zy4 = spline(zx(ic1),zf(ic1),lom(ib1))
    	 ib = [ib,ib1]
;   	 
	 zy = [zy1,zy2,zy3,zy4]	     
	 w = 10^lom(ib) &  f = 10^(-1.d0*zy)*flujo
	 if keyword_set(plot) then begin
    	    plot,w,f & oplot,out.lam,10^out.hnue,color=5
	    stop
    	 endif	
;# we have to consider also the wavelengths not included in the spectrum
	 ia = sort(out.lam)
	 wk = out.lam(ia)
	 fk = 10^out.hnue(ia)
	 wwm = min(w)  &  wwx = max(w)
	 ix1 = where(wk lt wwm)  &  ix2 = where(wk gt wwx)
    	 wtemp = [wk(ix1),w,wk(ix2)]
	 ftemp = [fk(ix1),f,fk(ix2)]	     
;# compute the photometric quantities    	 
    	 starmag3,wtemp,ftemp,indata.r,magnitudes	    	 
	endif
	
;# search for information cocerning chemical abundances	
	abun = findfile(dir+'ABUNDAN',count=ic_abun)
	if ic_abun eq 1 then $
	  rd_fstwnd_abundan,dir=dir,abundan	

;# which version has been used?
	version = fstwnd_version(dir=dir)

;# we can now write the ascii files 	    
    if (strlen(colita) gt 0) then fileout = file+colita+'.ascii' $
	    else fileout=file+'.ascii'	    
	    
	get_lun,unit2    
	openw,unit2,dir+fileout
    printf,unit2,'FASTWIND spectrum ('+version+') --- model : ',catname
    printf,unit2,strtrim(sc1,2)
    printf,unit2,strtrim(sc2,2)
	if (clumping_info) then printf,unit2,strtrim(sc3,2)

	if ic_abun eq 1 then begin
	   iel = n_elements(abundan.labl)
	   printf,unit2,'Abundances:     N(X)/N(H)      Mass fraction '
	   for k=0,iel-1,3 do begin
	       printf,unit2,abundan.labl(k),abundan.nxh(k), $
		     abundan.massf(k),abundan.labl(k+1),abundan.nxh(k+1), $
		     abundan.massf(k+1),abundan.labl(k+2),abundan.nxh(k+2), $
		     abundan.massf(k+2), $
	      format='(3(1X,A2,2X,2(G12.5,3X)))'	    
	  endfor  
	endif
	
	printf,unit2,' '      &    printf,unit2,'ascii file   ',strtrim(file,2)
	printf,unit2,'format: lambda (AE)    norm. flux  ' 
    printf,unit2,'data'   &    printf,unit2,n_elements(omega)
	
	for j=0l,long(n_elements(omega))-1l do begin
            printf,unit2,omega(j),flujo(j) 
	endfor	
	close,unit2 & free_lun,unit2
	        
	if keyword_set(sed) then begin
	    w = wtemp   &   f = ftemp     ;;old name        fileflux = filen + '__flux.hnue'
        fileflux = filen + '__fstwnd.nhue' 
	    get_lun,uni2
	    openw,unit2,dir+fileflux+'.ascii'
        printf,unit2,'FASTWIND SED ('+version+') --- model : ',catname
	    printf,unit2,'WARNING: this is not an exact solution!!! '
        printf,unit2,strtrim(sc1,2)
    	printf,unit2,strtrim(sc2,2)
	    if (clumping_info) then printf,unit2,strtrim(sc3,2)
	    if (ic_abun eq 1) then begin	      
	       iel = n_elements(abundan.labl)
	       printf,unit2,'Abundances:     N(X)/N(H)      Mass fraction '
	       for k=0,iel-1,3 do begin
	          printf,unit2,abundan.labl(k),abundan.nxh(k), $
		      abundan.massf(k),abundan.labl(k+1),abundan.nxh(k+1), $
		      abundan.massf(k+1),abundan.labl(k+2),abundan.nxh(k+2), $
		      abundan.massf(k+2), $
	            format='(3(1X,A2,2X,2(G12.5,3X)))'	    
	      endfor  
  	    endif
	    printf,unit2,'Log L/Lsun '+strtrim(string(alog10(magnitudes.lumi)),2)	    
;##
;##  BC is written just after the colours (same row, since v6)
;##	    
	    printf,unit2,format='(2X,A4,7X,A4)','Mv','Mbol'	    
	    printf,unit2,format='(1X,2(F7.3,4X))',magnitudes.v,magnitudes.mbol
	    printf,unit2,format='(1X,8(A5,3X),"BC")',magnitudes.colorname
	    printf,unit2,format='(1X,8(F7.3,3X),"BC")',magnitudes.color
	    printf,unit2,' '
            printf,unit2,'ascii file   ',strtrim(fileflux,2)
	    printf,unit2,'format: lambda (AE)    H_nue (erg/cm^2/s/Hz)  ' 	
            printf,unit2,'data'
            printf,unit2,n_elements(f)
	    for j=0l,long(n_elements(w))-1l do begin
                printf,unit2,w(j),f(j)		
	    endfor
	    close,unit2 & free_lun,unit2
	endif   
	
	if not keyword_set(quiet) then begin    	
	   print,''
	   print,'FASTWIND spectrum ready in ' + dir
	   print,format='(" wavelength range : ",F7.2," - ",F8.2," (AE) ")',lm,lx
	   print,' file root name   : ' + strtrim(file,2)
	   if keyword_set(sed) then $
	      print,' SED (H_nue)      : ',strtrim(fileflux,2)
	   print,''
    endif
	
    if not keyword_set(nocompress) then begin
       spawn,'gzip -f '+dir+strtrim(file,2)+'.ascii'
	if keyword_set(sed) then $
	       spawn,'gzip -f '+dir+strtrim(fileflux,2)+'.ascii'
	endif

    if keyword_set(outname) then outname = dir+fileout       
    if (detail) then spawn,'gunzip OUT.*gz' 

    return
end
	
