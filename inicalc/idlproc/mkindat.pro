pro mkindat,majorcat,fulledit=fulledit

common indat,model,optneupdate,opthei,istart,imore, $
  optmixed,teff,logg,rstar, $
  rmax,tmin, $
  mdot,vmin,vinf,beta,vdiv, $
  yhe,ihe, $
  optmod,optlucy,megas,optaccel,optcmf, $
  vturb,z,lines,linesinmodel, $
  enatcor,expansion,set_first,set_step, $
  clf,vclf_s,vclf_m, $
  hopfself, $
  qinf,q0,gamma
common all, allpara

  if n_params() eq 0 then begin
  majorcat='.'
  endif

  spawn,'rm indat.aux'

  line1='                                          model'
  line2='                                          optneupdate, opthei, itfirst, itmore'
  line3='                                          optmixed = 0.'
  line4='                                          Teff, log g, Rstar'
  line5='                                          Rmax, Tmin'
  line6='                                                     Mdot, vmin, Vinf, beta, vdiv'
  line7='                                          Yhe, ihe'
  line8='                                          optmod=f, optlucy, megas, optaccel, optcmg'
  line9='                                          vturb, Z, lines, lines_in_model'
 line10='                                          enatcor, expansion=f, set_first, set_step'
 line11='                                          enhancement-factor, vcl_start, vcl_max'
 line12='                                          hopfself'
 line13='                                          q_inf, q_0, gamma'
  space='  '

  
  print,' create new INDAT.DAT from scratch : 0'
  print,' modify existing INDAT.DAT         : 1'
  read,opt

  if opt eq 0 then begin
; create from scratch

  model="'"+majorcat+"/Test'"

  optneupdate='T'
  opthei='T'
  istart='0'
  imore='100'

  optmixed='0.'
  
  teff='30000.'
  logg='3.0'
  rstar='20.'
  
  rmax='120.'
  tmin='0.6'

  mdot='1.e-6'
  vmin='0.1'
  vinf='1500.'
  beta='1.0'
  vdiv='0.1'

  yhe='0.1'
  ihe='2'
 
  optmod='F'
  optlucy='T'
  megas='F'
  optaccel='T'
  optcmf='T'

  vturb='15.'
  z='1.0'
  lines='T'
  linesinmodel='T'

  enatcor='T'
  expansion='F'
  set_first='1'
  set_step='2'

  clf='1.'
  vclf_s='0.1'
  vclf_m='0.3'
  
  hopfself='F'

  qinf='0.'
  q0='0.'
  gamma='0.'
  endif else begin
    if opt eq 1 then begin
; select file
    print,'select file to copy'
    indat=dialog_pickfile(path=majorcat, get_path=dir, $
			  filter='INDAT.DAT', $
		          tit='select INDAT.DAT file')
    if(indat eq '') then begin
      print,' no file chosen!'
      return
    endif

    openr,1,indat
; read selected INDAT.DAT
    a=' '
    readf,1,a
; replace commas with whitespace
    aa=strjoin(strsplit(a,',',/extract),' ')
; replace apostrophes with whitespace
    aa=strjoin(strsplit(a,' ',/extract),' ')
    stri=str_sep(aa,' ')
    b=where(stri ne '')
    str=stri(b)
    model=str(0)
    len=strlen(model)
    if strpos(model,"'",1) ne len-1 then begin
    model=model+"'"
    endif
    
    readf,1,a
    aa=strjoin(strsplit(a,',',/extract),' ')
    stri=str_sep(aa,' ')
    b=where(stri ne '')
    str=stri(b)
    optneupdate=str(0)
    opthei=str(1)
    istart=str(2)
    imore=str(3)
    
    readf,1,a
    aa=strjoin(strsplit(a,',',/extract),' ')
    stri=str_sep(aa,' ')
    b=where(stri ne '')
    str=stri(b)
    optmixed=strtrim(string(float(str(0))),2)

    readf,1,a
    aa=strjoin(strsplit(a,',',/extract),' ')
    stri=str_sep(aa,' ')
    b=where(stri ne '')
    str=stri(b)
    teff=strtrim(string(float(str(0))),2)
    logg=strtrim(string(float(str(1))),2)
    rstar=strtrim(string(float(str(2))),2)

    readf,1,a
    aa=strjoin(strsplit(a,',',/extract),' ')
    stri=str_sep(aa,' ')
    b=where(stri ne '')
    str=stri(b)
    rmax=strtrim(string(float(str(0))),2)
    tmin=strtrim(string(float(str(1))),2)

    readf,1,a
    aa=strjoin(strsplit(a,',',/extract),' ')
    stri=str_sep(aa,' ')
    b=where(stri ne '')
    str=stri(b)
    mdot=strtrim(string(float(str(0))),2)
    vmin=strtrim(string(float(str(1))),2)
    vinf=strtrim(string(float(str(2))),2)
    beta=strtrim(string(float(str(3))),2)
    vdiv=strtrim(string(float(str(4))),2)

    readf,1,a
    aa=strjoin(strsplit(a,',',/extract),' ')
    stri=str_sep(aa,' ')
    b=where(stri ne '')
    str=stri(b)
    yhe=strtrim(string(float(str(0))),2)
    ihe=strtrim(string(float(str(1))),2)

    readf,1,a
    aa=strjoin(strsplit(a,',',/extract),' ')
    stri=str_sep(aa,' ')
    b=where(stri ne '')
    str=stri(b)
    optmod=str(0)
    optlucy=str(1)
    megas=str(2)
    optaccel=str(3)
    optcmf=str(4)

    readf,1,a
    aa=strjoin(strsplit(a,',',/extract),' ')
    stri=str_sep(aa,' ')
    b=where(stri ne '')
    str=stri(b)
    vturb=strtrim(string(float(str(0))),2)
    z=strtrim(string(float(str(1))),2)
    lines=str(2)
    linesinmodel=str(3)

    readf,1,a
    aa=strjoin(strsplit(a,',',/extract),' ')
    stri=str_sep(aa,' ')
    b=where(stri ne '')
    str=stri(b)
    enatcor=str(0)
    expansion=str(1)
    set_first=str(2)
    set_step=str(3)

    readf,1,a
    aa=strjoin(strsplit(a,',',/extract),' ')
    stri=str_sep(aa,' ')
    b=where(stri ne '')
    str=stri(b)
    clf=strtrim(string(float(str(0))),2)
    vclf_s=strtrim(string(float(str(1))),2)
    vclf_m=strtrim(string(float(str(2))),2)
    
    if eof(1) then begin
    hopfself='F'
    qinf='0.'
    q0='0.'
    gamma='0.'
    goto,cont
    endif

    readf,1,a
    aa=strjoin(strsplit(a,',',/extract),' ')
    stri=str_sep(aa,' ')
    b=where(stri ne '')
    str=stri(b)
    hopfself=str(0)

    if eof(1) then begin
    qinf='0.'
    q0='0.'
    gamma='0.'
    goto,cont
    endif

    readf,1,a
    aa=strjoin(strsplit(a,',',/extract),' ')
    stri=str_sep(aa,' ')
    b=where(stri ne '')
    str=stri(b)
    qinf=strtrim(string(float(str(0))),2)
    q0=strtrim(string(float(str(1))),2)
    gamma=strtrim(string(float(str(2))),2)

    cont:close,1

    endif else begin
    print,' wrong option',fix(opt),' try again!'
    return
    endelse
  endelse
;---------------------------------------------------------------------------
;modify values
  if keyword_set(fulledit) then begin
      call_procedure,'modinput',/full
    endif else begin
      allpara=0
      call_procedure,'modinput'
      if (allpara eq 1) then call_procedure,'modinput',/full
    endelse
  
    
;construct lines
    strput,line1,model,0
    strput,line2,optneupdate+space+opthei+space+istart+space+imore,0
    strput,line3,optmixed,0
    strput,line4,teff+space+logg+space+rstar,0
    strput,line5,rmax+space+tmin,0
    strput,line6,mdot+space+vmin+space+vinf+space+beta+space+vdiv,0
    strput,line7,yhe+space+ihe,0
    strput,line8,optmod+space+optlucy+space+megas+space+optaccel+space+optcmf,0
    strput,line9,vturb+space+z+space+lines+space+linesinmodel,0
    strput,line10,enatcor+space+expansion+space+set_first+space+set_step,0
    strput,line11,clf+space+vclf_s+space+vclf_m,0
    strput,line12,hopfself,0
    strput,line13,qinf+space+q0+space+gamma,0

;---------------------------------------------------------------------------
;write file
  openw,1,'indat.aux'
  printf,1,line1
  printf,1,line2
  printf,1,line3
  printf,1,line4
  printf,1,line5
  printf,1,line6
  printf,1,line7
  printf,1,line8
  printf,1,line9
  printf,1,line10
  printf,1,line11
  printf,1,line12
  printf,1,line13
  close,1
    
;test file and check whether model already exists
check:
  rindat1,'indat.aux',model
  modelnoapo=strmid(model,1,strlen(model)-2)
  itest=file_test(modelnoapo+'/INDAT.DAT')
  itest1=file_test(modelnoapo+'/indat.dat')
  
  
confirm:  
if(itest eq 1 or itest1 eq 1) then begin
  print
  print,' model ',model,' already exists!'
  print,' if you proceed, this will be overwritten after start!'
  print,' confirm if this is actually desired! (type  yes or no)'
  print

  a=' '
  read,a
  if a eq 'no' then begin
     spawn,'jed indat.aux'   
     goto,check  
   endif else begin
     
   if a ne 'yes' then begin
     goto,confirm   
   endif
  endelse
endif
; check whether model exists in majorcat

  if (majorcat ne '.') then begin
  l=strpos(model,majorcat)
  if(l eq -1) then begin
  print,' something wrong with model (filepath?)!'
  print,' majorcat = ',majorcat
  print,' model = ',model
  close,/all
  return
  endif
  endif
  
  spawn,'mkdir '+model
  spawn,' cp indat.aux '+model+'/INDAT.DAT'
  print,' INDAT.DAT created in ',model
  return
  
end

pro modinput,full=full
common indat,model,optneupdate,opthei,istart,imore, $
  optmixed,teff,logg,rstar, $
  rmax,tmin, $
  mdot,vmin,vinf,beta,vdiv, $
  yhe,ihe, $
  optmod,optlucy,megas,optaccel,optcmf, $
  vturb,z,lines,linesinmodel, $
  enatcor,expansion,set_first,set_step, $
  clf,vclf_s,vclf_m, $
  hopfself, $
  qinf,q0,gamma

base = widget_base(title='Select parameters',/frame,/column)
row1 = widget_base(base,/row,/frame)

modlabel = widget_label(row1,value='  Model:')
modtext = widget_text(row1, $
	  value = model,/editable,xsize=50, $
          event_pro="modvalue",uvalue='model',/all_events)

base1=widget_base(base,/frame,/column)

if keyword_set(full) then begin
titbase1=widget_label(base1,value='editable parameters (all) for INDAT.DAT')
endif else begin
titbase1=widget_label(base1,value='editable parameters (non-default) for INDAT.DAT')
endelse

if keyword_set(full) then begin
row2 = widget_base(base1,/row)
optneupdatelabel = widget_label(row2,value=' ne_upd:')
optneupdatetext = widget_text(row2, $
	value = optneupdate,/editable,xsize=2, $
        event_pro="modvalue",uvalue='optneupdate',/all_events)

optheilabel = widget_label(row2,value='opthei:')
optheitext = widget_text(row2, $
	value = opthei,/editable,xsize=2, $
        event_pro="modvalue",uvalue='opthei',/all_events)

istartlabel = widget_label(row2,value='itfirst:')
istarttext = widget_text(row2, $
	value = istart,/editable,xsize=5, $
        event_pro="modvalue",uvalue='istart',/all_events)

imorelabel = widget_label(row2,value='itmore:')
imoretext = widget_text(row2, $
	value = imore,/editable,xsize=5, $
        event_pro="modvalue",uvalue='imore',/all_events)
endif else begin

;  optneupdate='T'
;  opthei='T'

row2 = widget_base(base1,/row)
istartlabel = widget_label(row2,value='itfirst:')
istarttext = widget_text(row2, $
	value = istart,/editable,xsize=5, $
        event_pro="modvalue",uvalue='istart',/all_events)

imorelabel = widget_label(row2,value='itmore:')
imoretext = widget_text(row2, $
	value = imore,/editable,xsize=5, $
        event_pro="modvalue",uvalue='imore',/all_events)  
endelse  

if keyword_set(full) then begin
row3 = widget_base(base1,/row)
optmixedlabel = widget_label(row3,value='  mixed:')
optmixedtext = widget_text(row3, $
	value = optmixed,/editable,xsize=3, $
        event_pro="modvalue",uvalue='optmixed',/all_events)
endif else begin

;  optmixed='0.'

endelse

row4 = widget_base(base1,/row)
tefflabel = widget_label(row4,value='   Teff:')
tefftext = widget_text(row4, $
	value = teff,/editable,xsize=10, $
        event_pro="modvalue",uvalue='teff',/all_events)

logglabel = widget_label(row4,value='log g:')
loggtext = widget_text(row4, $
	value = logg,/editable,xsize=10, $
        event_pro="modvalue",uvalue='logg',/all_events)

rstarlabel = widget_label(row4,value='Rstar:')
rstartext = widget_text(row4, $
	value = rstar,/editable,xsize=10, $
        event_pro="modvalue",uvalue='rstar',/all_events)

if keyword_set(full) then begin
row5 = widget_base(base1,/row)
rmaxlabel = widget_label(row5,value='   Rmax:')
rmaxtext = widget_text(row5, $
	value = rmax,/editable,xsize=10, $
        event_pro="modvalue",uvalue='rmax',/all_events)

tminlabel = widget_label(row5,value='Tmin(start):')
tmintext = widget_text(row5, $
	value = tmin,/editable,xsize=10, $
        event_pro="modvalue",uvalue='tmin',/all_events)
endif else begin

;  rmax='120.'
;row5 = widget_base(base1,/row)

tminlabel = widget_label(row4,value='Tmin(start):')
tmintext = widget_text(row4, $
	value = tmin,/editable,xsize=10, $
        event_pro="modvalue",uvalue='tmin',/all_events)

endelse
  
row6 = widget_base(base1,/row)
mdotlabel = widget_label(row6,value='   Mdot:')
mdottext = widget_text(row6, $
	value = mdot,/editable,xsize=15, $
        event_pro="modvalue",uvalue='mdot',/all_events)

if keyword_set(full) then begin
vminlabel = widget_label(row6,value='vmin:')
vmintext = widget_text(row6, $
	value = vmin,/editable,xsize=5, $
        event_pro="modvalue",uvalue='vmin',/all_events)
endif else begin

;  vmin='0.1'

endelse  

vinflabel = widget_label(row6,value='Vinf:')
vinftext = widget_text(row6, $
	value = vinf,/editable,xsize=7, $
        event_pro="modvalue",uvalue='vinf',/all_events)

betalabel = widget_label(row6,value='beta:')
betatext = widget_text(row6, $
	value = beta,/editable,xsize=5, $
        event_pro="modvalue",uvalue='beta',/all_events)

if keyword_set(full) then begin
vdivlabel = widget_label(row6,value='vdiv:')
vdivtext = widget_text(row6, $
	value = vdiv,/editable,xsize=5, $
        event_pro="modvalue",uvalue='vdiv',/all_events)
endif else begin

;  vdiv='0.1'

endelse  

row7 = widget_base(base1,/row)
yhelabel = widget_label(row7,value='   Y_He:')
yhetext = widget_text(row7, $
	value = yhe,/editable,xsize=5, $
        event_pro="modvalue",uvalue='yhe',/all_events)

ihelabel = widget_label(row7,value='I_He:')
ihetext = widget_text(row7, $
	value = ihe,/editable,xsize=5, $
        event_pro="modvalue",uvalue='ihe',/all_events)

if keyword_set(full) then begin
row8 = widget_base(base1,/row)
optmodlabel = widget_label(row8,value=' optmod:')
optmodtext = widget_text(row8, $
	value = optmod,/editable,xsize=2, $
        event_pro="modvalue",uvalue='optmod',/all_events)

optlucylabel = widget_label(row8,value='optlucy:')
optlucytext = widget_text(row8, $
	value = optlucy,/editable,xsize=2, $
        event_pro="modvalue",uvalue='optlucy',/all_events)

megaslabel = widget_label(row8,value='megas:')
megastext = widget_text(row8, $
	value = megas,/editable,xsize=2, $
        event_pro="modvalue",uvalue='megas',/all_events)

optaccellabel = widget_label(row8,value='optaccel:')
optacceltext = widget_text(row8, $
	value = optaccel,/editable,xsize=2, $
        event_pro="modvalue",uvalue='optaccel',/all_events)

optcmflabel = widget_label(row8,value='optcmf:')
optcmftext = widget_text(row8, $
	value = optcmf,/editable,xsize=2, $
        event_pro="modvalue",uvalue='optcmf',/all_events)
endif else begin

;  optmod='F'
;  optlucy='T'
;  megas='F'
;  optaccel='T'
;  optcmf='T'

endelse  


row9 = widget_base(base1,/row)
vturblabel = widget_label(row9,value='  vturb:')
vturbtext = widget_text(row9, $
	value = vturb,/editable,xsize=8, $
        event_pro="modvalue",uvalue='vturb',/all_events)

zlabel = widget_label(row9,value='Z:')
ztext = widget_text(row9, $
	value = z,/editable,xsize=10, $
        event_pro="modvalue",uvalue='z',/all_events)

if keyword_set(full) then begin
lineslabel = widget_label(row9,value='lines:')
linestext = widget_text(row9, $
	value = lines,/editable,xsize=2, $
        event_pro="modvalue",uvalue='lines',/all_events)

linesinmodellabel = widget_label(row9,value='lines_in_model:')
linesinmodeltext = widget_text(row9, $
	value = linesinmodel,/editable,xsize=2, $
        event_pro="modvalue",uvalue='linesinmodel',/all_events)
endif else begin

;  lines='T'
;  linesinmodel='T'

endelse

if keyword_set(full) then begin
row10 = widget_base(base1,/row)
enatcorlabel = widget_label(row10,value='enatcor:')
enatcortext = widget_text(row10, $
	value = enatcor,/editable,xsize=2, $
        event_pro="modvalue",uvalue='enatcor',/all_events)

expansionlabel = widget_label(row10,value='expansion:')
expansiontext = widget_text(row10, $
	value = expansion,/editable,xsize=2, $
        event_pro="modvalue",uvalue='expansion',/all_events)

setfirstlabel = widget_label(row10,value='set_first:')
setfirsttext = widget_text(row10, $
	value = set_first,/editable,xsize=3, $
        event_pro="modvalue",uvalue='set_first',/all_events)

setsteplabel = widget_label(row10,value='set_step:')
setsteptext = widget_text(row10, $
	value = set_step,/editable,xsize=3, $
        event_pro="modvalue",uvalue='set_step',/all_events)

row11 = widget_base(base1,/row)
clflabel = widget_label(row11,value='enhfac.:')
clftext  = widget_text(row11, $
	value = clf,/editable,xsize=5, $
        event_pro="modvalue",uvalue='clf',/all_events)

vclslabel = widget_label(row11,value='vcl_start:')
vclstext = widget_text(row11, $
	value = vclf_s,/editable,xsize=5, $
        event_pro="modvalue",uvalue='vclf_s',/all_events)

vclmlabel = widget_label(row11,value='vcl_max:')
vclmtext = widget_text(row11, $
	value = vclf_m,/editable,xsize=5, $
        event_pro="modvalue",uvalue='vclf_m',/all_events)


row12 = widget_base(base1,/row)
hopflabel = widget_label(row12,value='   Hopf:')
hopftext = widget_text(row12, $
	value = hopfself,/editable,xsize=2, $
        event_pro="modvalue",uvalue='hopfself',/all_events)

row13 = widget_base(base1,/row)

qinflabel = widget_label(row13,value='  q_inf:')
qinftext = widget_text(row13, $
	value = qinf,/editable,xsize=5, $
        event_pro="modvalue",uvalue='qinf',/all_events)

q0label = widget_label(row13,value='q_0:')
q0text = widget_text(row13, $
	value = q0,/editable,xsize=5, $
        event_pro="modvalue",uvalue='q0',/all_events)

gammalabel = widget_label(row13,value='gamma:')
gammatext = widget_text(row13, $
	value = gamma,/editable,xsize=5, $
        event_pro="modvalue",uvalue='gamma',/all_events)
endif else begin

;  enatcor='T'
;  expansion='F'
;  set_first='1'
;  set_step='2'
;  clf='1.'
;  vclf_s='0.1'
;  vclf_m='0.3'


row12 = widget_base(base1,/row)

optlucylabel = widget_label(row12,value='optlucy:')
optlucytext = widget_text(row12, $
	value = optlucy,/editable,xsize=2, $
        event_pro="modvalue",uvalue='optlucy',/all_events)

hopflabel = widget_label(row12,value='   Hopf:')
hopftext = widget_text(row12, $
	value = hopfself,/editable,xsize=2, $
        event_pro="modvalue",uvalue='hopfself',/all_events)

row13 = widget_base(base1,/row)

qinflabel = widget_label(row13,value='  q_inf:')
qinftext = widget_text(row13, $
	value = qinf,/editable,xsize=5, $
        event_pro="modvalue",uvalue='qinf',/all_events)

q0label = widget_label(row13,value='q_0:')
q0text = widget_text(row13, $
	value = q0,/editable,xsize=5, $
        event_pro="modvalue",uvalue='q0',/all_events)

gammalabel = widget_label(row13,value='gamma:')
gammatext = widget_text(row13, $
	value = gamma,/editable,xsize=5, $
        event_pro="modvalue",uvalue='gamma',/all_events)
endelse

if(not keyword_set(full)) then $
button1 = widget_button(base, value='ALL Parameters',/frame, uvalue='all')
button2 = widget_button(base, value='Klick here when finished with editing', $
			/frame,uvalue='done')

widget_control, base, /realize
xmanager, 'modinput', base
end

pro modinput_event, ev
common all, allpara

if ev.select then begin
    widget_control,ev.id,get_uvalue=what
    case what of
    'all': begin
           allpara=1
	   widget_control, ev.top, /destroy
	   end
    'done': widget_control, ev.top, /destroy
    endcase
endif
end


pro modvalue, ev

widget_control,ev.id,get_uvalue=what
widget_control,ev.id,get_value=val1

common indat,model,optneupdate,opthei,istart,imore, $
  optmixed,teff,logg,rstar, $
  rmax,tmin, $
  mdot,vmin,vinf,beta,vdiv, $
  yhe,ihe, $
  optmod,optlucy,megas,optaccel,optcmf, $
  vturb,z,lines,linesinmodel, $
  enatcor,expansion,set_first,set_step, $
  clf,vclf_s,vclf_m, $
  hopfself, $
  qinf,q0,gamma

case what of
  'model': model=val1
  'optneupdate': optneupdate=val1
  'opthei': opthei=val1
  'istart': istart=val1
  'imore': imore=val1
  'optmixed': optmixed=val1
  'teff': teff=val1
  'logg': logg=val1
  'rstar': rstar=val1
  'rmax': rmax=val1
  'tmin': tmin=val1
  'mdot': mdot=val1
  'vmin': vmin=val1
  'vinf': vinf=val1
  'beta': beta=val1
  'vdiv': vdiv=val1
  'yhe': yhe=val1
  'ihe': ihe=val1
  'optmod': optmod=val1
  'optlucy': optlucy=val1
  'megas': megas=val1
  'optaccel': optaccel=val1
  'optcmf': optcmf=val1
  'vturb': vturb=val1
  'z': z=val1
  'lines': lines=val1
  'linesinmodel': linesinmodel=val1
  'enatcor': enatcor=val1
  'expansion': expansion=val1
  'set_first': set_first=val1
  'set_step': set_step=val1
  'clf': clf=val1
  'vclf_s': vclf_s=val1
  'vclf_m': vclf_m=val1
  'hopfself': hopfself=val1
  'qinf': qinf=val1
  'q0': q0=val1
  'gamma': gamma=val1

endcase

end
