pro mkjob,majorcat=majorcat,vmic1=vmic1
common mkj, jn, vmic_st

set=1
if not keyword_set(majorcat) then begin
  set=0
  majorcat=' '
  spawn,'pwd >outdir.idl'
  openr,4,'outdir.idl'
  readf,4,majorcat
  print,majorcat
  close,4
  spawn,'rm outdir.idl'
endif

vmic_st='10'

jnstart:
base = widget_base(title='make job',/frame,/column)
label = widget_label(base,value=' give in jobname (example: test)')
text =  widget_text(base, $
	  value = '',/editable,xsize=50, $
          event_pro="mkjobvalue",uvalue='jobname')
widget_control, base, /realize
widget_control, text,/input_focus
xmanager, 'mkjob', base
if strtrim(jn,2) eq '' then begin
print,'specify job-file'
goto, jnstart
endif

openw,3,jn
start:
indat=dialog_pickfile(path=majorcat, get_path=dir,filter='INDAT.DAT', $
		      tit='select INDAT.DAT file')
if(indat eq '') then begin
      print,' no file chosen!'
      close,3
      return
endif

if set eq 0 then begin
  len=strlen(majorcat)
  len1=strlen(dir)
  dir=strmid(dir,len+1,len1-len-2)
endif else begin  
  l1=strpos(dir,majorcat)
  if l1 eq -1 then begin
    print,'wrong input of majorcat, try agin'
    close,/all
  return
  endif
  dir=strmid(dir,l1)
  len=strlen(dir)
  dir=strmid(dir,0,len-1)
endelse

        printf,3,'cp ',(dir+'/INDAT.DAT'),' INDAT.DAT'
	printf,3,'pnlte_A10HHe.eo'

	printf,3,'cat >IN << EOF'
        printf,3,dir
	printf,3,'EOF'
        printf,3,'ptotout_A10HHe.eo <IN'
  
	printf,3,'cat >IN << EOF'
        printf,3,dir

 if keyword_set(vmic1) then begin

;vmic:  
print,'input value of vmic = ',vmic1	  
;  print,'Do you like a different value? (no=0 / yes=1)'
;  read,i
;  if i ne 0 and i ne 1 then goto, vmic
;    if i eq 1 then begin
;    print,' give in vmic (example: 10.)'
;    read,vmic
;    endif
  vmic=float(vmic1)
  vmic_st=strtrim(string(vmic),2)
endif else begin
   vmic_start:
   base = widget_base(title='make job',/frame,/column)
   label = widget_label(base,value='give in vmic (in km/s')
   text =  widget_text(base, $
	  value = vmic_st,/editable,xsize=50, $
          event_pro="mkjobvalue",uvalue='vmic')
   widget_control, base, /realize
   widget_control, text,/input_focus
   xmanager, 'mkjob', base
   if strtrim(vmic_st,2) eq '' then begin
     print,'specify vmic'
     goto, vmic_start
   endif  
endelse  

        printf,3,vmic_st
	printf,3,'0'
	printf,3,'EOF'
	printf,3,'pformalsol_A10HHe.eo <IN'

        printf,3,'rm ',(dir+'/CONT_FORMAL*')
        printf,3,'rm ',(dir+'/ENION')
        printf,3,'rm ',(dir+'/*POP')
        printf,3,'rm ',(dir+'/MAX_CHANGE')
        printf,3,'rm ',(dir+'/METAL_RESTART')
        printf,3,'rm ',(dir+'/MODEL')
        printf,3,'rm ',(dir+'/TEMP')
        printf,3,'rm ',(dir+'/*LA.dat')
        printf,3,'rm ',(dir+'/TAU_ROS')
	
        printf,3,'cat >>DONE_'+jn+' << EOF' 
        printf,3,dir,' finished'
	printf,3,'EOF'	

mes='more models to calculate with job '+jn+'?'
yes=dialog_message(mes,/question,/default_no)
if yes eq 'Yes' then goto, start

spawn,'chmod u+x '+jn
close,3
print,' job file: ',jn,' written'
return

end

pro mkjobvalue, ev
common mkj, jn, vmic_st

widget_control,ev.id,get_uvalue=what
widget_control,ev.id,get_value=val1

case what of
  'jobname': begin
             jn=val1
             widget_control,ev.top,/destroy 
             end
  'vmic': begin
             vmic_st=val1
             widget_control,ev.top,/destroy 
             end
endcase

end
