;
; Procedure to read a FASTWIND spec file (two columns ascii file with a header)
;
; Honolulu, Feb 2005
;
; v0
; v1
; v1.1   June 2006, output format changed to (x,y)

pro rd_fstwndspec,dir=dir,tmpdir=tmpdir,head=head,file,xx,yy

if not keyword_set(dir) then dir='./'
if not keyword_set(tmpdir) then tmpdir='./tmp/'

fich = dir+file
i = strpos(file,'.gz')
if i ge 0 then begin
    spawn,'gunzip '+fich
    fich = dir+strmid(strtrim(file,2),0,i)
;;    print,fich
;;    stop
endif    

sc=' '
i = 0l
x = 0.d0
y = 0.d0
verdad = 1

get_lun,u1
openr,u1,fich 	; reading the header
while ( not eof(u1) and verdad ) do begin
    readf,u1,sc
    sc=strtrim(sc,2)  ;; & print,sc
    if (strmid(sc,0,4) eq 'data') then verdad=0 $ 	
      else i=i+1
endwhile
ihead = i+1
header = strarr(ihead) + ''

readf,u1,idata ; number of data rows
;;dat={ l : dblarr(idata), f : dblarr(idata)}

xx = dblarr(idata) + 0.d0
yy = dblarr(idata) + 0.d0

if keyword_set(head) then begin
    close,u1
    openr,u1,fich
    for k=0l,ihead - 1l do begin
    	readf,u1,sc
	header(k) = sc 
    endfor
    head = header
endif    	
for i=0l,idata-1l do begin
    readf,u1,x,y     
;;    dat.l(i) = x
;;    dat.f(i) = y
    xx(i) = x
    yy(i) = y
endfor
close,u1
free_lun,u1

s=findfile(fich+'.gz')
if s(0) eq '' then $
   spawn,'gzip '+fich

return
end
    
