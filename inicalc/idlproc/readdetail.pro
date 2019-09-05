pro readdetail, model, dat

  readkur,'$HOME/detail/struct/'+model+'_struct',dat1  
  
  file='$HOME/detail/pops/'+model+'_pop'

  nd=90
  
; only for H and He  
  openr,1,file
  a=''
  readf,1,a

; at first, H
  while (strpos(a,'ATOM H ') eq -1) do begin
    readf,1,a
  endwhile
  print,1,a

; first level H  
  readf,1,a
  str=strsplit(a,' ',/extract)
  print,str(0)
  ilev=1
  levnam=[str(0)]
  occ=fltarr(nd)
  readf,1,occ
  occnum=occ


; next levels H
  readf,1,a
  while a ne ' 0' do begin
  str=strsplit(a,' ',/extract)
  print,str(0)
  ilev=ilev+1
  levnam=[levnam,str(0)]
  readf,1,occ
  occnum=[occnum,occ]
  readf,1,a
  endwhile

  point_lun,1,0

; now He
  while (strpos(a,'ATOM HE ') eq -1) do begin
    readf,1,a
  endwhile
  print,1,a

; first level He  
  readf,1,a
  str=strsplit(a,' ',/extract)
  print,str(0)
  ilev=ilev+1
  levnam=[levnam,str(0)]
  readf,1,occ
  occnum=[occnum,occ]


; next levels He
  readf,1,a
  while a ne ' 0' do begin
  str=strsplit(a,' ',/extract)
  print,str(0)
  ilev=ilev+1
  levnam=[levnam,str(0)]
  readf,1,occ
  occnum=[occnum,occ]
  readf,1,a
  endwhile
  close,1

; finished with reading
  print
  print,' number of levels = ',ilev
  print,' levels:'
  print,levnam

  occnum=reform(occnum,nd,ilev,/overwrite)

  occ=fltarr(ilev,nd)
  for i=0,ilev-1 do begin
    occ(i,*)=occnum(*,i)
  endfor

  h1lab=where(strpos(levnam,'H1') eq 0)  
  h2lab=where(strpos(levnam,'H2') eq 0)

  he1lab=where(strpos(levnam,'HE1') eq 0)
  he2lab=where(strpos(levnam,'HE2') eq 0)
  he3lab=where(strpos(levnam,'HE3') eq 0)

  h1tot=fltarr(nd)
  h2tot=fltarr(nd)
  he1tot=fltarr(nd)
  he2tot=fltarr(nd)
  he3tot=fltarr(nd)

  for i=0,nd-1 do begin
    h1tot(i)=total(occ(h1lab,i))
    h2tot(i)=total(occ(h2lab,i))
    he1tot(i)=total(occ(he1lab,i))
    he2tot(i)=total(occ(he2lab,i))
    he3tot(i)=total(occ(he3lab,i))
  endfor  

  htot=fltarr(nd)
  hetot=fltarr(nd)

  for i=0,nd-1 do begin
    htot(i)=h1tot(i)+h2tot(i)
    hetot(i)=he1tot(i)+he2tot(i)+he3tot(i)
  endfor  
 
  ifrac=fltarr(3,2,nd)
  ifrac(0,0,*)=h1tot(*)/htot(*)
  ifrac(1,0,*)=h2tot(*)/htot(*)

  ifrac(0,1,*)=he1tot(*)/hetot(*)
  ifrac(1,1,*)=he2tot(*)/hetot(*)
  ifrac(2,1,*)=he3tot(*)/hetot(*)

ifre=(size(dat1.lam))(1)

; note: dep = occnumbers!

dat={name: '',  $
     p: fltarr(nd), t: fltarr(nd), xne: fltarr(nd), taur: fltarr(nd), $
     m: fltarr(nd), grad: fltarr(nd), $
     lam: fltarr(ifre), hnue: fltarr(ifre), trad: fltarr(ifre), $
     levnam: strarr(ilev), dep: fltarr(ilev,nd), ifrac: fltarr(3,2,nd)}


dat.name=dat1.name
dat.p=dat1.p
dat.t=dat1.t
dat.xne=dat1.xne
dat.taur=dat1.taur
dat.m=dat1.m
dat.grad=dat1.grad
dat.lam=dat1.lam
dat.hnue=dat1.hnue
dat.trad=dat1.trad
dat.levnam=levnam
dat.dep=occ
dat.ifrac=ifrac

return
end
  
