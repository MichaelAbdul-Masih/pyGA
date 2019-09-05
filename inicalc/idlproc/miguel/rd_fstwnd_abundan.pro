pro rd_fstwnd_abundan,dir=dir,abun

  filename = 'ABUNDAN'
  iel = 30    ; number of chemical elements considered in FASTWIND 8.5.2

  idel = ['H','He','Li','Be','B','C','N','O','F','Ne','Na','Mg', $
	'Al','Si','P','S','Cl','Ar','K','Ca','Sc','Ti','V', $
	'Cr','Mn','Fe','Co','Ni','Cu','Zn']

  nxh = dblarr(iel) + 0.d0
  massf = dblarr(iel) + 0.d0
  
  fich = dir+filename

  get_lun,u1
  openr,u1,fich
  sc = ''
  x1 = 0.d0
  x2 = 0.d0
  x3 = 0.d0
  x4 = 0.d0
  x5 = 0.d0
  x6 = 0.d0
  for k=0,2 do begin
    readf,u1,sc
  endfor  
  for k=0,4 do begin
    readf,u1,x1,x1,x2,x2,x3,x3,x4,x4,x5,x5,x6,x6
    j = indgen(6) + k*6
    x = [x1,x2,x3,x4,x5,x6]
    nxh(j) = x
  endfor
  for k=0,12 do begin
    readf,u1,sc
  endfor
  for k=0,4 do begin
    readf,u1,x1,x1,x2,x2,x3,x3,x4,x4,x5,x5,x6,x6
    j = indgen(6) + k*6
    x = [x1,x2,x3,x4,x5,x6]
    massf(j) = x
  endfor

  close,u1
  free_lun,u1


  abun = { labl : strarr(iel), nxh : dblarr(iel), $
	massf : dblarr(iel) }

  abun.labl  = idel
  abun.nxh   = nxh
  abun.massf = massf
  return

end  
  
  
    
