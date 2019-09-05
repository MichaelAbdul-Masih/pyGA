function fstwnd_version,dir=dir

  if not keyword_set(dir) then dir='./'
  filename = 'FASTWIND.LOG'
  fich = dir + filename

  sc = ''
  ver = ''
  
  get_lun,u1
  openr,u1,fich

  verdad = 1
  while verdad and not eof(u1) do begin
    readf,u1,sc
    i = strcmp(sc,'VERSION',/FOLD_CASE)
    if i ne -1 then verdad = 0
  endwhile
  if verdad then ver = '' $
   else begin      
    sc = strtrim(sc,1)
    ib = strpos(sc,'VERSION')
    il = strlen(sc)
    sc = strtrim(strmid(sc,ib+7,il-ib-7),1)
    ib = strpos(sc,' ')
    ver = strtrim(strmid(sc,0,ib),2)
  endelse
  
  close,u1
  free_lun,u1

  return,ver

end  

  
  
