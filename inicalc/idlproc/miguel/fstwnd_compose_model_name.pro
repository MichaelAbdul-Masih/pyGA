; prepare FASTWIND model name from a set of parameters
;
function fstwnd_compose_model_name,teff,logg,vt,he,vtf,logq,beta,vterm,met
  
  steff = strtrim(string(teff),2)
  ip = strpos(steff,'.')
  steff = 't'+strtrim(strmid(steff,0,ip),2)

  slogg = strtrim(string(logg),2)
  ip = strpos(slogg,'.')
  slogg = 'g'+strtrim(strmid(slogg,0,ip),2) + strtrim(strmid(slogg,ip+1,2),2)

  svt = strtrim(string(vt),2)
  ip  = strpos(svt,'.')
  if ip ne -1 then $
     svt = 'x'+strtrim(strmid(svt,0,ip),2) $
  else svt = 'x'+strtrim(string(svt),2)    

  she = strtrim(string(he),2)
  ip  = strpos(she,'.')
  she = 'He'+strtrim(strmid(she,ip,3),2)

  svtf = strtrim(string(vtf),2)
  ip  = strpos(svtf,'.')
  if ip ne -1 then $
     svtf = 'xf'+strtrim(strmid(svtf,0,ip),2) $
  else svtf = 'xf'+strtrim(string(svtf),2)    
  
  sq = strtrim(string(logq),2)
  ip = strpos(sq,'.')
  sq = 'q' + strtrim(strmid(sq,1,ip+2),2)
  
  svterm = strtrim(string(vterm),2)
  ip = strpos(svterm,'.')
  svterm = 'vt'+strtrim(strmid(svterm,0,ip),2)

  sbeta = strtrim(string(beta),2)
  ip = strpos(sbeta,'.')
  sbeta = 'b'+strtrim(strmid(sbeta,0,ip+3),2)  
  
  head = 'p'
  met = alog10(met) 
  if met lt 0.d0 then begin
    head = 'm'
    met  = met * (-1.d0)
  endif 
  saux = strtrim(strmid(strtrim(string(met),2),0,3),2)
  ip = strpos(saux,'.')
  smet = strtrim(strmid(saux,0,ip),2) + strtrim(strmid(saux,ip+1,1),2)
  smet = head + smet      
  model_name = steff+slogg+svt+smet+she+svtf+sq+sbeta+svterm   
  
  return,model_name 

end  
