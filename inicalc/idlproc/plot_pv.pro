pro plot_pv,model,star,ext=ext,vsini=vsini

  loadct,12
  if not keyword_set(vsini) then vsini=0.

;  nm=(size(model,/dim))(0)
;  nex=(size(model,/dim))(0)
  nm=n_elements(model)
  nex=n_elements(ext)

  if nm ne nex then begin
    print,'dimensions not compatible'
    return
  endif
   
  if nm eq 1 then begin
    halpha,model+'/OUT.PV_1_'+ext,x,y,vsini=vsini
    return
  endif

  if(nm eq 2) then begin
  halpha,model(1)+'/OUT.PV_1_'+ext(1),x1,y1,vsini=vsini
  halpha,model(0)+'/OUT.PV_1_'+ext(0),x,y,vsini=vsini
  maxy=max([y,y1])
  plot,x,y,yrange=[0,maxy]
  oplot,x1,y1,col=200
  return
  endif

  if(nm eq 3) then begin
  halpha,model(2)+'/OUT.PV_1_'+ext(2),x2,y2,vsini=vsini
  halpha,model(1)+'/OUT.PV_1_'+ext(1),x1,y1,vsini=vsini
  halpha,model(0)+'/OUT.PV_1_'+ext(0),x,y,vsini=vsini
  maxy=max([y,y1,y2])
  plot,x,y,yrange=[0,maxy]
  oplot,x1,y1,col=200
  oplot,x2,y2,col=100
  return
  endif

end  
