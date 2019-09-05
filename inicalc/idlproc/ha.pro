pro ha,file,tit
rtab,x,y,file,161,6,3,5
ymax=max(y)
ymin=min(y)
plot,x,y,xtitle='lambda (A)',ytitle='emergent profile',title=tit, $
xrange=[6540.,6580],yrange=[ymin-0.1,ymax+0.1]
return
end