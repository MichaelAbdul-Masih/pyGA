function icaspect, ratio0, margin=margin0

;;; like aspect but takes care of !p.multi 

pos = aspect(ratio0, margin=margin0)
if(!p.multi(1) le 1 and !p.multi(2) le 1) then return, pos

pmultiAspectRatio = (1.0*!p.multi(2)>1)/(1.0*!p.multi(1)>1)
ratio = ratio0 * pmultiAspectRatio

pos = aspect(ratio, margin=margin0)


pmx = !p.multi(1)>1
pmy = !p.multi(2)>1

pwin = !p.multi(0)

if(pwin eq 0) then $
	ypos = pmy-1 $
else $
	ypos = (pwin-1)/pmx 

if(pwin eq 0) then $
	xpos = 0 $
else $
	xpos = pmx-1-((pwin-1) - ypos*pmx)


xmin = pos(0)
ymin = pos(1)
xmax = pos(2)
ymax = pos(3)


boxsize_x = 1.0/pmx
boxsize_y = 1.0/pmy

xmin = xmin*boxsize_x + xpos*boxsize_x
xmax = xmax*boxsize_x + xpos*boxsize_x
ymin = ymin*boxsize_y + ypos*boxsize_y
ymax = ymax*boxsize_y + ypos*boxsize_y


pos = [xmin, ymin, xmax, ymax]

return, pos
end
