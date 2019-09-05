pro color

window,colors=32
wdelete,0

red=255*[0,0,1,0,1,1,0,1,0.7]
blue=255*[0,0,0,0,0,1,1,1,0.7]
green=255*[0,0,0,1,1,0,1,1,0.7]

;red(0)=54
;green(0)=67
;blue(0)=141

tvlct,red,green,blue
print,'1: black'
print,'2: red'
print,'3: green'
print,'4: yellow'
print,'5: magenta'
print,'6: light blue'
print,'7: white'
return
end
