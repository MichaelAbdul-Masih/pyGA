pro cont,lam,prof,profnew
cursor,x1,y1
stop
cursor,x2,y2
a=(x1-x2)/(y1-y2)
b=y1-a*x1
profnew=prof
contnew=prof
contnew=a*lam+b
profnew=prof/contnew
oplot,lam,profnew
return
end