PRO ILUPAL
;
;	VERSION 1.03
;	            (27-FEB-98)
;
;	REPITE LA TABLA DE COLORES DEFINIDA CON R,G,B DE TAL
;	MODO QUE EL ULTIMO COLOR SEA SIEMPRE BLANCO.
;	PARA DEFINIR UNA NUEVA TABLA BASTA ANYADIR VALORES
;	A R,G,B.
;	POR ALGUNA RAZON, EL MAXIMO NUMERO DE COLORES QUE PERMITE DEFINIR
;	IDL NO ES 256, SINO QUE PARECE PARECE DEPENDER DE ALGUNA
;	INICIALIZACION. POR ELLO, SE HACE NECESARIO AVERIGUAR
;	PREVIAMENTE ESE MAXIMO. 
;
; 30 orange 40 magenta 50 tuerkis 60 gelb 70 gruen 80 blau 90 rot 100 weiss

x1=255
x2=255
x3=255
tvlct,x1,x2,x3
tvlct,x1,x2,x3,/get
;
maxcol= n_elements(x1)
white= 255B
;
red= bytarr(maxcol)
green= bytarr(maxcol)
blue= bytarr(maxcol)
;
R=[0,255,234,0  ,0  ,255,0  ,255,255,0  ,175]
G=[0,255,0  ,140,216,235,235,0  ,148,126,0  ]
B=[0,255,0  ,234,0  ,0  ,228,200,0  ,0  ,201]
;
ncol= n_elements(r)
nrep= (maxcol-1)/ncol
nrep= fix(nrep)
for i= 0, nrep do begin
   k1= i*ncol
   k2= min([(i+1)*ncol-1,maxcol])
   k3= k2-k1-1
   red(k1:k2-1)= byte(R(0:k3))
   green(k1:k2-1)= byte(G(0:k3))
   blue(k1:k2-1)= byte(B(0:k3))
endfor
red(maxcol-1)= white
green(maxcol-1)= white
blue(maxcol-1)= white
;
;TVLCT, R, G, B
TVLCT, red, green, blue
;
END
