PRO Rartemio,file,w,f,text,ncol=ncl,plot=pl,cdw=dw,delmax,delmin,HELP=he

	IF KEYWORD_SET(he) THEN BEGIN
	  PRINT,'ADAPTED FROM A PREVIOUS PROGRAM BY ILU'
	  PRINT,' '
	  PRINT,'PROCEDURE TO READ AN ASCII SPECTRUM WITH COMMENTS'
	  PRINT,'AT THE BEGINNING AND AN UNKOWN NUMBER OF ROWS'
	  PRINT,'DEFAULT NUMBER OF COLUMNS IS FOUR (2 WAVES, 2 FLUXES)'
	  PRINT,'IT IS ASSUMED THAT THE SPECTRUM BEGINS WHEN THE'
	  PRINT,'PROGRAM FINDS ONLY NUMBERS, POINTS, THE + AND -'
	  PRINTS,'SIGNS AND THE E AND D LETTERS'
	  PRINT,''
	  PRINT,'CALLING SEQUENCE: RASCII_LIN,FILE,W,F,TEXT,DELMAX,DELMIN'
	  PRINT,''
	  PRINT,'"FILE" IS THE ASCCI FILE TO BE READ'
	  PRINT,'"W" & "F" ARE THE WAVELENGTH AND THE FLUX ARRAYS'
	  PRINT,'KEYWORDS: "CDW" (TO CALCULATE THE MAXIMUM AND MINIMUM  AMS'
	  PRINT,'	   PRO PIXELS AND PUT THEM IN "DELMAX" AND "DELMIN"'
	  PRINT,'          "PLOT" PARA DIBUJAR EL ESPECTRO'
	  PRINT,'          NUMBER OF COLUMNS DIVIDED BY TWO (DEFAULT IS 2)'
	  RETURN
	ENDIF
	if(keyword_Set(ncl)) then nc= ncl else nc=2
	wf=DBLARR(nc,60000)
	WDUMMY= ''
	sdummy=[' ','0','1','2','3','4','5','6','7','8','9','.','-','+','E','D']
	GET_LUN,unit
	ON_IOERROR,BAD			
	TEXT=''
;Si hay error de lectura se va a BAD
	OPENR,unit,file
ragain:
	  readf,unit,wdummy
	  TEXT=[TEXT,WDUMMY]
          ldummy=strlen(wdummy)
	  for idum=0,ldummy-1 do begin
	     kdum=strmid(wdummy,idum,1)
	     icomp=where(kdum eq sdummy)
	     if (icomp(0) eq -1) then goto,ragain
	  endfor
	  NT=N_ELEMENTS(TEXT)
	  IF(NT LE 2) THEN TEXT='' ELSE TEXT=TEXT(1:NT-2)
	  wdummy= strtrim(wdummy,1)
	  iblank= strpos(wdummy,' ')
	  w0= double(strmid(wdummy,0,iblank))
	  ilast=strlen(wdummy)-iblank
	  w1= double(strmid(wdummy,iblank+1,ilast-1))
	READF,unit,wf
;
;lee el fichero hasta que encuentre el final dando un error que lo envia a BAD
	BAD:
;
;
;	wf2=wf(where (wf NE 0.) )	;WF2 ES AHORA UN VECTOR
;convertir el vector wf2 en una matriz de dimensiones 2 y N_ELEMENTS(wf2)/2	
;	wf3=REFORM(wf2,2,N_ELEMENTS(wf2)/2)
;	w=wf3(0,*)
;	f=wf3(1,*)
;
	pn=WHERE (wf(0,*) NE 0.,n)
	w=reform(wf(0,0:n-1))
	f=reform(wf(1,0:n-1))	   		
	w=[w0,w]
	f=[w1,f]
	CLOSE,unit
	FREE_LUN,unit
	if(keyword_set(pl)) then PLOT,w,f
	n=N_ELEMENTS(w)
	w=w(0:n-1)
	f=f(0:n-1)
	IF (KEYWORD_SET(dw)) THEN BEGIN
	  delmax=0.00001
	  delmin=5.0
	  FOR i=0l,n-2 DO BEGIN
	     del=w(i+1)-w(i)
	     delmax=MAX([del,delmax])
	     delmin=MIN([del,delmin])
	ENDFOR
	ENDIF
	
	END
	
