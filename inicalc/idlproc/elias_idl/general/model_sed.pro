;***********************************************************************

PRO model_sed, nu_in, nufnu_in, bandhz, z, nu_out, nufnu_out, onufnu_out, unit, nufnu0, sedmodel=sedmodel

;+
; NAME:
;	model_sed
;
; PURPOSE:
;	takes input frequencies and fluxes for an object at
;       a given redshift and returns a model sed 
;
;
; CATEGORY:
;	analysis
;
; CALLING SEQUENCE:
;	
;
; INPUTS:
;	
;         nu_in :  vector of observed frequencies Log10(Hz)
;         nufnu_in: vector of observed fluxes Log10(Wm-2Hz-1)
;         bandhz:  bandwidth  in hz
;         z :       object redshift
;         unit: unit to which to write results
;
; OPTIONAL INPUTS:
;	
;	
; KEYWORD PARAMETERS:
;	
;	sedmodel: model sed to be used: starburst, starburst2, cirrus or 
;		  Seyfert - default is starburst
;
;
; OUTPUTS:
;	nu_out:  model_sed frequencies Log10(Hz)
;       nufnu_out: fir model sed fluxes  Log10(Wm-2Hz-1)
;       nufnu0: model nuFnu values at 0.3, 0.8, 15 and 60 microns
;
; OPTIONAL OUTPUTS:
;	
;
; COMMON BLOCKS:
;	
;
; SIDE EFFECTS:
;	
;
; RESTRICTIONS:
;	
;
; PROCEDURE:
;	
;
; EXAMPLE:
;	
;
; MODIFICATION HISTORY:
; 	Written by:	Seb Oliver 30th October 1996
;	7  November 1996: modified to include new cirrus SED as well
;	11 November 1996: modified to run over full set of BC95 models	
;	17 February 1997: modified to add model keyword
;	7 March 1997: modified to add starburst2 SED
;-


nurange=[9.,16.]
nu_out=findgen(1000)/1000.*(nurange(1)-nurange(0))+nurange(0)

; c in micron s^-1 and A s^-1
log_c_mic=14.477
log_c_A=18.477
log_z1=alog10(1.+z)


;rest frame frequency of observations in Log10(hz) and 
;rest fram lambda in log10(microns)
nu_in0=nu_in+log_z1
lam_in0_mic=10^(log_c_mic-nu_in0)
lam_in_mic=10^(log_c_mic-nu_in)
lam_in_A=10^(log_c_A-nu_in)
; wavelength in A
lam_in0_A=10^(log_c_A-nu_in0)


; bandwidth in Micron
band_mic=bandhz*lam_in_mic^2/10^log_c_mic
band_A=bandhz*lam_in_A^2/10^log_c_A


log_lam_out0_mic=log_c_mic-nu_out-log_z1
log_lam_out0_A=log_c_A-nu_out-log_z1

band0_A=band_A/10^(log_z1)
band0_mic=band_mic/10^(log_z1)

; total flux in bands / divided by bandwidth
flux_in=10^nufnu_in

; set up arrays for scaling between FIR and optical
isc=findgen(60)
lgscl=(isc-30)/5.
scale=10^lgscl

;loop over Bruzual and Charlot 1995 SEDs

;spawn,'ls /iso/arch/soft/lib/bc95/*_sp*1.ised',bc95_seds
;nseds=n_elements(bc95_seds)
;minchi=fltarr(nseds,2)
;bestk=fltarr(nseds,2)
;bestl=fltarr(nseds,2)
;norml=fltarr(nseds,2,n_elements(isc),221)

; or just the 1 Gyr burst model

bc95_seds='/iso/arch/soft/lib/bc95/bc95_1gyr_sp_1.ised'
nseds=1
minchi=fltarr(nseds)
bestk=fltarr(nseds)
bestl=fltarr(nseds)
norml=fltarr(nseds,n_elements(isc),221)



;for i=0,nseds-1 do begin

;	just the one BC95 SED

	i=0

;	read in the BC95 SED and write its name to the output file

;	readbc95,bc95_seds(i),ks,iw,t,w,f	

	readbc95,bc95_seds,ks,iw,t,w,f	

	printf,unit,' '

	printf,unit,' '

	printf,unit,bc95_seds(i)

; 	erase IRAS wavebands in optical SED, and replace them by a Rayleigh-Jeans power law

	fir=where(w gt 25000)

	lastopt=fir(0)-1

	lastlam=w(lastopt)

	lastf=f(lastopt,*)

	for m=0,n_elements(fir)-1 do begin

		firentry=fir(m)

		lam=w(firentry)
		
		for j=0,n_elements(lastf)-1 do begin

			f(firentry,j)=(lastlam/lam)^4*lastf(j)

		endfor
	
	endfor

;	read in the FIR SED at the desired wavelengths

	if (keyword_set(sedmodel) eq 0) then sedmodel= 'starburst'

	if (sedmodel eq 'starburst') then begin

		printf,unit,' '

		printf,unit,'starburst SED'

		firsed=newstarburst_sed_band(lam_in0_mic,band0_mic)

	endif else begin


		if (sedmodel eq 'starburst2') then begin

			printf,unit,' '

			printf,unit,'starburst2 SED - low Z'

			firsed=starburst2_sed_band(lam_in0_mic,band0_mic)

		endif else if (sedmodel eq 'cirrus') then begin

			printf,unit,' '

			printf,unit,'cirrus SED'

			firsed=newcirrus_sed_band(lam_in0_mic,band0_mic)	
		endif else begin

			printf,unit,' '

			printf,unit,'Seyfert SED'

			firsed=seyfert_sed_band(lam_in0_mic,band0_mic)

		endelse

	endelse

; 	initialise model nu fnu parameters
	model=flux_in*0.
	good=where(nu_in ne 0. and nufnu_in ne 0. and bandhz ne 0.)
	chi2=fltarr(n_elements(isc),ks)


;	loop over SEDs in file

	for k=0l,ks-1l do begin

;		integrate over passbands

		bc95=bc95_sed_band(f(*,k),w,lam_in0_A,band0_A)

;		loop over optical-IR scalings			

		for l=0,n_elements(isc)-1 do begin

; 			converting model from in-band flux to observed nuFnu
  
			model(good)=(firsed(good)*scale(isc(l))+bc95(good)) * 10^nu_in(good) / bandhz(good)

			norml(i,l,k)=total(flux_in(good))/total(model(good))
  			model=model*norml(i,l,k)
  			chi2(l,k)=total((model(good)-flux_in(good))^2)

		endfor
	
	endfor

	minchi(i)=min(chi2,iminchi2)
	bestk(i)=iminchi2/n_elements(isc)
	bestl(i)=iminchi2-(n_elements(isc)*bestk(i))

	printf,unit,minchi(i),scale(bestl(i)),t(bestk(i))

;endfor


;return best fit SED to be plotted by sed.pro and also get restframe nuFnu values at 0.3, 
;0.8, 15 and 60 microns

lam_opt=[3000.0,8000.0,150000.0,600000.0]
lam_fir=[0.3,0.8,15.0,60.0]

bestfit=min(minchi,iminchi)
;ir=iminchi/nseds
;op=iminchi-nseds*ir

op=0

epoch=bestk(iminchi)
scaleno=isc(bestl(iminchi))
bestscale=scale(scaleno)

;normalisation=norml(op,ir,scaleno,epoch)
normalisation=norml(op,scaleno,epoch)

readbc95,bc95_seds,ks,iw,t,w,f	

; erase IRAS wavebands in optical SED, replacing them by a Rayleigh-Jeans power law
	
fir=where(w gt 25000)

;f(fir,*)=0.

lastopt=fir(0)-1

lastlam=w(lastopt)

lastf=f(lastopt,*)

for i=0,n_elements(fir)-1 do begin

	firentry=fir(i)

	lam=w(firentry)

	for j=0,n_elements(lastf)-1 do begin

		f(firentry,j)=(lastlam/lam)^4*lastf(j)

	endfor

endfor

bc95_nufnu=bc95_sed_band(f(*,epoch),w,10^log_lam_out0_A)

nufnu0_opt=bc95_sed_band(f(*,epoch),w,lam_opt)

if (sedmodel eq 'starburst') then begin

	fir_nufnu=newstarburst_sed_band(10.0^log_lam_out0_mic,/radio)

	nufnu0_fir=newstarburst_sed_band(lam_fir,/radio)

endif else if (sedmodel eq 'starburst2') then begin

	fir_nufnu=starburst2_sed_band(10.0^log_lam_out0_mic,/radio)	
	
	nufnu0_fir=starburst2_sed_band(lam_fir,/radio)

endif else if (sedmodel eq 'cirrus') then begin

	fir_nufnu=newcirrus_sed_band(10.0^log_lam_out0_mic)	
	
	nufnu0_fir=newcirrus_sed_band(lam_fir)

endif else begin

	fir_nufnu=seyfert_sed_band(10.0^log_lam_out0_mic)

	nufnu0_fir=seyfert_sed_band(lam_fir)

endelse

nufnu_out=alog10((fir_nufnu*bestscale+bc95_nufnu)*normalisation)

onufnu_out=nufnu_out

onufnu_out=alog10(bc95_nufnu*normalisation)

nufnu0=alog10((nufnu0_fir*bestscale+nufnu0_opt)*normalisation)

; work out predicted SCUBA fluxes at 450 and 850 um

scuba1=450.0/(1.+z) & scuba2=850.0/(1.+z)

scuba_nu1=2.998d8/450.0d-6  & scuba_nu2=2.998d8/850.0d-6 

scuba_lam=[1.0,scuba1,scuba2]

nufnu0_scuba=newstarburst_sed_band(scuba_lam,/radio)

nufnu0_scuba=nufnu0_scuba*bestscale*normalisation

scuba_flux1=nufnu0_scuba(1)/scuba_nu1*1.d26*1000.0

scuba_flux2=nufnu0_scuba(2)/scuba_nu2*1.d26*1000.0

print,'predicted flux at 450 um = ',scuba_flux1,' mJy' 

print,'predicted flux at 850 um = ',scuba_flux2,' mJy' 

;save

END





