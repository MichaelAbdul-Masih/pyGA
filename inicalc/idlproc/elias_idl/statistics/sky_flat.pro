PRO SKY_FLAT, SKY, FLAT, FLATVAR, method=method, niter=niter, $
              bin=bin, dig_wid=dig_wid,hist=hist,gparams=gparams,$
              mask=mask, smooth=smooth, $
              stdev=stdev, fwhm_sig=fwhm_sig, fwtm_sig=fwtm_sig, $
              cum2_sig=cum2_sig, cum3_sig=cum3_sig

;+ 
;  NAME:
;       SKY_FLAT
;  PURPOSE: 
;
;       Takes a 3-D image assumed (i,j,t) 
;       Computes  estimates of the 2-D "sky" level from a
;
;
;  CATEGORY:
;          Statistics
;  CALLING SEQUENCE: 
;          SKY_FLAT, SKY,   FLAT, FLATVAR , method=method, niter=niter
;  INPUTS: 
;
;    SKY:  Array of data values (3-D)
;
;  OPTIONAL INPUT PARAMETERS: 
;    
;  KEYED INPUTS: 
;      method :  passed to sky_stats
;      niter  :  number of iterations for trimming and estimating binsize
;      bin    :  if set to a value on input this is the binsize used
;             :  if set to a variable then on output it is array with the
;                binsize used at each pixel
;      dig_wid:  if set then the binsize is set so that digitisation
;                at the level of dig_wid is not a problem
;                dig_wid is an array with same size as cross section of sky
;                or a scalar
;      mask:     Array with 0 indicating value to be used
;      
;  KEYED OUTPUTS: 
;
;      hist: returns the last histogram used
;      gparams: returns the last gaussian parameters fitted used
;
;  OUTPUTS: 
;    FLAT: Estimate of 2-D sky/flat level
;    FLATVAR:      "    "  variance about this average
; 
;  OPTIONAL OUTPUT PARAMETERS: 
;
;  EXAMPLE: 
;
;  ALGORITHM: 
;
;  DEPENDENCIES: 
;
;  COMMON BLOCKS: 
;
;  SIDE EFFECTS: 
;
;  RESTRICTIONS: 
;        UNTESTED
;        Gaussuian fitting only option
;        Does not yet work on 2-D arrag e.g. PHOT data
;        Doesn not check for errors in Sky_stats
;
;  CALLED PROCEDURES AND FUNCTIONS: 
;  MODIFICATION HISTORY: 
;     21-Jun-1996  Seb Oliver
;     25 Nov 1998  outputs converted to scalars if only 1 element present
;		Steve Serjeant
;     10-Feb-1999  Seb Oliver mask added
;     22-Jul-1999 FWHM and cumalant quartiles returned Seb Oliver
;-
 
; ------------------------------------------------------------
;  common blocks 
; ------------------------------------------------------------
 

; ------------------------------------------------------------
;  on error conditions
; ------------------------------------------------------------
 ON_ERROR,  2
 

; ------------------------------------------------------------
;  parameter checking
; ------------------------------------------------------------
   if not keyword_set(method) then method='gfit'
   dims=size(sky)
   if dims(0) ne 3 or n_params() lt 2  then begin
     message, 'Calling sequenc: sky_flat, sky{3-D}, flat, flatvar'
   endif
   if not keyword_set(niter) then niter=1
   if keyword_set(bin) then bin0=bin else bin0=0
;; If user has supplied a scalar for dig_wid then the variable
;; "constant_dig" is set to 1
   constant_dig = 0B
   if keyword_set(dig_wid) then begin
      dsiz=size(dig_wid) 
      if(dsiz((size(dsiz))(1)-1) eq 1) then begin
         digi = dig_wid 
         constant_dig = 1B
      endif else if(dsiz(1) ne dims(1) or dsiz(2) ne dims(2))then message,$
        'Dig_wid must by 2-D image with cross section of sky cube'
   endif else digi=0

; ------------------------------------------------------------
;  function body
; ------------------------------------------------------------


  flat=fltarr(dims(1),dims(2)) & flatvar=flat & bin=flat
  stdev = fltarr(dims(1),dims(2))
  fwhm_sig = fltarr(dims(1),dims(2))
  fwtm_sig = fltarr(dims(1),dims(2))
  cum2_sig = fltarr(dims(1),dims(2))
  cum3_sig = fltarr(dims(1),dims(2))

  for j=0,dims(2)-1 do begin
     for i=0,dims(1)-1 do begin
        s1d=sky(i,j,*)
        if keyword_set(mask) then s1d=s1d(where(mask(i,j,*) eq 0L))
; first do a quick pass with to set initial trimming limits
        binsize=bin0
        if (keyword_set(dig_wid) and not constant_dig) then $
                digi=dig_wid(i,j)

        sky_stats,s1d,skymean,skyvar,method=method, $
                  binsize=binsize, dig_wid=digi

        for iter=0, niter-1 do begin

; 5 sigma clipping
           skysig=sqrt(skyvar)
           slow=skymean-5.*skysig & shi=skymean+5.*skysig
           use=where(s1d ge slow and s1d le shi)           
           no_use=where(s1d lt slow or s1d gt shi)

; Fitting to clipped data
           binsize=bin0
           sky_stats,s1d(use),skymean,skyvar,method=method,$
                     /initial,binsize=binsize,dig_wid=digi,hist=hist,gparams=gparams, $
            fwhm=fwhm, fwtm=fwtm, cum_sigmas2=cum_sigmas2, moms=moms
           bin(i,j)=binsize
           fwhm_sig[i, j] = (fwhm[1]-fwhm[0])/(2.*sqrt(2.*alog(2.)))
           fwtm_sig[i, j]= (fwtm[1]-fwtm[0])/(2.*sqrt(2.*alog(10.)))
           cum_sigmas = (cum_sigmas2-cum_sigmas2[3])/[-3., -2., -1., 1., 1., 2., 3.]
           cum2_sig[i, j] = cum_sigmas[5]
           cum2_sig[i, j] = cum_sigmas[6]
           stdev [i, j] = sqrt(moms[1])

        endfor

; error checking
;        if !error ne 0 or !err ne 0 then begin
;            print, !err_string + 'at:', i, j
;           !error = 0 & !err =0 
;        endif

        flat(i,j)=skymean
        flatvar(i,j)=skyvar
        

     endfor
  endfor

;; Added conversion to scalars if 1 element only present.
;; S Serjeant 25 Nov 1998.
 if(n_elements(flat) eq 1) then flat = scalar(flat)
 if(n_elements(flatvar) eq 1) then flatvar = scalar(flatvar)

 END


