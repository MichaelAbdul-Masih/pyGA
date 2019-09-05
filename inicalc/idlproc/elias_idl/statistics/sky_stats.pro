PRO SKY_STATS, SKY0,  SKYMEAN,  SKYVAR, SKYSKEW, SKYKUR, METHOD=METHOD, $
               HIST=HIST, binsize=binsize, g1=g1, chi2=chi2, initial=initial, $
               dig_wid=dig_wid,gparams=gparams, ierr=ierr, nozerofit=nozerofit, $
               fwhm=fwhm, cum_sigmas2=cum_sigmas2, moms=moms, fwqm=fwqm, fwtm=fwtm
;+ 
;  NAME:
;       SKY_STATS
;  PURPOSE: 
;
;       Computes  estimates of the "sky" level from a
;       data array, together with variance and in some cases
;       skewness and kurtosis
;       Any triming (e.g. for Cosmic rays) should be done before
;       hand i.e. this can be called iteratively
;
;
;
;  CATEGORY:
;          Statistics
;  CALLING SEQUENCE: 
;          SKY_STATS, SKY0,  SKYMEAN,  SKYVAR, [SKYSKEW, SKYKUR, 
;          METHOD=METHOD, HIST=HIST, binsize=binsize, g1=g1, chi2=chi2, 
;          /initial]
;  INPUTS: 
;
;    SKY0:  Array of data values (any dimension)
;
;  OPTIONAL INPUT PARAMETERS: 
;    
;  KEYED INPUTS: 
;    METHOD: Method for computing mean (Default gfit)
;          = moments  standard moments analysis
;          = mmm      DAOphot mmm method
;          = gfit     Gaussian Fit to histogram
;          = qgfit    Gaussian Fit + quadratic to histogram
;          = wgfit    Gaussian Fit weighted by number in bins
;    HIST:  If set HIST is a (2 by N) array containg a histogram of the counts
;    BINSIZE: If set then is  the width of the bins used
;           : if set to a variable with value 0 then the binsize used is returned
;    G1:  If set returns gaussian fitting function
;    CHI2: CHI^2 of Gaussian fit
;    INITIAL: If set then the values of skymean and skyvar are used to 
;             as initial guess in order to estimate bin widths
;             otherwise a moments analysis is used
;             N.B. After the initial guess the binsize is then 
;                  further adjusted by estimating the variance
;                  from the peak of the histogram
;    DIG_WID: For digitised data this specifies the digitisation
;             levels and bin widths are not allowed to drop below this
;             Also the data values are "blured" on the digitisation scale
;             using a uniform random distribution.
;    NOZEROFIT: Suppress fitting of empty bins in histogram
;
;    FWHM:  estimate of the start and end position of the Half maxima
;          points i.e. real
;          FWHM=FWHM[1]-FWHM[0]=2.*sqrt(2.*alog(2.))*sigma for a Gaussian
;    FWQM:  estimate of the start and end position of the Half maxima
;          points i.e. real
;          FWQM=FWQM[1]-FWQM[0]=2.*sqrt(2.*alog(4.))*sigma for a Gaussian
;
;    CUM_SIGMAS2:  estimates of the 
;      [0.0013498980,0.022750132,  0.15865525, 0.50000000, 0.84134475,
;      0.97724987 , 0.99865010]  quartiles (corresponding to [-3,-2,-1,0,1,2,3] sigma
;      for a Gaussian) from the cumulative distribution
;
;    moms:  resturns output from MOMENT function
;
;  OUTPUTS: 
;    SKYMEAN: Estimate of average sky level
;    SKYVAR:      "    "  variance about this average
; 
;  OPTIONAL OUTPUT PARAMETERS: 
;    SKYKUR:      "    "  kurtosis
;    SKYKUR:      "    "  kurtosis
;    IERR: =0 initially
;          =ierr+1 if too few points
;          =ierr+2 if all data the same
;          =ierr+4 if iccurvefit failed to converge
;          =ierr+8 on failiure in calculations of fwhm etc. 
;
;  EXAMPLE:
;
; IDL> ran=randomn(seed,100000)*2.
; IDL> mo=moment(ran)
; IDL> print,mo(0),sqrt(mo(1))/sqrt(100000)
; IDL> sky_stats,ran,sm,'gfit',g1=g1,hist=h1
; IDL> print,sm
; IDL> plot,h1(0,*),h1(1,*),psym=10
; IDL> oplot,h1(0,*),g1
; IDL> ran1=round(ran)
; IDL> sky_stats,ran,sm1,'gfit',g1=g1,hist=h1,dig_wid=1.
; IDL> ran2=round(ran)+0.1
; IDL> sky_stats,ran2,sm2,'gfit',g1=g1,hist=h1,dig_wid=1.
; IDL> print,mo(0),sqrt(mo(1))/sqrt(100000),sm,sm1,sm2
;
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
;        Gaussuian fitting at moment allows a second order fit to the
;        background under the Gaussian
;  CALLED PROCEDURES AND FUNCTIONS: 
;  MODIFICATION HISTORY: 
;     21-Jun-1996  Seb Oliver
;      6-Feb-1997 Blurring of digitised data introduced, for simplicity
;                 and speed this is done using a randomisation approach.
;     13-mar-1998 check for few elements or all data the same value Seb
;     16-Mar-1998 NOZEROFIT keyword and IERR flag added - Steve Serjeant
;                 Also uses iccurvefit instead of curvefit 
;     7-Sept-1998 Bug fix in nozerofit option - previously the option
;		  was never being implemented. S Serjeant
;     19-May-1999 Outputs useful vbles when too few points to fit 
;		  or data all the same S Serjeant
;     22-Jul-1999 FWHM and cumalant quartiles returned Seb Oliver v1.2
;     26-Jul-1999 Added checks for >1 element in interpol calls.
;                 Having 1 element may be symptomatic of other errors
;                 so added ierr = ierr+8 in these and related
;                 situations. 
;                 Stephen Serjeant version 1.3
;     28 Aug 1999 Changed loop variables from int to long
;                 s serjeant version 1.4
;-
   
; ------------------------------------------------------------
;  common blocks 
; ------------------------------------------------------------
   

; ------------------------------------------------------------
;  on error conditions
; ------------------------------------------------------------
   ON_ERROR,2
   

; ------------------------------------------------------------
;  parameter checking
; ------------------------------------------------------------
   if not keyword_set(method) then method='gfi'


   ierr=0

   case strlowcase(strmid(method,0,3)) of
      'mom': imeth=1
      'mmm': imeth=2
      'gfi': imeth=3
      'qgf': imeth=4
      'wgf': imeth=5
      else: begin
         print,'Using Gaussian Fit to Histogram'
         imeth=3
      end
   endcase


   if not keyword_set(dig_wid) then begin
      dig_wid=0.
      sky=sky0
   endif else begin
      blur=(randomu(seed,n_elements(sky0))-0.5)*dig_wid
      sky=sky0+blur
   endelse


; ------------------------------------------------------------
;  function body
; ------------------------------------------------------------

   if n_elements(sky) lt 4 then begin
      message,'Too few points to fit',/info
      skymean=total(sky)/n_elements(sky)
      if n_elements(sky) gt 1 then $
       skyvar=total((sky-skymean)^2)/(n_elements(sky)-1)$
      else skyvar=0
      ierr = ierr + 1
      return
   endif

   minmax0=minmax(sky)
   if minmax0(0)-minmax0(1) eq 0 then begin
      message,'Data all the same',/info
      skymean=minmax0(0)
      skyvar=0
      ierr = ierr + 2
      return
   endif


; moments analysis done if specified or if skymean and skyvar from 
; calling routine are not to be used as initial guesses
   moms=moment(sky)
 
   if imeth eq 1 or (imeth gt 2 and not keyword_set(initial)) then begin

      skymean=moms(0)
      skyvar=moms(1)
      skyskew=moms(2)
      skykur=moms(3)
   endif

; doing DAOPHOT method
   if(imeth eq 2) then begin 
      mmm,sky,skymean,skyvar,skyskew
      skyvar=skyvar^2
   endif


; If we want a histogram or we are fitting a Gaussian we need the
; histogram
   if(keyword_set(hist) or imeth eq 3 or imeth eq 4 or imeth eq 5) then begin

; setting bin_size to be such that there are roughly 
; 5% Possion errors (i.e. 400 points) in a bin at 1 sigma
; although they are not allowed to get larger than 0.5 sigma    
; Also bin width is not allowed smaller than dig_wid

      if not keyword_set(binsize)  then begin 

         binsize=min([0.5, 400./n_elements(sky)/0.242])*sqrt(skyvar)

; checking that the binsize is not larger than 0.5 sigma as
; estimated from the histogram peak
;         if not keyword_set(initial) then begin
         y = round( ( sky / binsize))
         h1 = histogram( y )
         sigma=n_elements(sky)/max(h1)*binsize/sqrt(2.*!pi)
         if binsize gt 0.5 * sigma then binsize=0.5*sigma

; Setting the binsiz to an integer multiple of the 
; digitisation scale
         if(dig_wid gt 0)then binsize=(fix(binsize/dig_wid)+1.)*dig_wid
;            if binsize lt dig_wid then binsize=dig_wid

;         endif
      endif


; Compute the histogram and abcissa.

      y = round( ( sky / binsize))

      h1 = histogram( y )
      N_hist = N_elements( h1 )
      x1 = lindgen( N_hist ) * binsize + min(y*binsize) 
      cum_hist = fltarr(n_hist)
      cum_hist[0] = h1[0]
      FOR i=1L, long(n_hist)-1L DO cum_hist[i] = cum_hist[i-1]+h1[i]

      hist=transpose([[x1],[h1], [cum_hist]])

      norm_cum_hist = cum_hist/max(cum_hist)
      cum_sigmas0 = double([-3., -2., -1., 0., 1., 2., 3.])
      cum_sigmas1 = gauss_pdf(cum_sigmas0)
      if(n_elements(x1) gt 1) then $ ;; don't call interpol if only 1 element
        cum_sigmas2 = interpol(x1, norm_cum_hist, cum_sigmas1)
;      stop

      fwhm = fltarr(2)
      fwqm = fltarr(2)
      fwtm = fltarr(2)
      max_h = max(h1, imax)

      if(imax ne 0 and (n_hist-imax) ne 0) then begin
          lhs = findgen(imax)
          rhs = findgen(n_hist-imax)+imax
          lhs = lhs[sort(h1[lhs])]
          rhs = rhs[sort(h1[rhs])]


          if(n_elements(lhs) gt 1) then $ ;; don't call interpol if only 1 element
            fwhm[0] = interpol(x1[lhs], h1[lhs], max_h/2.)
          if(n_elements(rhs) gt 1) then $ 
            fwhm[1] = interpol(x1[rhs], h1[rhs], max_h/2.)
          
          if(n_elements(lhs) gt 1) then $ 
            fwqm[0] = interpol(x1[lhs], h1[lhs], max_h/4.)
          if(n_elements(rhs) gt 1) then $ 
            fwqm[1] = interpol(x1[rhs], h1[rhs], max_h/4.)
          
          if(n_elements(lhs) gt 1) then $ 
            fwtm[0] = interpol(x1[lhs], h1[lhs], max_h/10.)
          if(n_elements(rhs) gt 1) then $ 
            fwtm[1] = interpol(x1[rhs], h1[rhs], max_h/10.)

          if(n_elements(rhs) le 1 or n_elements(lhs) le 1) then begin
              message,'Data too skewed for FWHM calculation - dig_wid too big?',/info
              ierr=ierr+8
          endif
      endif else begin
          message,'Data too skewed for FWHM calculation - dig_wid too big?',/info
          ierr = ierr+8
      endelse

   endif

; Doing Gaussian Fitting Method
   if (imeth eq 3 or imeth eq 5) then begin

;      gparams=[n_elements(sky)/binsize/sqrt(skyvar*2.*!pi),$
;               skymean,sqrt(skyvar)]

; First Guess at parameters from max of histogram this is a very
; robust estimate

      dn=max(h1,imax)
      skymean=x1(imax)
      skysig=n_elements(sky)/dn*binsize/sqrt(2.*!pi)
      gparams=[dn,skymean,skysig]


      if imeth eq 3 then begin
; weights set equal
         w=replicate(1.,n_hist)
      endif else begin
; weights are 1/variance 
         icgaussian,x1,gparams,w
         w=1./w
      endelse

;;;;  IF NOZEROFIT THEN DELETE ZERO DATA POINTS
      if(keyword_set(nozerofit)) then begin
         index = where(h1 eq 0, count0)
         index2 = where(h1 ne 0, count1)
         if(count0 ne 0 and count1 ne 0) then begin
            x1 = x1(index2)
            h1 = h1(index2)
            w  = w(index2)
         endif
      endif

      good=where(w gt 0.,count)
      if (count gt 3)then begin
         curvefit_error = 0
         initial_gparams = gparams
         g1=iccurvefit(x1,h1,w,gparams,funct='icgaussian',chi2=chi2,$
                       ierr=curvefit_error)
         if(curvefit_error eq 1) then begin
            gparams = initial_gparams
            ierr = ierr + 4
         endif
      endif else begin
         ierr=ierr+1
         message,'Not enough points',/info
      endelse
      skymean=gparams(1)
      skyvar=gparams(2)^2

   endif

; Doing Gaussian + quadratic Fitting Method
   if (imeth eq 4) then begin

; First Guess at parameters from max of histogram this is a very
; robust estimate

      dn=max(h1,imax)
      skymean=x1(imax)
      skysig=n_elements(sky)/dn*binsize/sqrt(2.*!pi)
      gparams=[dn,skymean,skysig]

;;;;  IF NOZEROFIT THEN DELETE ZERO DATA POINTS
      if(keyword_set(nozerofit)) then begin
         index = where(h1 eq 0, count0)
         index2 = where(h1 ne 0, count1)
         if(count0 ne 0 and count1 ne 0) then begin
            x1 = x1(index2)
            h1 = h1(index2)
         endif
      endif

      g1=gaussfit(x1,h1,gparams)
      skymean=gparams(1)
      skyvar=gparams(2)^2
   endif

END


