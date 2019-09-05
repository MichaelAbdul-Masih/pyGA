function ks3dsig, Zinput, Ninput, RHOinput
;+
; NAME:
;       ks3dsig
;
; PURPOSE:
;     Returns significance levels for the three dimensional
;     Kolmogorov-Smirnov test, as given in Fasano & Franceschini 1987
;     Monthly Notices of the Royal Astronomical Society 225, 155.
;     For values between those given in table B1 of this paper,
;     an interpolation is used.
;
;     Distribution is different at N percent significance if
;     the output from this routine is less than N
;     IE 99.0 is a great fit, 1.0 is a bad fit.
;     Confidence levels are 100 - output
;      
;     Stephen Serjeant Imperial College London 17 Dec 1997
;     
; CALLING SEQUENCE:
;     significance = ks3dsig(Z,N,RHO)
;
; INPUTS:
;     Zinput: 3D KS statistic, = D * sqrt(Ninput) where Ninput=sample size
;             Defined as ``absolute maximum difference (multiplied by
;                          sqrt(N)) between the observed and predicted
;                          normalised integral distributions cumulated
;                          within the eight volumes of three-dimensional
;                          space defined for each data point (Xi,Yi,Zi) by
;                          (x<Xi,y<Yi,z<Zi), ..., (x>Xi,y>Yi,z>Zi)
;                                  (i=1,...,n) ''
;     Ninput: sample size
;             For two distribution case, use N1*N2/(N1+N2)
;             both here and in convertion of D to Z
;     RHOinput: mean average Pearson correlation coefficient
;               of the 3 projections of the data 
;               (ie X vs Y, Y vs Z, and X vs Z)
;
; OPTIONAL INPUTS:
;     none
;
; KEYWORD PARAMETERS:
;       none
;
; EXAMPLE:
;     significance = ks3dsig(Z,N,RHO)
;
; MODIFICATION HISTORY:
;       Written by: Stephen Serjeant Imperial College London 17 Dec 1997
;       Mon Jan 19 1998: Interpolation of significance levels
;       is now linear in s=alog10(100-significance) instead of linear in
;       significance itself
;       Wed Jan 28 1998: linear interpolation used for confidence < 30%
;       Output now 100-previous output, ie 95 is a great fit at 0.1 is
;       a bad fit. Stephen Serjeant
;-
 
on_error, 2

if(n_params() ne 3) then begin
   print,'Usage: significance = ks3dsig(Z,N,RHO)'
   return,0
endif

Z_3d = fltarr(9,6,6) ;;; significance, n, rho

a1=[$
.992,1.05,1.10,1.16,1.22,1.30,1.42,1.53,1.75,$
1.02,1.07,1.13,1.19,1.26,1.34,1.47,1.57,1.80,$
1.06,1.12,1.18,1.24,1.31,1.39,1.52,1.63,1.84,$
1.12,1.17,1.23,1.29,1.36,1.44,1.56,1.68,1.88,$
1.15,1.21,1.27,1.33,1.41,1.48,1.60,1.71,1.88,$
1.22,1.28,1.33,1.39,1.46,1.54,1.66,1.76,1.95]
                                             
a2=[$
.960,1.01,1.07,1.12,1.19,1.27,1.39,1.51,1.74,$
.980,1.03,1.09,1.15,1.22,1.30,1.43,1.55,1.78,$
1.02,1.08,1.14,1.20,1.27,1.37,1.50,1.61,1.86,$
1.07,1.12,1.18,1.24,1.32,1.41,1.54,1.67,1.90,$
1.11,1.17,1.22,1.29,1.36,1.45,1.58,1.70,1.93,$
1.17,1.24,1.29,1.36,1.42,1.50,1.62,1.72,1.94]
                                             
a3=[$
.940,.990,1.05,1.11,1.17,1.26,1.38,1.49,1.72,$
.960,1.01,1.07,1.13,1.20,1.29,1.42,1.53,1.78,$
1.00,1.06,1.12,1.18,1.25,1.34,1.47,1.58,1.83,$
1.04,1.10,1.16,1.22,1.30,1.38,1.52,1.63,1.86,$
1.09,1.15,1.20,1.26,1.33,1.42,1.56,1.66,1.91,$
1.15,1.20,1.26,1.32,1.39,1.47,1.61,1.72,1.91]
                                             
a4=[$
.914,.970,1.02,1.09,1.15,1.24,1.36,1.48,1.72,$
.927,.985,1.04,1.10,1.17,1.26,1.39,1.51,1.76,$
.970,1.02,1.08,1.14,1.22,1.31,1.44,1.55,1.80,$
1.01,1.06,1.12,1.19,1.25,1.34,1.48,1.59,1.83,$
1.05,1.10,1.16,1.22,1.29,1.37,1.52,1.62,1.86,$
1.11,1.16,1.21,1.27,1.33,1.42,1.54,1.64,1.90]
                                             
a5=[$
.890,.942,1.00,1.06,1.12,1.21,1.33,1.45,1.66,$
.893,.946,1.00,1.06,1.13,1.22,1.36,1.48,1.74,$
.929,.982,1.04,1.10,1.17,1.26,1.40,1.52,1.79,$
.960,1.01,1.07,1.13,1.20,1.29,1.43,1.56,1.83,$
.990,1.05,1.16,1.23,1.32,1.32,1.46,1.59,1.83,$
1.06,1.11,1.16,1.22,1.29,1.37,1.48,1.60,1.85]
                                             
a6=[$
.846,.895,.953,1.01,1.08,1.16,1.30,1.41,1.63,$
.844,.896,.947,1.01,1.08,1.17,1.31,1.44,1.70,$
.867,.917,.968,1.03,1.10,1.19,1.33,1.47,1.75,$
.890,.943,.997,1.06,1.13,1.22,1.37,1.51,1.77,$
.926,.974,1.03,1.09,1.15,1.24,1.40,1.52,1.83,$
.958,1.01,1.07,1.13,1.19,1.28,1.41,1.53,1.84 ]

a=[a1,a2,a3,a4,a5,a6]

i=-1
for rho=0,5 do begin
    for n=0,5 do begin
        for sig=0,8 do begin
            i=i+1                                 
            z_3d(sig,n,rho) = a(i)
        endfor
    endfor
endfor

;;; blah blah
n=[10.0,20.0,50.0,100.0,200.0,500.0]
rho=[0.0,0.5,0.6,0.7,0.8,0.9]
significance=[30.0,40.0,50.0,60.0,70.0,80.0,90.0,95.0,99.0]
significance=alog10(100.0-significance)

;;; Now interpolate to get a significance at this Z.
;;; First, interpolate a line of the Z_3d cube for this particular
;;; correlation coefficient. Use N=Ninput, rho=RHOinput_eff and
;;; sig=significance(array). Output should be a 1D array.
;;; Perhaps use log(N) instead of N.

if(RHOinput lt 0.5)  then RHOinput_eff=0.0 else RHOinput_eff=RHOinput
if(RHOinput gt 0.95) then begin
   print,'These data are so stronly correlated that the 3D test'
   print,'breaks down. A 2D test is recommended.'
   return,0
endif

;;; first get effective array elements
ninput_i=interpol(findgen(n_elements(n)),n,Ninput)
rhoinput_eff_i=interpol(findgen(n_elements(rho)),rho,rhoinput_eff)
;;; then interpolate
result = interpolate(z_3d,$
   findgen(n_elements(significance)),ninput_i,rhoinput_eff_i,/grid)

;;;;; Second, interpolate this array to find where it equals Zinput.
if(zinput ge min(result)) then begin
   sig_interpol=interpol(significance,result,zinput)
   sig_interpol = 10.0^sig_interpol 
endif else begin
   junk=[0.0,100.0-10.0^significance(0)]
   sig_interpol=100.0-interpol(junk,[0.0,result(0)],zinput)
endelse

return, sig_interpol
end

