PRO plot_sky_stats, sky, _extra=e
SKY_STATS, sky,  SKYMEAN,  SKYVAR, SKYSKEW, SKYKUR, METHOD='gfit',$
  HIST=HIST,g1=g1,gparams=gparams,fwhm=fwhm,cum_sigmas2=cum_sigmas2, moms=moms, fwqm=fwqm, fwtm=fwtm

plot, hist[0, *], hist[1, *], psym=10, _extra=e
oplot, hist[0, *], g1, color=2
oplot, hist[0, *], hist[2, *]/max(hist[2, *])*max(hist[1, *]), color=3

fwhm_sig = (fwhm[1]-fwhm[0])/(2.*sqrt(2.*alog(2.)))

fwqm_sig = (fwqm[1]-fwqm[0])/(2.*sqrt(2.*alog(4.)))

fwtm_sig = (fwtm[1]-fwtm[0])/(2.*sqrt(2.*alog(10.)))

xyouts, 0.9*!x.crange[0]+0.1*!x.crange[1], 0.1*!y.crange[0]+0.9*!y.crange[1], '!17 Gfit:  '+string(sqrt(skyvar), format='(f7.4)')

xyouts, 0.9*!x.crange[0]+0.1*!x.crange[1], 0.2*!y.crange[0]+0.8*!y.crange[1], '!17 STDEV: '+string(sqrt(moms[1]), format='(f7.4)')
xyouts, 0.9*!x.crange[0]+0.1*!x.crange[1], 0.3*!y.crange[0]+0.7*!y.crange[1], '!17 FWHM:  '+string(fwhm_sig, format='(f7.4)')
xyouts, 0.9*!x.crange[0]+0.1*!x.crange[1], 0.35*!y.crange[0]+0.65*!y.crange[1], '!17 FWQM:  '+string(fwqm_sig, format='(f7.4)')
xyouts, 0.9*!x.crange[0]+0.1*!x.crange[1], 0.4*!y.crange[0]+0.6*!y.crange[1], '!17 FWTM:  '+string(fwtm_sig, format='(f7.4)')

cum_sigmas = (cum_sigmas2-cum_sigmas2[3])/[-3., -2., -1., 1., 1., 2., 3.]

xyouts, 0.9*!x.crange[0]+0.1*!x.crange[1], 0.45*!y.crange[0]+0.55*!y.crange[1], '!17 CUM-3:  '+string(cum_sigmas[0], format='(f7.4)')
xyouts, 0.9*!x.crange[0]+0.1*!x.crange[1], 0.5*!y.crange[0]+0.5*!y.crange[1], '!17 CUM-2:  '+string(cum_sigmas[1], format='(f7.4)')
xyouts, 0.9*!x.crange[0]+0.1*!x.crange[1], 0.6*!y.crange[0]+0.4*!y.crange[1], '!17 CUM-1:  '+string(cum_sigmas[2], format='(f7.4)')
xyouts, 0.9*!x.crange[0]+0.1*!x.crange[1], 0.7*!y.crange[0]+0.3*!y.crange[1], '!17 CUM+1:  '+string(cum_sigmas[4], format='(f7.4)')
xyouts, 0.9*!x.crange[0]+0.1*!x.crange[1], 0.8*!y.crange[0]+0.2*!y.crange[1], '!17 CUM+2:  '+string(cum_sigmas[5], format='(f7.4)')
xyouts, 0.9*!x.crange[0]+0.1*!x.crange[1], 0.9*!y.crange[0]+0.1*!y.crange[1], '!17 CUM+3:  '+string(cum_sigmas[6], format='(f7.4)')
END
