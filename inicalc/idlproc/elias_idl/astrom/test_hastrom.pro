
;reads in DSS image

im=readfits('/arch/maps/dss/AFGL_4463S.fits.gz',hd)


; copies image and header
hd2=hd
im2=im

;converts header to non-GSSS system
GSSS_STDAST, hd2

;rotates image through 90 degress
hrotate,im2,hd2,im3,hd3,1

; uses HASTROM to rotate back to original non-GSSS  reference frame

hastrom,im3,hd3,im4,hd4,hd2

; plots up difference image which should be flat
implot,im4-im2,min=-100,max=100
implot,im4-shift(im2,-1,1),min=-100,max=100

; uses ICHASTROM to rotate back to original non-GSSS  reference frame using nearest neighbour

ichastrom,im3,hd3,im4,hd4,hd2,interp=-1
window,1
implot,im-im4
window,2
implot,im-shift(im,1,-1)
implot,im4-shift(im,1,-1)

; uses ICHASTROM to rotate back to original non-GSSS  reference frame using nearest neighbour and higher resolution

ichastrom,im3,hd3,im4,hd4,hd2,interp=-1,ngrid=5,degree=4
window,1
implot,im-im4
window,2
implot,im-shift(im,1,-1)
implot,im4-shift(im,1,-1)

; uses ICHASTROM to rotate back to original GSSS  reference frame



ichastrom,im3,hd3,im4,hd4,hd,interp=-1
window,1
implot,im-im4,min=-100,max=100
window,2
implot,im-shift(im,1,-1)
implot,im4-shift(im,1,-1)

; uses ICHASTROM to rotate back to original GSSS  reference frame

ichastrom,im3,hd3,im4,hd4,hd,interp=1,ngrid=25,degree=4
window,1
implot,im-im4,min=-100,max=100
window,2
implot,im-shift(im,1,-1)
implot,im4-shift(im,1,-1)

