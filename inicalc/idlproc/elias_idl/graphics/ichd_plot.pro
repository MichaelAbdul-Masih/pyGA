	pro ichd_plot,hd,im

	colours
; input header makes dummy array and plots it up

	naxis1=sxpar(hd,'NAXIS1')
	naxis2=sxpar(hd,'NAXIS2')

	im=intarr(naxis1,naxis2)

	icplot,im,hd,respen=11
	end

