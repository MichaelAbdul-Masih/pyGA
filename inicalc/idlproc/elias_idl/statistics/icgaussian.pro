pro icgaussian, xi, parms, gfun, pderiv
;+
; NAME:
;	ICGAUSSIAN
;
; PURPOSE:
;	Compute the 1-D Gaussian function and optionally the derivative 
;	at an array of points.
;
; CALLING:
;	icgaussian , xi, parms, gfun, [ pderiv ])
;
; INPUTS:
;	xi = array, independent variable of Gaussian function.
;
;	parms = parameters of Gaussian, 2 or 3 element array:
;		parms(0) = maximum value (factor) of Gaussian,
;		parms(1) = mean value (center) of Gaussian,
;		parms(2) = standard deviation (sigma) of Gaussian.
;		(if parms has only 2 elements then sigma taken from common).
;
; OUTPUT:
;       gfun = array gaussian at xi
;
; OPTIONAL OUTPUT:
;	pderiv = optional output of partial derivatives,
;		computed only if parameter is present in call.
;
;		pderiv(*,i) = partial derivative at all xi absisca values
;		with respect to parms(i), i=0,1,2.
;
;	Function returns array of Gaussian evaluated at xi.
;
; EXAMPLE:
;	Evaulate a Gaussian centered at x=0, with sigma=1, and a peak value
;	of 10 at the points 0.5 and 1.5.   Also compute the derivative
;
;	IDL> f = gaussian( [0.5,1.5], [10,0,1], DERIV )
;	==> f= [8.825,3.25].   DERIV will be a 2 x 3 array containing the
;	numerical derivative at the two point with respect to the 3 parameters.
; 
; COMMON BLOCKS:
;	
; HISTORY:
;	Written, Frank Varosi NASA/GSFC 1992.
;       Adapted to work in curvefit Seb Oliver (ICSTM) July 1996
;-

	on_error,2

	Nparmg = N_elements( parms )
	parms = float( parms )
	if (Nparmg GE 3) then sigma = parms(2)

	z = ( xi - parms(1) )/sigma
	zz = z*z
	gauss2 = fltarr( N_elements( zz ) )
	w = where( zz LT 172, nw )
	if (nw GT 0) then gauss2(w) = exp( -zz(w) / 2 )

	if N_params() GE 3 then begin

		pderiv = fltarr( N_elements( xi ), Nparmg )
		fsig = parms(0) / sigma

		pderiv(0,0) = gauss2
		pderiv(0,1) = gauss2 * z * fsig

		if (Nparmg GE 3) then  pderiv(0,2) = gauss2 * zz * fsig
	   endif

         gfun=parms(0) * gauss2


end
