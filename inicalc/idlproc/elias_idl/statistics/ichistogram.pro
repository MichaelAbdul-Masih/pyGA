PRO ichistogram, arr, xhist, yhist, weight, min=min, BIN=bin, $
           ANONYMOUS_ = dummy_, _EXTRA = _extra, $
           reverse_indices=reverse_indices
;+
; NAME:
;	ICHISTOGRAM
; PURPOSE:
;	Calculates the histogram of an array with the corresponding abcissa.
;
; CALLING SEQUENCE:
;	ichistogram, arr, xhist, yhist, [, BIN=bin,  
;
; INPUTS:
;	arr - The array to plot the histogram of.   It can include negative
;		values, but non-integral values will be truncated.              
;
; OPTIONAL OUTPUTS:
;	xhist - X vector used in making the plot  
;		( = indgen( N_elements(h)) * bin + min(arr) )
;	yhist - Y vector used in making the plot  (= histogram(arr/bin))
;
; OPTIONAL INPUT KEYWORDS:
;	BIN -  The size of each bin of the histogram,  scalar (not necessarily
;		integral).  If not present (or zero), the bin size is set to 1.
;
;		Any input keyword that can be supplied to the PLOT procedure
;		can also be supplied to ICHISTOGRAM.
; EXAMPLE:
;	Create a vector of 1000 values derived from a gaussian of mean 0,
;	and sigma of 1.    Plot the histogram of these value with a bin
;	size of 0.1
;
;	IDL> a = randomn(seed,1000)
;	IDL> ichistogram,a, bin = 0.1
;
; MODIFICATION HISTORY:
;       Copied from ASTROLIB plotist subroutine with plotting removed
;                   Seb Oliver August 1996.
;-
;			Check parameters.
 On_error,2

 if N_params() LT 1 then begin   
	print, 'Syntax - ichistogram, arr, [ xhist, yhist , BIN=,...plot_keywords]'
	return
 endif

 if N_elements( arr ) LT 2 then message, $
      'ERROR - Input array must contain at least 2 elements'
 arrmin = min( arr, MAX = arrmax)
 if ( arrmin EQ arrmax ) then message, $
       'ERROR - Input array must contain distinct values'

 if not keyword_set(BIN) then bin = 1. else bin = float(abs(bin))

; Compute the histogram and abcissa.


 if not keyword_set(min) then min=min(arr)

; yhist = histogram( y, _EXTRA = _extra, reverse_indices=reverse_indices)
 yhist = histogram( arr, min=min, binsize=bin,_EXTRA = _extra, reverse_indices=reverse_indices)
 
 N_hist = N_elements( yhist )
 xhist = (lindgen( N_hist )+0.5) * bin + min


; checking if weight is set
; changing yhist to floating point
  yhist=float(yhist)
  if n_params() eq 4 then begin
    for ihist=0,n_hist-1 do begin
       if reverse_indices(ihist) ne reverse_indices(ihist+1) then  begin

          yhist(ihist)=total( weight($
reverse_indices(reverse_indices(ihist):reverse_indices(ihist+1)-1)))

	endif
    endfor
  endif




 return
 end
