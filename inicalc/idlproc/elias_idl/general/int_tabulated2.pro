        function int_tabulated2,x0,f0
;+
; NAME:
;       INT_TABULATED2
;
; PURPOSE:
;       This function integrates a tabulated set of data { x(i) , f(i) },
;       on the closed interval [min(X) , max(X)].
;
; CATEGORY:
;       Numerical Analysis.
;
; CALLING SEQUENCE:
;       Result = INT_TABULATED(X, F)
;
; INPUTS:
;       X:  The tabulated X-value data. This data may be irregularly
;           gridded and in random order.
;       F:  The tabulated F-value data. Upon input to the function
;           X(i) and F(i) must have corresponding indices for all
;	    values of i. If X is reordered, F is also reordered.
;
;       X and F must be of floating point or double precision type.
;
; KEYWORD PARAMETERS:
;
; OUTPUTS:
;       This fuction returns the integral of F computed from the tabulated
;	data in the closed interval [min(X) , max(X)].
;
; RESTRICTIONS:
;
; PROCEDURE:
;       INT_TABULATED2 
;       has a similar functionality to the intrinsci function
;       int_tabulated but uses the trapizium rule and is more robust
;       to rapidly varying functions
;
;
; EXAMPLES:
;       Example 1:
;
; MODIFICATION HISTORY:
;           Written by:  Seb Oliver c. 1997
;           Modified:   any modifications here
;-
;***********************************************************************
       x=x0(sort(x0))
       f=f0(sort(x0))
       imax=n_elements(x)-1

       binsize=shift(x-shift(x,2),-1)
       binsize(0)=x(1)-x(0)
       binsize(imax)=x(imax)-x(imax-1)
       binsize=binsize/2.

       int_tabulated2=total(binsize*f)

       return,int_tabulated2
       end

;***********************************************************************
