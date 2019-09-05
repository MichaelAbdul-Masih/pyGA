;$Id: chisqr_pdf.pro,v 1.3 1997/01/15 03:11:50 ali Exp $
;
; Copyright (c) 1994-1997, Research Systems, Inc.  All rights reserved.
;       Unauthorized reproduction prohibited.
;+
; NAME:
;       IC_CHISQR_PDF
;
; PURPOSE: 
;       This function computes the probabilty (p) such that:
;                   Probability(X <= v) = p
;       where X is a random variable from the Chi-square distribution
;       with (df) degrees of freedom.
;
; CATEGORY:
;       Statistics.
;
; CALLING SEQUENCE:
;       Result = chisqr_pdf(V, DF)
;
; INPUTS:
;       V:    A scalar of type integer, float or double that specifies 
;             the cutoff value.
;
;      DF:    A positive scalar of type integer, float or double that
;             specifies the degrees of freedom of the Chi-square distribution.
;
; EXAMPLES:
;       Compute the probability that a random variable X, from the Chi-square 
;       distribution with (DF = 3) degrees of freedom, is less than or equal 
;       to 6.25. The result should be 0.899939 
;         result = chisqr_pdf(6.25, 3)
;
;       Compute the probability that a random variable X, from the Chi-square
;       distribution with (DF = 3) degrees of freedom, is greater than 6.25. 
;       The result should be 0.100061
;         result = 1 - chisqr_pdf(6.25, 3)
;
; REFERENCE:
;       ADVANCED ENGINEERING MATHEMATICS (seventh edition)
;       Erwin Kreyszig
;       ISBN 0-471-55380-8
;
; MODIFICATION HISTORY:
;       Modified by:  GGS, RSI, July 1994
;                     Minor changes to code. New documentation header.
;	30 Oct 98: Now uses ic_igamma_pdf. Renamed ic_chisqr_pdf. S Serjeant
;			
;-

function ic_chisqr_pdf, x, df, thresh=thresh

  on_error, 2  ;Return to caller if error occurs.

  if x le 0 then return, 0.0 else begin
    gres = ic_igamma_pdf(df/2.0, x/2.0, thresh=thresh)
    if gres ne -1 then return, gres else $
      message, 'Computational error detected.'
  end

end

