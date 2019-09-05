function chisq, x, df
  ; calculates the pdf of the chisq-distribution, at chi2=x and for df d.o.f
  df2=0.5*df
  denom=2.^df2*gamma(df2)
  return,exp(-0.5*x)*x^(df2-1.)/denom
end
