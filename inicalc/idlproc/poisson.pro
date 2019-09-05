pro poisson,prob,eventno,mean
  prob=exp(-double(mean))
  for i=1,eventno do begin
  prob=prob*double(mean)/double(i)
  endfor

return
end
