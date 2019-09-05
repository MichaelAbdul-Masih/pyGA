pro p_ran,like,lr,pran

; like is vector of random LR values, lr is LR value for particular association

count=0

result=where(like gt lr,count)

pran=float(count)/float(n_elements(like))

if (lr lt 0.0) then pran=1.0

end
