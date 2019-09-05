seed = -1
sky0 = randomn(seed, 10000)

setps


plot_sky_stats, sky0, xtitle='!17 Signal', ytitle='!17 Frequency',title='!17 Unit Gaussian'


sky9 = smooth(sky0, 9)



plot_sky_stats, sky9, xtitle='!17 Signal', ytitle='!17 Frequency',title='!17 Unit Gaussian, Smoothed X 9'


endps

$ghostview idl.ps & 


$lppost2 idl.ps
 
;----------------------------------------------------------------------

setps
x = 2.*!pi*findgen(n_elements(sky0))/n_elements(sky0)
y = sin(x*50.)

plot, x, sky0, psym=3
oplot, x, y
plot, x, sky0+y


sky1 = sky0+y

sky100 = smooth(sky1, 100)


plot_sky_stats, sky1, xtitle='!17 Signal', ytitle='!17 Frequency',title='!17 Unit Gaussian +Unit Sinusoid every 200'
plot_sky_stats, sky100, xtitle='!17 Signal', ytitle='!17 Frequency',title='!17 Unit Gaussian +Unit Sinusoid every 200', $
   sub='Smoothed every 100'


endps


$lppost2 idl.ps
 
