import numpy as np
import matplotlib.pyplot as plt
import sys


def calc_phase(period, t0, t):
    t_dif = np.array(t) - t0
    phase = (t_dif%period*1.0)/period
    return phase


path = '/scratch/leuven/324/vsc32406/'
model = sys.argv[2]
run = sys.argv[1]

if run[-1] != '/':
    run += '/'

x = np.loadtxt(path + run + 'Models/'+ model + '.txt').T

lc = np.loadtxt('/data/leuven/324/vsc32406/Phoebe_fitting/lc.kepler.data').T

p = calc_phase(3.1415926, 0, lc[0])

plt.plot(p, lc[1], 'o')
plt.plot(x[0], x[1])
plt.show()
