import numpy as np
import os
from subprocess import Popen, PIPE
import sys
sys.path.append('/home/michael/research/')
#import astro_functions as af
import shutil

file_loc = os.path.dirname(os.path.realpath(__file__))

os.chdir(file_loc)

if len(sys.argv) > 2:
    micro_turb = str(sys.argv[2])
else:
    micro_turb = str(10.0)

directory = str(sys.argv[1])
os.system('./pnlte_A10HHeNCOPSi.eo')
q = Popen('./ptotout_A10HHeNCOPSi.eo', stdin=PIPE)
q.communicate(os.linesep.join([directory]))
r = Popen('./pformalsol_A10HHeNCOPSi.eo', stdin=PIPE)
r.communicate(os.linesep.join([directory, micro_turb, '0.1']))

os.chdir('/STER/michael/VFTS_352/Fastwind/')

path = file_loc + '/' + directory

#w,f = af.fw_pipeline(path)
#np.savetxt(path + 'combined_spec.txt', np.array([w,f]).T)

shutil.copytree(path, '/STER/michael/VFTS_352/Fastwind/results/' + directory)

if file_loc != '/STER/michael/VFTS_352/Fastwind/TEMPLATE':
    shutil.rmtree(file_loc)
