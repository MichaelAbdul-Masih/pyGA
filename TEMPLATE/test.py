import numpy as np
import os
from subprocess import Popen, PIPE
import sys
import shutil
import glob
# from schwimmbad import MPIPool
import functools
import getopt
import tarfile




def main():

    models = np.loadtxt(sys.argv[1], dtype = 'str')
    model_number = int(sys.argv[2])

    model = models[model_number]

    directory = 'Grid_' + model
    os.makedirs(directory)

    if sys.argv[3] == 'True':
        np.savetxt(directory + '/spamms.txt', np.array([0,1]))
        print('SPAMS grid')
    else:
        print('regular FW grid')

    with tarfile.open('%s.tgz'%directory, "w:gz") as tar:
        tar.add(directory, arcname=os.path.basename(directory))



if __name__ == "__main__":
    main()
