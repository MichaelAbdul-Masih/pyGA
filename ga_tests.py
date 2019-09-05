import numpy as np
import phoebe
import GA
import shutil
import os
import functools
from schwimmbad import MPIPool
import sys
import time
import glob
from PyAstronomy.pyasl import rotBroad, instrBroadGaussFast


def create_GA_directory(object_name):
    shutil.copytree('Inputs/' + object_name, object_name)
    run_dir = object_name + '/Run'
    output_dir = object_name + '/Output'
    os.mkdir(run_dir)
    os.mkdir(output_dir)
    shutil.copytree('TEMPLATE', run_dir + '/TEMPLATE')
    shutil.copytree('inicalc', run_dir + '/inicalc')
    return run_dir, output_dir


def create_model_directory(run_dir, param_set):
    model_dir = '/'.join([run_dir, param_set['run_id']])
    shutil.copytree(run_dir + 'TEMPLATE', model_dir)
    return model_dir


def create_INDAT_file(run_dir, param_set):
    gen_number = param_set['run_id'].split('_')[0]
    path = '/'.join([run_dir, param_set['run_id']])
    with open(path + '/INDAT.DAT', 'w') as f:
        name = param_set['run_id']
        f.write('\'' + name + '\'\n')

        f.write(' T T           0         100\n')
        f.write('  0.000000000000000E+000\n')

        teff = 45000
        logg = 4.2
        radius = 7.2
        f.write('   '.join([str(teff), str(logg), str(radius)]) + '\n')

        f.write('   120.000000000000       0.600000000000000\n')

        mdot = 10**6.5
        v_min = 0.1
        v_inf = 2500
        beta = 0.8
        v_trans = 0.1
        f.write('   '.join([str(mdot), str(v_min), str(v_inf), str(beta), str(v_trans)]) + '\n')

        He = 0.1
        num_e = 2
        f.write('   '.join([str(He), str(num_e)]) + '\n')

        f.write(' F T F T T\n')

        micro = 15.0
        Z = 0.5
        f.write('   '.join([str(micro), str(Z)]) + ' T T\n')

        f.write(' T F           1           2\n')
        f.write(' 1.000       0.1 0.2\n')

        C = 7.5
        N = 8.0
        O = 8.5
        f.write('\n'.join(['C    ' + str(C), 'N    ' + str(N), 'O    ' + str(O)]) + '\n')





def run_fastwind():
    x = 0

def broaden(model_dir, lines_dic, param_set):
    lines = glob.glob(model_dir + '/OUT.*')
    for line in lines:
        line_name = line.split('.')[-1].split('_')[0]
        x = np.genfromtxt(i, max_rows = 161).T
        wavelength = x[2]
        flux = x[4]
        flux = instrBroadGaussFast(wavelength, flux, lines_dic[line_name]['resolution'], maxsig=5.0, edgeHandling="firstlast")
        flux = rotBroad(wavelength, flux, limbDark, param_set['vrot'])
        '''FINISH THIS!!!!!!!'''

def calculate_chi2():
    x = 0

def read_ini_file(object_name):
    ini_file = glob.glob(object_name + '/*.ini')[0]
    lines_dic = {}
    with open(ini_file, 'r') as f:
        data = f.readlines()
        num_lines = int(data[2][:-1])
        for i in range(num_lines):
            line_name, resolution = data[i*5 + 3][:-1].split(' ')
            lower_bound, upper_bound = data[i*5 + 4][:-1].split(' ')
            gamma = data[i*5 + 5][:-1]
            norm_w1, norm_y1, norm_w2, norm_y2 = data[i*5 + 6][:-1].split(' ')
            line_dic = {'line_name':line_name, 'resolution':int(resolution), 'lower_bound':float(lower_bound), 'upper_bound':float(upper_bound), 'gamma':float(gamma), 'norm_w1':float(norm_w1), 'norm_y1':float(norm_y1), 'norm_w2':float(norm_w2), 'norm_y2':float(norm_y2)}
            lines_dic[line_name] = line_dic

        param_names = ['teff', 'logg', 'mdot', 'vinf', 'beta', 'He', 'micro', 'vrot', 'macro', 'N', 'C', 'O', 'Si', 'P']
        params = GA.Parameters()
        for i in range(len(param_names)):
            lower, upper, step = data[i + num_lines*5+4][:-1].split(' ')
            if float(upper) - float(lower) != 0:
                sig_digits = np.ceil(np.log10(float(upper) - float(lower))) - np.floor(np.log10(float(step)))
                params.add(param_names[i], float(lower), float(upper), int(sig_digits))

        Z = data[num_lines*5 + 5 + len(param_names)].split(' ')[0]
        K_mag = data[num_lines*5 + 6 + len(param_names)].split(' ')[0]

    return lines_dic, params, Z, K_mag
