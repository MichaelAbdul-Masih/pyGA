import numpy as np
from scipy.interpolate import splrep, splev
import os
from subprocess import Popen, PIPE
import sys
import shutil
import glob
# from schwimmbad import MPIPool
import functools
import getopt
import tarfile
import astropy.units as u
import astropy.constants as C



# main_path = '/home/mabdul-m/Modgrid/'
# save_path = '/home/mabdul-m/Modgrid/Complete/'
# rerun_path = '/home/mabdul-m/Modgrid/to_rerun/'
metallicity = 1.0
# rerun = False
# alter_temp = 100
# alter_logg = -0.003
# alter_r = 0.02

number_of_lines = 140


'''
-------------------------------------------------------------------------------
FASTWIND Calculations
-------------------------------------------------------------------------------
'''

def calc_vinf(M, R, Z):
    vesc = np.sqrt(2 * C.G.to('km3 / (Msun s2)') * M *u.Msun / (R* u.Rsun).to('km')).value
    vinf = 2.65 * vesc * Z**0.13
    return vinf


def spectroscopic_mass(logg, r):
    g = 10**logg / 100. * u.m / (u.s)**2
    M = (g * (r * C.R_sun.to('m'))**2 /C.G).to('Msun')
    return M.value


def calc_mass_loss_rate(M, R, T, Z, v_ratio = 2.6):
    # All values must be in solar Units
    L = R**2 * (T/5777.) ** 4
    T_bump = (61.2 + 2.59 * (-13.636+ 0.889 * np.log10(Z)))*1000
    if T >= T_bump:
        Mdot = -6.697 + 2.194 * np.log10(L*10**-5) - 1.313 * np.log10(M/30.) - 1.226 * np.log10(v_ratio/2.) + 0.933 * np.log10(T/40000.) - 10.92 * (np.log10(T/40000.))** 2 + 0.85 * np.log10(Z)
    else:
        Mdot = -6.688 + 2.210 * np.log10(L*10**-5) - 1.339 * np.log10(M/30.) - 1.601 * np.log10(v_ratio/2.) + 1.07 * np.log10(T/20000.) + 0.85 * np.log10(Z)
    return 10**Mdot


def prep_run(model):
    #----------Default A Parameters---------
    b = 0.8
    # vinf = 3000             #approximately for 30 Msol star using vinf =2.65*vesc*(0.5)**0.13
    micro = 10.

    runs = []

    t = model.split('_')[0][1:]
    g = model.split('_')[1][1:]
    r = model.split('_')[2][1:]
    cno = model.split('_')[3][3:]
    h = model.split('_')[4][2:]

    # M = 30
    M = spectroscopic_mass(float(g), float(r))
    vinf = calc_vinf(M, float(r), metallicity)
    mdot = calc_mass_loss_rate(M, float(r), float(t), metallicity)

    # path = main_path + 'Staging/folder_' + str(run_number).zfill(7)
    # shutil.copytree(main_path + 'TEMPLATE', path)
    directory = 'Grid_' + model
    os.makedirs(directory)

    with open('INDAT_SAMPLE_ABUNDANCES_FULL.DAT', 'rt') as fin:
        with open('INDAT.DAT', 'wt') as fout:
            for line in fin:
                line = line.replace('Name', str(directory))
                line = line.replace('temp', str(t))
                line = line.replace('logg', str(g))
                line = line.replace('radius', str(r))
                line = line.replace('mdot', str(mdot))
                line = line.replace('beta', str(b))
                line = line.replace('vinf', str(vinf))
                line = line.replace('vturb', str(micro))
                line = line.replace('metallicity', str(metallicity))
                line = line.replace('He_abundance', str(h))
                line = line.replace('N_abundance', ('N  ' + str(cno)))
                line = line.replace('C_abundance', ('C  ' + str(cno)))
                line = line.replace('O_abundance', ('O  ' + str(cno)))
                line = line.replace('Si_abundance', ('SI  ' + str(cno)))
                fout.write(line)

    return directory, micro



def run_fastwind(directory, micro_turb, model, SPAMMS = False):

    os.system('timeout 2h ./pnlte_A10HHeCnewNOSi.eo > temp.txt')
    q = Popen('./ptotout_A10HHeCnewNOSi.eo', stdin=PIPE)
    q.communicate(input=directory.encode())
    if SPAMMS == True:
        r = Popen('./pformalsol_spamms_A10HHeCnewNOSi.eo', stdin=PIPE)
    else:
        r = Popen('./pformalsol_A10HHeCnewNOSi.eo', stdin=PIPE)
    r.communicate('\n'.join([directory, str(micro_turb), '0']).encode())


    # os.chdir(main_path)
    #
    # path = file_loc + '/' + directory
    #
    # if len(glob.glob(path + '/*')) >= 211:
    #     shutil.move(path, save_path + directory)
    #
    #     with open('complete.txt', 'a') as file_object:
    #         file_object.write('\n'+model)
    # else:
    #     shutil.move(file_loc + '/temp.txt', path + '/temp.txt')
	# shutil.move(path, rerun_path + directory)
    #     with open('rerun.txt', 'a') as file_object:
    #         file_object.write('\n'+model)
    #
    # if file_loc != main_path + 'TEMPLATE':
    #     shutil.rmtree(file_loc)


'''
-------------------------------------------------------------------------------
SPAMMS Input Grid Calculations
-------------------------------------------------------------------------------
'''



def data_array(i_file):
    i_data = np.loadtxt(i_file).T
    p_index, pray, lam_index, lam, i_abs, i_em, w1, w2 = i_data
    p_index -= 1
    lam_index -= 1
    array = []
    for i in range(int(max(p_index)) + 1):
        array.append([])
        for j in range(int(max(lam_index)) + 1):
            array[i].append([i_data.T[k] for k in range(len(i_data.T)) if p_index[k] == i and lam_index[k] == j][0])
    return np.array(array)


def fit_splines(i_data):
    w = i_data[0, :, 3]
    p = i_data[:, 0, 1]
    p = p/p[-1]*120
    z = np.linspace(0, p[-1], 6301)
    splines = []
    for i in range(len(w)):
        splines.append(splrep(p,i_data[:,i, 4] + i_data[:,i, 5],k=2))
    return splines, w


def read_splines(splines, w, p_value):
    profile = []
    for i in range(len(w)):
        profile.append(splev(p_value, splines[i]))

    return np.array(profile).T


def linear_eq(x1, x2, y1, y2):
    x1, x2, y1, y2 = np.array([x1, x2, y1, y2]) * 1.0
    m = (y2 - y1) / (x2 - x1)
    b = y1 - m*x1
    return m, b


def determine_I_mu_sw(i_data, wind_line_formation_region=120, spls = False):
    wlfr = wind_line_formation_region

    thetas_deg = np.linspace(0,90,9001)
    thetas = thetas_deg * np.pi/180
    mu = np.cos(thetas)
    mu.sort()

    mu_crit_wind = np.sqrt(wlfr**2 - 1)/wlfr

    prays_phot = np.sqrt(1 - mu**2)
    prays_wind = np.sqrt(wlfr**2 - (mu_crit_wind * mu * wlfr)**2)

    splines, w = fit_splines(i_data)
    if spls:
        return splines

    star_profs = read_splines(splines, w, prays_phot)
    wind_profs = read_splines(splines, w, prays_wind)

    return mu, star_profs, wind_profs


def convert(splines, save_path, line_name):
    x = splines
    w = x[0]
    splines = x[1:].T

    wlfr = 120

    pray = np.linspace(0,1,101)
    profs = read_splines(splines, w, pray)

    prayw = np.linspace(1,wlfr,101)
    profsw = read_splines(splines, w, prayw)

    np.savetxt(save_path + '/' + line_name + 'phot_101.txt', profs)
    np.savetxt(save_path + '/' + line_name + 'wind_101.txt', profsw)
    np.save(save_path + '/' + line_name + '_wl.npy', w)


def convert_splines(i_file, f_file, save_path):
    idata = data_array(i_file)

    f_data = np.genfromtxt(f_file, max_rows = 161).T
    wf = f_data[2]
    ff = f_data[3]*f_data[4]

    splines = determine_I_mu_sw(idata, 120, spls = True)

    spline_save = save_path + '/OUT_spline.' + i_file.split('.')[-1]
    array = [wf]
    array.extend(np.array(splines).T)

    return np.array(array)


def convert_all(path, micro):
    files = glob.glob(path + '/OUT_IEM.*')
    save_path = path[5:] + '_LPs'
    os.makedirs(save_path)
    for f in files:
        intensity_file = f.split('/')[-1]
        flux_file = 'OUT.' + intensity_file.split('.')[-1] + '_VT%s'%str(int(micro)).zfill(3)
        splines = convert_splines(path + '/' + intensity_file, path + '/' + flux_file, save_path)
        convert(splines, save_path, f.split('.')[-1])

    if len(glob.glob(save_path + '/*')) != number_of_lines*3:
        shutil.move(save_path, save_path + '_failed')
        save_path = save_path + '_failed'

    return save_path






def run(model_bundle):
    # a bit of housekeeping:
    model, run_number = model_bundle

    # first we need to set up the fastwind run
    directory, micro = prep_run(model, run_number)

    # now we can actually run fastwind
    run_fastwind(path, directory, micro, model)

    with tarfile.open('%s.tgz'%directory, "w:gz") as tar:
        tar.add(directory, arcname=os.path.basename(directory))



def main():

    models = np.loadtxt(sys.argv[1], dtype = 'str')
    model_number = int(sys.argv[2])

    model = models[model_number]

    SPAMMS = False
    if sys.argv[3] == 'True':
        SPAMMS = True

    directory, micro = prep_run(model)
    run_fastwind(directory, micro, model, SPAMMS = SPAMMS)

    with tarfile.open('%s.tgz'%directory, "w:gz") as tar:
        tar.add(directory, arcname=os.path.basename(directory))

    if SPAMMS == True:
        directory_spamms = convert_all(directory, micro)
        with tarfile.open('%s.tgz'%directory_spamms, "w:gz") as tar:
            tar.add(directory_spamms, arcname=os.path.basename(directory_spamms))







if __name__ == "__main__":
    main()
