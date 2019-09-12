import numpy as np
import GA
import shutil
import os
import functools
from schwimmbad import MPIPool
import sys
import time
import glob
from collections import OrderedDict
# from PyAstronomy.pyasl import rotBroad, instrBroadGaussFast
from subprocess import Popen, PIPE
import getopt


def create_GA_directory(object_name, cont = False):
    run_dir = object_name + '/Run'
    output_dir = object_name + '/Output'
    if not cont:
        shutil.copytree('Inputs/' + object_name, object_name)
        os.mkdir(run_dir)
        os.mkdir(output_dir)
        shutil.copytree('TEMPLATE', run_dir + '/TEMPLATE')
        shutil.copytree('inicalc', run_dir + '/inicalc')
    return run_dir, output_dir


def read_ini_file(object_name):
    ini_file = glob.glob(object_name + '/*.ini')[0]
    lines_dic = OrderedDict()
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
            #print(np.ceil(np.log10(abs(float(upper) - float(lower)))))
            sig_digits = np.ceil(np.log10(abs(float(upper) - float(lower)))) - np.floor(np.log10(float(step)))
            sig_digits += 2
            if np.log10(abs(float(upper) - float(lower)))%1 == 0:
                sig_digits +=1
            if (float(upper) - float(lower)) != 0:
                params.add(param_names[i], float(lower), float(upper), int(sig_digits))

        Z = data[num_lines*5 + 5 + len(param_names)].split(' ')[0]
        K_mag = data[num_lines*5 + 6 + len(param_names)].split(' ')[0]

        pop = data[num_lines*5 + 8 + len(param_names)].split(' ')[0][:-1]
        gens = data[num_lines*5 + 9 + len(param_names)].split(' ')[0][:-1]

    return lines_dic, params, Z, K_mag, int(pop), int(gens)


def renormalize_spectra(lines_dic, object_name):
    keys = lines_dic.keys()
    wave, flux, err = np.loadtxt(object_name + '/' + object_name + '.norm').T
    for key in keys:
        inds = [i for i in range(len(wave)) if wave[i] >= lines_dic[key]['lower_bound'] and wave[i] <= lines_dic[key]['upper_bound']]
        m = (lines_dic[key]['norm_y2'] - lines_dic[key]['norm_y1']) / (lines_dic[key]['norm_w2'] - lines_dic[key]['norm_w1'])
        b = lines_dic[key]['norm_y1'] - m * lines_dic[key]['norm_w1'] + 1
        y = m * wave[inds] + b
        lines_dic[key]['norm_wave'] = wave[inds]
        lines_dic[key]['norm_flux'] = flux[inds]/y
        lines_dic[key]['norm_err'] = err[inds]
    return lines_dic


def create_model_directory(run_dir, param_set):
    model_dir = '/'.join([run_dir, param_set['run_id']])
    shutil.copytree(run_dir + '/TEMPLATE', model_dir)
    os.mkdir(model_dir + '/' + param_set['run_id'])
    return model_dir


def create_INDAT_file(run_dir, param_set, metallicity):
    gen_number = param_set['run_id'].split('_')[0]
    path = '/'.join([run_dir, param_set['run_id']])
    with open(path + '/INDAT.DAT', 'w') as f:
        name = param_set['run_id']
        f.write('\'' + name + '\'\n')

        f.write(' T T           0         100\n')
        f.write('  0.000000000000000E+000\n')

        teff = param_set['teff']
        logg = param_set['logg']
        radius = 7.2
        f.write('   '.join([str(teff), str(logg), str(radius)]) + '\n')

        f.write('   120.000000000000       0.600000000000000\n')

        mdot = 10**param_set['mdot']
        v_min = 0.1
        v_inf = param_set['vinf']
        beta = param_set['beta']
        v_trans = 0.1
        f.write('   '.join([str(mdot), str(v_min), str(v_inf), str(beta), str(v_trans)]) + '\n')

        He = param_set['He']
        num_e = 2
        f.write('   '.join([str(He), str(num_e)]) + '\n')

        f.write(' F T F T T\n')

        micro = 15.0
        Z = metallicity
        f.write('   '.join([str(micro), str(Z)]) + ' T T\n')

        f.write(' T F           1           2\n')
        f.write(' 1.000       0.1 0.2\n')

        C = param_set['C']
        N = param_set['N']
        O = param_set['O']
        f.write('\n'.join(['C    ' + str(C), 'N    ' + str(N), 'O    ' + str(O)]) + '\n')





def run_fastwind(run_dir, output_dir, lines_dic, Z, param_set):
    model_dir = create_model_directory(run_dir, param_set)
    create_INDAT_file(run_dir, param_set, Z)

    os.chdir(model_dir)
    try:
        os.system('timeout 1h ./pnlte_A10HHeNCOPSi.eo > temp.txt')
        # os.system('timeout 1h ./pnlte_A10HHeNCOPSi.eo > /dev/null')
        np.savetxt('temp1.txt', np.array([param_set['run_id'], '15.0 0.1', '0']), fmt='%s')
        os.system('./pformalsol_A10HHeNCOPSi.eo < temp1.txt > temp2.txt')
        # r = Popen('./pformalsol_A10HHeNCOPSi.eo > temp.txt', stdin=PIPE)
        # r.communicate(os.linesep.join([param_set['run_id'], '15.0 0.1', '0']))
    except:
        pass
    os.chdir('../../../')

    lines = glob.glob(model_dir + '/' + param_set['run_id'] + '/OUT.*')

    param_list_return = [param_set[i] for i in param_set.keys()]
    if len(lines) <= 1:
        param_list_return.append(999999999)
        param_list_return.append(0.0)
        param_list_return.extend(np.zeros_like(lines_dic.keys()))
        shutil.rmtree(model_dir)
        print('failed: ' + param_set['run_id'])
        return param_list_return

    for line in lines:
        line_name = line.split('.')[-1].split('_')[0]
        x = np.genfromtxt(line, max_rows = 161).T
        wavelength = x[2]
        flux = x[4]
        new_line_file_name = model_dir + '/' + param_set['run_id'] + '/' + line_name + '.prof'
        np.savetxt(new_line_file_name, np.array([wavelength, flux]).T, header = '#161     #0', comments = '')

    total_chi2 = 0
    total_red_chi2 = 0
    total_deg_of_freedom = 0
    line_fitnesses = []
    out_mod_dir = '/'.join([output_dir, param_set['run_id'].split('_')[0], param_set['run_id']])
    os.mkdir(out_mod_dir)
    for line in lines_dic.keys():
        new_line_file_name = model_dir + '/' + param_set['run_id'] + '/' + line + '.prof'
        os.system('python broaden.py -f ' + new_line_file_name + ' -r ' + str(lines_dic[line]['resolution']) + ' -v ' + str(param_set['vrot']) + ' -m -1')
        chi2, dof = calculate_chi2(new_line_file_name + '.fin', lines_dic[line])
        total_chi2 += chi2
        red_chi2 = chi2/(dof - len(param_set.keys()))
        total_red_chi2 += red_chi2
        total_deg_of_freedom += dof
        line_fitnesses.append(1./red_chi2)
        shutil.copy(new_line_file_name + '.fin', out_mod_dir + '/.')

    shutil.copy(model_dir + '/' + param_set['run_id'] + '/INDAT.DAT', '/'.join([output_dir, param_set['run_id'].split('_')[0], param_set['run_id'], '.']))
    os.system('tar -czf ' + out_mod_dir + '.tar.gz ' + out_mod_dir)
    shutil.rmtree(out_mod_dir)

    shutil.rmtree(model_dir)
    #print(total_chi2, total_deg_of_freedom)
    total_deg_of_freedom -= len(param_set.keys())
    total_chi2 /= (total_deg_of_freedom)
    fitness = 1./total_red_chi2 * len(lines_dic.keys())
    param_list_return.append(total_chi2)
    param_list_return.append(fitness)
    param_list_return.extend(line_fitnesses)
    # shutil.rmtree(model_dir)
    return param_list_return

        # os.system(f'python broaden.py -f {new_line_file_name} -r {lines_dic[line_name]["resolution"]} -v {param_set["vrot"]}')


def dopler_shift(w, rv):
    c = 299792.458
    return w*c/(c-rv)


def calculate_chi2(exp_fname, line_dic):
    w_broad, f_broad = np.loadtxt(exp_fname).T
    w_shifted = dopler_shift(w_broad, line_dic['gamma'])
    observed_wave = line_dic['norm_wave']
    observed_flux = line_dic['norm_flux']
    observed_err = line_dic['norm_err']
    expected_flux = np.interp(observed_wave, w_shifted, f_broad)
    chi2 = np.sum(((observed_flux - expected_flux) / observed_err)**2)
    deg_of_freedom = len(observed_wave)
    return chi2, deg_of_freedom



pool = MPIPool()
if not pool.is_master():
    pool.wait()
    sys.exit(0)

start_time_prog = time.time()

object_name = 'vfts352a_uvALL91'
cont = False

opts, args = getopt.getopt(sys.argv[1:], 'co:p:', ['continue', 'object=', 'pop_size=', 'population_size'])
for opt, arg in opts:
    if opt in ('-c', '--continue'):
        cont = True
    if opt in ('-o', '--object'):
        object_name = str(arg)


print('Creating GA directory...')
run_dir, output_dir = create_GA_directory(object_name, cont)
print('Reading ini file...')
lines_dic, params, Z, K_mag, population_size, number_of_generations = read_ini_file(object_name)
lines_dic = renormalize_spectra(lines_dic, object_name)


for opt, arg in opts:
    if opt in ('-p', '--pop_size', '--population_size'):
        population_size = int(arg)


keep_files = False

outfile = output_dir + '/chi2.txt'
mutfile = output_dir + '/mutation_by_gen.txt'
popfile = output_dir + '/raw_pop.npy'


starting_generation = 0
mutation_rate = 0.005

if cont:
    print('Loading chromosome...')
    population_raw = np.load(popfile)
    x = np.loadtxt(mutfile)
    starting_generation, mutation_rate = int(x[-1][0]), float(x[-1][1])
    try:
        shutil.rmtree('/'.join([object_name, 'Output', str(starting_generation).zfill(4)]))
        x = glob.glob('/'.join([object_name, 'Run', '*_*']))
        pool.map(shutil.rmtree, x)
    except:
        pass

else:
    print('Creating chromosome...')
    population_raw = GA.create_chromosome(params, population_size)

    np.savetxt(output_dir + '/params.txt', np.array([[params[i].name, params[i].min, params[i].max, params[i].precision] for i in params.keys()]), fmt='%s')

    with open(outfile, 'w') as f:
        f.write('#' + ' '.join(params.keys()) + ' run_id chi2 fitness ' + ' '.join(lines_dic.keys()))

    with open(mutfile, 'w') as f:
        f.write('#Generation Mutation_rate\n')
        f.write(' '.join([str(starting_generation), str(mutation_rate)]) + '\n')



best_fitness = 0
best_mods = []

number_of_lines = len(list(lines_dic.keys()))


for generation in range(starting_generation, number_of_generations):
    gen_start_time = time.time()

    population = GA.batch_translate_chromosomes(params, population_raw, generation)
    print('Generation : ' +  str(generation))
    os.mkdir(output_dir + '/' + str(generation).zfill(4))
    gen_fitnesses = pool.map(functools.partial(run_fastwind, run_dir, output_dir, lines_dic, Z), population)
    with open(outfile, 'a') as f:
        np.savetxt(f, np.array(gen_fitnesses), fmt='%s')

    fitness = np.array(np.array(gen_fitnesses)[:,-1*number_of_lines -1], dtype='float')
    #print(fitness)
    population_raw = GA.crossover_and_mutate_raw(population_raw, fitness, mutation_rate)
    mutation_rate = GA.adjust_mutation_rate(mutation_rate, fitness)

    np.save(popfile, np.array(population_raw))
    with open(mutfile, 'a') as f:
        f.write(' '.join([str(generation + 1), str(mutation_rate)]) + '\n')

    if np.max(fitness) > best_fitness:
        best_fitness = np.max(fitness)
        best_mod = population[np.argmax(fitness)]
    best_mods.append(best_mod)

    gen_time = time.time()
    print('Since start: ' + str(gen_time - start_time_prog))
    print('Gen time: ' + str(gen_time - gen_start_time))


pool.close()

print(best_fitness)
print(best_mod)
exit()
