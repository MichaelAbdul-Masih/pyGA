'''
v1.0
26/11/2019
This code was developed by Michael Abdul-Masih and Calum Hawcroft
Instituut voor Sterrenkunde, KU Leuven, Belgium
michael.abdulmasih@kuleuven.be
calum.hawcroft@kuleuven.be
'''

import numpy as np
import pyGA as GA
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
import re


def create_GA_directory(object_name, cont = False):
    """
    Creates all directories needed for a full GA run. This file should exist in a folder which already contains:
    Inputs/ - Contains a folder of input files for a given object e.g. VFTS352/ - Containing my_inifile, par_VFTS352.job, pfw_VFTS352.ini, VFTS352.norm. Can have multiple folders for different objects.
    inicalc/ - Should be the current version of inicalc required to run FASTWIND.
    TEMPLATE/ - Should be current version of all files & executables required to run FASTWIND from said folder.
    broaden.py, fastwind_ga.py, GA.py, GA_analysis.py, par_VFTS352.job
    See README for more detailed info on required files & folders.
    """
    '''Must give an object name, corresponding to an object data folder in Inputs/. Also the flag -c if the run is a continuation.'''
    #Make Run/ and Output/ directories within the object directory.
    #The Run folder contains all currently running models and Output contains only the final outputs from each model that we want to keep.
    run_dir = object_name + '/Run'
    output_dir = object_name + '/Output'
    #if not continuing the run from a previously calculated generation then do as below, if continuing then all of this was exectued in the initial run.
    if not cont:
    #Copy input folder for given from Inputs/ folder to current folder - this will now be the directory for the current run.
        shutil.copytree('Inputs/' + object_name, object_name)
        os.mkdir(run_dir)
        os.mkdir(output_dir)
    #copy all necessary files to run FASTWIND into the run directory
        shutil.copytree('TEMPLATE', run_dir + '/TEMPLATE')
        shutil.copytree('inicalc', run_dir + '/inicalc')
    return run_dir, output_dir
#Returns are the path to the Run and Output directories from the inital launch/root directory.


def read_ini_file(object_name):
    """
    Reads and parses ini file, selecting all information required to create an initial population of models & progress with fitting.
    Ini file starts with header, then a full list of all lines & regions we want to fit over, followed by the input parameter set with bounds and sampling steps to create a population of models.
    These parameters are then given to individual FASTWIND runs.
    The new inifile, redesigned to including fitting over FASTWIND clumping factors, also allows for any parameter to be given as a fixed constant.
    The parameter set includes all values that are required to write a FASTWIND INDAT.DAT file, as a result, any number of these parameters will need to be fixed depending on the purpose of the GA run.
    To fix a parameter give the constant value, followed by two '-1.0' flags, seperated by spaces.
    The population size and number of generations are specified at the end of the inifile.
    """
    '''Must give an object name, corresponding to an object data folder in Inputs/.'''
    #Defining the ini file which is specified as the *.ini file in the Inputs, not the my_inifile.
    ini_file = glob.glob(object_name + '/*.ini')[0]
    #lines_dic will contain all the line regions we preform fitting over.
    lines_dic = OrderedDict()
    with open(ini_file, 'r') as f:
    #Here we parse everything from the ini file. Data is the file, num_lines is the number of lines given as a line at the top of the file, not calculated.
    #For every block of inputs for a given line we read in the line name, followed by the resolution. The next line are the upper and lower fitting bounds for the line. Gamma is the radial velocity on the next line.
    #The next line is for renormalisation. If renormalistion is required as described by the GA_analysis setup routine these are two wavelength anchor points and their respective normalisation shifts.
    #Finally there is a flag for each line of 1.0, not used in this version.
    #All of these values are stored in a dictionary of lines.
        data = f.readlines()
        num_lines = int(data[2][:-1])
        for i in range(num_lines):
            line_name, resolution = re.split(r'\s+', data[i*5 + 3][:-1])
            lower_bound, upper_bound = re.split(r'\s+', data[i*5 + 4][:-1])
            gamma = data[i*5 + 5][:-1]
            norm_w1, norm_y1, norm_w2, norm_y2 = re.split(r'\s+', data[i*5 + 6][:-1])
            line_dic = {'line_name':line_name, 'resolution':int(resolution), 'lower_bound':float(lower_bound), 'upper_bound':float(upper_bound), 'gamma':float(gamma), 'norm_w1':float(norm_w1), 'norm_y1':float(norm_y1), 'norm_w2':float(norm_w2), 'norm_y2':float(norm_y2)}
            lines_dic[line_name] = line_dic

            #Now we read in the parameters we are fitting for. Read in order of parameter, this order is created by GA_Analysis setup (although this does need to be updated. We read in the lower and upper bounds followed by the sampling step for each parameter.
            #We also read in parameters as constants if they are followed by the '1.0' flags
            param_names = ['teff', 'logg', 'mdot', 'vinf', 'beta', 'He', 'micro', 'vrot', 'macro', 'N', 'C', 'O', 'Si', 'P', 'fcl', 'fic', 'fvel', 'h', 'vcl', 'vclmax', 'metallicity', 'radius', 'vmin', 'vtrans', 'nume']
            params = GA.Parameters()
            constants = {}
            for i in range(len(param_names)):
                lower, upper, step, comment = re.split(r'\s+', data[i + num_lines*5+4][:-1])

                if float(step) <= 0.0:
                    constants[param_names[i]] = float(lower)
                else:
                    sig_digits = np.ceil(np.log10(abs(float(upper) - float(lower)))) - np.floor(np.log10(float(step)))
                    sig_digits += 2
                    if np.log10(abs(float(upper) - float(lower)))%1 == 0:
                        sig_digits +=1
                    if (float(upper) - float(lower)) != 0:
                        params.add(param_names[i], float(lower), float(upper), int(sig_digits))

            #Here we take the population size and number of generations. These can also be specified on command line at the run launch.
            pop = data[num_lines*5 + 5 + len(param_names)].split(' ')[0]
            gens = data[num_lines*5 + 6 + len(param_names)].split(' ')[0]
    return lines_dic, params, constants, int(pop), int(gens)
#Returns are the lines_dic containing the list of lines we are fitting over and the wavelength ranges. The parameters we fit over, the parameters we keep constant. Finally the population size and number of generations.

def renormalize_spectra(lines_dic, object_name):
    """
    Applies renormalisation specified in the GA_analysis setup function.
    """
    '''Must give the dictionary containing all lines and the wavelength range information. Must give an object name, corresponding to an object data folder in Inputs/.'''
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
#Returns the line list(dictionary) with the renormalisation correction taken into account.

def create_model_directory(run_dir, param_set):
    """
    Creates model directory for an individual FASTWIND run. Each model is given a run_id going from 0000 to the specified population size.
    This also copies all the files and executables needed to run FASTWIND into the model directory.
    """
    '''Must give the run directory, produced by the create_GA_directory function. Must give parameter set produced by read_ini_file function.'''
    model_dir = '/'.join([run_dir, param_set['run_id']])
    shutil.copytree(run_dir + '/TEMPLATE', model_dir)
    os.mkdir(model_dir + '/' + param_set['run_id'])
    return model_dir
#Returns full path to model directory within the Run/ folder.

def assign_param(par, param_set, constants):
    """
    Decides whether a parameter, which needs to be written into the INDAT file, should be taken from the list of fitted parameters or constants.
    Essentially just checks whether a parameter name exists in the parameter list or the constant list.
    """
    '''Must give a parameter name you are interested in, eg teff or logg. Give parameter set and list of constants, created by the read_ini_file function.'''
    if par in param_set.keys():
        print(param_set[par])
        value = param_set[par]
    else:
        value = str(constants[par])
    return value
#Returns the value attributed to the parameter provided for inquiry.


def create_INDAT_file(run_dir, param_set, constants):
    """
    Creates an individual INDAT file required to execute FASTWIND (contains all FW inputs).
    """
    '''Must give run directory produced by create_GA_directory function. Must give parameter set and list of constants produced by read_ini_file function.'''
    #Set generation number, all of the INDAT files- 40554249 go into a mdoel directory in the run folder.
    gen_number = param_set['run_id'].split('_')[0]
    path = '/'.join([run_dir, param_set['run_id']])
    #This first pulls the relevant parameters and then writes the INDAT in the necessary format.
    #The INDAT should automatically write the INDAT in the necessary format to include the physics specified, eg if the winds or abundances are flagged to constant the INDAT will be written accordingly.
    with open(path + '/INDAT.DAT', 'w') as f:

        name = param_set['run_id']
        teff = assign_param('teff', param_set, constants)
        logg = assign_param('logg', param_set, constants)
        radius = assign_param('radius', param_set, constants)
        mdot = 10**float((assign_param('mdot', param_set, constants)))
        v_min = assign_param('vmin', param_set, constants)
        v_inf = assign_param('vinf', param_set, constants)
        beta = assign_param('beta', param_set, constants)
        v_trans = assign_param('vtrans', param_set, constants)
        He = assign_param('He', param_set, constants)
        num_e = assign_param('nume', param_set, constants)
        micro = assign_param('micro', param_set, constants)
        Z = assign_param('metallicity', param_set, constants)
        C = assign_param('C', param_set, constants)
        N = assign_param('N', param_set, constants)
        O = assign_param('O', param_set, constants)
        Si = assign_param('Si', param_set, constants)
        P = assign_param('P', param_set, constants)
        fcl = assign_param('fcl', param_set, constants)
        fic = assign_param('fic', param_set, constants)
        fvel = assign_param('fvel', param_set, constants)
        h = assign_param('h', param_set, constants)
        vcl = assign_param('vcl', param_set, constants)
        vclmax = assign_param('vclmax', param_set, constants)

        f.write('\'' + name + '\'\n')
        f.write(' T T           0         100\n')
        f.write('  0.000000000000000E+000\n')
        f.write('   '.join([str(teff), str(logg), str(radius)]) + '\n')
        f.write('   120.000000000000       0.600000000000000\n')
        f.write('   '.join([str(mdot), str(v_min), str(v_inf), str(beta), str(v_trans)]) + '\n')
        f.write('   '.join([str(He), str(num_e)]) + '\n')
        f.write(' F T F T T\n')
        f.write('   '.join([str(micro), str(Z)]) + ' T T\n')
        f.write(' T F           1           2\n')

        if fcl == -1:
            f.write(' 1.000       0.1 0.2\n')
        elif fic == -1:
            f.write(' ' + str(fcl) + '      0.5000E-01  0.1000\n')
        elif fic != -1:
            f.write(' THICK\n')
            f.write(' '.join([str(fcl), str(vcl), str(vclmax)]) + '\n')
            f.write(' '.join([str(fic), str(vcl), str(vclmax)]) + '\n')
            f.write(' '.join([str(fvel), str(vcl), str(vclmax)]) + '\n')
            f.write(' '.join([str(h), str(vcl), str(vclmax)]) + '\n')

        if float(C) >= 0:
            f.write('C    ' + str(C) + '\n')

        if float(O) >= 0:
            f.write('N    ' + str(N) + '\n')

        if float(O) >= 0:
            f.write('O    ' + str(O) + '\n')

        if float(Si) >= 0:
            f.write('SI    ' + str(Si) + '\n')

        if float(P) >= 0:
            f.write('P    ' + str(P) + '\n')
#Creates INDAT.DAT file, no returns.


def run_fastwind(run_dir, output_dir, constants, lines_dic, param_set):
    """
    Function to run FASTWIND and process the produced line profiles with the broadening script.
    Also is where the function to calculate chi2 is called, so fitnesses are stored by this function.
    """
    '''Must provide run and output directories created by create_GA_directory. Must give parameter set, list of contants and dictionary of line information, produced by read _ini_file.'''
    #Create the model directory, named between 0 and max population eg 250 then create an INDAT for each model.
    model_dir = create_model_directory(run_dir, param_set)
    create_INDAT_file(run_dir, param_set, constants)
    #Move into the model directory and run FASTWIND, this will execute all the command line prompts required to run a FASTWIND model.
    #Notice each command line executable is given with a timeout, the time required to run the average FASTWIND model depends on the physics included, so the timeout given can vary.
    os.chdir(model_dir)
    try:
        os.system('timeout 1h ./pnlte_A10HHeNCOPSi.eo > temp.txt')
        # os.system('timeout 1h ./pnlte_A10HHeNCOPSi.eo > /dev/null')
        micro = str(assign_param('micro', param_set, constants))
        np.savetxt('temp1.txt', np.array([param_set['run_id'], micro + '0.1', '0']), fmt='%s')
        os.system('./pformalsol_A10HHeNCOPSi.eo < temp1.txt > temp2.txt')
        # r = Popen('./pformalsol_A10HHeNCOPSi.eo > temp.txt', stdin=PIPE)
        # r.communicate(os.linesep.join([param_set['run_id'], '15.0 0.1', '0']))
    except:
        pass
    os.chdir('../../../')
    #All line profiles created by FASTWIND are named as OUT.* so these files are all listed.
    lines = glob.glob(model_dir + '/' + param_set['run_id'] + '/OUT.*')
    #If there are no lines in 'lines' as defined above, that means the model failed to run, as a result fill the parameter list output with dummy values.
    param_list_return = [param_set[i] for i in param_set.keys()]
    if len(lines) <= 1:
        param_list_return.append(999999999)
        param_list_return.append(0.0)
        param_list_return.extend(np.zeros_like(list(lines_dic.keys()),dtype='float'))
        shutil.rmtree(model_dir)
        print('failed: ' + param_set['run_id'])
        return param_list_return
    #Takes the line profile files produced  by FASTWIND and reduces them to *.prof files which contain only wavelength and flux columns.
    for line in lines:
        try:
            line_name = line.split('.')[-1].split('_')[0]
            x = np.genfromtxt(line, max_rows = 161).T
            wavelength = x[2]
            flux = x[4]
            new_line_file_name = model_dir + '/' + param_set['run_id'] + '/' + line_name + '.prof'
            np.savetxt(new_line_file_name, np.array([wavelength, flux]).T, header = '#161     #0', comments = '')
        except:
            param_list_return = [param_set[i] for i in param_set.keys()]
            param_list_return.append(999999999)
            param_list_return.append(0.0)
            param_list_return.extend(np.zeros_like(list(lines_dic.keys()),dtype='float'))
            shutil.rmtree(model_dir)
            return param_list_return

    total_chi2 = 0
    total_red_chi2 = 0
    total_deg_of_freedom = 0
    line_fitnesses = []
    out_mod_dir = '/'.join([output_dir, param_set['run_id'].split('_')[0], param_set['run_id']])
    os.mkdir(out_mod_dir)
    #For each line *.prof file run the broaden.py script to correct for intstrumental, radial and macroturbulent broadening. The broaden produces *.fin files which are the final broadened line profiles.
    #The chi square is then calculated for the model. The line profiles are sent to the Output/ folder for the appropriate model and the chi2 is written to the chi2.txt file in Output/.
    #The try:except here is to remove any models which produce incorrect line profiles, liekly due to a bug in FASTWIND.
    for line in lines_dic.keys():
        new_line_file_name = model_dir + '/' + param_set['run_id'] + '/' + line + '.prof'
        macro = str(assign_param('macro', param_set, constants))
        vrot = str(assign_param('vrot', param_set, constants))
        broaden = os.system('timeout 5m python broaden.py -f ' + new_line_file_name + ' -r ' + str(lines_dic[line]['resolution']) + ' -v ' + vrot + ' -m ' + macro)
        if broaden == 0:
            chi2, dof = calculate_chi2(new_line_file_name + '.fin', lines_dic[line])
            total_chi2 += chi2
            red_chi2 = chi2/(dof - len(param_set.keys()))
            total_red_chi2 += red_chi2
            total_deg_of_freedom += dof
            line_fitnesses.append(1./red_chi2)
            shutil.copy(new_line_file_name + '.fin', out_mod_dir + '/.')
        else:
            param_list_return = [param_set[i] for i in param_set.keys()]
            param_list_return.append(999999999)
            param_list_return.append(0.0)
            param_list_return.extend(np.zeros_like(list(lines_dic.keys()),dtype='float'))
            shutil.rmtree(model_dir)
            shutil.rmtree(out_mod_dir)
            return param_list_return

    shutil.copy(model_dir + '/' + param_set['run_id'] + '/INDAT.DAT', '/'.join([output_dir, param_set['run_id'].split('_')[0], param_set['run_id'], '.']))
    os.system('tar -czf ' + out_mod_dir + '.tar.gz -C ' + out_mod_dir + ' .')
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
#Returns the list of parameters with fitness values attached

        # os.system(f'python broaden.py -f {new_line_file_name} -r {lines_dic[line_name]["resolution"]} -v {param_set["vrot"]}')


def dopler_shift(w, rv):
    c = 299792.458
    return w*c/(c-rv)

def calculate_chi2(exp_fname, line_dic):
    """
    Calculate chi-square for each line profile *.fin, in this case the chi-square is weighted by the error on the observed data points.
    The degree of freedom is measured to calculate a reduced chi-square.
    """
    '''Must give the filename of the line profile produced by FASTWIND. Must give the dictionary of lines to fit against the data.'''
    w_broad, f_broad = np.loadtxt(exp_fname).T
    w_shifted = dopler_shift(w_broad, line_dic['gamma'])
    observed_wave = line_dic['norm_wave']
    observed_flux = line_dic['norm_flux']
    observed_err = line_dic['norm_err']
    expected_flux = np.interp(observed_wave, w_shifted, f_broad)
    chi2 = np.sum(((observed_flux - expected_flux) / observed_err)**2)
    deg_of_freedom = len(observed_wave)
    return chi2, deg_of_freedom
#Returns a chi2 for the fit of the model profile to observed profile. Also gives the degrees of freedom for the chi2.

#This is the MPI Pool, needed to communicate between nodes on HPC. Or between cores on any system.
pool = MPIPool()
if not pool.is_master():
    pool.wait()
    sys.exit(0)
#Track the time for completion and optimisation.
start_time_prog = time.time()

#Here we start to run the GA.
"""SHOULD EDIT THIS SO THE OBJECT_NAME IS TAKEN FROM AN INPUT, NOT DEFINED HERE"""
object_name = 'vfts352a_uvALL91'
cont = False

#Creating options for launching the run, if -c is specified the run will continue from the last generation. -p sets the object name.
opts, args = getopt.getopt(sys.argv[1:], 'co:p:', ['continue', 'object=', 'pop_size=', 'population_size'])
for opt, arg in opts:
    if opt in ('-c', '--continue'):
        cont = True
    if opt in ('-o', '--object'):
        object_name = str(arg)

#first by giving our object name to the function to create the overall directory. This then returns the paths for the output & run directories.
print('Creating GA directory...')
run_dir, output_dir = create_GA_directory(object_name, cont)
# Now we call the function to read the ini file & this returns all the inputs we need like the line list and parameter set etc.
print('Reading ini file...')
lines_dic, params, constants, population_size, number_of_generations = read_ini_file(object_name)
#Now we update the lines for any renormalisation.
lines_dic = renormalize_spectra(lines_dic, object_name)

#More options for launch, -p sets the population size.
for opt, arg in opts:
    if opt in ('-p', '--pop_size', '--population_size'):
        population_size = int(arg)

#Decide whether we want to keep FW models, or just the line profiles, or the convolved/broadened profiles. (feature not fully implemented)
keep_files = False

#Create paths to output files. chi2.txt keeps all the parameters and fitnesses for each model. mutation_by_gen.txt tracks the mutuation rate. raw_pop is used to track the last generation, so we can start again from said generation. This file contains raw chromosomes.
outfile = output_dir + '/chi2.txt'
mutfile = output_dir + '/mutation_by_gen.txt'
popfile = output_dir + '/raw_pop.npy'

#specify initial generation/starting point and inital mutation rate. (more info on mutation rates is available in the pikaia documentation).
starting_generation = 0
mutation_rate = 0.05

#If the -c continue option is selected we start from the last generation, restarting the generation which was partially complete when the run ended.
if cont:
    print('Loading chromosome...')
#assign the population_raw, which is the chromosomes for a population of models
    population_raw = np.load(popfile)
    x = np.loadtxt(mutfile)
    starting_generation, mutation_rate = int(x[-1][0]), float(x[-1][1])
    try:
        shutil.rmtree('/'.join([object_name, 'Output', str(starting_generation).zfill(4)]))
        x = glob.glob('/'.join([object_name, 'Run', '*_*']))
        pool.map(shutil.rmtree, x)
    except:
        pass

#If -c is not selected we launch the GA as normal. Create an inital population based on input parameter ranges and population size.
else:
    print('Creating chromosome...')
#The initial raw population of chromosomes is created.
    population_raw = GA.create_chromosome(params, population_size)

    np.savetxt(output_dir + '/params.txt', np.array([[params[i].name, params[i].min, params[i].max, params[i].precision] for i in params.keys()]), fmt='%s')

    with open(outfile, 'w') as f:
        f.write('#' + ' '.join(params.keys()) + ' run_id chi2 fitness ' + ' '.join(lines_dic.keys()) + '\n')

    with open(mutfile, 'w') as f:
        f.write('#Generation Mutation_rate\n')
        f.write(' '.join([str(starting_generation), str(mutation_rate)]) + '\n')



best_fitness = 0
best_mods = []

number_of_lines = len(list(lines_dic.keys()))

#Iteration loop to progress through generations of models.
for generation in range(starting_generation, number_of_generations):
    gen_start_time = time.time()

#Population is converted from raw chromosomes to input parameters useable by FASTWIND.
    population = GA.batch_translate_chromosomes(params, population_raw, generation)
    print('Generation : ' +  str(generation))
    os.mkdir(output_dir + '/' + str(generation).zfill(4))
    gen_fitnesses = pool.map(functools.partial(run_fastwind, run_dir, output_dir, constants, lines_dic), population)
    with open(outfile, 'a') as f:
        np.savetxt(f, np.array(gen_fitnesses), fmt='%s')

    fitness = np.array(np.array(gen_fitnesses)[:,-1*number_of_lines -1], dtype='float')
    #print(fitness)
    if np.max(fitness) > best_fitness:
        best_fitness = np.max(fitness)
        best_mod = population[np.argmax(fitness)]
        best_mod_raw = population_raw[np.argmax(fitness)]
    elif best_mod_raw != population_raw[np.argmax(fitness)]:
        population_raw = np.delete(population_raw, np.argmin(fitness))
        population_raw = np.append(population_raw, best_mod_raw)
    best_mods.append(best_mod)
#With results of fitness from previous generation the next generation is created.
    population_raw = GA.crossover_and_mutate_raw(population_raw, fitness, mutation_rate)
#Mutuation rate is adjust based on mutation rate of previous generation, to maximise effectiveness of exploration.
    mutation_rate = GA.adjust_mutation_rate(mutation_rate, fitness, mut_rate_min = .005)
#save the new sets of raw chromosomes for the population a textfile.
    np.save(popfile, np.array(population_raw))
#Track mutation rate per generation.
    with open(mutfile, 'a') as f:
        f.write(' '.join([str(generation + 1), str(mutation_rate)]) + '\n')
#Track time for generation completion and overall GA run time.
    gen_time = time.time()
    print('Since start: ' + str(gen_time - start_time_prog))
    print('Gen time: ' + str(gen_time - gen_start_time))

#Finished! Close the MPI pool.
pool.close()

#Now that the GA run is complete print the best model parameters and fitness.
print(best_fitness)
print(best_mod)
exit()
