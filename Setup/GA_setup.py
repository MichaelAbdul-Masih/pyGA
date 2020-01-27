#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Dec 10 10:30:15 2019

@author: calum
"""
import numpy as np
import re
import sys
import os
pwd = os.getcwd()
sys.path.append('/'.join(pwd.split('/')[:-1]))
from collections import OrderedDict
import matplotlib.pyplot as plt
from pathlib import Path
import pyGA as GA


#Provide object name on command line and source all setup files
object_name = sys.argv[1]
setup_info = object_name + '/' + object_name + '_info.setup'
setup_params = object_name + '/' + object_name + '_params.setup'
setup_lines = object_name + '/' + object_name + '_lines.setup'
output_directory = '../Inputs/' + object_name + '/'
inifile = output_directory + 'pfw_' + object_name + '.ini'
inifile_new = output_directory + 'pfw_' + object_name + '_new.ini'
norm_file = object_name + '/' + object_name + '.norm'

###############################################################################

def read_setup_lines(setup_lines):
    """
    Reads line list from setup lines file. All setup files should be kept in a folder named after the object.
    The lines file is structured with one spectral line per line, with info seperated by spaces, in the document as follows:
    LINE_ID RESOLUTION LEFT_WAVELENGTH LEFT_OFFSET RIGHT_WAVELENGTH RIGHT_OFFSET WEIGHT
    Note that the wavelength ranges supplied here are used for plotting with accurate wavelengths selected by the user, so these should be wide estimates.
    Note that a spectral line can be excluded by adding a # as the first character in the document.
    """
    '''Must give a setup lines file path, should be generated automatically relative to the user given object name.'''
    global leftwvl
    with open(setup_lines, 'r') as f:
        line_data = f.readlines()
        #print(line_data)
        for i in line_data:
            if i[0][0] == '#':
                line_data.remove(i)

        lines = {}

        for i in line_data:
            (line_id, res, leftwvl, leftoff, rightwvl, rightoff, weight) = re.split(r'\s+', i[:-1])
            lines[(line_id)] = {'res':res, 'leftwvl':leftwvl, 'leftoff':leftoff, 'rightwvl':rightwvl, 'rightoff':rightoff, 'weight':weight}

    return(lines)
#Returns lines dictionary, set of spectral lines used for fitting with relevant line information.

def read_setup_info(setup_info):
    """
    Reads information needed to setup a GA run that is not a parameter, eg walltime of GA_run, population size etc.
    """
    '''Must give a setup info file path, should be generated automatically relative to the user given object name.'''
    with open(setup_info, 'r') as f:
        info_data = f.readlines()
        info = {}

        for i in info_data:
            (info_value, info_name) = re.split(r'\s+', i[:-1])
            info[(info_name)] = info_value


    return(info)
#Returns info dictionary, set of constraints and corresponding values needed to run the GA.


def read_setup_params(setup_params):
    """
    Reads input fitting parameters for the GA, in this case we assume the parameters and ranges to be complete in the setup file.
    """
    '''Must give a setup parameters file path, should be generated automatically relative to the user given object name.'''
    with open(setup_params, 'r') as f:
        params_data = f.readlines()
        #print(params_data)

        return(params_data)
#Returns fitting parameters for the GA, returns exactly as presented in the setup file, not modified.

###############################################################################

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
    #lines_dic will contain all the line regions we preform fitting over.
    lines_dic = OrderedDict()
    with open(inifile, 'r') as f:
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

    return lines_dic, params, constants
#Returns are the lines_dic containing the list of lines we are fitting over and the wavelength ranges. The parameters we fit over, the parameters we keep constant. Finally the population size and number of generations.



#Check whether a previous version of inifile exists and will be modified or if new setup is initialised.
if os.path.exists(output_directory) == False:
    os.mkdir(output_directory)


if Path(inifile).is_file():
    print ("Inifile already exists! Will write to new inifile.")
    lines_dic, prev_params, constants = read_ini_file(inifile)
else:
    print ("No previous inifile present.")


###############################################################################

#Call functions to read setup files and store info in dictionaires.

params = read_setup_params(setup_params)

info = read_setup_info(setup_info)

lines = read_setup_lines(setup_lines)

###############################################################################

def boundaries(norm_file, lines):
    """
    Function used to define fitting boundaries for each spectral line.
    Using the rough estimates of line boundaries given in the setup, the function will plot the line and the user can select accurate fitting boundaries.
    When a line is initially presented a leftmouse click will select the left fitting boundary, you can click on multiple points but the function
    will take the latest selection as the final boundary value.
    You must then press Enter to confirm the selection and you will be presented the line again to select the right boundary.
    If you press Enter without clicking the function will take the value given in the setup file (not recommended), however
    if you have already run the setup and an inifile exists with accurate ranges it will take these instead.
    """
    '''Must given norm file to plot the spectrum and lines dictionary to iterate through relevant spectral lines.'''
    norm = np.genfromtxt(norm_file)
    wave_norm = norm[:,0]
    flux_norm = norm[:,1]

    lines_bounds = {}

    for line_id in lines:


        f, ax = plt.subplots()
        ax.set_xlim((float(lines[line_id]['leftwvl']), float(lines[line_id]['rightwvl'])))
        ind = [i for i in range(len(wave_norm)) if wave_norm[i] > float(lines[line_id]['leftwvl']) and wave_norm[i] < float(lines[line_id]['rightwvl'])]
        flux_lims = flux_norm[ind]
        ax.set_ylim(min(flux_lims)-0.05, max(flux_lims)+0.05)
        ax.set_title('Select left fitting boundary (press Enter when ready)')
        ax.plot(wave_norm,flux_norm)
        f.canvas.mpl_connect('button_press_event', clicker)
        f.canvas.mpl_connect('key_press_event', onkey)
        manager = plt.get_current_fig_manager()
        manager.window.showMaximized()
        plt.show()
        leftbound = click_value[0]

        f, ax = plt.subplots()
        ax.set_xlim((float(lines[line_id]['leftwvl']), float(lines[line_id]['rightwvl'])))
        ax.set_ylim(min(flux_lims)-0.05, max(flux_lims)+0.05)
        ax.set_title('Select right fitting boundary (press Enter when ready)')
        ax.plot(wave_norm,flux_norm)
        f.canvas.mpl_connect('button_press_event', clicker)
        f.canvas.mpl_connect('key_press_event', onkey)
        manager = plt.get_current_fig_manager()
        manager.window.showMaximized()
        plt.show()
        rightbound = click_value[0]

        lines_bounds[(line_id)] = {'leftbound':leftbound, 'rightbound':rightbound}

    return lines_bounds
#Returns dictionary of fitting boundaries attached to each spectral line.

def normalisation(norm_file, lines, lines_bounds):
    """
    Function used to adjust normalisation around a given spectral line. NOT to be used to normalise spectrum, only for small adjustments.
    Must provide a normalised spectrum.
    Each line profile will be shown and you can select continuum levels either side of the line in a similar fashion as the line boundary selection.
    The fastwind_ga file will then readjust the normalisation during the GA run, this setup will just provide values and not manipulate the spectrum.
    """
    '''Must provide normalised spectrum file path, dictionary of line list and dictionary of line fitting boundaries.'''
    norm = np.genfromtxt(norm_file)
    wave_norm = norm[:,0]
    flux_norm = norm[:,1]
    global leftnwvl
    global leftnflux
    global line_id

    lines_norm = {}

    for line_id in lines:

        clicker.has_been_called = False


        f, ax = plt.subplots()
        ax.set_xlim((float(lines[line_id]['leftwvl']), float(lines[line_id]['rightwvl'])))
        ind = [i for i in range(len(wave_norm)) if wave_norm[i] > float(lines[line_id]['leftwvl']) and wave_norm[i] < float(lines[line_id]['rightwvl'])]
        flux_lims = flux_norm[ind]
        ax.set_ylim(min(flux_lims)-0.05, max(flux_lims)+0.05)
        ax.set_title('Select left continuum value (press Enter when ready)')
        ax.plot(wave_norm,flux_norm)
        ax.axhline(y=1, linestyle='-', color='black')
        f.canvas.mpl_connect('button_press_event', clicker)
        f.canvas.mpl_connect('key_press_event', onkey)
        manager = plt.get_current_fig_manager()
        manager.window.showMaximized()
        plt.show()

        if clicker.has_been_called == True:
            leftnwvl = click_value[0]
            leftnflux = click_value[1]
        else:
            leftnwvl = lines_bounds[line_id]['leftbound']
            leftnflux = 1

        clicker.has_been_called = False

        f, ax = plt.subplots()
        ax.set_xlim((float(lines[line_id]['leftwvl']), float(lines[line_id]['rightwvl'])))
        ax.set_ylim(min(flux_lims)-0.05, max(flux_lims)+0.05)
        ax.set_title('Select right continuum value (press Enter when ready)')
        ax.plot(wave_norm,flux_norm)
        ax.axhline(y=1, linestyle='-', color='black')
        f.canvas.mpl_connect('button_press_event', clicker)
        f.canvas.mpl_connect('key_press_event', onkey)
        manager = plt.get_current_fig_manager()
        manager.window.showMaximized()
        plt.show()

        if clicker.has_been_called == True:
            rightnwvl = click_value[0]
            rightnflux = click_value[1]
        else:
            rightnwvl = lines_bounds[line_id]['rightbound']
            rightnflux = 1


        lines_norm[(line_id)] = {'leftnwvl':leftnwvl, 'leftnflux':leftnflux, 'rightnwvl':rightnwvl, 'rightnflux':rightnflux}



    return lines_norm
#Returns dictionary of values to adjust normalisation around given spectral lines, these values are used in the GA, spectrum is not affected now.


#Functions used to store location of mouse on click event and close plots on Enter key press.
def clicker(event):
    global click_value
    if event.inaxes:
        click_value = [event.xdata, event.ydata]
        plt.axvline(x=event.xdata, linestyle='-', color='red')
        print(click_value)
        clicker.has_been_called = True
        plt.draw()

def onkey(event):
    if event.key == 'enter':
        plt.close()

def onkey_quit(event):
    if event.key == 'q':
        click_value[0] = 0
        plt.close()

###############################################################################

#Gives user option to start setup from scratch or to edit individual spectral lines based on a previous setup.

selection = input(" 1. Run full setup \n 2. Redo a single line \n")

print(selection)

if selection == '1':
    lines_bounds = boundaries(norm_file, lines)
    lines_norm = normalisation(norm_file, lines, lines_bounds)
    print(lines_norm)
'''
if selection == '2':
    lines_bounds = boundaries(norm_file, lines)

if selection == '3':
    lines_norm = normalisation(norm_file, lines, lines_bounds)
'''
if selection == '2':

    lines_select = {}

    for line_id in lines.keys():
        print(str(line_id))

    line_input = input('Please enter line ID: ')
    line_select = lines[line_input]

    lines_select[(line_input)] = line_select

    print(lines_select)

    print(lines_select[line_input])

    print(lines_select[line_input]['leftwvl'])

    print(lines_dic)

    lines_bounds = boundaries(norm_file, lines_select)
    lines_norm = normalisation(norm_file, lines_select, lines_bounds)


###############################################################################

def write_inifile(inifile, lines, lines_norm, lines_bounds, info, params):
    """
    Function to write inifile needed by GA, written in legacy format.
    All information is supplied by the setup files and by the user as they run the setup script. The given inifile should be ready to run the GA.
    """
    '''Must give inifile path, line list dictionary, dictionary of values to adjust normalisation, dictionary of line fitting bounds and list of fitting parameters.'''

    if Path(inifile).is_file():
        print ("Inifile already exists! Will write to new inifile.")
        with open(inifile_new, 'w') as f:
            f.write('"'+str(info['filepath'])+str(object_name)+'/'+str(object_name)+'.norm" '+str(object_name)+'\n')
            f.write('\n')
            f.write(str(len(lines))+'\n')
            for line_id in lines_norm.keys():
                f.write(str(line_id)+' '+lines[line_id]['res']+'\n')
                f.write(str(lines_bounds[line_id]['leftbound'])+' '+str(lines_bounds[line_id]['rightbound'])+'\n')
                f.write(str(info['Radial_Velocity'])+'\n')
                f.write(str(lines_norm[line_id]['leftnwvl'])+' '+str(lines_norm[line_id]['leftnflux']-1.)+' '+str(lines_norm[line_id]['rightnwvl'])+' '+str(lines_norm[line_id]['rightnflux']-1.)+'\n')
                #LINES NORM NEEDS AN 'if no norm points selected print line_bounds and 0 0
                f.write(str(lines[line_id]['weight'])+'\n')

                if line_id in lines_norm.keys():
                    del lines_dic[line_id]

            for line_id in lines_dic.keys():
                f.write(str(line_id)+' '+str(lines_dic[line_id]['resolution'])+'\n')
                f.write(str(lines_dic[line_id]['lower_bound'])+' '+str(lines_dic[line_id]['upper_bound'])+'\n')
                f.write(str(info['Radial_Velocity'])+'\n')
                f.write(str(lines_dic[line_id]['norm_w1'])+' '+str(lines_dic[line_id]['norm_y1'])+' '+str(lines_dic[line_id]['norm_w2'])+' '+str(lines_dic[line_id]['norm_y2'])+'\n')
                f.write(str(lines[line_id]['weight'])+'\n')

            f.write('\n')
            for i in params:
                f.write(str(i))
            f.write('\n')
            f.write(str(info['Population_Size'])+' #Population_Size'+'\n')
            f.write(str(info['Number_of_Generations'])+' #Number_of_Generations')

    else:
        print ("No previous inifile present.")
        with open(inifile, 'a+') as f:
            f.write('"'+str(info['filepath'])+str(object_name)+'/'+str(object_name)+'.norm" '+str(object_name)+'\n')
            f.write('\n')
            f.write(str(len(lines))+'\n')
            for line_id in lines_norm.keys():
                f.write(str(line_id)+' '+lines[line_id]['res']+'\n')
                f.write(str(lines_bounds[line_id]['leftbound'])+' '+str(lines_bounds[line_id]['rightbound'])+'\n')
                f.write(str(info['Radial_Velocity'])+'\n')
                f.write(str(lines_norm[line_id]['leftnwvl'])+' '+str(lines_norm[line_id]['leftnflux']-1.)+' '+str(lines_norm[line_id]['rightnwvl'])+' '+str(lines_norm[line_id]['rightnflux']-1.)+'\n')
                #LINES NORM NEEDS AN 'if no norm points selected print line_bounds and 0 0
                f.write(str(lines[line_id]['weight'])+'\n')
            f.write('\n')
            for i in params:
                f.write(str(i))
            f.write('\n')
            f.write(str(info['Population_Size'])+' #Population_Size'+'\n')
            f.write(str(info['Number_of_Generations'])+' #Number_of_Generations')
#Writes inifile in new text file, no retuns.

#Call function to write inifile.
write_inifile(inifile, lines, lines_norm, lines_bounds, info, params)
