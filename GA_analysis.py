# PyGA analysis script
# Sarah Brands
# s.a.brands@uva.nl
# Created on: 19-11-2019
# Latest change: 22-11-2019
# Probabilty function and read in of parameters from script of Calum Hawcroft
#    and Michael Abdul-Masih
# Tested with python 2.7 on Mac

import __future__
import os
import sys
import math
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from matplotlib import cm
from matplotlib import colors
from collections import OrderedDict
from scipy import stats
from PyPDF2 import PdfFileMerger, PdfFileReader
import cv2 #only required if make_paramspace_avi=True
import seaborn as sns
import img2pdf # for saving the scatter plots (fitness vs parameter) in
                      # PNG and then transforming them to pdf (otherwise they load
                      # ridiculously slow)
import pylab

''' ----------------------------------------------------------------------'''
''' ----------------------------------------------------------------------'''
'''                               INPUT                                   '''
''' ----------------------------------------------------------------------'''
''' ----------------------------------------------------------------------'''

''' ------------------------------------------'''
'''             Specify directories           '''
''' ------------------------------------------'''

# The output (pdf files, avi files) is written to outpath, in a subdirectory
# with the name of the run (will be made if it doesn't exits yet)
# The data should be stored in datapath/run_name There it should have the
# structure as in 'Outputs' of pyGA (so with a dir Output inside)
# The line profiles per model should either be in the 0000_0000.tar.gz archive,
# or alternatively if already unpacked, the line profiles should be in a folder
# names 0000_0000 in the parent directory 0000. (example numbers)
outpath = '/Users/sarah/Dropbox/projects/zeta_per_model/ga_analysis/ga_output/'
datapath = '/Users/sarah/pyGA/'

''' ------------------------------------------'''
'''      which plots do you want to make?     '''
''' ------------------------------------------'''
# The pdf of a plot is saved and will be included in the full report once
# it is made, so setting a plot on 'False' here, means only that it is
# not generated _again_, not that it is not included in the full report.

# Specify which plots you want to be generated.
# full_short_manual can have the following values:
# 'full' =  all possible plots (will take a while)
# 'short' = does not generate correlation and fitness plots per line
# 'manual' = only selected plots, to be specified below
full_short_manual = 'short'

# If full_short_manual is set to 'manual'
make_fitnessdistribution_plot = False
make_fitnessdistribution_per_line_plot = False
make_chi2pgen_plot = False
make_lineprofiles_plot = False # Will take a long time depending on how many models are included.
make_correlation_plot = False # will always be done when make_correlation_per_line_plot = True
make_correlation_per_line_plot = True # This takes a lot of time

# Videos (in full or manual only)
make_paramspace_avi = False

''' ------------------------------------------'''
'''          Extra settings for plots         '''
''' ------------------------------------------'''

''' Specify parameter pairs '''
# Only used if make_paramspace_avi = True
# For each pair, an .avi is made that shows where in the parameter space
# models are calculated per generation
param_pair_list_avi = [  ['teff', 'logg']
                        ,['teff', 'He']
                        ,['vrot', 'macro']
                        ,['Si', 'C']
                        ,['Si', 'teff']
                        ]

''' Adding an extra (model)spectrum to the lineprofile plots ** Optional '''
    # Set include_extra_spectra to True to plot extra spectra or models over
    # the GA output and the .norm data.
    # Specify paths pointing to your spectra

include_extra_spectrum = False #If making lineprofiles plot, include an extra spectrum for comparison?

# Add up to 2 extra spectra or models to the plots
# If you only want to add one, set the other path equal to ''
# Currently no error bars are supported as I only loaded cmfgen models
extra_spectrum_path1 = '/Absolute/path/to/other/spectrum/here/spectrum-file-without-errors.spec'
extra_spectrum_path2 = '/Absolute/path/to/other/spectrum/here/spectrum-file-without-errors.spec'

''' Specify how to select the models in the lineprofile plots'''
# Should be set to 'P' if you want it to correspond with the error bars pyGA gives on parameters.

# Choose between:
# - best_models_cutoff_method = 'P':
#    model selection based on proper chi square based probabilities
# - best_models_cutoff_method = 1.05 or other float:
#    model selection based on the reduced chi squared value (the float)
#    !!! no proper statistics done !!! Just to artificially show more models #FIXME)
best_models_cutoff_method = 'P'

''' Set number of plots per page '''

nrows_lineprofileplot = 6
ncols_lineprofileplot = 3

nrows_fitnessparamplot = 4
ncols_fitnessparamplot = 3

''' ----------------------------------------------------------------------'''
''' ----------------------------------------------------------------------'''
'''        Script starts reading data and making plots here               '''
''' ----------------------------------------------------------------------'''
''' ----------------------------------------------------------------------'''

''' ------------------------------------ '''
'''    Reading files and other things    '''
'''  that are useful for several plots   '''
''' -------------------------------------'''

if make_correlation_per_line_plot:
    make_correlation_plot = True

''' File names of output pdfs '''
# Sub output pdfs. Numbering determines the order in which they will come
# in the final report.

print("Loading stuff...")
name_fitnessdistribution_plot = "1_fitnessdistribution_short_GAreport.pdf"
name_lineprofiles_plot = "2_lineprofiles_short_GAreport.pdf"
name_correlation_plot = "3_correlation_short_GAreport.pdf"
name_chi2pergen_plot = "4_chi2pergen_short_GAreport.pdf"
name_fitness_per_line_plot = "5_fitness_param_perline_GAreport.pdf"
name_correlation_per_line_plot = "6_correlation_perline_GAreport.pdf"

def calculateP(params, lc, chi2, normalize = False):
    degreesFreedom = len(lc) - len(params)
    if normalize:
        scaling = np.min(chi2)
    else: scaling = 1
    chi2 = (chi2 * degreesFreedom) / scaling
    probs = np.zeros_like(chi2)
    for i in range(len(chi2)):
        probs[i] = stats.chi2.sf(chi2[i], degreesFreedom)

    return probs

def parallelcrop(list1, list2, start_list1, stop_list1, list3='', list4=''):

    list1 = np.array(list1)
    list2 = np.array(list2)

    minarg = np.argmin(np.abs(list1-start_list1))
    maxarg = np.argmin(np.abs(list1-stop_list1))

    newlist1 = list1[minarg:maxarg]
    newlist2 = list2[minarg:maxarg]

    if list3 != '':
        newlist3 = list3[minarg:maxarg]
        if list4 != '':
            newlist4 = list4[minarg:maxarg]
            return newlist1, newlist2, newlist3, newlist4
        else:
            return newlist1, newlist2, newlist3

    return newlist1, newlist2

def cm_rgba(x):
    setcmap = cm.jet
    setnorm = colors.Normalize(0.0, 1.0)
    setscalarMap = cm.ScalarMappable(norm=setnorm, cmap=setcmap)
    return setscalarMap.to_rgba(x)


''' Defining paths '''

run = sys.argv[1]
if run[-1] != '/':
    run +='/'
runname = run[:-1]

plotpath = outpath + run
if not os.path.isdir(plotpath):
    os.system("mkdir " + plotpath)
datapath = datapath + run
datapath_output = datapath + 'Output/'

name_fullreport_pdf = plotpath + 'full_report_' + run[:-1] + ".pdf"
name_shortreport_pdf = plotpath + 'short_report_' + run[:-1] + ".pdf"

''' Reading output files '''

x = pd.read_csv(datapath_output + 'chi2.txt', sep=' ')
x = x.rename(columns = {'#teff': 'teff'})

''' Extracting generation id from run_id '''
maxgenid = 0
all_genid = []
for arun_id in x['run_id']:
    maxgenid = max(float(arun_id[:4]), maxgenid)
    all_genid.append(arun_id[:4])

x = x.assign(gen_id=all_genid)
maxgenid = str(int(maxgenid)).zfill(4)
print('Last generation: ' + str(maxgenid))
unique_genid = np.unique(all_genid)

''' Evaluate chi2 per generation '''
median_chi2_per_gen = []
mean_chi2_per_gen = []
lowest_chi2_per_gen = []
for a_gen_id in unique_genid:
    x_1gen = x[x['run_id'].str.contains(a_gen_id + '_')]
    chi2val = x_1gen['chi2'].values
    chi2val = chi2val[chi2val <= 1e6]
    median_chi2_per_gen.append(np.median(chi2val))
    mean_chi2_per_gen.append(np.mean(chi2val))
    lowest_chi2_per_gen.append(np.min(chi2val))

''' Read in mutation per generation '''
mutationfile = datapath_output + 'mutation_by_gen.txt'
mutationpergen = np.genfromtxt(mutationfile)
if len(mutationpergen) != len(median_chi2_per_gen):
    mutationpergen = mutationpergen[:-(len(mutationpergen)-len(median_chi2_per_gen))]
generation, mutation = mutationpergen.T

''' Normfile path, parameter file read in, P value calculation'''
normpath = datapath + runname + '.norm'
normspectrum = np.loadtxt(normpath)

params = np.loadtxt(datapath_output + 'params.txt', dtype='str')

min_redchi2_value = min(x['chi2'])
print('Min chi2: ' + str(min_redchi2_value))

probabilities = calculateP(params, normspectrum, x['chi2'], True)
x = x.assign(P=probabilities)

params_dic = OrderedDict()
params_error = OrderedDict()

min_p = 0.05
best = pd.Series.idxmax(x['P'])
ind = x['P'] > min_p

for i in params:
    params_dic[i[0]] = [float(i[1]), float(i[2])]
    params_error[i[0]] = [min(x[i[0]][ind]), max(x[i[0]][ind]), x[i[0]][best]]

param_keys = params_dic.keys()

''' Get linelist from inifile '''

inifile = datapath + 'pfw_' + run[:-1] + '.ini'
print('Reading ini file: ' + inifile)

fini = open(inifile)
for i, line in enumerate(fini):
    if i == 2:
        numlines = int(line)
fini.close()

print('Number of diagnostic lines: ' + str(numlines))
counter = 0
linenames = []
linestarts = []
linestops = []
line_norm_starts = []
line_norm_stops = []
line_norm_leftval = []
line_norm_rightval = []
fini = open(inifile)
for i, line in enumerate(fini):
    if counter < numlines:
        if (i == (3 + counter * 5)):
            line = line.strip()
            line = line.split(' ')[0]
            linenames.append(line)
        elif (i == (4 + counter * 5)):
            line = line.strip()
            linemin, linemax = line.split(' ')
            linestarts.append(float(linemin))
            linestops.append(float(linemax))
        elif (i == (6 + counter * 5)):
            line = line.strip()
            l1, l2, l3, l4 = line.split(' ')
            line_norm_starts.append(float(l1))
            line_norm_leftval.append(float(l2))
            line_norm_stops.append(float(l3))
            line_norm_rightval.append(float(l4))
            counter = counter + 1
fini.close()

''' Loading extra spectra '''

if extra_spectrum_path1 == '' and extra_spectrum_path1 == '':
    include_extra_spectra = False

if make_lineprofiles_plot and include_extra_spectrum:

    print('Reading extra spectra or models...')
    if extra_spectrum_path1 == '':
        pass
    elif os.path.exists(extra_spectrum_path1):
        cmfgenwave1, cmfgenflux1 = np.genfromtxt(extra_spectrum_path1).T
    else:
        print("Cannot plot extra spectra, file not found: " + extra_spectrum_path1)

    if extra_spectrum_path2 == '':
        pass
    elif os.path.exists(extra_spectrum_path2):
        cmfgenwave2, cmfgenflux2 = np.genfromtxt(extra_spectrum_path2).T
    else:
        print("Cannot plot extra spectra, file not found: " + extra_spectrum_path2)

''' Absolutely ridiculous workaround to get a legend on the param vs fitness plots '''
# Namely here I create a pdf that contains the colorbar,
# and then later I import the image of that colorbar into the figure

colorbar_jet_legend = "colorbar_generations_jet.jpg"

a = np.array([[0,1]])
plt.figure(figsize=(9, 3.5))
img = plt.imshow(a, cmap="jet")
plt.gca().set_visible(False)
cax = plt.axes([0.05, 0.3, 0.9, 0.1])#[0.3, 0.3, 0.8, 0.8])
cbar = plt.colorbar(cax=cax, orientation='horizontal', ticks=np.linspace(0,1,5))
cbar.ax.set_xticklabels(['0', str(int(0.25*float(maxgenid))), str(int(0.5*float(maxgenid))), str(int(0.75*float(maxgenid))), str(int(float(maxgenid)))],
    fontsize=30)
cbar.ax.set_xlabel('Generation',fontsize=30)
plt.savefig(colorbar_jet_legend, dpi=300)
plt.close()

''' ----------------------------------------------------------------------'''
'''                          Make pdfs with plots                         '''
''' ----------------------------------------------------------------------'''

''' ------------------------------------------'''
'''     Plot chi2 as function of generation   '''
''' ------------------------------------------'''
if make_chi2pgen_plot or full_short_manual in ('short', 'full'):

    print("Making chi2 as a function of generation plots ...")

    pdfs_chi2pgen = []

    mingen = 35
    last_X_generations = int(float(maxgenid)-mingen)
    generation_crop, mutation_crop = parallelcrop(generation, mutation, float(mingen), float(maxgenid))

    plotscalefactor = 12.0
    if maxgenid > last_X_generations + 10:
        fig, ax = plt.subplots(6,1, figsize=(plotscalefactor*1,plotscalefactor*1.41))
        make_zoom = True
    else:
        fig, ax = plt.subplots(3,1, figsize=(plotscalefactor*1,plotscalefactor*1.41))
        make_zoom = False

    ax[0].plot(median_chi2_per_gen, label=r'median $\chi^2$', color='C0')
    ax[1].plot(mean_chi2_per_gen, label=r'mean $\chi^2$', color='C0')
    ax[2].plot(lowest_chi2_per_gen, label=r'lowest $\chi^2$', color='C0')
    ax_0 = ax[0].twinx()
    ax_0.plot(generation, mutation, color='C1', label='mutation rate')
    ax_1 = ax[1].twinx()
    ax_1.plot(generation, mutation, color='C1', label='mutation rate')
    ax_2 = ax[2].twinx()
    ax_2.plot(generation, mutation, color='C1', label='mutation rate')

    # after 35 generations we expect the solution to have more or less converged
    # so if there are more than 45 generations carried out, we also make a zoomed
    # plot of the chi2 behavior
    if make_zoom:
        xaxisvalue = np.linspace(mingen+1, float(maxgenid), (float(maxgenid)-mingen))
        ax[3].plot(xaxisvalue, median_chi2_per_gen[-last_X_generations:], label=r'median $\chi^2$', color='C0')
        ax[4].plot(xaxisvalue, mean_chi2_per_gen[-last_X_generations:], label=r'mean $\chi^2$', color='C0')
        ax[5].plot(xaxisvalue, lowest_chi2_per_gen[-last_X_generations:], label=r'lowest $\chi^2$', color='C0')
        vspanvalues0_y = ax[3].get_ylim()
        vspanvalues1_y = ax[4].get_ylim()
        vspanvalues2_y = ax[5].get_ylim()
        ax_3 = ax[3].twinx()
        ax_3.plot(generation_crop, mutation_crop, color='C1', label='mutation rate')
        ax_4 = ax[4].twinx()
        ax_4.plot(generation_crop, mutation_crop, color='C1', label='mutation rate')
        ax_5 = ax[5].twinx()
        ax_5.plot(generation_crop, mutation_crop, color='C1', label='mutation rate')
        ax[3].set_ylabel(r'median $\chi^2$')
        ax[4].set_ylabel(r'mean $\chi^2$ p')
        ax[5].set_ylabel(r'lowest $\chi^2$')
        ax_3.set_ylabel('mutation rate')
        ax_4.set_ylabel('mutation rate')
        ax_5.set_ylabel('mutation rate')
        ax[3].axvspan(mingen, maxgenid, alpha=0.2, color='yellow')
        ax[4].axvspan(mingen, maxgenid, alpha=0.2, color='yellow')
        ax[5].axvspan(mingen, maxgenid, alpha=0.2, color='yellow')
        ax[3].set_title(r'median $\chi^2$ per generation (last ' + str(int(last_X_generations)) + ' generations)')
        ax[4].set_title(r'mean $\chi^2$ per generation (last ' + str(int(last_X_generations)) + ' generations)')
        ax[5].set_title(r'lowest $\chi^2$ per generation (last ' + str(int(last_X_generations)) + ' generations)')
        ax[3].set_xlabel('Generation')
        ax[4].set_xlabel('Generation')
        ax[5].set_xlabel('Generation')
        ax[3].legend(loc='upper left')
        ax[4].legend(loc='upper left')
        ax[5].legend(loc='upper left')
        ax_3.legend(loc='upper right')
        ax_4.legend(loc='upper right')
        ax_5.legend(loc='upper right')
        plt.tight_layout(rect=[0.0,0.5,.7,1.0]) # rect adds extra whitespace so the size of these plots
                                                # is more balanced with respect to the lineprofile plots etc.

    ax[0].set_ylabel(r'median $\chi^2$')
    ax[1].set_ylabel(r'mean $\chi^2$ p')
    ax[2].set_ylabel(r'lowest $\chi^2$')
    ax_0.set_ylabel('mutation rate')
    ax_1.set_ylabel('mutation rate')
    ax_2.set_ylabel('mutation rate')
    ax[0].set_title(r'median $\chi^2$ per generation')
    ax[1].set_title(r'mean $\chi^2$ per generation')
    ax[2].set_title(r'lowest $\chi^2$ per generation')
    ax[0].set_xlabel('Generation')
    ax[1].set_xlabel('Generation')
    ax[2].set_xlabel('Generation')
    ax[0].legend(loc='upper left')
    ax[1].legend(loc='upper left')
    ax[2].legend(loc='upper left')
    ax_0.legend(loc='upper right')
    ax_1.legend(loc='upper right')
    ax_2.legend(loc='upper right')

    if make_zoom:
        ax[0].axvspan(mingen, maxgenid, alpha=0.2, color='yellow')
        ax[1].axvspan(mingen, maxgenid, alpha=0.2, color='yellow')
        ax[2].axvspan(mingen, maxgenid, alpha=0.2, color='yellow')

    if not make_zoom:
        plt.tight_layout(rect=[0.0,0.5,.7,1.0])# rect adds extra whitespace so the size of these plots
                                                # is more balanced with respect to the lineprofile plots etc.
        plt.savefig(plotpath + name_chi2pergen_plot)
        plt.close()

    if make_zoom:
        plt.tight_layout(rect=[0.0,0.0,0.7,1.0])
        plt.savefig(plotpath + name_chi2pergen_plot)
        plt.close()

''' ------------------------------------------'''
'''   Plot fitness as function of parameter   '''
''' ------------------------------------------'''
if make_fitnessdistribution_plot or full_short_manual in ('short', 'full'):

    print("Making fitness vs parameter plots...")

    nrows_ppage = nrows_fitnessparamplot # Can be changed, all lines will be plotted, the number of
                    # number of pages will be adapted accordingly.
    ncols_ppage = ncols_fitnessparamplot # Same holds for the columns.
    plotscalefactor = 12.0 # This sets the 'absolute size' of the line profile
                             # plots. The plots are always on A4 format, but the
                             # size of the labels are relative to the size of the
                             # plotting canvas. Therefore a higher number here
                             # means smaller labels on the plots.
    plots_ppage = nrows_ppage * ncols_ppage
    npages = int(math.ceil(1.0*len(param_keys) / plots_ppage))
    print("Plotting fitness vs parameter on " + str(int(npages)) + " page(s).")

    gen_id = map(lambda q: float(q[:4]), x['run_id'])
    gen_id_scaled = np.array(gen_id) / max(gen_id)
    scatter_colors = cm_rgba(gen_id_scaled)

    fitparam_jpg_names = []
    lp = -1 # paramater counter
    for apage in xrange(npages):
        fig, ax = plt.subplots(nrows_ppage, ncols_ppage, figsize=(plotscalefactor*1., plotscalefactor*1.41))
        for arow in xrange(nrows_ppage):
            for acol in xrange(ncols_ppage):
                lp = lp + 1 # Next parameter is plotted
                im1 = ax[arow,acol].scatter(x[param_keys[lp]].values, x['fitness'].values,
                    s=10.0, c=scatter_colors)
                ax[arow,acol].set_ylim(0, 1.1*np.max(x['fitness'].values))
                ax[arow,acol].axvspan(params_error[param_keys[lp]][0], params_error[param_keys[lp]][1], alpha=0.3, color='red')
                ax[arow,acol].set_xlim(params_dic[param_keys[lp]][0], params_dic[param_keys[lp]][1])
                ax[arow,acol].set_title(param_keys[lp])#, fontsize=14)
                ax[arow,acol].axvspan(params_error[param_keys[lp]][0],
                    params_error[param_keys[lp]][1], alpha=0.3, color='red')

                print(param_keys[lp] + ' - ' + str(params_error[param_keys[lp]][2]) + '    [' + str(params_error[param_keys[lp]][0]) + ', ' +  str(params_error[param_keys[lp]][1]) + ']')

        # Load the earlier produced colorbar/legend.
        # Sorry for this very desparate workaround
        imlegend = plt.imread(colorbar_jet_legend)
        newax = fig.add_axes([0.72, 0.885, 0.25, 0.2], anchor='C')
        newax.imshow(imlegend)
        newax.axis('off')

        plt.suptitle('Fitness vs. parameter (all lines)', fontsize=16)
        plt.tight_layout(rect=[0, 0.00, 1.0, 0.95])

        fit_param_pagename = plotpath + 'overview_' + str(int(apage)) + '.jpg'
        fitparam_jpg_names.append(fit_param_pagename)
        plt.savefig(fit_param_pagename, dpi=400)

    with open(plotpath + name_fitnessdistribution_plot, "wb") as out_file:
        out_file.write(img2pdf.convert(fitparam_jpg_names))

    for ajpg in fitparam_jpg_names:
        os.system("rm " + ajpg)

    # # Merge all line profile plot pages into one document.
    # if os.path.isfile(plotpath + name_fitnessdistribution_plot):
    #     os.system("rm " + plotpath + name_fitnessdistribution_plot)
    # merger = PdfFileMerger()
    # for filename in fitparam_pdf_names:
    #     merger.append(PdfFileReader(file(filename, 'rb')))
    # merger.write(plotpath + name_fitnessdistribution_plot)
    #
    #
    # # Remove the individual pages after the file has been merged.
    # for filename in fitparam_pdf_names:
    #     os.system("rm " + filename)

''' ------------------------------------------'''
'''               Plot line profiles          '''
''' ------------------------------------------'''

if make_lineprofiles_plot or full_short_manual in ('short', 'full'):

    print("Making lineprofile plots...")

    def renormalize_line(wave, flux, l1, l2, l3, l4):
        ''' Renormalises the line with a straight line, given the values in
            the ini file. (As in pyGA).

            Input:
            - wave = wavelength array
            - flux = flux array (already more or less normalised)
            - l1 = left wavelength point
            - l2 = left continuum offset at l1
            - l3 = right wavelength point
            - l4 = right continuum offset at l3

            Output:
            - renormflux = renormalised flux
        '''

        m = (l4 - l2) / (l3 - l1)
        b = l2 - m * l3 + 1
        y = m * wave + b
        renormflux = flux/y

        return renormflux

    ''' Selecting which models to plot and unpacking them '''

    # 1. Selection and identification of best fitting model
    # #FIXME see comments on 'best_models_cutoff_method' parameter at the top

    scaled_redchi2 = x['chi2']/min_redchi2_value
    x = x.assign(scaledRedChi2=scaled_redchi2)
    x['scaledRedChi2'] = scaled_redchi2
    if best_models_cutoff_method == 'P':
        x_best = x.loc[x['P'] > 0.05]
    else:
        x_best = x.loc[x['scaledRedChi2'] < best_models_cutoff_method]

    the_best_model = x.loc[x['scaledRedChi2'] == 1.0] #FIXME should instead just select lowest chi2
    print("Amount of models with lowest chi2 is: " + str(int(len(the_best_model))))
    the_best_model = the_best_model.iloc[0]
    the_best_model = the_best_model['run_id']
    print("The best model: " + the_best_model)
    bestmodeldir = datapath_output +  the_best_model[:4] + '/' + the_best_model

    # 2. Saving directories of the to be plotted models.
    best_models = x_best['run_id'].values
    best_models_dirs = []
    best_models_tar = []
    for bm in best_models:
        best_models_dirs.append(datapath_output +  bm[:4] + '/' + bm)
        best_models_tar.append(datapath_output + bm[:4] + '/' + bm + '.tar.gz')

    # 3. Unzipping the profile tars of the to be plotted models.
    # Note that it doesn't explicitly check whether all the profiles are
    # there, but only whether there is already a folder with the name of
    # the model. If there is not, it assumes there is a unzipped tar.gz
    # file and it starts unpacking (and removes the tar.gz afterwards)
    print("Loading " + str(len(x_best)) + " best fitting models...")
    for bmtar, bmdir in zip(best_models_tar, best_models_dirs):
        if not os.path.isdir(bmdir):
            mkdircommand = 'mkdir ' + bmdir
            untarcommand = 'tar -C ' + bmdir + ' -xzf ' + bmtar
            rmtarcommand = 'rm ' + bmtar
            os.system(mkdircommand)
            os.system(untarcommand)
            os.system(rmtarcommand)

    ''' Setup page layout '''
    nrows_ppage = nrows_lineprofileplot # Can be changed, all lines will be plotted, the number of
                    # number of pages will be adapted accordingly.
    ncols_ppage = ncols_lineprofileplot # Same holds for the columns.
    plotscalefactor = 12.0 # This sets the 'absolute size' of the line profile
                             # plots. The plots are always on A4 format, but the
                             # size of the labels are relative to the size of the
                             # plotting canvas. Therefore a higher number here
                             # means smaller labels on the plots.
    plots_ppage = nrows_ppage * ncols_ppage
    npages = int(math.ceil(1.0*numlines / plots_ppage))
    print("Plotting the line profiles on " + str(int(npages)) + " pages.")

    ''' Reading the spectrum '''
    wave, flux, error = np.genfromtxt(normpath).T

    lc = -1 # line counter
    line_pdf_names = []

    # For each page we set up a grid of axes, each plotting the models and
    # data of one diagnostic line.
    for apage in xrange(npages):
        fig, ax = plt.subplots(nrows_ppage, ncols_ppage, figsize=(plotscalefactor*1., plotscalefactor*1.41))
        for arow in xrange(nrows_ppage):
            for acol in xrange(ncols_ppage):
                lc = lc + 1 # Next diagnostic line is plotted

                if lc < numlines: # As long as not all the lines are plotted.
                    print("Plotting " + linenames[lc])

                    ''' Plot the models '''
                    for bmdir in best_models_dirs:
                        linefile_tmp = bmdir + '/' + linenames[lc] + '.prof.fin'
                        linewave_tmp, lineflux_tmp = np.genfromtxt(linefile_tmp).T
                        if bmdir != bestmodeldir:
                            # The x best models in green
                            ax[arow, acol].plot(linewave_tmp, lineflux_tmp, color='green')
                        else:
                            # The best fitting model in red, note the high zorder so that it is on top.
                            # +20 in zorder is a bit random, but I thought, I don't now how many
                            # other things line axhlines, have orders higher than the amount of lines.
                            ax[arow, acol].plot(linewave_tmp, lineflux_tmp, color='red', zorder=len(best_models_dirs)+20, lw=2.0)

                    ''' Plot the data '''
                    # The spectra are cropped because otherwise plotting
                    # will take up a lot of memory. (The set_xlim might therefore
                    # be a bit unnecessary)
                    wave_tmp, flux_tmp = parallelcrop(wave, flux, linestarts[lc], linestops[lc])
                    wave_tmp, error_tmp = parallelcrop(wave, error, linestarts[lc], linestops[lc])
                    ax[arow, acol].axhline(1.0, color='grey', lw=1.0)
                    ax[arow, acol].set_title(linenames[lc])
                    ax[arow, acol].set_xlim(linestarts[lc], linestops[lc])
                    flux_renorm_tmp = renormalize_line(wave_tmp, flux_tmp, line_norm_starts[lc], line_norm_leftval[lc], line_norm_stops[lc], line_norm_rightval[lc])
                    ax[arow, acol].errorbar(wave_tmp, flux_renorm_tmp, yerr=error_tmp, marker='o', markersize=0.1, linestyle='None', color='black')

                    ''' Plot extra spectrum if wanted '''
                    if include_extra_spectrum:
                        try:
                            cmfgenwave_tmp, cmfgenflux_tmp = parallelcrop(cmfgenwave1, cmfgenflux1, linestarts[lc], linestops[lc])
                            ax[arow, acol].plot(cmfgenwave_tmp, cmfgenflux_tmp, color='blue', lw=2.0, zorder=len(best_models_dirs)+20)
                        except:
                            print(">> Something might have gone wrong with plotting the extra spectra ")
                        try:
                            cmfgenwave_tmp, cmfgenflux_tmp = parallelcrop(cmfgenwave2, cmfgenflux2, linestarts[lc], linestops[lc])
                            ax[arow, acol].plot(cmfgenwave_tmp, cmfgenflux_tmp, color='orange', lw=2.0, zorder=len(best_models_dirs)+21)
                        except:
                            print(">> Something might have gone wrong with plotting the extra spectra ")
                else:
                    # We loop through all axes (rows, cols) that are set up,
                    # but if there are not enough lines to fill a page axes
                    # are removed so that the space is truly empty.
                    ax[arow, acol].axis('off')

        plt.tight_layout()

        # Saving page in pdf. Pdfs will be sticked later.
        pdfname_tmp = plotpath + 'line_profiles_p' + str(int(apage)) + '.pdf'
        line_pdf_names.append(pdfname_tmp)
        plt.savefig(pdfname_tmp)
        plt.close()
        print("Lineprofiles: saved page " + str(int(apage+1)) + " out of " + str(int(npages)) + " pages.")

    # Merge all line profile plot pages into one document.
    if os.path.isfile(plotpath + name_lineprofiles_plot):
        os.system("rm " + plotpath + name_lineprofiles_plot)
    merger = PdfFileMerger()
    for filename in line_pdf_names:
        merger.append(PdfFileReader(file(filename, 'rb')))
    merger.write(plotpath + name_lineprofiles_plot)

    # Remove the individual pages after the file has been merged.
    for filename in line_pdf_names:
        os.system("rm " + filename)

''' ------------------------------------------'''
'''   Videos of parameter space exploration   '''
''' ------------------------------------------'''
if (full_short_manual == 'manual' and make_paramspace_avi) or full_short_manual in ('full'):

    for param_pair in param_pair_list_avi:

        xparam = param_pair[0]
        yparam = param_pair[1]
        print("Making video of paramater space exploration of " + xparam + ", " + yparam)

        imnames = xparam + '_' + yparam + '_exploration'

        for the_genid in unique_genid:

            x_specific_gen = x.loc[x['gen_id'] == the_genid]
            p1_tmp = x_specific_gen[xparam].values
            p2_tmp = x_specific_gen[yparam].values
            chi_tmp = x_specific_gen['chi2'].values

            chi_tmp_colorscale = chi_tmp/(np.median(chi_tmp)*10)
            color_array = cm_rgba(chi_tmp_colorscale)

            themutationrate = mutation[int(the_genid)]
            themutationrate = '{0:.4f}'.format(themutationrate)
            fig, ax = plt.subplots()
            ax.set_title("Gen = " + the_genid + ", Mutation rate = " + themutationrate)
            ax.scatter(p1_tmp, p2_tmp, c=color_array)
            ax.set_xlim(*params_dic[xparam])
            ax.set_ylim(*params_dic[yparam])
            ax.set_xlabel(xparam)
            ax.set_ylabel(yparam)
            plt.savefig(plotpath + imnames + str(the_genid) + '.png')
            plt.close()

        image_folder = plotpath
        video_name = plotpath + '00_' + imnames + '.avi'

        images = [img for img in os.listdir(image_folder) if (img.startswith(imnames) and img.endswith('.png'))]
        frame = cv2.imread(os.path.join(image_folder, images[0]))
        height, width, layers = frame.shape

        video = cv2.VideoWriter(video_name, 0, 10, (width,height))

        for image in images:
            video.write(cv2.imread(os.path.join(image_folder, image)))
            os.system("rm " + os.path.join(image_folder, image))

        cv2.destroyAllWindows()
        video.release()



''' ------------------------------------------'''
'''   Plots of correlation between parameters '''
''' ------------------------------------------'''

if make_correlation_plot or full_short_manual in ('short', 'full'):
    print("Making plots of correlation between parameters")

    paramlist = params_dic.keys()

    nbins1 = 10
    nbins2 = nbins1

    colorbar_chi2 = 'viridis'
    colorbar_other = 'viridis_r'

    plotscalefactor = 12.0

    correlation_plot_list = ['chi2']
    if make_correlation_per_line_plot or full_short_manual in ('full'):

        for diagnostic in linenames:
            correlation_plot_list.append(diagnostic)
        all_jpg_names = []

    make_legend = True
    for which_plot in correlation_plot_list:

        fig, ax = plt.subplots(len(paramlist)-1,len(paramlist)-1, figsize=(plotscalefactor*1.0, plotscalefactor*1.0))
        if which_plot == 'chi2':
            the_colormap = colorbar_chi2
        else:
            the_colormap = colorbar_other

        if which_plot == 'chi2':
            print("Making correlation plot for all lines together ")
        else:
            print("Making correlation plot for " + which_plot)

        param_pairs_correlation = []
        for ii in xrange(len(paramlist)):
            for jj in xrange(len(paramlist)):
                xparam = paramlist[ii]
                yparam = paramlist[jj]
                if (xparam != yparam) and ([yparam, xparam] not in param_pairs_correlation):

                    #print("Assessing correlation between " + xparam + " and " + yparam )

                    param_pairs_correlation.append([xparam, yparam])

                    p1_tmp = x[xparam].values
                    p2_tmp = x[yparam].values
                    chi_tmp = x[which_plot].values

                    p1min, p1max = params_dic[xparam]
                    p2min, p2max = params_dic[yparam]

                    p1_space = np.linspace(p1min, p1max, nbins1)
                    p2_space = np.linspace(p2min, p2max, nbins2)
                    p1_lowbound = p1_space[:-1]
                    p1_upbound = p1_space[1:]
                    p2_lowbound = p2_space[:-1]
                    p2_upbound = p2_space[1:]

                    chi2_matrix = []

                    for nb in xrange(nbins1-1):
                        matrix_axis = []
                        for nb in xrange(nbins2-1):
                            matrix_axis.append([])
                        chi2_matrix.append(matrix_axis)

                    for ap1, ap2, achi in zip(p1_tmp, p2_tmp, chi_tmp):
                        i1 = 0
                        for p1low, p1up in zip(p1_lowbound, p1_upbound):
                            i2 = 0
                            for p2low, p2up in zip(p2_lowbound, p2_upbound):
                                if (p1low < ap1) and (p1up > ap1) and (p2low < ap2) and (p2up > ap2):
                                    if achi != 999999999:
                                        chi2_matrix[i1][i2].append(float(achi))
                                i2 = i2 + 1
                            i1 = i1 + 1

                    for i1 in xrange(nbins1-1):
                        for i2 in xrange(nbins2-1):
                            if len(chi2_matrix[i1][i2]) > 0:
                                median_tmp = np.median(chi2_matrix[i1][i2])
                            else:
                                median_tmp = float('nan')
                            chi2_matrix[i1][i2] = median_tmp

                    if which_plot == 'chi2' and (np.log10(np.nanmax(chi2_matrix)) - np.log10(np.nanmin(chi2_matrix)) > 2.0):
                        # Fidle with scale of correlation plot
                        # if the differences between the chi2s are very large, it may be
                        # better to use a log scale
                        chi2_matrix = np.log10(chi2_matrix)
                        #print("Using log scale for correlation plot ")
                    else:
                        chi2_matrix = np.array(chi2_matrix)

                    chi2_matrix = chi2_matrix.T # so that x and y correspond with how we normally define x and y.
                    ax[jj-1][ii].imshow(chi2_matrix, cmap=the_colormap)
                    ax[jj-1][ii].set_yticks([])
                    ax[jj-1][ii].set_xticks([])
                    if jj == len(paramlist)-1:
                        ax[jj-1][ii].set_xlabel(xparam)
                    if ii == 0:
                        ax[jj-1][ii].set_ylabel(yparam)

                else:
                    if jj != 0 and ii < len(paramlist)-1:
                        ax[jj-1][ii].axis('off')
                        if jj == 1 and ii == len(paramlist)-2:
                            if which_plot == 'chi2':
                                ax[jj-1][ii].text(0.5, 0.8, 'Correlation\n(all lines)', ha='center', va='center', transform=ax[jj-1][ii].transAxes)
                            else:
                                ax[jj-1][ii].text(0.5, 0.8, 'Correlation\n(' + which_plot + ')', ha='center', va='center',transform=ax[jj-1][ii].transAxes)
                        if jj == 1 and ii == len(paramlist)-2:
                            if which_plot == 'chi2':
                                if the_colormap.endswith('_r'):
                                    the_colormap = the_colormap[-2]
                                else:
                                    the_colormap = the_colormap + '_r'
                            else:
                                doreverse = False
                            img = sns.heatmap(np.array([[0,1]]), ax=ax[jj-1][ii], mask=np.array([[True,True]]),
                            cbar_kws={"orientation": "horizontal"}, cmap=the_colormap)
                            cbar = img.collections[0].colorbar
                            cbar.set_ticks([.2, .8])
                            cbar.set_ticklabels(['<-- worse fit', '\nbetter fit -->'])

        plt.tight_layout()
        plt.subplots_adjust(hspace=0.02, wspace=0.05)
        if which_plot == 'chi2':
            plt.savefig(plotpath + name_correlation_plot) #FIXME I would like to have this figure too, on A4 page format...
                                                      # However editing canvas size or rect in tight_layout spreads the plots over
                                                      # the full A4, I want them an the top of middle, but in a square, not spread out :(
            plt.close()
        else:
            the_jpg_name = plotpath + 'correlation_' + which_plot + '.jpg'
            all_jpg_names.append(the_jpg_name)
            plt.savefig(the_jpg_name)
            plt.close()

    if make_correlation_per_line_plot or full_short_manual in ('full'):
        with open(plotpath + name_correlation_per_line_plot, "wb") as out_file:
            out_file.write(img2pdf.convert(all_jpg_names))

        for ajpg in all_jpg_names:
            os.system("rm " + ajpg)


''' ------------------------------------------'''
'''  Plot fitn per line as function of param  '''
''' ------------------------------------------'''

if (full_short_manual == 'manual' and make_fitnessdistribution_per_line_plot) or full_short_manual in ('full'):

    print("Making fitness per parameter plots per diagnostic line...")

    fitparam_jpg_names = []

    for diagnostic in linenames:

        nrows_ppage = nrows_fitnessparamplot # Can be changed, all lines will be plotted, the number of
                        # number of pages will be adapted accordingly.
        ncols_ppage = ncols_fitnessparamplot # Same holds for the columns.
        plotscalefactor = 12.0 # This sets the 'absolute size' of the line profile
                                 # plots. The plots are always on A4 format, but the
                                 # size of the labels are relative to the size of the
                                 # plotting canvas. Therefore a higher number here
                                 # means smaller labels on the plots.
        plots_ppage = nrows_ppage * ncols_ppage
        npages = int(math.ceil(1.0*len(param_keys) / plots_ppage))
        print("Plotting fitness vs parameter for " + diagnostic + " on " + str(int(npages)) + " page(s).")

        gen_id = map(lambda q: float(q[:4]), x['run_id'])
        gen_id_scaled = np.array(gen_id) / max(gen_id)
        scatter_colors = cm_rgba(gen_id_scaled)

        lp = -1 # paramater counter
        for apage in xrange(npages):
            fig, ax = plt.subplots(nrows_ppage, ncols_ppage, figsize=(plotscalefactor*1., plotscalefactor*1.41))
            for arow in xrange(nrows_ppage):
                for acol in xrange(ncols_ppage):
                    lp = lp + 1 # Next parameter is plotted
                    line_fitness = x[diagnostic].values
                    ax[arow,acol].scatter(x[param_keys[lp]].values, line_fitness,
                        s=10.0, c=scatter_colors, label=diagnostic)
                    ax[arow,acol].set_ylim(0, 1.1*np.max(line_fitness))
                    ax[arow,acol].axvspan(params_error[param_keys[lp]][0], params_error[param_keys[lp]][1], alpha=0.3, color='red')
                    ax[arow,acol].set_xlim(params_dic[param_keys[lp]][0], params_dic[param_keys[lp]][1])
                    ax[arow,acol].set_title(param_keys[lp])#, fontsize=14)
                    ax[arow,acol].axvspan(params_error[param_keys[lp]][0],
                        params_error[param_keys[lp]][1], alpha=0.3, color='red')
                    ax[arow,acol].legend(loc=2)

            # Load the earlier produced colorbar/legend.
            # Sorry for this very desparate workaround
            imlegend = plt.imread(colorbar_jet_legend)
            newax = fig.add_axes([0.72, 0.885, 0.25, 0.2], anchor='C')
            newax.imshow(imlegend)
            newax.axis('off')

            plt.suptitle('Fitness vs. parameter (' + diagnostic + ')', fontsize=16)
            plt.tight_layout(rect=[0, 0.00, 1.0, 0.95])
            fit_param_pagename_jpg = plotpath + 'overview_' + diagnostic + '_' + str(int(apage)) + '.jpg'
            fitparam_jpg_names.append(fit_param_pagename_jpg)
            plt.savefig(fit_param_pagename_jpg)#, dpi=100)
            plt.close()

    with open(plotpath + name_fitness_per_line_plot, "wb") as out_file:
        out_file.write(img2pdf.convert(fitparam_jpg_names))

    for ajpg in fitparam_jpg_names:
        os.system("rm " + ajpg)



''' --------------- OUTPUT -------------------'''
'''   Merge all available pdfs into rerport   '''
''' ------------------------------------------'''

''' Short report '''
# Does not contain the correlation and fitness plots per line.
# In the end, merge all line profile plot pages into one document.
osld = os.listdir(plotpath)
report_pdfs = []
for afile in osld:
    if afile.endswith('_GAreport.pdf') and 'short' in afile:
        report_pdfs.append(plotpath + afile)

if os.path.isfile(name_shortreport_pdf):
    os.system("rm " + name_shortreport_pdf)
merger = PdfFileMerger()
for filename in report_pdfs:
    merger.append(PdfFileReader(file(filename, 'rb')))
merger.write(name_shortreport_pdf)
print("Short report saved to:")
print(name_shortreport_pdf)

''' Full report '''
# Containing all plots
# In the end, merge all line profile plot pages into one document.
osld = os.listdir(plotpath)
report_pdfs = []
for afile in osld:
    if afile.endswith('_GAreport.pdf'):
        report_pdfs.append(plotpath + afile)

if os.path.isfile(name_fullreport_pdf):
    os.system("rm " + name_fullreport_pdf)
merger = PdfFileMerger()
for filename in report_pdfs:
    merger.append(PdfFileReader(file(filename, 'rb')))
merger.write(name_fullreport_pdf)
print("Full report saved to:")
print(name_fullreport_pdf)
