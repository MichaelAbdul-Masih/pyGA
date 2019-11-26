#! /usr/bin/env python
"""

GA_analysis.py
Version 2.0

MORE INFO: http://staff.science.uva.nl/~ftramper --> GA


Enthought:
http://www.enthought.com/products/edudownload.php

Matplotlib:
http://matplotlib.sourceforge.net/users/installing.html

SciPy:
http://www.scipy.org/Installing_SciPy

Paramiko:
http://www.lag.net/paramiko/


"""

#IMPORT LIBRARIES

import os
import tarfile
import fnmatch
import sys
import string
import shutil
import time
import datetime
import copy as cp
from math import ceil
from PyPDF2 import PdfFileMerger
from PIL import Image
import glob
try:
    import matplotlib as mpl
    import matplotlib.pyplot as plt
    import matplotlib.style
    mpl.style.use('classic')
    from matplotlib import cm
    from pylab import *
    from matplotlib.backends.backend_pdf import PdfPages
except:
    print 'WARNING - Needs to have the MatPlotLib package installed.'
    print 'WARNING - Install instructions at http://matplotlib.sourceforge.net/users/installing.html'
    exit()
try:
    import numpy
    from scipy.stats import stats
    from scipy.interpolate import interp1d
    from scipy.interpolate import griddata
except:
    print 'WARNING - Needs to have the SciPy package installed.'
    print 'WARNING - Install instructions at http://www.scipy.org/Installing_SciPy'
    exit()
try:
    import paramiko
except:
    print 'WARNING - Needs to have the Paramiko package installed.'
    print 'WARNING - Install instructions at http://www.lag.net/paramiko/'
    exit()


#GLOBAL STUFF

maindir = os.getcwd()
username = '' #LISA, leave as empty string except for testing
password = '' #LISA, leave as empty string except for testing
FWversion = '10.3.1' # Change when using new Fastwind version (used in .par file)
GAversion = '1.33' # Change when using new GA version (used in .par file)
version = '1.33'
object = ''
minimumP = 0.05 # Cut-off P-value when calculating errors
numberOfParameters = 16 # Total number of parameters
objectFlag = False
setupFlag = False
models = []
lines = []
clickValue = [0,0]

fit_parameters = ['Teff', 'logg', 'mdot', 'beta', 'vinf', 'NHe', 'vtur', 'vrot', 'nitrogen', 'carbon', 'oxygen', 'silicon', 'fcl', 'fic', 'fvel']

all_parameters = ['Teff', 'logg', 'mdot', 'vinf', 'beta', 'NHe', 'vtur', 'vrot', 'vmacro', 'nitrogen', 'carbon', 'oxygen', 'silicon', 'fcl', 'fic', 'fvel', 'luminosity', 'dmom', 'mass', 'radius', 'fitness', 'chi2', 'P']

diagnostic_parameters = ['fitness', 'chi2', 'P']

all_parameter_names_dict = {'Teff':'Teff', 'logg':'log g', 'mdot':'log Mdot', 'vinf':'v_inf', 'beta':'beta', 'NHe':'nHe/nH', 'vtur':'v_tur', 'vrot':'vsin(i)', 'vmacro':'v_macro', 'nitrogen':'nitrogen', 'carbon':'carbon', 'oxygen':'oxygen', 'silicon':'silicon', 'fcl':'fcl', 'fic':'fic', 'fvel':'fvel', 'phosphorus':'phosphorus', 'luminosity':'luminosity', 'dmom':'dmom', 'mass':'mass', 'radius':'radius', 'fitness':'Fitness', 'chi2':'Chi2', 'P':'Probability'}

read_dict = {'Teff':1, 'logg':2, 'mdot':3, 'vinf':4, 'beta':5, 'NHe':6, 'vtur':7, 'vrot':8, 'vmacro':9, 'nitrogen':10, 'carbon':11, 'oxygen':12, 'phosphorus':14, 'silicon':13, 'fcl':14, 'fic':15, 'fvel':16}

#CLASSES FOR STORING STUFF

class model():
    pass

class setup():
    pass

class lineParameters():
    pass


################
### START-UP ###
################

def startUp():
    global object, objectFlag
    while True:
        os.chdir(maindir)
        object = ''
        objectFlag = False
        printHeader()
        print 'Must be run from your GA_Analysis directory'
        print 'Current directory is: ' + os.getcwd()
        print
        new, running, finished = searchJobs()
    #Check for object from command line, if its there continue where needed
        try:
            object = sys.argv[1]
            objectFlag = True
        except:
            objectFlag = False
        if objectFlag:
            if checkFile(object+'.norm', maindir+'/new/'): createJob(object)
            elif checkFile(object+'.run', maindir + '/running/'): resumeMonitoring(object)
            elif checkFile(object, maindir): postAnalysis(object)
            else:
                print 'Input object \''+object+'\' not found!!'
                raw_input('Press Enter to continue')
                restart()
        else:
        #No input object given, print main menu
            print 'MAIN MENU'
            print
            print '1. Create new job'
            print '2. Resume monitoring a running job'
            print '3. Open finished job'
            print '0. Exit'
            print
            choice = raw_input('Choose an option: ')
            if choice is '1': createJob(new)
            elif choice is '2': resumeMonitoring(running)
            elif choice is '3': postAnalysis(finished)
            elif choice is '0':
                print 'Bye bye'
                exit()


def searchJobs():
    #Checks the contents of the directories, determines what is running and finished and which jobs can be created
    new = []
    running = []
    finished = []
    #The following loop determines all finished jobs
    for fileName in os.listdir(os.getcwd()):
        if not fnmatch.fnmatch(fileName, 'setup'):
            if not fnmatch.fnmatch(fileName, 'running'):
                if not fnmatch.fnmatch(fileName, 'new'):
                    if os.path.isdir(fileName):
                        finished.append(fileName)
    #Determine running jobs
    for fileName in os.listdir('running'):
        if fnmatch.fnmatch(fileName, '*.run'):
            running.append(string.strip(fileName,'.run'))
    #Determine possible new jobs
    for fileName in os.listdir('new'):
        if fnmatch.fnmatch(fileName, '*.norm'):
            new.append(string.strip(fileName,'.norm'))
    return new, running, finished


####################
### JOB CREATION ###
####################

def createJob(fileList):
    printHeader()
    global object
    print 'NEW OBJECT'
    print
    if objectFlag:
        print 'Creating new object \'' + object + '\'!'
        print
    if not objectFlag:
        if len(fileList) == 0:
            print 'No norm files present'
            raw_input('Press Enter to continue')
            return
        else:
            print 'Norm files present: '+str(len(fileList))
            j = 1
            for i in range(len(fileList)):
                print str(j) + '. ' + fileList[i]
                j = j + 1
            print '0. Main Menu'
            print
            choice = ''
            while choice is '':
                choice = raw_input('Choose an object to create: ')
                if choice is '0': return
                try:
                    choice = int(choice)
                    if choice > (j - 1):
                        print 'Incorrect choice'
                        choice = ''
                    if choice < 1:
                        print 'Incorrect choice'
                        choice = ''
                except:
                    print 'Incorrect choice'
                    choice = ''
            object = fileList[choice-1]
    try:
        os.mkdir(maindir+'/new/'+object)
    except:
        print 'Directory already exists!'
        choice = ''
        while choice is '':
            choice = raw_input('Do you want to continue (y/n)?' )
            if choice is 'y': break
            if choice is 'n': restart()
            else: choice = ''
    os.chdir(maindir+'/new/'+object)
    shutil.move('../'+object+'.norm',object+'.norm')
    print 'Changed to object directory: '+os.getcwd()
    print
    try:
        setup = maindir+'/setup/'+sys.argv[2]+'.setup'
        setup = readSetupFile(setup)
        global setupFlag
        setupFlag = True
        print 'Input setup file \'' + sys.argv[2] + '.setup\' found!'
    except:
        print 'No setup file specified or setup file not found.'
    if not setupFlag:
        print 'The following setup files are available:'
        fileList = os.listdir(maindir+'/setup')
        j = 1
        fileNames = []
        for i in range(len(fileList)):
            if fnmatch.fnmatch(fileList[i],'*.setup'):
                print '* '+str(j)+'. ' + fileList[i]
                j = j + 1
                fileNames.append(fileList[i])
        print
        choice = ''
        while choice is '':
            choice = raw_input('Choose a setup option: ')
            try:
                if int(choice) < int(j):
                    setup = maindir+'/setup/' + fileNames[int(choice)-1]
                    setup = readSetupFile(setup)
                    print
                    print fileNames[int(choice)-1] + ' succesfully read in.'
                else:
                    print 'Incorrect choice'
                    choice = ''
            except:
                print 'Incorrect choice'
                choice = ''
    print
    writeIniFile(setup)
    checkNorm()
    setup = updateSetup(setup)
    writeIniFile(setup)
    writeJobFile(setup)
    print ''
    choice = ''
    while choice is '':
        choice = raw_input('Do you want to monitor the LISA session (y/n)? ')
        if choice is 'y': monitor = True
        elif choice is 'n': monitor = False
        else:
            print 'Incorrect choice, try again'
            choice = ''
    doLisa(monitor)

def updateSetup(result):
    print
    print 'Updating inifile...'
    global lines
    result.lines=[]
    for i in range(result.numberOfLines):
        id = lines[i].ID
        resolution = lines[i].resolution
        leftLambda = lines[i].WLleft
        rightLambda = lines[i].WLright
        leftOffset = lines[i].fluxCorrectionLeft
        rightOffset = lines[i].fluxCorrectionRight
        weight = lines[i].weight
        result.lines.append([id, leftLambda, leftOffset, rightLambda, rightOffset, weight, resolution])
    print
    return result


def readSetupFile(fileName):
    # Read in an existing setup file
    try:
        #Check for RV and magnitude
        result = setup()
        result.rv, result.magnitude = checkForObject()
        global username
        if result.rv is '': result.rv = float(raw_input('Enter the radial velocity of ' + object + ': '))
        file = open(fileName, 'r')
        fileContent_tmp = file.readlines()
        file.close()
        fileContent=[]
        # Remove the empty lines:
        for line in fileContent_tmp:
            if not line.strip():
                continue
            else: fileContent.append(line)
        # Assign first part of file to parameters
        currentLine=1
        columns = fileContent[currentLine].strip().split()
        result.username = columns[0]
        username = columns[0]
        currentLine = currentLine + 1
        columns = fileContent[currentLine].strip().split()
        result.walltime = columns[0]
        currentLine = currentLine + 1
        columns = fileContent[currentLine].strip().split()
        result.nodes = columns[0]
        currentLine = currentLine + 1
        columns = fileContent[currentLine].strip().split()
        result.numberOfIndividuals = int(columns[0])
        currentLine = currentLine + 1
        columns = fileContent[currentLine].strip().split()
        result.numberOfGenerations = int(columns[0])
        currentLine = currentLine + 1
        columns = fileContent[currentLine].strip().split()
        result.errors = int(columns[0])
        currentLine = currentLine + 1
        columns = fileContent[currentLine].strip().split()
        result.metallicity = float(columns[0])
        result.N = float(columns[1])
        result.C = float(columns[2])
        result.O = float(columns[3])
        result.Si = float(columns[4])
        result.P = float(columns[5])
        if result.metallicity == 0: result.metallicity = raw_input('Enter the metallicity of ' + object + ': ')
        currentLine = currentLine + 1
        columns = fileContent[currentLine].strip().split()
        result.zdependance = int(columns[0])
        currentLine = currentLine + 1
        columns = fileContent[currentLine].strip().split()
        result.magnitudechoice = int(columns[0])
        if result.magnitude is '':
            if result.magnitudechoice == 0: result.magnitude = float(raw_input('Enter the V magnitude of ' + object + ': '))
            elif result.magnitudechoice == 1: result.magnitude = float(raw_input('Enter the K magnitude of ' + object + ': '))
            elif result.magnitudechoice == -10: result.magnitude = float(raw_input('Enter the radius of ' + object + ': '))
            else:
                print 'Illegal input option at magnitude line (must be 0 or 1 or -10)'
                raw_input('Press Enter to continue')
                restart()

        print "Read in the parameters"
        # Read in the parameters
        currentLine = currentLine + 1
        columns = fileContent[currentLine].strip().split()
        result.Teff_min, result.Teff_max, result.Teff_step = float(columns[0]), float(columns[1]), float(columns[2])
        currentLine = currentLine + 1
        columns = fileContent[currentLine].strip().split()
        result.logg_min, result.logg_max, result.logg_step = float(columns[0]),float(columns[1]), float(columns[2])
        currentLine = currentLine + 1
        columns = fileContent[currentLine].strip().split()
        result.mdot_min, result.mdot_max, result.mdot_step = float(columns[0]),float(columns[1]), float(columns[2])
        currentLine = currentLine + 1
        columns = fileContent[currentLine].strip().split()
        result.vinf_min, result.vinf_max, result.vinf_step = float(columns[0]),float(columns[1]), float(columns[2])
        currentLine = currentLine + 1
        columns = fileContent[currentLine].strip().split()
        result.beta_min, result.beta_max, result.beta_step = float(columns[0]),float(columns[1]), float(columns[2])
        currentLine = currentLine + 1
        columns = fileContent[currentLine].strip().split()
        result.NHe_min, result.NHe_max, result.NHe_step = float(columns[0]),float(columns[1]), float(columns[2])
        currentLine = currentLine + 1
        columns = fileContent[currentLine].strip().split()
        result.vtur_min, result.vtur_max, result.vtur_step = float(columns[0]),float(columns[1]), float(columns[2])
        currentLine = currentLine + 1
        columns = fileContent[currentLine].strip().split()
        result.vrot_min, result.vrot_max, result.vrot_step = float(columns[0]),float(columns[1]), float(columns[2])
        currentLine = currentLine + 1
        columns = fileContent[currentLine].strip().split()
        result.vmacro_min, result.vmacro_max, result.vmacro_step = float(columns[0]),float(columns[1]), float(columns[2])
#Nitrogen
        currentLine = currentLine + 1
        columns = fileContent[currentLine].strip().split()
        result.nitrogen_min, result.nitrogen_max, result.nitrogen_step = float(columns[0]), float(columns[1]), float(columns[2])
        result.nitrogen = True
#Carbon
        currentLine = currentLine + 1
        columns = fileContent[currentLine].strip().split()
        result.carbon_min, result.carbon_max, result.carbon_step = float(columns[0]), float(columns[1]), float(columns[2])
        result.carbon = True
# Oxygen
        currentLine = currentLine + 1
        columns = fileContent[currentLine].strip().split()
        result.oxygen_min, result.oxygen_max, result.oxygen_step = float(columns[0]), float(columns[1]), float(columns[2])
        result.oxygen = True
        print 'Oxygen',result.oxygen_min,result.oxygen_max,result.oxygen_step
# Silicon
        currentLine = currentLine + 1
        columns = fileContent[currentLine].strip().split()
        result.silicon_min, result.silicon_max, result.silicon_step = float(columns[0]), float(columns[1]), float(columns[2])
        result.silicon = True
        print 'Silicon',result.silicon_min,result.silicon_max,result.silicon_step
# Phosphorus
        currentLine = currentLine + 1
        columns = fileContent[currentLine].strip().split()
        result.phosphorus_min, result.phosphorus_max, result.phosphorus_step = float(columns[0]), float(columns[1]), float(columns[2])
        result.phosphorus = True
        print 'Phosphorus',result.phosphorus_min,result.phosphorus_max,result.phosphorus_step
# fcl
        currentLine = currentLine + 1
        columns = fileContent[currentLine].strip().split()
        result.fcl_min, result.fcl_max, result.fcl_step = float(columns[0]), float(columns[1]), float(columns[2])
        result.fcl = True
        print 'fcl',result.fcl_min,result.fcl_max,result.fcl_step
# fic
        currentLine = currentLine + 1
        columns = fileContent[currentLine].strip().split()
        result.fic_min, result.fic_max, result.fic_step = float(columns[0]), float(columns[1]), float(columns[2])
        result.fic = True
        print 'fic',result.fic_min,result.fic_max,result.fic_step
# fvel
        currentLine = currentLine + 1
        columns = fileContent[currentLine].strip().split()
        result.fvel_min, result.fvel_max, result.fvel_step = float(columns[0]), float(columns[1]), float(columns[2])
        result.fvel = True
        print 'fvel',result.fvel_min,result.fvel_max,result.fvel_step
# h
        currentLine = currentLine + 1
        columns = fileContent[currentLine].strip().split()
        result.h_min, result.h_max, result.h_step = float(columns[0]), float(columns[1]), float(columns[2])
        result.h = True
        print 'h',result.h_min,result.h_max,result.h_step
# vcl
        currentLine = currentLine + 1
        columns = fileContent[currentLine].strip().split()
        result.vcl_min, result.vcl_max, result.vcl_step = float(columns[0]), float(columns[1]), float(columns[2])
        result.vcl = True
        print 'vcl',result.vcl_min,result.vcl_max,result.vcl_step
# vclmax
        currentLine = currentLine + 1
        columns = fileContent[currentLine].strip().split()
        result.vclmax_min, result.vclmax_max, result.vclmax_step = float(columns[0]), float(columns[1]), float(columns[2])
        result.vclmax = True
        print 'vclmax',result.vclmax_min,result.vclmax_max,result.vclmax_step
# metallicity
        currentLine = currentLine + 1
        columns = fileContent[currentLine].strip().split()
        result.Z_min, result.Z_max, result.Z_step = float(columns[0]), float(columns[1]), float(columns[2])
        result.Z = True
        print 'Z',result.Z_min,result.Z_max,result.Z_step
# radius
        currentLine = currentLine + 1
        columns = fileContent[currentLine].strip().split()
        result.radius_min, result.radius_max, result.radius_step = float(columns[0]), float(columns[1]), float(columns[2])
        result.radius = True
        print 'radius',result.radius_min,result.radius_max,result.radius_step
# vmin
        currentLine = currentLine + 1
        columns = fileContent[currentLine].strip().split()
        result.vmin_min, result.vmin_max, result.vmin_step = float(columns[0]), float(columns[1]), float(columns[2])
        result.vmin = True
        print 'vmin',result.vmin_min,result.vmin_max,result.vmin_step
# vtrans
        currentLine = currentLine + 1
        columns = fileContent[currentLine].strip().split()
        result.vtrans_min, result.vtrans_max, result.vtrans_step = float(columns[0]), float(columns[1]), float(columns[2])
        result.vtrans = True
        print 'vtrans',result.vtrans_min,result.vtrans_max,result.vtrans_step
# nume
        currentLine = currentLine + 1
        columns = fileContent[currentLine].strip().split()
        result.nume_min, result.nume_max, result.nume_step = float(columns[0]), float(columns[1]), float(columns[2])
        result.nume = True
        print 'nume',result.nume_min,result.nume_max,result.nume_step

        print
        print "Reading the line parameters"
        # Read in the line parameters
        result.numberOfLines = 0
        result.lines=[]
        while currentLine <= len(fileContent)-2:
            currentLine += 1
            if fileContent[currentLine][0] is '#':
                columns = fileContent[currentLine].strip().split()
                print 'Skipping ', columns
                currentLine += 2
            else:
                columns = fileContent[currentLine].strip().split()
                id = columns[0]
                resolution = int(columns[1])
                print id, resolution
                currentLine = currentLine + 1
                columns = fileContent[currentLine].strip().split()
                leftLambda = correctRV(float(columns[0]), result.rv)
                rightLambda = correctRV(float(columns[2]), result.rv)
                leftOffset = float(columns[1])
                rightOffset = float(columns[3])
                print leftLambda,leftOffset,rightLambda,rightOffset
                currentLine = currentLine + 1
                columns = fileContent[currentLine].strip().split()
                weight = float(columns[0])
                print weight
                result.lines.append([id, leftLambda, leftOffset, rightLambda, rightOffset, weight, resolution])
                result.numberOfLines += 1
        return result
    except:
        print 'ERROR - Could not read in ' + fileName
        print 'ERROR - Check your file'
        print 'ERROR - Error message: ', sys.exc_info()[1]
        raw_input('Press Enter to continue')
        restart()

def checkForObject():
    #Checks if RV and magnitude is known
    rv = []
    magnitude = []
    id = []
    fileName = maindir+'/object_data.dat'
    file = open(fileName, 'r')
    fileContent = file.readlines()
    file.close()
    for i in range(1,len(fileContent)):
        columns = fileContent[i].strip().split()
        if columns[0] == object:
            print 'RV and magnitude information for \''+ object + '\' found!'
            return float(columns[1]), float(columns[2])
    return '',''

def correctRV(wavelength, rv):
    result = round(wavelength / (1 - (rv*1e13 / 299792458e10)),2)
    return result

def writeIniFile(setup):
    # Write the .ini file
    fileName = 'pfw_' + object + '.ini'
    file = open(fileName, 'w')
    file.write('\"/home/' + setup.username + '/GA_ANALYSIS/' + object + '/' + object + '.norm\" ' + object + ' ' + str(setup.errors) + '\n')
    file.write('\n')
    file.write(str(setup.numberOfLines) + '\n')
    for i in range(setup.numberOfLines):
        tmp = setup.lines[i]
        file.write(tmp[0] + ' ' + str(tmp[6]) + '\n')
        file.write(str(tmp[1]) + ' ' + str(tmp[3]) + '\n')
        file.write(str(setup.rv) + '\n')
        file.write(str(tmp[1]) + ' ' + str(tmp[2]) + ' ' + str(tmp[3]) + ' ' + str(tmp[4]) + '\n' )
        file.write(str(tmp[5]) + '\n')
    file.write('\n')
    file.write(str(setup.Teff_min) + ' ' + str(setup.Teff_max) + ' ' + str(setup.Teff_step) + ' #Effective_Temperature_(teff)' + '\n')
    file.write(str(setup.logg_min) + ' ' + str(setup.logg_max) + ' ' + str(setup.logg_step) + ' #Surface_Gravity_(logg)' + '\n')
    file.write(str(setup.mdot_min) + ' ' + str(setup.mdot_max) + ' ' + str(setup.mdot_step) + ' #Mass_Loss_Rate_(mdot)' + '\n')
    file.write(str(setup.vinf_min) + ' ' + str(setup.vinf_max) + ' ' + str(setup.vinf_step) + ' #Terminal_Wind_Velocity_(vinf)' + '\n')
    file.write(str(setup.beta_min) + ' ' + str(setup.beta_max) + ' ' + str(setup.beta_step) + ' #Beta_Wind_Velocity_(beta)' + '\n')
    file.write(str(setup.NHe_min) + ' ' + str(setup.NHe_max) + ' ' + str(setup.NHe_step) + ' #H/He_(He)' + '\n')
    file.write(str(setup.vtur_min) + ' ' + str(setup.vtur_max) + ' ' + str(setup.vtur_step) + ' #Microturbulence_(micro)' + '\n')
    file.write(str(setup.vrot_min) + ' ' + str(setup.vrot_max) + ' ' + str(setup.vrot_step) + ' #Rotational_Velocity_vsini_(vrot)' + '\n')
    file.write(str(setup.vmacro_min) + ' ' + str(setup.vmacro_max) + ' ' + str(setup.vmacro_step) + ' #Macroturbulence_(macro)' + '\n')
#    if setup.nitrogen:
    file.write(str(setup.nitrogen_min) + ' ' + str(setup.nitrogen_max) + ' ' + str(setup.nitrogen_step) + ' #Nitrogen_Abundance_(N)' + '\n')
#    if setup.carbon:
    file.write(str(setup.carbon_min) + ' ' + str(setup.carbon_max) + ' ' + str(setup.carbon_step) + ' #Carbon_Abundance_(C)' + '\n')
#    if setup.oxygen:
    file.write(str(setup.oxygen_min) + ' ' + str(setup.oxygen_max) + ' ' + str(setup.oxygen_step) + ' #Oxygen_Abundance_(O)' + '\n')
#    if setup.silicon:
    file.write(str(setup.silicon_min) + ' ' + str(setup.silicon_max) + ' ' + str(setup.silicon_step) + ' #Silicon_Abundance_(Si)' + '\n')
#    if setup.phosphorus:
    file.write(str(setup.phosphorus_min) + ' ' + str(setup.phosphorus_max) + ' ' + str(setup.phosphorus_step) + ' #Phosphorus_Abundance_(P)' + '\n')
    file.write(str(setup.fcl_min) + ' ' + str(setup.fcl_max) + ' ' + str(setup.fcl_step) + ' #Clumping_Factor_(fcl)' + '\n')
    file.write(str(setup.fic_min) + ' ' + str(setup.fic_max) + ' ' + str(setup.fic_step) + ' #Interclump_Density_(fic)' + '\n')
    file.write(str(setup.fvel_min) + ' ' + str(setup.fvel_max) + ' ' + str(setup.fvel_step) + ' #Velocity_Filling_Factor_(fvel)' + '\n')
    file.write(str(setup.h_min) + ' ' + str(setup.h_max) + ' ' + str(setup.h_step) + ' #Porosity_length_(h)' + '\n')
    file.write(str(setup.vcl_min) + ' ' + str(setup.vcl_max) + ' ' + str(setup.vcl_step) + ' #Clumping_Onset_Velocity_(vcl)' + '\n')
    file.write(str(setup.vclmax_min) + ' ' + str(setup.vclmax_max) + ' ' + str(setup.vclmax_step) + ' #Clumping_Maximum_Velocity_(vclmax)' + '\n')
    file.write(str(setup.Z_min) + ' ' + str(setup.Z_max) + ' ' + str(setup.Z_step) + ' #Metallicity_(Z)' + '\n')
    file.write(str(setup.radius_min) + ' ' + str(setup.radius_max) + ' ' + str(setup.radius_step) + ' #Radius_(radius)' + '\n')
    file.write(str(setup.vmin_min) + ' ' + str(setup.vmin_max) + ' ' + str(setup.vmin_step) + ' #vmin_(vmin)' + '\n')
    file.write(str(setup.vtrans_min) + ' ' + str(setup.vtrans_max) + ' ' + str(setup.vtrans_step) + ' #vtrans_(vtrans)' + '\n')
    file.write(str(setup.nume_min) + ' ' + str(setup.nume_max) + ' ' + str(setup.nume_step) + ' #Number_of_electrons_(nume)' + '\n')
    file.write('\n')
    file.write(str(setup.numberOfIndividuals) + ' #Population_Size' + '\n')
    file.write(str(setup.numberOfGenerations) + ' #Number_of_Generations' + '\n')
    file.close()
    shutil.copyfile(fileName, 'my_inifile')
    print 'Created ' + fileName
    return

def writeJobFile(setup):
    # write the .job file
    fileName = 'par_' + object + '.job'
    file = open(fileName, 'w')
    file.write('#PBS -N ' + object + '\n')
    file.write('#PBS -lwalltime=' + setup.walltime + '\n')
    file.write('#PBS -lnodes=' + setup.nodes + ':ppn=16\n')
    file.write('#PBS -S /bin/bash\n')
    file.write('#PBS -r n\n')
    file.write('\n')
    file.write('export FWver=V' + FWversion + '\n')
    file.write('export GA_DIR=/home/' + setup.username + '/GA_ANALYSIS/\n')
    file.write('export FW_DIR=pikaia/FASTWIND/${FWver}/\n')
    file.write('export INICALC_DIR=${FW_DIR}/inicalc/\n')
    file.write('export PIKAIA_DIR=/home/' + setup.username + '/pikaia\n')
    file.write('export PICKYFW_DIR=${PIKAIA_DIR}/utils/picky_fw/ver_' + GAversion + '\n')
    file.write('\n')
    file.write('module load fortran/intel openmpi/intel\n')
    file.write('module load mpicopy\n')
    file.write('module load paffinity\n')
    file.write('\n')
    file.write('mpicopy ${INICALC_DIR}/bin ${INICALC_DIR}/HOPFPARA_ALL* ${INICALC_DIR}\n')
    file.write('mpicopy ${PIKAIA_DIR}/bin_'+GAversion+'/setupFW.sh\n')
    file.write('mpicopy ${PIKAIA_DIR}/bin_'+GAversion+'/runFW_'+GAversion+'.sh\n')
    file.write('mpicopy ${PIKAIA_DIR}/bin_'+GAversion+'/cleanFW.sh\n')
    file.write('mpicopy ${PIKAIA_DIR}/bin_'+GAversion+'/convolve\n')
    file.write('mpicopy ${PIKAIA_DIR}/bin_'+GAversion+'/resample\n')
    file.write('mpicopy ${PIKAIA_DIR}/bin_'+GAversion+'/FW2Spec.pl\n')
    file.write('mpicopy ${GA_DIR}/'+object+'/my_inifile\n')
    file.write('\n')
    file.write('cd $TMPDIR\n')
    file.write('\n')
    file.write('mpiexec --mca btl ^openib ${PICKYFW_DIR}/pickyfw2 << EOF\n')
    file.write('\"/home/' + setup.username + '/GA_ANALYSIS/' + object + '/pfw_' + object + '.ini\"\n')
    file.write('EOF\n')
    file.close()
    print 'Created '+ fileName
    return


def checkNorm():
    #Checks the normfile
    global lines
    normFileOriginal = maindir + '/new' + '/' + object + '/' + object + '_original.norm'
    normFile = maindir + '/new' + '/' + object + '/' + object + '.norm'
    iniFile = maindir + '/new/' + object + '/pfw_'+object+'.ini'
    lines = readIni(iniFile)
    shutil.copyfile(normFile, normFileOriginal)
    supplyRanges(normFileOriginal)
    while True:
        plotModel(False) # Plot the normfile with fitting ranges
        choice = ''
        while choice is '':
            choice = raw_input('Use these fitting ranges (y/n)? ')
            if choice is not 'y' and choice is not 'n': choice = ''
        if choice is 'y': break
        elif choice is 'n':
            #temporary, remove the dir and copy the normfile to original
            #            shutil.copyfile(normFileOriginal, normFile)
            #shutil.copyfile(normFile, maindir+'/new/'+object+'.norm')
            #files = os.listdir(maindir+'/new/'+object)
            #for i in range(len(files)): os.remove(maindir+'/new/'+object+'/'+files[i])
            #os.rmdir(maindir+'/new/'+object)
            lines = readIni(iniFile)
            supplyRanges(normFileOriginal)
        else: choice = ''
    newNorm = checkNegative(normFileOriginal)
    choice = ''
    while choice is '':
        print
        choice = raw_input('Do you want to clip cosmics/nebular contamination (y/n)? ')
        if choice is not 'y' and choice is not 'n': choice = ''
        if choice is 'y': newNorm = clipSpectrum(newNorm)
        if choice is 'n': break
    choice = ''
    while choice is '':
        print
        choice = raw_input('Do you want to adjust the normalization (y/n)? ')
        if choice is not 'y' and choice is not 'n': choice = ''
        if choice is 'y': adjustNormalization(newNorm)
        if choice is 'n': break
    if len(newNorm) == 2:
        print 'No error spectrum found...'
        print 'Starting SNR calculation...'
        print
        newNorm = doSNR(newNorm)
    else: reduceNorm(newNorm)
    writeNorm(newNorm, normFile)


def adjustNormalization(norm):
    print 'Select the the continuum level at each fitting boundary.'
    print 'Press enter after each selection.'
    print 'If nothing is selected the continuum correction will be set to 0.'
    originalNorm = norm
    global lines
    global clickValue
    while True:
        clickValue=[0,0]
        for i in range(len(lines)):
            def on_key(event):
                if event.key == 'enter':
                    plt.close()
            ID = lines[i].ID
            RV = lines[i].RV
            WLleft = lines[i].WLleft
            WLright = lines[i].WLright
            delta = (WLright - WLleft)*1.5
            wlc,flux = selectWavelength(norm[0],norm[1],WLleft-delta,WLright+delta)
            fig = figure(figsize=(15,4))
            xlim(wlc[0]-delta,wlc[len(wlc)-1]+delta)
            xlabel('Wavelength',fontsize=11)
            ylabel('Normalized flux',fontsize=11)
            title('Select left continuum level for ' + ID + ' (press Enter when ready)')
            plot(wlc,flux,'.',color='black')
            axvline(x=lines[i].WLleft, linestyle='--', color='green')
            axvline(x=lines[i].WLright, linestyle='--', color='green')
            axhline(y=1, linestyle='-', color='black')
            print 'Pick left continuum level for ' + ID
            fig.canvas.mpl_connect('button_press_event', clicker2)
            fig.canvas.mpl_connect('key_press_event', on_key)
            manager = plt.get_current_fig_manager()
            manager.window.showMaximized()
            show()
            if not clickValue[0] is 0:
                leftValue = round(clickValue[1],3)
            else: leftValue = 1
            clickValue = [0,0]
            fig = figure(figsize=(15,4))
            xlim(wlc[0]-delta,wlc[len(wlc)-1]+delta)
            xlabel('Wavelength',fontsize=11)
            ylabel('Normalized flux',fontsize=11)
            title('Select right continuum level for ' + ID + ' (press Enter when ready)')
            plot(wlc,flux,'.',color='black')
            axvline(x=lines[i].WLleft, linestyle='--', color='green')
            axvline(x=lines[i].WLright, linestyle='--', color='green')
            axhline(y=1, linestyle='-', color='black')
            print 'Pick right continuum level for ' + ID
            fig.canvas.mpl_connect('button_press_event', clicker2)
            fig.canvas.mpl_connect('key_press_event', on_key)
            manager = plt.get_current_fig_manager()
            manager.window.showMaximized()
            show()
            if not clickValue[0] is 0:
                rightValue = round(clickValue[1],3)
            else: rightValue = 1
            clickValue = [0,0]
            lines[i].fluxCorrectionLeft = leftValue - 1
            lines[i].fluxCorrectionRight = rightValue - 1
        plotModel(False)
        choice = ''
        while choice is not 'y' or choice is not 'n':
            choice = raw_input('Accept the normalized spectrum (y/n)? ')
            if choice is 'n':
                norm = originalNorm
                break
            if choice is 'y': break
        if choice is 'y': break



def reduceNorm(norm):
    #reduce the size of the normfile to the regions around the fitted lines
    print 'Reducing normfile size to fitted lines'
    print
    wlc = []
    flux = []
    error = []
    for j in range(len(lines)):
        delta = (lines[j].WLright - lines[j].WLleft)
        wlc_tmp, flux_tmp, error_tmp = selectWavelength2(norm, lines[j].WLleft - delta, lines[j].WLright + delta)
        for i in range(len(wlc_tmp)):
            wlc.append(wlc_tmp[i])
            flux.append(flux_tmp[i])
            error.append(error_tmp[i])
    result = [wlc, flux, error]
    return result


def checkNegative(normFile):
    #checks negative flux in normfile and removes it
    print
    print 'Checking for negative or zero flux...'
    norm = readNorm(normFile)
    wlc=[]
    flux = []
    error = []
    for i in range(len(norm[1])):
        if norm[1][i] > 0:
            wlc.append(norm[0][i])
            flux.append(norm[1][i])
            if len(norm) == 3: error.append(norm[2][i])
        else: print 'Removed negative or zero flux at ' + str(norm[0][i])
    if len(norm) == 3: result = [wlc,flux,error]
    else: result = [wlc, flux]
    return result


def clipSpectrum(norm):
    print 'Select the regions you want to clip.'
    print 'Press enter after each selection.'
    print 'If nothing is selected the next line profile will be shown.'
    originalNorm = norm
    global clickValue
    while True:
        clickValue=[0,0]
        for i in range(len(lines)):
            def on_key(event):
                if event.key == 'enter':
                    plt.close()
            ID = lines[i].ID
            RV = lines[i].RV
            WLleft = lines[i].WLleft
            WLright = lines[i].WLright
            delta = (WLright - WLleft)/25.
            while True:
                wlc,flux = selectWavelength(norm[0],norm[1],WLleft-delta,WLright+delta)
                fig = figure(figsize=(15,4))
                xlim(wlc[0]-delta,wlc[len(wlc)-1]+delta)
                xlabel('Wavelength',fontsize=11)
                ylabel('Normalized flux',fontsize=11)
                title('Select left wavelength for clipping of ' + ID + ' (press Enter when ready)')
                plot(wlc,flux,'.',color='black')
                axvline(x=lines[i].WLleft, linestyle='--', color='green')
                axvline(x=lines[i].WLright, linestyle='--', color='green')
                axhline(y=1, linestyle='-', color='black')
                print 'Pick left boundary wavelength for clipping of ' + ID
                fig.canvas.mpl_connect('button_press_event', clicker)
                fig.canvas.mpl_connect('key_press_event', on_key)
                manager = plt.get_current_fig_manager()
                manager.window.showMaximized()
                show()
                if not clickValue[0] is 0:
                    leftValue = round(clickValue[0],2)
                else: break
                clickValue = [0,0]
                fig = figure(figsize=(15,4))
                xlim(wlc[0]-delta,wlc[len(wlc)-1]+delta)
                xlabel('Wavelength',fontsize=11)
                ylabel('Normalized flux',fontsize=11)
                title('Select right wavelength for clipping of' + ID + ' (press Enter when ready)')
                plot(wlc,flux,'.',color='black')
                axvline(x=lines[i].WLleft, linestyle='--', color='green')
                axvline(x=lines[i].WLright, linestyle='--', color='green')
                axvline(x=leftValue, linestyle = '-', color='red')
                axhline(y=1, linestyle='-', color='black')
                print 'Pick right boundary wavelength for clipping of ' + ID
                fig.canvas.mpl_connect('button_press_event', clicker)
                fig.canvas.mpl_connect('key_press_event', on_key)
                manager = plt.get_current_fig_manager()
                manager.window.showMaximized()
                show()
                if not clickValue[0] is 0:
                    rightValue = round(clickValue[0],2)
                clickValue = [0,0]
                norm = clip(norm, leftValue, rightValue)
        writeNorm(norm, maindir + '/new' + '/' + object + '/' + object + '.norm')
        plotModel(False)
        choice = ''
        while choice is not 'y' or choice is not 'n':
            choice = raw_input('Accept the clipped spectrum (y/n)? ')
            if choice is 'n':
                norm = originalNorm
                break
            if choice is 'y': break
        if choice is 'y': break
    return norm


def clip(norm, leftValue, rightValue):
    wlc=[]
    flux=[]
    if len(norm) == 3: error=[]
    for i in range(len(norm[0])):
        if norm[0][i] < leftValue or norm[0][i] > rightValue:
            wlc.append(norm[0][i])
            flux.append(norm[1][i])
            if len(norm) == 3: error.append(norm[2][i])
    if len(norm) == 2: result = [wlc, flux]
    else: result = [wlc, flux, error]
    return result


def doSNR(norm):
    print 'Starting SNR calculation...'
    raw_input('Press Enter to start... ')
    print
    global clickValue
    error = []
    wlc_result = []
    flux_result = []
    clickValue = [0,0]
    for i in range(len(lines)):
        def on_key(event):
            if event.key == 'enter':
                plt.close()

        ID = lines[i].ID
        RV = lines[i].RV
        WLleft = lines[i].WLleft
        WLright = lines[i].WLright
        delta = (WLright - WLleft)
        wlc,flux = selectWavelength(norm[0],norm[1],WLleft-delta,WLright+delta)
        fig = figure(figsize=(10,2.5))
        xlim(wlc[0]-delta,wlc[len(wlc)-1]+delta)
        xlabel('Wavelength',fontsize=11)
        ylabel('Normalized flux',fontsize=11)
        title('Select left boundary wavelength for SNR of ' + ID + ' (press Enter when ready)')
        plot(wlc,flux,'.',color='black')
        axhline(y=1, linestyle='-', color='black')
        print 'Pick left boundary wavelength for SNR of ' + ID
        fig.canvas.mpl_connect('button_press_event', clicker)
        fig.canvas.mpl_connect('key_press_event', on_key)
        show()
        if not clickValue[0] is 0:
            leftValue = round(clickValue[0],2)
        clickValue = [0,0]
        fig = figure(figsize=(10,2.5))
        xlim(wlc[0]-delta,wlc[len(wlc)-1]+delta)
        xlabel('Wavelength',fontsize=11)
        ylabel('Normalized flux',fontsize=11)
        title('Select right boundary wavelength for SNR of' + ID + ' (press Enter when ready)')
        plot(wlc,flux,'.',color='black')
        axvline(x=leftValue, linestyle = '-', color='red')
        axhline(y=1, linestyle='-', color='black')
        print 'Pick right boundary wavelength for SNR of ' + ID
        fig.canvas.mpl_connect('button_press_event', clicker)
        fig.canvas.mpl_connect('key_press_event', on_key)
        show()
        if not clickValue[0] is 0:
            rightValue = round(clickValue[0],2)
        clickValue = [0,0]
        snr = calculateSNR(norm, leftValue, rightValue, ID)
        for j in range(len(wlc)):
            wlc_result.append(wlc[j])
            flux_result.append(flux[j])
            error.append(1./snr)
    result = [wlc_result, flux_result, error]
    return result


def calculateSNR(norm, left, right, ID):
    wlc, flux = selectWavelength(norm[0], norm[1], left, right)
    variance = numpy.var(flux)
    sigma = math.sqrt(variance)
    for i in range(len(flux)): flux[i] = flux[i]/sigma
    result = numpy.median(flux)
    print 'SNR for ' + ID + ' = ' + str(result)
    print
    return result


def writeNorm(norm, normFile):
    #writes the data in norm to normFile
    print 'Writing new norm file...'
    file = open(normFile, 'w')
    file.write('#'+str(len(norm[0]))+' # 0\n')
    for i in range(len(norm[0])):
        if len(norm) == 3:
            file.write(str(norm[0][i]) + ' ' + str(norm[1][i]) + ' ' + str(norm[2][i]) + '\n')
        else: file.write(str(norm[0][i]) + ' ' + str(norm[1][i]) + ' ' + '\n')
    file.close()


def clicker(event):
    global clickValue
    if event.inaxes:
        clickValue = [event.xdata, event.ydata]
        axvline(x=event.xdata,linestyle='-',color='red')
        draw()

def clicker2(event):
    global clickValue
    if event.inaxes:
        clickValue = [event.xdata, event.ydata]
        axhline(y=event.ydata, linestyle='-', color='red')
        draw()


def supplyRanges(normFile):
    global lines
    # manually adjust the fitted ranges
    global clickValue
    norm = readNorm(normFile)
    for i in range(len(lines)):
        def on_key1(event):
            if event.key == 'enter':
                plt.close()

        def on_key2(event):
            if event.key == 'enter':
                plt.close()
                WLright = clickValue[1]

        ID = lines[i].ID
        RV = lines[i].RV
        WLleft = lines[i].WLleft
        WLright = lines[i].WLright
        weight = lines[i].weight
        delta = (WLright - WLleft)
        wlc,flux = selectWavelength(norm[0],norm[1],WLleft-delta,WLright+delta)

        fig = figure(figsize=(15,4))
        xlim(wlc[0]-delta,wlc[len(wlc)-1]+delta)
        xlabel('Wavelength',fontsize=11)
        ylabel('Normalized flux',fontsize=11)
        title('Adjust left boundary wavelength for ' + ID + ' (press Enter when ready)')
        plot(wlc,flux,'.',color='black')
        axvline(x=WLleft,linestyle='--',color='green')
        axvline(x=WLright,linestyle='--', color='green')
        axhline(y=1, linestyle='-', color='black')
        print 'Pick left boundary wavelength for ' + ID
        fig.canvas.mpl_connect('button_press_event', clicker)
        fig.canvas.mpl_connect('key_press_event', on_key1)
        manager = plt.get_current_fig_manager()
        manager.window.showMaximized()
        show()
        if not clickValue[0] is 0:
            WLleft = round(clickValue[0],2)
        clickValue = [0,0]
        fig = figure(figsize=(15,4))
        xlim(wlc[0]-delta,wlc[len(wlc)-1]+delta)
        xlabel('Wavelength',fontsize=11)
        ylabel('Normalized flux',fontsize=11)
        title('Adjust right boundary wavelength for ' + ID + ' (press Enter when ready)')
        plot(wlc,flux,'.',color='black')
        axvline(x=WLleft,linestyle='--',color='green')
        axvline(x=WLright,linestyle='--', color='green')
        axhline(y=1, linestyle='-', color='black')
        print 'Pick right boundary wavelength for ' + ID
        fig.canvas.mpl_connect('button_press_event', clicker)
        fig.canvas.mpl_connect('key_press_event', on_key2)
        manager = plt.get_current_fig_manager()
        manager.window.showMaximized()
        show()
        lines[i].WLleft = WLleft
        if not clickValue[0] is 0:
            lines[i].WLright = round(clickValue[0],2)
        clickValue = [0,0]


################################
### LISA / RESUME MONITORING ###
################################

def doLisa(monitor):
    global object, objectFlag
    # Connects to LISA, uploads the files, and downloads em when the job is done
    printHeader()
    print 'Setting up LISA connection for ' + object
    print
    lisassh = lisaConnect()
    print
    uploadFiles(lisassh)
    print
    id = submitJob(lisassh, monitor)
    filename = object+'.run'
    if monitor:
        print
        downloadResults(lisassh)
        choice = ''
        while choice is '':
            choice = raw_input('Do you want to remove the files from LISA (y/n)? ')
            if choice is not 'y' and choice is not 'n': choice = ''
            if choice is 'y': cleanLisa(lisassh)
        cleanUp()
        os.remove(maindir+'/running/'+filename)
        objectFlag = True # Needed for swithching to the postAnalysis part
        postAnalysis(object, firstRun=True)
    else:
        raw_input('Job information saved. Press Enter to continue.')
        restart()
    return

def resumeMonitoring(fileList):
    #resumes monitoring a Lisa session
    global object, objectFlag
    printHeader()
    if objectFlag:
        print 'Resuming monitoring of \'' + object + '\''
        print
        fileName = object+'.run'
    if not objectFlag:
        if len(fileList) is 0:
            print 'No jobs are currently running!'
            raw_input('Press enter to continue...')
            return
        print 'The following jobs are currently running: '
        print
        for i in range(len(fileList)):
            print str(i+1)+'. '+fileList[i]
        print '0. Main Menu'
        choice = ''
        while choice is '':
            choice = raw_input('Choose a job: ')
            if choice is '0':
                return
            else:
                try:
                    fileName = fileList[int(choice)-1]+'.run'
                except:
                    print 'Incorrect choice, try again...'
                    choice = ''
        object = string.strip(fileList[int(choice)-1],'.run')
    file = open('running/'+fileName)
    id = string.strip(file.readline(),'\n')
    startQueue = file.readline()
    lisassh = lisaConnect()
    print
    print 'Job monitoring will start in 10 seconds...'
    monitorJob(lisassh, id, startQueue)
    print
    downloadResults(lisassh)
    print
    choice = ''
    while choice is '':
        choice = raw_input('Do you want to remove the files from LISA (y/n)? ')
        if choice is not 'y' and choice is not 'n': choice = ''
        if choice is 'y': cleanLisa(lisassh)
    cleanUp()
    os.remove(maindir+'/running/'+fileName)
    objectFlag = True # Needed for swithching to the postAnalysis part
    postAnalysis(object, firstRun=True)
    return


def cleanLisa(ssh):
    # Removes the object directory from LISA
    choice = ''
    while choice is '':
        print
        choice = raw_input('This will remove the directory GA_ANALYSIS/'+object+'/ and its contents from your LISA account. Are you sure (y/n)? ')
        if choice is not 'y' and choice is not 'n': choice = ''
        if choice is 'n': return
        if choice is 'y': break
    ftp = ssh.open_sftp()
    print 'Downloading filelist...'
    ftp.chdir('GA_ANALYSIS')
    removed = 0
    try:
        fileList = ftp.listdir(object+ '/')
    except:
        raw_input('Directory not found! Press Enter to continue... ')
        return
    for i in range(len(fileList)):
        printHeader()
        print
        print 'CLEANING LISA'
        print
        print 'Removing GA_ANALYSIS/' + object + '/' + fileList[i] + ' from your LISA account'
        print 'Currently at file '+str(i+1)+' of '+str(len(fileList))+ ' ('+str(round((float(i+1)/float(len(fileList)))*100,2))+'%)'
        ftp.remove(object + '/' + fileList[i])
        removed += 1
    ftp.rmdir(object)
    print
    raw_input('Removal of GA_ANALYSIS/' + object + '/ completed. Press Enter to continue... ')
    return


def lisaConnect():
    # Creates a SSH connection with LISA
    print 'Attempting to connect to LISA...'
    ssh = paramiko.SSHClient()
    ssh.set_missing_host_key_policy(paramiko.AutoAddPolicy()) # We trust LISA, so auto add the host key
    connected = False
    global username
    global password
    while not connected:
        try:
            if username is '':
                username = raw_input('Enter your LISA username: ')
            if password is '':
                password = raw_input('Enter LISA password for user \'' + username + '\': ')
            ssh.connect('lisa.surfsara.nl', username=username, password=password)
            connected = True
        except:
            print 'Could not connect to LISA. Check your login information.'
            print
            username = ''
            password = ''
    print 'Connection established!'
    print
    return ssh

def uploadFiles(ssh):
    # Uploads the files to LISA
    print 'Uploading files...'
    uploaded = False
    while not uploaded:
        try:
            files = os.listdir(maindir+'/new/'+object)
            ftp = ssh.open_sftp() # sftp connection
            directory = 'GA_ANALYSIS/'+object+'/'
            stdin, stdout, stderr = ssh.exec_command('mkdir '+directory) #create object directory on lisa
            for i in range(len(files)):
                ftp.put(files[i],directory+files[i])
                print files[i]+' uploaded...'
            ftp.close()
            uploaded = True
        except:
            pass
    return

def submitJob(ssh, monitor):
    # Submits the job to LISA and monitors it
    startQueue = ''
    startRun = ''
    print 'Submitting job to LISA'
    stdin, stdout, stderr = ssh.exec_command('cd GA_ANALYSIS/'+object+'; qsub par_'+object+'.job')
    tmpid = stdout.readlines()
    id = ''
    for ch in tmpid[0]:
        if ch is '.': break
        else: id = id + ch
    print 'Job submitted to LISA. Job ID = ' + id
    startQueue = datetime.datetime.now()
    filename= maindir+'/running/'+object+'.run'
    file = open(filename,"w")
    file.write(id+'\n')
    file.write(str(startQueue))
    file.close()
    shutil.move(maindir+'/new/'+object, maindir+'/running/'+object)
    if monitor:
        print 'Job monitoring will start in 10 seconds...'
        monitorJob(ssh, id, startQueue)
    return id

def monitorJob(ssh, id, startQueue):
    # Monitors a job on lisa
    startRun = datetime.datetime.now()
    status = 'Q'
    while status is 'Q' or status is 'R':
        try:
            print
            print
            print 'Press Ctrl-C to interrupt monitoring.'
            time.sleep(10)
            status = checkStatus(ssh, id, status, startQueue, startRun)
        except:
            print
            print 'Job monitoring interrupted! Restarting in 3 seconds...'
            time.sleep(3)
            restart()
    print 'The job with ID '+id+' has finished!'
    return

def checkStatus(ssh, id, result, startQueue, startRun):
    # Checks the status of a job
    printHeader()
    print 'LISA monitoring of job ID '+id+' ('+object+')'
    try:
        stdin, stdout, stderr = ssh.exec_command('qstat '+id)
    except:
        print 'Connection to LISA lost. Attempting reconnect...'
        ssh.connect('lisa.surfsara.nl', username=username, password=password)
        return result
    status = stdout.readlines()
    try:
        status = status[2]
        status = status.split()[4].strip()
        result = status
    except:
        print 'Job seems to have finished, rechecking status in 2 minutes...'
        print 'Press ctrl-C to interrupt and start donwloading.'
        try:
            time.sleep(120)
            stdin, stdout, stderr = ssh.exec_command('qstat '+id)
            status = stdout.readlines()
            status = status[2]
            status = status.split()[4].strip()
            result = status
        except:
            return 'F'
    print
    print 'Job submitted: ' + str(startQueue)
    if result is 'R':
        print 'Running since: ' + str(startRun)
    print 'Current time:  ' + str(datetime.datetime.now())
    print
    if result is 'Q': print 'Job is currently in queue...'
    elif result is 'R': print 'Job is currently running...'
    return result

def downloadResults(ssh):
    # Creates object dir
    # Downloads the results from LISA
    os.chdir(maindir)
    try:
        os.mkdir(object)
    except:
        print 'Directory already exists!'
        print 'Resuming download...'
    os.chdir(object)
    ftp = ssh.open_sftp()
    ftp.chdir('GA_ANALYSIS/'+object)
    print 'Downloading filelist...'
    fileList = ftp.listdir('.')
    fileName = 'downloaded.list'
    if not checkFile(fileName, maindir+'/'+object):
        downloadFile = open(maindir+'/'+object+'/'+fileName,'w') # Create the file if its not there yet
        fileContent = []
    else:
        downloadFile = open(maindir+'/'+object+'/'+fileName,'r+') # Else open the file for reading + writing
        fileContent = downloadFile.readlines()
        for i in range(len(fileContent)):
            fileContent[i] = fileContent[i].strip().split()
            fileContent[i] = fileContent[i][0]
    for i in range(len(fileList)):
        printHeader()
        print 'DOWNLOADING RESULTS'
        print
        print 'Currently at file '+str(i+1)+' of '+str(len(fileList))+ ' ('+str(round((float(i+1)/float(len(fileList)))*100,2))+'%)'
        try:

            fileContent.index(fileList[i])
            print fileList[i] + ' already downloaded!'
        except:
            print 'Downloading '+fileList[i]
            ftp.get(fileList[i],fileList[i])
            downloadFile.write(fileList[i]+'\t'+str(datetime.datetime.now())+'\n')
    ftp.close()
    downloadFile.close()
    shutil.copyfile(maindir+'/running/'+object+'.run', maindir+'/'+object+'/'+object+'.run')
    try:
        shutil.rmtree(maindir+'/running/'+object)
    except:
        print
    os.chdir(maindir)
    return



def cleanUp():
    moveIndatFiles()
    moveProfileFiles()
    try:
        os.mkdir(maindir+'/'+object+'/plots')
    except:
        pass
    #Cleans up the object dir by moving all the profiles and indat files to directories


def moveIndatFiles():
    printHeader()
    print '* MOVING INDAT FILES'
    try:
        os.chdir(maindir+'/'+object)
        os.mkdir('indat')
    except:
        print 'Indat directory already exists or object directory not found'
    try:
        fileList = []
        for fileName in os.listdir(os.getcwd()):
            if fnmatch.fnmatch(fileName, '*indat.dat'):
                fileList.append(fileName)
        for i in range(len(fileList)):
            shutil.move(fileList[i],'indat/'+fileList[i])
    except:
        print
    return


def moveProfileFiles():
    print '* MOVING PROFILE FILES'
    try:
        os.chdir(maindir+'/'+object)
        os.mkdir('profiles')
    except:
        print 'Profile directory already exists or object directory not found'
    try:
        fileList = []
        for fileName in os.listdir(os.getcwd()):
            if fnmatch.fnmatch(fileName, '*.gz'):
                fileList.append(fileName)
        for i in range(len(fileList)):
            shutil.move(fileList[i],'profiles/'+fileList[i])
    except:
        print
    return



#####################
### POST-ANALYSIS ###
#####################

def postAnalysis(fileList, firstRun = False):
    printHeader()
    # Prints best model to screen, then shows menu options
    global object, models, lines
    if objectFlag:
        fileName = maindir+'/'+object+'/pfw_'+object+'.par'
        fileName2 = maindir+'/'+object+'/pfw_'+object+'.ini'
    if not objectFlag:
        if len(fileList) == 0:
            print 'No finished objects'
            raw_input('Press Enter to continue')
            return
        else:
            print 'Finished objects: '+str(len(fileList))
            j = 1
            for i in range(len(fileList)):
                print str(j) + '. ' + fileList[i]
                j = j + 1
            print '0. Main Menu'
            print
            choice = ''
            while choice is '':
                choice = raw_input('Choose an object to open: ')
                if choice is '0': return
                try:
                    choice = int(choice)
                    if choice > (j - 1):
                        print 'Incorrect choice'
                        choice = ''
                    if choice < 1:
                        print 'Incorrect choice'
                        choice = ''
                except:
                    print 'Incorrect choice'
                    choice = ''
            object = fileList[choice-1]
            fileName =  maindir+'/'+object+'/pfw_'+object+'.par'
            fileName2 = maindir+'/'+object+'/pfw_'+object+'.ini'
    lines = readIni_new(fileName2) #Reads the fitted lines from the .ini file
    models = readPar(fileName) #Reads the models from the .par file
    calculateP(normalize = True) #Calculate the P-values
    cleanUp() #just in case
    if firstRun: makeAllPlots()
    analysisMenu()


def calculateLuminosity(radius, temperature):
    solarRadius = 6.955e10
    kb = 5.6704e-5
    solarLuminosity = 3.939e33
    result = 4 * pi * (radius*solarRadius)**2 * kb * temperature**4
    result = log10(result / solarLuminosity)
    return result

def calculateDmom(mdot, vinf, radius):
    solarMass = 1.98893e33
    yearToSecond = 31556926
    result = (10**mdot * solarMass / yearToSecond) * (vinf * 1e5) * sqrt(radius)
    result = log10(result)
    return result

def calculateMass(logg, radius):
    solarMass = 1.98893e33
    solarRadius = 6.955e10
    gravitationalC = 6.673e-8
    result = 10**logg * (radius*solarRadius)**2 / gravitationalC
    result = result / solarMass
    return result


def readPar(fileName):
    # Reads in the .par file
#    try:
    modelList = []
    file = open(fileName, 'r')
    fileContent = file.readlines()
    file.close
    currentLine=1
    generation = 0
    print 'start_reading'
    for i in range(len(fileContent)):
        if string.find(fileContent[currentLine],'av fit:') is not -1:
            currentLine += 2 #Check for the deviding line between generations
            if currentLine > len(fileContent)-1:
                break #Catch end of file
        columns = fileContent[currentLine].strip().split() #Read in the parameter line
#            if len(columns) == 18:
#                nitrogen = True
#            else:
#                nitrogen = False
        print columns
        tmpModel = model()
        for i in read_dict.keys():
            setattr(tmpModel, i, float(columns[read_dict[i]]))
        '''tmpModel.Teff = float(columns[1])
        tmpModel.logg = float(columns[2])
        tmpModel.mdot = float(columns[3])
        tmpModel.vinf = float(columns[4])
        tmpModel.beta = float(columns[5])
        tmpModel.NHe = float(columns[6])
        tmpModel.vtur = float(columns[7])
        tmpModel.vrot = float(columns[8])
        tmpModel.vmacro = float(columns[9])
#            if nitrogen:
        tmpModel.nitrogen = float(columns[10])
        tmpModel.carbon = float(columns[11])
        tmpModel.oxygen = float(columns[12])
        tmpModel.phosphorus = float(columns[13])
        tmpModel.silicon = float(columns[14])'''
#                k = 1
#            else:
#                k = 0
#
#                tmpModel.nitrogen = False
        tmpModel.radius = float(columns[18])
        tmpModel.fitness = float(columns[19])
        tmpModel.exeTime = float(columns[20])
        tmpModel.number = int(columns[21])
        tmpModel.generation = int(columns[23])
        tmpModel.ID = columns[24]
        currentLine +=1
        columns = fileContent[currentLine].strip().split() #chi^2 line
        tmpModel.chi2 = float(columns[3])
        tmpModel.origchi2 = float(columns[3])
        currentLine +=1
        columns = fileContent[currentLine].strip().split() #Read in the line fitness line
        tmpModel.converged = False
        for i in range(1, len(columns)):
            print(lines[i-2].ID)
            setattr(tmpModel, lines[i-2].ID, float(columns[i]))
            all_parameter_names_dict[lines[i-2].ID] = lines[i-2].ID
        for i in range(len(columns)-2):
            if float(columns[i+1]) != -1: tmpModel.converged = True
        currentLine += 1
        i = currentLine
        tmpModel.luminosity = calculateLuminosity(tmpModel.radius, tmpModel.Teff)
        tmpModel.dmom = calculateDmom(tmpModel.mdot, tmpModel.vinf, tmpModel.radius)
        tmpModel.mass = calculateMass(tmpModel.logg, tmpModel.radius)
        modelList.append(tmpModel)
        if currentLine > len(fileContent)-1:
            break #Catch end of file
    print '* ' + fileName + ' read in succesfully'
    return modelList
#    except:
#        print 'ERROR - Could not read in ' + fileName
#        print 'ERROR - Check your file'
#        print 'ERROR - Error message: ', sys.exc_info()[1]
#        raw_input('Press Enter to continue... ')
#        restart()
    return


def readIni_new(fileName, parameters = False):
    try:
        result = [] # Stores the information about the fitted lines
        file = open(fileName, 'r')
        fileContent_tmp = file.readlines()
        file.close()
        fileContent = []
        for line in fileContent_tmp:
            if not line.strip():
                continue
            else: fileContent.append(line)
        # The above reads in the file and gets rid of the empty lines
        numberOfLines = int(fileContent[1])
        currentLine = 2 # Keeps track of the position in the file
        for i in range(numberOfLines):
            # Read in the line data
            tmpline = lineParameters()
            columns = fileContent[currentLine].strip().split()
            tmpline.ID = columns[0]
            tmpline.resolution = int(columns[1])
            currentLine += 1
            columns = map(float, fileContent[currentLine].strip().split())
            tmpline.WLleft = columns[0]
            tmpline.WLright = columns[1]
            currentLine += 1
            tmpline.RV = float(fileContent[currentLine])
            currentLine += 1
            columns = map(float, fileContent[currentLine].strip().split())
            tmpline.fluxCorrectionLeft = columns[1]
            tmpline.fluxCorrectionRight = columns[3]
            currentLine += 1
            tmpline.weight = float(fileContent[currentLine])
            currentLine += 1
            result.append(tmpline)
        if parameters:
            par = []
            for i in range(numberOfParameters):
                columns = fileContent[currentLine].strip().split()
                par.append([float(columns[0]), float(columns[1]), float(columns[2])])
                currentLine += 1
            par.append([0,10,0]) #luminosity
            par.append([0,50,0]) #dmom
            par.append([0,1000,0]) #mass
            par.append([0,100,0]) #radius
            pars = {}
            for i in read_dict.keys():
                pars[i] = par[read_dict[i] - 1]
            pars['luminosity'] = par[-4]
            pars['dmom'] = par[-3]
            pars['mass'] = par[-2]
            pars['radius'] = par[-1]
            return pars
        return result
    except:
        print 'ERROR - Could not read in ' + fileName
        print 'ERROR - Check your file'
        print 'ERROR - Error message: ', sys.exc_info()[1]
        raw_input('Press Enter to continue... ')
        restart()



def readIni(fileName, parameters = False):
    try:
        result = [] # Stores the information about the fitted lines
        file = open(fileName, 'r')
        fileContent_tmp = file.readlines()
        file.close()
        fileContent = []
        for line in fileContent_tmp:
            if not line.strip():
                continue
            else: fileContent.append(line)
        # The above reads in the file and gets rid of the empty lines
        numberOfLines = int(fileContent[1])
        currentLine = 2 # Keeps track of the position in the file
        for i in range(numberOfLines):
            # Read in the line data
            tmpline = lineParameters()
            columns = fileContent[currentLine].strip().split()
            tmpline.ID = columns[0]
            tmpline.resolution = int(columns[1])
            currentLine += 1
            columns = map(float, fileContent[currentLine].strip().split())
            tmpline.WLleft = columns[0]
            tmpline.WLright = columns[1]
            currentLine += 1
            tmpline.RV = float(fileContent[currentLine])
            currentLine += 1
            columns = map(float, fileContent[currentLine].strip().split())
            tmpline.fluxCorrectionLeft = columns[1]
            tmpline.fluxCorrectionRight = columns[3]
            currentLine += 1
            tmpline.weight = float(fileContent[currentLine])
            currentLine += 1
            result.append(tmpline)
        if parameters:
            par = []
            for i in range(numberOfParameters):
                columns = fileContent[currentLine].strip().split()
                par.append([float(columns[0]), float(columns[1]), float(columns[2])])
                currentLine += 1
            par.append([0,10,0]) #luminosity
            par.append([0,50,0]) #dmom
            par.append([0,1000,0]) #mass
            par.append([0,100,0]) #radius
            return par
        return result
    except:
        print 'ERROR - Could not read in ' + fileName
        print 'ERROR - Check your file'
        print 'ERROR - Error message: ', sys.exc_info()[1]
        raw_input('Press Enter to continue... ')
        restart()


def readNorm(fileName):
    try:
        result = []
        wlc = []
        flux = []
        error = []
        file = open(fileName, 'r')
        fileContent = file.readlines()
        currentLine = 0
        if fileContent[0][0] is '#':
            currentLine += 1
        for i in range(len(fileContent)-1):
            columns = map(float, fileContent[currentLine].strip().split())
            wlc.append(columns[0])
            flux.append(columns[1])
            if len(columns) == 3: error.append(columns[2])
            currentLine += 1
        file.close()
        if len(columns) == 3: result = [wlc,flux,error]
        else: result = [wlc, flux]
        return result
    except:
        print 'ERROR - Could not read in ' + fileName
        print 'ERROR - Check your file (must be 3 columns: wlc, flux, error)'
        print 'ERROR - Error message: ', sys.exc_info()[1]
        raw_input('Press Enter to continue')
        restart()
    return


def findBestModel():
    result = 0
    currentBest = 1e6
    for i in range(len(models)):
        if models[i].origchi2 < currentBest:
            result = i
            currentBest = models[i].origchi2
    return result


def printBestModel_new():
    printHeader()
    i = findBestModel()
    bestModel = models[i]
#    if bestModel.nitrogen:
#        k = 1
#    else:
#        k = 0
    errorRanges = findErrorRanges_new()
    print
    print '    ****************'
    print '    ** BEST MODEL **'
    print '    ****************'
    print
    print '    Object:', object
    print '    Best model: #', bestModel.number, 'at generation', bestModel.generation, 'chi2', bestModel.origchi2
    print '    Model ID: ' + bestModel.ID
    print
    print '    Model parameters \tErrors'
    print '    ------------------------------------------------------------'

    for i in fit_parameters:
        if vars(bestModel)[i] > 1000:
            print '      ' + all_parameter_names_dict[i].ljust(12) + ': ' + str(int(vars(bestModel)[i])) + ' [' + str(int(errorRanges[i][0])) + ' - ' + str(int(errorRanges[i][1])) +']'
        else:
            print '      ' + all_parameter_names_dict[i].ljust(12) + ': ' + str(round(vars(bestModel)[i],2)) + ' [' + str(round(errorRanges[i][0],2)) + ' - ' + str(round(errorRanges[i][1],2)) +']'
    print
    print
    for i in list(set(all_parameters)-set(fit_parameters)-set(diagnostic_parameters)):
        print '      ' + all_parameter_names_dict[i].ljust(12) + ': ' + str(round(vars(bestModel)[i],2)) + ' [' + str(round(errorRanges[i][0],2)) + ' - ' + str(round(errorRanges[i][1],2)) +']'

    choice = raw_input('Press 1 to write to file, or Enter to continue. ')
    if choice is '1': printBestModelFile()
    return

def printBestModel():
    printHeader()
    i = findBestModel()
    bestModel = models[i]
#    if bestModel.nitrogen:
#        k = 1
#    else:
#        k = 0
    errorRanges = findErrorRanges()
    print
    print '    ****************'
    print '    ** BEST MODEL **'
    print '    ****************'
    print
    print '    Object:', object
    print '    Best model: #', bestModel.number, 'at generation', bestModel.generation, 'chi2', bestModel.origchi2
    print '    Model ID: ' + bestModel.ID
    print
    print '    Model parameters \tErrors'
    print '    ------------------------------------------------------------'
    print '      Teff      : '+ str(int(bestModel.Teff))+' [' + str(int(errorRanges[0][0])) + ' - ' + str(int(errorRanges[0][1])) +']'
    print '      log g     : '+ str(bestModel.logg)+' \t[' + str(errorRanges[1][0]) + ' - ' + str(errorRanges[1][1]) +']'
    print '      log Mdot  : '+ str(bestModel.mdot)+' \t[' + str(errorRanges[2][0]) + ' - ' + str(errorRanges[2][1]) +']'
    print '      v_inf     : '+ str(int(bestModel.vinf))+' \t[' + str(errorRanges[3][0]) + ' - ' + str(errorRanges[3][1]) +']'
    print '      beta      : '+ str(bestModel.beta)+' \t[' + str(errorRanges[4][0]) + ' - ' + str(errorRanges[4][1]) +']'
    print '      NHe       : '+ str(bestModel.NHe)+' \t[' + str(errorRanges[5][0]) + ' - ' + str(errorRanges[5][1]) +']'
    print '      v_tur     : '+ str(bestModel.vtur)+' \t[' + str(errorRanges[6][0]) + ' - ' + str(errorRanges[6][1]) +']'
    print '      v_macro   : '+ str(bestModel.vmacro)+' \t[' + str(errorRanges[8][0]) + ' - ' + str(errorRanges[8][1]) +']'
    print '      v_rot     : '+ str(bestModel.vrot)+' \t[' + str(errorRanges[7][0]) + ' - ' + str(errorRanges[7][1]) +']'
    print '      radius    : '+ str(round(bestModel.radius,2)) + ' \t[' + str(round(errorRanges[14][0],2)) + ' - ' + str(round(errorRanges[14][1],2)) +']'
#    if bestModel.nitrogen:
    print '      nitrogen    : '+ str(round(bestModel.nitrogen  ,2)) + ' \t[' + str(round(errorRanges[ 9][0],2)) + ' - ' + str(round(errorRanges[9][1],2)) +']'
    print '      carbon      : '+ str(round(bestModel.carbon    ,2)) + ' \t[' + str(round(errorRanges[10][0],2)) + ' - ' + str(round(errorRanges[10][1],2)) +']'
    print '      oxygen      : '+ str(round(bestModel.oxygen    ,2)) + ' \t[' + str(round(errorRanges[11][0],2)) + ' - ' + str(round(errorRanges[11][1],2)) +']'
    print '      phosphorus  : '+ str(round(bestModel.phosphorus,2)) + ' \t[' + str(round(errorRanges[12][0],2)) + ' - ' + str(round(errorRanges[12][1],2)) +']'
    print '      silicon     : '+ str(round(bestModel.silicon   ,2)) + ' \t[' + str(round(errorRanges[13][0],2)) + ' - ' + str(round(errorRanges[13][1],2)) +']'

    print
    print
    print '      luminosity : '+ str(round(bestModel.luminosity,2))+' \t[' + str(round(errorRanges[15][0],2)) + ' - ' + str(round(errorRanges[15][1],2)) +']'
    print '      dmom       : '+ str(round(bestModel.dmom,2))+' \t[' + str(round(errorRanges[16][0],2)) + ' - ' + str(round(errorRanges[16][1],2)) +']'
    print '      mass       : '+ str(round(bestModel.mass,2))+' \t[' + str(round(errorRanges[17][0],2)) + ' - ' + str(round(errorRanges[17][1],2)) +']'
    choice = raw_input('Press 1 to write to file, or Enter to continue. ')
    if choice is '1': printBestModelFile()
    return


def printBestModelFile():
    bestModel = models[findBestModel()]
    errorRanges = findErrorRanges()
    fileName = object+'_bestmod.txt'
    if checkFile(fileName, maindir+'/'+object):
        choice = ''
        while choice is not 'y' or 'n':
            choice = raw_input('\'' + str(fileName) + '\' already exists. Overwrite (y/n)? ')
            if choice is 'n': return
            elif choice is 'y': break
    file = open(maindir+'/'+object+'/'+fileName,'w')
    file.write('\n')
    file.write('    ****************\n')
    file.write('    ** BEST MODEL **\n')
    file.write('    ****************\n')
    file.write('\n')
    file.write('    Object: '+ str(object) + '\n')
    file.write('    Best model: # ' + str(bestModel.number) + ' at generation ' + str(bestModel.generation) + ' with fitness ' + str(bestModel.fitness) + '\n')
    file.write('\n')
    file.write('    Model parameters \tErrors\n')
    file.write('    ------------------------------------------------------------\n')
    file.write('      Teff      : ' + str(int(bestModel.Teff)) + ' [' + str(int(errorRanges[0][0])) + ' - ' + str(int(errorRanges[0][1])) +']\n')
    file.write('      log g     : ' + str(bestModel.logg) + ' \t[' + str(errorRanges[1][0]) + ' - ' + str(errorRanges[1][1]) +']\n')
    file.write('      log Mdot  : ' + str(bestModel.mdot) + ' \t[' + str(errorRanges[2][0]) + ' - ' + str(errorRanges[2][1]) +']\n')
    file.write('      v_inf     : ' + str(int(bestModel.vinf)) + ' \t[' + str(errorRanges[3][0]) + ' - ' + str(errorRanges[3][1]) +']\n')
    file.write('      beta      : ' + str(bestModel.beta) + ' \t[' + str(errorRanges[4][0]) + ' - ' + str(errorRanges[4][1]) +']\n')
    file.write('      NHe       : ' + str(bestModel.NHe) + ' \t[' + str(errorRanges[5][0]) + ' - ' + str(errorRanges[5][1]) +']\n')
    file.write('      v_tur     : ' + str(bestModel.vtur) + ' \t[' + str(errorRanges[6][0]) + ' - ' + str(errorRanges[6][1]) +']\n')
    file.write('      v_macro   : ' + str(bestModel.vmacro) + ' \t[' + str(errorRanges[8][0]) + ' - ' + str(errorRanges[8][1]) +']\n')
    file.write('      v_rot     : ' + str(bestModel.vrot) + ' \t[' + str(errorRanges[7][0]) + ' - ' + str(errorRanges[7][1]) +']\n')
    file.write('      radius    : ' + str(round(bestModel.radius,2)) + '\n')
    file.write('\n')
    file.write('\n')
    file.close()
    raw_input('Output written to \'' + str(fileName) + '\'. Press Enter to continue. ')
    return


def analysisMenu():
    while(True):
        printHeader()
        print 'ANALYSIS MENU:'
        print
        print '1. Show best model parameters'
        print '2. Make plots'
        print '3. Look up a model'
        print '4. Make pdf with results'
        print '5. Remove from LISA'
        print '0. Main menu'
        print
        choice = raw_input('Choose an option: ')
        if choice is '1': printBestModel_new()
        if choice is '2': makePlots_new()
        if choice is '3': findModel()
        if choice is '4': makeAllPlots_new()
        if choice is '5':
            ssh = lisaConnect()
            cleanLisa(ssh)
        if choice is '0': restart()
        else:
            continue


def makePlots_new():
    while(True):
        printHeader()
        print 'PLOTTING MENU ('+object+')'
        print
        print 'add a \'p\' to the end of your selection for pdf output'
        print
        print '1. Plot best model'
        print '2. Plot best models'
        print '3. Plot a model'
        print '4. Plot parameter ranges'
        print '5. Plot parameter correlations '
        print '6. Plot convergence by generation'
        print '7. Make full pdf report '
        print '0. Return to Analysis menu'
        print
        choice = raw_input('Choose an option: ')
        if 'p' in choice:
            pdf = True
            choice = choice.strip(' p')
        else:
            pdf = False
        if choice == '1': plotModel(model = models[findBestModel()], bestModelFlag = True, pdf = pdf)
        if choice == '2': plotModels(model = models[findBestModel()], bestModelFlag = True, pdf = pdf)
        if choice == '4': makeParameterplots(pdf)
        if choice == '5': plot_correlation('fitness', pdf = pdf)
        if choice == '6': makeConvergenceplots(pdf)

        if choice == '3':
            print
            number = int(raw_input('Enter model number: '))
            generation = int(raw_input ('Enter model generation: '))
            try:
                tmpModel = findModelID(number,generation)
                plotModel(model = tmpModel, pdf = pdf)
            except:
                raw_input('Invalid input. Press Enter to continue. ')
                choice = ''
        if choice is '7': makeAllPlots_new()
        if choice is '0': break
    return


def makeConvergenceplots(pdf):
    while(True):
        printHeader()
        print 'CONVERGENCE PLOTTING MENU ('+object+')'
        print
        print '1. Fittness'
        print '2. Chi2'
        print '3. Probability'
        for i in range(len(fit_parameters)):
            print str(i+4) + '. ' + all_parameter_names_dict[fit_parameters[i]]
        print '0. Return to previous menu'
        print
        choice = raw_input('Choose an option: ')
        if choice == '1': plot_convergence('fitness', 'fitness', pdf = pdf);break
        if choice == '2': plot_convergence('fitness', 'chi2', pdf = pdf);break
        if choice == '3': plot_convergence('fitness', 'P', pdf = pdf);break
        if choice in [str(i+4) for i in range(len(fit_parameters))]: plot_convergence('fitness', fit_parameters[int(choice)-4], pdf = pdf);break;break
        if choice is '0': break
    return


def makeParameterplots(pdf):
    while(True):
        printHeader()
        print 'PARAMETER PLOTTING MENU ('+object+')'
        print
        print '1. Fittness'
        print '2. Chi2'
        print '3. Probability'
        for i in range(len(lines)):
            print str(i+4) + '. ' + lines[i].ID
        print '0. Return to previous menu'
        print
        choice = raw_input('Choose an option: ')
        if choice == '1': plotResults_new('fitness', pdf = pdf);break
        if choice == '2': plotResults_new('chi2', pdf = pdf);break
        if choice == '3': plotResults_new('P', pdf = pdf);break
        if choice in [str(i+4) for i in range(len(lines))]: plotResults_new(lines[int(choice) - 4].ID, pdf = pdf);break;break
        if choice is '0': break
    return


def makePlots():
    while(True):
        printHeader()
        print 'PLOTTING MENU ('+object+')'
        print
        print '1. Plot best model'
        print '2. Plot best model (pdf)'
        print '3. Plot parameter ranges'
        print '4. Plot parameter ranges (pdf)'
        print '5. Plot a model'
        print '6. Plot a model (pdf)'
        print '7. Make full pdf report '
        print '8. Plot parameter ranges (CHI2)'
        print '9. Plot parameter ranges (CHI2) (png)'
        print '10. Plot parameter ranges (Prob)'
        print '11. Plot parameter ranges (Prob) (png)'
        print '12. Plot parameter correlations '
        print '13. Plot convergence by generation'
        print '0. Return to Analysis menu'
        print
        choice = raw_input('Choose an option: ')
        if choice == '1': plotModel(model = models[findBestModel()], bestModelFlag = True)
        #if choice is '1': plotModels(model = models[findBestModel()])
        if choice == '2': plotModels(model = models[findBestModel()], bestModelFlag = True, pdf = True)
        if choice == '3': plotResults_new('fitness')
        if choice == '4': plotResults_new('fitness', pdf = True)
        if choice == '8': plotResults_new('chi2')
        if choice == '9': plotResults_new('chi2', pdf = True)
        if choice == '10': plotResults_new('P')
        if choice == '11': plotResults_new('P', pdf = True)
        if choice == '12': plot_correlation('fitness')
        if choice == '13': plot_convergence('fitness')

        if choice == '5' or choice is '6':
            print
            number = int(raw_input('Enter model number: '))
            generation = int(raw_input ('Enter model generation: '))
            try:
                tmpModel = findModelID(number,generation)
                if choice is '5': plotModel(model = tmpModel)
                if choice is '6': plotModel(model = tmpModel, pdf = True)
            except:
                raw_input('Invalid input. Press Enter to continue. ')
                choice = ''
        if choice is '7': makeAllPlots()
        if choice is '0': break
    return


def makeAllPlots_new():
    printHeader()
    fileName = maindir+'/'+object+'/plots/'+object+'.pdf'
    path = maindir+'/'+object+'/plots/temp/'
    try: shutil.rmtree(path)
    except: pass
    os.mkdir(path)
    im_num = 1
    #pdfFile = FPDF('L', 'mm', 'A4')
    print 'MAKING PLOTS'
    print

    im_num = plotBestModel(multi = True, im_num = im_num, path = path)
    im_num = plotModel(model = models[findBestModel()], multi = True, im_num = im_num, path = path)
    im_num = plotModels(model = models[findBestModel()], bestModelFlag = True, im_num = im_num, path = path, multi=True)
    im_num = plotResults_new('fitness',multi = True, im_num = im_num, path = path)
    im_num = plotResults_new('chi2',multi = True, im_num = im_num, path = path)
    im_num = plotResults_new('P',multi = True, im_num = im_num, path = path)
    im_num = plot_convergence_all('fitness', multi = True, im_num = im_num, path = path)
    im_num = plot_convergence_all('fitness', overall = True, multi = True, im_num = im_num, path = path)
    im_num = plot_convergence('fitness', 'fitness', multi = True, im_num = im_num, path = path)
    im_num = plot_convergence('chi2', 'chi2', multi = True, im_num = im_num, path = path)
    im_num = plot_correlation('fitness', multi = True, im_num = im_num, path = path)
    im_num = plot_correlation('chi2', multi = True, im_num = im_num, path = path)

    files = glob.glob(path+'*.pdf')
    files.sort()
    merger = PdfFileMerger()

    for pdf in files:
        merger.append(pdf)

    merger.write(fileName)

    print 'Plot saved as \'' + fileName + '\''
    plt.close('all')
    #shutil.rmtree(path)
    raw_input('Press Enter to continue...')
    return


def makeAllPlots():
    printHeader()
    fileName = maindir+'/'+object+'/plots/'+object+'.pdf'
    pdfFile = PdfPages(fileName)
    print 'MAKING PLOTS'
    print
    pdfFile = plotBestModel(multi = pdfFile)
    pdfFile = plotModel(model = models[findBestModel()], multi = pdfFile)
    pdfFile = plotResults_new('fitness',multi = pdfFile)
    pdfFile = plotResults_new('chi2',multi = pdfFile)
    pdfFile = plotResults_new('P',multi = pdfFile)
    #pdfFile = plot_correlation('fitness', multi = pdfFile)
    pdfFile.close()
    print 'Plot saved as \'' + fileName + '\''
    plt.close('all')
    raw_input('Press Enter to continue...')
    return


def findModelID(number,generation):
    for i in range(len(models)):
        if models[i].number == number and models[i].generation == generation: result = models[i]
    return result


def unpack(fileName):
    if checkFile(fileName+'.tar.gz', maindir+'/'+object+'/profiles/'):
        try:
            os.mkdir(maindir+'/'+object+'/profiles/'+fileName)
        except:
            pass
        file = tarfile.open(maindir+'/'+object+'/profiles/'+fileName+'.tar.gz')
        file.extractall(path=maindir+'/'+object+'/profiles/'+fileName)
        file.close()
    else:
        print fileName + '  not found!'
        return




def readProfiles(model):
    result = []
    flag = False
    ID = model.ID
    unpack(ID)
    directory = maindir+'/'+object+'/profiles/'+ID
    for j in range(len(lines)):
        os.chdir(directory)
        fileName = lines[j].ID + '.prof.fin'
        if checkFile(fileName, directory):
            file = open(fileName,'r')
            fileContent = file.readlines()
            file.close()
            currentLine = 1
            wavelength = []
            flux = []
            for k in range(len(fileContent)-1):
                columns = fileContent[currentLine].strip().split()
                wavelength.append(float(columns[0]))
                flux.append(float(columns[1]))
                currentLine += 1
            result.append([lines[j].ID,wavelength,flux])
        else:
            flag = True
            print 'WARNING - Missing line profile: '+fileName
    os.chdir(maindir)
    shutil.rmtree(maindir+'/'+object+'/profiles/'+ID)
    return result, flag


def selectWavelength2(norm, WLleft, WLright):
    result_error=[]
    result_wlc=[]
    result_flux=[]
    for i in range(len(norm[0])):
        if norm[0][i] >= WLleft:
            if norm[0][i] <= WLright:
                result_wlc.append(norm[0][i])
                result_flux.append(norm[1][i])
                result_error.append(norm[2][i])
    return result_wlc, result_flux, result_error


def selectWavelength (wlc, flux, WLleft, WLright):
    result_wlc=[]
    result_flux=[]
    for i in range(len(wlc)):
        if wlc[i] >= WLleft:
            if wlc[i] <= WLright:
                result_wlc.append(wlc[i])
                result_flux.append(flux[i])
    return result_wlc, result_flux


def plotModel(model, bestModelFlag=False, multi=False, pdf=False, im_num = False, path = '.'):
    def on_key1(event):
        if event.key == 'enter':
            plt.close()

    print 'Plotting profiles...'
    print
    if model:
        fileName = maindir+'/'+object+'/'+object+'.norm'
    else: fileName = maindir + '/new/' + object + '/' + object + '.norm'
    norm = readNorm(fileName)
    if model:
        profiles,flag = readProfiles(model)
        if flag:
            print 'WARNING - Line profiles could not be read in!'
            raw_input('WARNING - Press enter to continue.')
            return
    else: profiles = lines
    if pdf or multi: fig = plt.figure(figsize=(20,13))
    else:
        fig = plt.figure(figsize=(20,10))
    for i in range(len(profiles)):
        if model:
            for j in range(len(lines)):
                if lines[j].ID is profiles[i][0]:
                    ID = lines[j].ID
                    RV = lines[j].RV
                    WLleft = lines[j].WLleft
                    WLright = lines[j].WLright
                    leftOffset = 1 + lines[j].fluxCorrectionLeft
                    rightOffset = 1 + lines[j].fluxCorrectionRight
                    weight = lines[j].weight
            for k in range(len(profiles[i][1])):
                profiles[i][1][k] = profiles[i][1][k]*(1+RV/299792.)
        else:
            ID = lines[i].ID
            RV = lines[i].RV
            WLleft = lines[i].WLleft
            WLright = lines[i].WLright
            leftOffset = 1 + lines[i].fluxCorrectionLeft
            rightOffset = 1 + lines[i].fluxCorrectionRight
            weight = lines[i].weight
        delta = (WLright - WLleft)/15.
        wlc,flux = selectWavelength(norm[0],norm[1],WLleft-delta,WLright+delta)
        if not model:
            fluxCorrection = numpy.interp(wlc, [WLleft, WLright], [leftOffset, rightOffset])
            for j in range(len(flux)): flux[j] = flux[j] / fluxCorrection[j]
        fig.subplots_adjust(hspace=0.5)
        ax = fig.add_subplot(len(profiles)/3+1,3,i+1)
        if model: ax.plot(profiles[i][1],profiles[i][2],'r-')
        ax.set_xlim(wlc[0]-delta,wlc[len(wlc)-1]+delta)
        ax.set_xlabel('Wavelength',fontsize=11)
        ax.set_ylabel('Normalized flux',fontsize=11)
        ax.set_title(ID)
        ax.plot(wlc,flux,'.',color='black')
        ax.axvline(x=WLleft,linestyle='--',color='green')
        ax.axvline(x=WLright,linestyle='--', color='green')
        axhline(y=1, linestyle='-', color='black')
    if model:
        fig.suptitle('Model line profiles for '+object+' (model #'+str(model.number)+' of generation '+str(model.generation)+')', fontsize=20)
    else:
        fig.suptitle('Check new settings for ' + object + ' (Enter to close)', fontsize = 20)
    if not multi:
        if pdf:
            if bestModelFlag: fileName = maindir+'/'+object+'/plots/bestmodel_'+object+'.pdf'
            else: fileName = maindir+'/'+object+'/plots/'+object+'_gen'+str(model.generation)+'_ind'+str(model.number)+'.pdf'
            plt.savefig(fileName, orientation='landscape', format='pdf')
            print 'Plot saved as \''+ fileName +'\''
            raw_input('Press Enter to continue... ')
        else:
            fig.canvas.mpl_connect('key_press_event', on_key1)
            plt.show()
    else:
        if im_num:
            fileName = path + 'temp' + str(im_num).zfill(2)+ '.png'
            plt.savefig(fileName, format = 'png', dpi=300)
            im = Image.open(fileName)
            im = im.convert('RGB')
            im.save(path + 'temp' + str(im_num).zfill(2)+ '.pdf')
            im_num += 1
            return im_num
        else:
            multi.savefig(orientation='landscape')
            return multi
    return

def findModelswithinP():
	results = []
	for i in range(len(models)):
		if models[i].P > minimumP:
			results.append(i)
	return results


def plotModels(model, bestModelFlag=False, multi=False, pdf=False, im_num = False, path = '.'):
#     def on_key1(event):
#         if event.key == 'enter':
#             plt.close()
#	"Load table for SpT"
#	headers = None
#	data = dict()
#	path = '/Users/oscarin/Dropbox/GA_Results/tables_paper/'
#	fileSpT = path+'SpT_4_table_new.dat'
#	for line in open(fileSpT):
#		if headers == None:
#			headers = line.split()
#			for header in headers:
#				data[header] = list()
#			continue
#		values = line.split()
#		for index, header in enumerate(headers):
#			data[header].append(values[index])
#
#	for i in range(len(data['#VFTS'])):
#		print data['#VFTS'][i],data['SpT'][i]
#		if data['#VFTS'][i] == object:
#			SpT_star = data['SpT'][i]
#	""
    SpT_star = ""
    best_models = findModelswithinP()
    print len(best_models)
    counter = 0
    fileName = maindir+'/'+object+'/'+object+'.norm'
    norm = readNorm(fileName)
    fig = plt.figure(figsize=(20,13))

    #	for z in best_models[::50]:
    for z in best_models:
        #		print 'z, model[z]=',z,models[z].ID
        #		plotModel(model = models[i], pdf=True)
        #		flag = False
        try:
	    profiles,flag = readProfiles(models[z])
	    counter+=1
#	    print counter
	except:
#			from sys import exc_info
#			print exc_info(),z,models[z]
	    continue
	for i in range(len(profiles)):
	    if models[z]: # and not flag
		for j in range(len(lines)):
		    if lines[j].ID is profiles[i][0]:
			ID = lines[j].ID
			RV = lines[j].RV
			WLleft = lines[j].WLleft
			WLright = lines[j].WLright
			leftOffset = 1 + lines[j].fluxCorrectionLeft
			rightOffset = 1 + lines[j].fluxCorrectionRight
			weight = lines[j].weight
		for k in range(len(profiles[i][1])):
                    #					profiles[i][1][k] = profiles[i][1][k]*(1+RV/299792.)
		    profiles[i][1][k] = profiles[i][1][k]

	    else:
		ID = lines[i].ID
		RV = lines[i].RV
		WLleft = lines[i].WLleft
		WLright = lines[i].WLright
		leftOffset = 1 + lines[i].fluxCorrectionLeft
		rightOffset = 1 + lines[i].fluxCorrectionRight
		weight = lines[i].weight
	    delta = (WLright - WLleft)/15.
#			wlc,flux = selectWavelength(norm[0],norm[1],WLleft-delta,WLright+delta)
            wlc,flux, error = selectWavelength2(norm,WLleft-delta,WLright+delta)
	    error = np.double(error)
#			if not models[z]:
	    fluxCorrection = numpy.interp(wlc, [WLleft, WLright], [leftOffset, rightOffset])
	    for j in range(len(flux)): flux[j] = flux[j] / fluxCorrection[j]
	    fig.subplots_adjust(hspace=0.5)
	    ax = fig.add_subplot(len(profiles)/3+1,3,i+1)
	    if models[z] and models[z] != model:
		ax.plot(profiles[i][1],profiles[i][2],'r-',color='green',alpha=0.9,zorder=8)
	    if models[z] and models[z] == model:
		ax.plot(profiles[i][1],profiles[i][2],'r-',color='red',linewidth=1.5,zorder=9)

	    if counter == 1:
		wlc = np.double(wlc)
		flux = np.double(flux)
		wlc = wlc/(1+RV/299792.)
		ax.plot(wlc,flux,'.',color='black',zorder=10)
		ax.errorbar(wlc,flux,yerr=error/1.2,fmt='None',ecolor='black',zorder=10)
		ax.set_xlim(wlc[0]-delta,wlc[len(wlc)-1]+delta)
		ax.set_xlabel('Wavelength',fontsize=11)
		ax.set_ylabel('Normalized flux',fontsize=11)
		ax.set_title(ID)
		ax.axvline(x=WLleft/(1+RV/299792.),linestyle='--',color='green')
		ax.axvline(x=WLright/(1+RV/299792.),linestyle='--', color='green')
		axhline(y=1, linestyle='-', color='black')
		fig.suptitle('Best Models for '+object+': '+SpT_star, fontsize=20)

    if not multi:
	if pdf:
            #            if bestModelFlag: fileName = maindir+'/'+object+'/plots/plot_models_'+object+'.png'
            #            else: fileName = maindir+'/'+object+'/plots/'+object+'_gen'+str(model.generation)+'_ind'+str(model.number)+'.pdf'
	    fileName = maindir+'/'+object+'/plots/plot_models_'+object+'_ebs.png'
	    plt.savefig(fileName, orientation='landscape', format='png')
	    print 'Plot saved as \''+ fileName +'\''
#            raw_input('Press Enter to continue... ')
 	else:
 	    #fig.canvas.mpl_connect('key_press_event', on_key1)
 	    plt.show()
    else:
        if im_num:
            fileName = path + 'temp' + str(im_num).zfill(2)+ '.png'
            plt.savefig(fileName, format = 'png', dpi=300)
            im = Image.open(fileName)
            im = im.convert('RGB')
            im.save(path + 'temp' + str(im_num).zfill(2)+ '.pdf')
            im_num += 1
            return im_num
        else:
            multi.savefig(orientation='landscape')
            return multi
    return


def plot_correlation(diagnostic_name, multi = False, pdf = False, im_num = False, path = '.'):
    if not multi: printHeader()
    print 'Preparing parameter correlation plot...'
    print
    if pdf or multi: fig = plt.figure('main',figsize=(13,13))
    else: fig = plt.figure(figsize=(15,15))
    bestModel = models[findBestModel()]
    fileName = maindir + '/' + object +'/pfw_' + object + '.ini'
    xRanges = readIni_new(fileName, parameters = True)
    errorRanges = findErrorRanges_new()
    #    cmap = get_cmap('RdYlGn')
    cmap = get_cmap('gist_rainbow') #colormap
    vmin, vmax = vars(bestModel)[diagnostic_name]/3., vars(bestModel)[diagnostic_name]*2.2  #used to normalize colors, adjust when using other colormap

    fit_params_used = [i for i in fit_parameters if xRanges[i][2] > 0]
    fit_diag_param_vals = {}
    fit_diag_params = list(fit_params_used)
    fit_diag_params.extend(diagnostic_parameters)

    for i in fit_diag_params:
        fit_diag_param_vals[i] = []

    for i in range(len(models)):
        for j in fit_diag_params:
            fit_diag_param_vals[j].append(vars(models[i])[j])

    if diagnostic_name == 'chi2':
        sorted_indicies = np.argsort(fit_diag_param_vals[diagnostic_name])[::-1]
    else:
        sorted_indicies = np.argsort(fit_diag_param_vals[diagnostic_name])
    n_rows = len(fit_params_used) -1
    n_cols = n_rows
    for i in range(len(fit_params_used)-1):
        for j in range(i+1, len(fit_params_used)):
            fig_params = [n_rows,n_cols, (j-1)*n_rows + i+1, int(ceil(len(fit_parameters)/4.)), 4]
            plot_correlation_pannel(fit_params_used[i],fit_params_used[j], sorted_indicies, diagnostic_name, fit_diag_param_vals, fig, fig_params, vmin, vmax, cmap, bestModel, xRanges, multi)

    print
    fig.suptitle('Correlation based on ' + all_parameter_names_dict[diagnostic_name] + ' Between Parameters',fontsize=20)
    if not multi:
        if pdf:
            print 'Making pdf, this may take a minute...'
            fileName = maindir+'/'+object+'/plots/correlation_'+object+'.pdf'
            plt.plot()
            plt.savefig(fileName, orientation='landscape', format='pdf')
            print
            print 'Plot saved as \''+ fileName +'\''
            raw_input('Press Enter to continue... ')
        else:
            plt.show()
    else:
        print 'Making Parameters vs. '+ all_parameter_names_dict[diagnostic_name] +' plot (might take a minute)...'
        if im_num:
            plt.figure('main')
            print 'main'
            fileName = path + 'temp' + str(im_num).zfill(2)+ '.png'
            plt.savefig(fileName, orientation='portrait', format = 'png', dpi=300)
            im = Image.open(fileName)
            im = im.convert('RGB')
            im.save(path + 'temp' + str(im_num).zfill(2)+ '.pdf')
            im_num += 1
            for i in fit_params_used:
                print i
                plt.figure(i)
                fileName = path + 'temp' + str(im_num).zfill(2)+ '.png'
                plt.savefig(fileName, orientation='portrait', format = 'png', dpi=300)
                im = Image.open(fileName)
                im = im.convert('RGB')
                im.save(path + 'temp' + str(im_num).zfill(2)+ '.pdf')
                im_num += 1
            return im_num

        else:
            plt.figure('main')
            multi.savefig(orientation='landscape')
            for i in fit_params_used:
                print i
                plt.figure(i)
                multi.savefig(orientation='landscape')
            return multi
    return


def make_grid(param1, param2, diagnostic_param, diagnostic_name):
    sparce_grid = [[param1[0], param2[0]]]
    diag_vals = [diagnostic_param[0]]
    if diagnostic_name == 'chi2':
        min_diag = min(diagnostic_param)
        for i in range(len(diagnostic_param)):
            if [param1[i], param2[i]] not in sparce_grid:
                sparce_grid.append([param1[i], param2[i]])
                if 4*min_diag - diagnostic_param[i] < 0:
                    diag_vals.append(0)
                else:
                    diag_vals.append(4*min_diag - diagnostic_param[i])
    else:
        for i in range(len(diagnostic_param)):
            if [param1[i], param2[i]] not in sparce_grid:
                sparce_grid.append([param1[i], param2[i]])
                diag_vals.append(diagnostic_param[i])
    return np.array(sparce_grid).T[0][::-1], np.array(sparce_grid).T[1][::-1], diag_vals[::-1]


def fill_grid(param1, param2, diagnostic_param, param1_range, param2_range):
    if param1_range[2] <= 0 and param2_range[2] <= 0:
        return [diagnostic_param]
    elif param1_range[2] <= 0:
        param2_grid = np.arange(param2_range[0], param2_range[1] + 0.1*param2_range[2], param2_range[2])
        z = np.interp(param2_grid, param2, diagnostic_param)
        return z[None,:]
    elif param2_range[2] <= 0:
        param1_grid = np.arange(param1_range[0], param1_range[1] + 0.1*param1_range[2], param1_range[2])
        z = np.interp(param1_grid, param1, diagnostic_param)
        return z[:,None]
    else:
        param1_grid = np.arange(param1_range[0], param1_range[1] + 0.1*param1_range[2], param1_range[2])
        param2_grid = np.arange(param2_range[0], param2_range[1] + 0.1*param2_range[2], param2_range[2])
        z = griddata((param1, param2), diagnostic_param, (param1_grid[None,:], param2_grid[:,None]), method='linear')
        return z


def plot_correlation_pannel(param1, param2, sorted_indicies, diagnostic_param, fit_diag_param_vals, fig, fig_params, vmin, vmax, cmap, bestModel, xRanges, multi):
    print 'Preparing ' + all_parameter_names_dict[param1] + ' vs. ' + all_parameter_names_dict[param2] + '...'
    fig.subplots_adjust(hspace=0.000001, wspace=0.000001)
    ax = fig.add_subplot(fig_params[0],fig_params[1],fig_params[2])

    p1, p2, dp = make_grid(np.array(fit_diag_param_vals[param1])[sorted_indicies[::-1]], np.array(fit_diag_param_vals[param2])[sorted_indicies[::-1]], np.array(fit_diag_param_vals[diagnostic_param])[sorted_indicies[::-1]], diagnostic_param)
    z = fill_grid(p1, p2, dp, xRanges[param1], xRanges[param2])

    #ax.scatter(np.array(p1), np.array(p2), c = np.array(dp), vmin=vmin, vmax=vmax, cmap = cmap, edgecolors='none')
    ax.imshow(z, extent = [xRanges[param1][0], xRanges[param1][1], xRanges[param2][0], xRanges[param2][1]], aspect = 'auto', origin = 'lower')

    #ax.scatter(np.array(fit_diag_param_vals[param1])[sorted_indicies], np.array(fit_diag_param_vals[param2])[sorted_indicies], c = np.array(fit_diag_param_vals[diagnostic_param])[sorted_indicies], vmin=vmin, vmax=vmax, cmap = cmap, edgecolors='none')
    if fig_params[2]%fig_params[0] == 1:
        ax.set_ylabel(all_parameter_names_dict[param2], fontsize=11)
    else:
        ax.tick_params(labelleft='off')
    if fig_params[2] > (fig_params[0] * (fig_params[1] - 1)):
        ax.set_xlabel(all_parameter_names_dict[param1], fontsize=11)
        plt.xticks(rotation=60)
    else:
        ax.tick_params(labelbottom='off')
    ax.plot(vars(bestModel)[param1], vars(bestModel)[param2], 'x')
    #ax.set_title('Best model: ' + all_parameter_names_dict[fit_param] + ' = ' + str(vars(bestModel)[fit_param]), fontsize=12)
    ax.set_xlim(xRanges[param1][0], xRanges[param1][1])
    ax.set_ylim(xRanges[param2][0], xRanges[param2][1])
    if multi:
        plt.figure(param2, figsize=(20,13))
        plt.suptitle(all_parameter_names_dict[param2] + ' Correlation',fontsize=20)
        plt.subplots_adjust(hspace=0.4)
        ax1 = plt.subplot(fig_params[3], fig_params[4], fit_parameters.index(param1)+1)
        ax1.imshow(z, extent = [xRanges[param1][0], xRanges[param1][1], xRanges[param2][0], xRanges[param2][1]], aspect = 'auto', origin = 'lower')
        ax1.set_ylabel(all_parameter_names_dict[param2], fontsize=11)
        ax1.set_xlabel(all_parameter_names_dict[param1], fontsize=11)
        ax1.plot(vars(bestModel)[param1], vars(bestModel)[param2], 'x')
        ax1.set_xlim(xRanges[param1][0], xRanges[param1][1])
        ax1.set_ylim(xRanges[param2][0], xRanges[param2][1])

        plt.figure(param1, figsize=(20,13))
        plt.suptitle(all_parameter_names_dict[param1] + ' Correlation',fontsize=20)
        plt.subplots_adjust(hspace=0.4)
        ax2 = plt.subplot(fig_params[3], fig_params[4], fit_parameters.index(param2)+1)
        ax2.imshow(z.T, extent = [xRanges[param2][0], xRanges[param2][1], xRanges[param1][0], xRanges[param1][1]], aspect = 'auto', origin = 'lower')
        ax2.set_ylabel(all_parameter_names_dict[param1], fontsize=11)
        ax2.set_xlabel(all_parameter_names_dict[param2], fontsize=11)
        ax2.plot(vars(bestModel)[param2], vars(bestModel)[param1], 'x')
        ax2.set_xlim(xRanges[param2][0], xRanges[param2][1])
        ax2.set_ylim(xRanges[param1][0], xRanges[param1][1])


    #ax.grid(True)



def plot_convergence_all(diagnostic_name, overall = False, multi = False, pdf = False, im_num = False, path = '.'):
    # Concergence plot for each parameter
    if not multi: printHeader()
    print 'Preparing Convergence plots...'
    print
    if pdf or multi: fig = plt.figure(figsize=(20,13))
    else: fig = plt.figure(figsize=(20,10))
    bestModel = models[findBestModel()]
    fileName = maindir + '/' + object +'/pfw_' + object + '.ini'
    xRanges = readIni_new(fileName, parameters = True)

    n_rows = int(ceil(len(fit_parameters)/4.))
    n_cols = 4
    for j,i in enumerate(fit_parameters):
        plot_convergence('fitness', i, overall, xRanges, bestModel, multi = True, fig = fig, fig_params = [n_rows, n_cols, j+1])

    plt.suptitle('Parameter Convergence(' + all_parameter_names_dict[diagnostic_name] + ') by Generation for '+object,fontsize=20)
    if not multi:
        if pdf:
            print 'Making pdf, this may take a minute...'
            fileName = maindir+'/'+object+'/plots/convergence_all_'+object+'_' + all_parameter_names_dict[diagnostic_name]+'.png'
            plt.plot()
            plt.savefig(fileName, orientation='landscape', format='png')
            print
            print 'Plot saved as \''+ fileName +'\''
            raw_input('Press Enter to continue... ')
        else:
            plt.show()
    else:
        if im_num:
            fileName = path + 'temp' + str(im_num).zfill(2)+ '.png'
            plt.savefig(fileName, format = 'png', dpi=300)
            im = Image.open(fileName)
            im = im.convert('RGB')
            im.save(path + 'temp' + str(im_num).zfill(2)+ '.pdf')
            im_num += 1
            return im_num
        else:
            print 'Making Parameters vs. '+ all_parameter_names_dict[diagnostic_name] +' plot (might take a minute)...'
            multi.savefig(orientation='landscape')
            return multi

def plot_convergence(diagnostic_name, fit_param, overall = False, xRanges = False, bestModel = False, multi = False, im_num = False, pdf = False, path = False, fig = False, fig_params = [1,1,1]):
    print 'Preparing convergence ('+all_parameter_names_dict[fit_param]+') plot...'
    #print
    converg_params = {}
    converg_params['generation'] = []
    converg_params[diagnostic_name] = []
    converg_params[fit_param] = []
    for i in range(len(models)):
        converg_params['generation'].append(vars(models[i])['generation'])
        converg_params[diagnostic_name].append(vars(models[i])[diagnostic_name])
        converg_params[fit_param].append(vars(models[i])[fit_param])
    #print max(converg_params['generation'])
    if not fig:
        if pdf or multi: fig = plt.figure(figsize=(20,13))
        else: fig = plt.figure(figsize = (10,5))
        ax = fig.add_subplot(111)
    else:
        fig.subplots_adjust(hspace=0.4)
        ax = fig.add_subplot(fig_params[0],fig_params[1],fig_params[2])
        ax.set_title('Best model: ' + all_parameter_names_dict[fit_param] + ' = ' + str(vars(bestModel)[fit_param]), fontsize=12)
        ax.set_ylim(xRanges[fit_param][0], xRanges[fit_param][1])
    generations = np.arange(1, max(converg_params['generation'])+1, 1)
    diagnostics = []

    if diagnostic_name == 'chi2':
        best_diag = 999999999999999999999999
        best_mod = 0
        for i in generations:
            indecies = [j for j in range(len(converg_params['generation'])) if converg_params['generation'][j] == i]
            gen_params = np.array(converg_params[diagnostic_name])[indecies]
            best_diag_array = [best_diag, min(gen_params)]
            best_diag = min([best_diag, min(gen_params)])
            best_diag_ind = best_diag_array.index(best_diag)
            diagnostic_ind = list(gen_params).index(min(gen_params))
            if overall:
                best_mod = [best_mod, np.array(converg_params[fit_param])[indecies][diagnostic_ind]][best_diag_ind]
                diagnostics.append(best_mod)
            else:
                diagnostics.append(np.array(converg_params[fit_param])[indecies][diagnostic_ind])
    else:
        best_diag = 0
        best_mod = 0
        for i in generations:
            indecies = [j for j in range(len(converg_params['generation'])) if converg_params['generation'][j] == i]
            gen_params = np.array(converg_params[diagnostic_name])[indecies]
            best_diag_array = [best_diag, max(gen_params)]
            best_diag = max([best_diag, max(gen_params)])
            best_diag_ind = best_diag_array.index(best_diag)
            diagnostic_ind = list(gen_params).index(max(gen_params))
            if overall:
                best_mod = [best_mod, np.array(converg_params[fit_param])[indecies][diagnostic_ind]][best_diag_ind]
                diagnostics.append(best_mod)
            else:
                diagnostics.append(np.array(converg_params[fit_param])[indecies][diagnostic_ind])

    ax.plot(generations, diagnostics)
    ax.set_xlabel('Generation', fontsize=11)
    ax.set_ylabel(all_parameter_names_dict[fit_param], fontsize=11)
    if fig_params == [1,1,1]:
        plt.suptitle(all_parameter_names_dict[fit_param] + ' Convergence by Generation',fontsize=20)
    if pdf:
        print 'Making pdf, this may take a minute...'
        fileName = maindir+'/'+object+'/plots/convergence_'+object+'_' + all_parameter_names_dict[fit_param]+'.png'
        plt.plot()
        plt.savefig(fileName, orientation='landscape', format='png')
        print
        print 'Plot saved as \''+ fileName +'\''
        raw_input('Press Enter to continue... ')
    elif im_num:
        fileName = path + 'temp' + str(im_num).zfill(2)+ '.png'
        plt.savefig(fileName, format = 'png', dpi=300)
        im = Image.open(fileName)
        im = im.convert('RGB')
        im.save(path + 'temp' + str(im_num).zfill(2)+ '.pdf')
        im_num += 1
        return im_num
    elif not multi:
        plt.show()




def plotResults_new(diagnostic_name, multi=False, pdf = False, im_num = False, path = '.'):
    #Parameters vs fitness plot
    if not multi: printHeader()
    print 'Preparing parameters vs. '+all_parameter_names_dict[diagnostic_name]+' plot...'
    print
    if pdf or multi: fig = plt.figure(figsize=(20,13))
    else: fig = plt.figure(figsize=(20,10))
    bestModel = models[findBestModel()]
    fileName = maindir + '/' + object +'/pfw_' + object + '.ini'
    xRanges = readIni_new(fileName, parameters = True)
    errorRanges = findErrorRanges_new()
    #    cmap = get_cmap('RdYlGn')
    cmap = get_cmap('gist_rainbow') #colormap
    vmin, vmax = vars(bestModel)[diagnostic_name]/3., vars(bestModel)[diagnostic_name]*2.2  #used to normalize colors, adjust when using other colormap

    fit_diag_param_vals = {}
    fit_diag_params = list(fit_parameters)
    fit_diag_params.extend(diagnostic_parameters)
    fit_diag_params.extend([lines[i].ID for i in range(len(lines))])

    for i in fit_diag_params:
        fit_diag_param_vals[i] = []

    for i in range(len(models)):
        for j in fit_diag_params:
            fit_diag_param_vals[j].append(vars(models[i])[j])

    n_rows = int(ceil(len(fit_parameters)/4.))
    n_cols = 4
    for j,i in enumerate(fit_parameters):
        plot_results_pannel(i, diagnostic_name, fit_diag_param_vals, fig, [n_rows, n_cols, j+1], errorRanges[i], vmin, vmax, cmap, bestModel, xRanges)

    print
    plt.suptitle('Parameters vs '+all_parameter_names_dict[diagnostic_name]+' for '+object,fontsize=20)
    if not multi:
        if pdf:
            print 'Making pdf, this may take a minute...'
            fileName = maindir+'/'+object+'/plots/parameters_'+object+'_' + all_parameter_names_dict[diagnostic_name]+'.pdf'
            plt.plot()
            plt.savefig(fileName, orientation='landscape', format='pdf')
            print
            print 'Plot saved as \''+ fileName +'\''
            raw_input('Press Enter to continue... ')
        else:
            plt.show()
    else:
        if im_num:
            fileName = path + 'temp' + str(im_num).zfill(2)+ '.png'
            plt.savefig(fileName, format = 'png', dpi=300)
            im = Image.open(fileName)
            im = im.convert('RGB')
            im.save(path + 'temp' + str(im_num).zfill(2)+ '.pdf')
            im_num += 1
            return im_num
        else:
            print 'Making Parameters vs. '+ all_parameter_names_dict[diagnostic_name] +' plot (might take a minute)...'
            multi.savefig(orientation='landscape')
            return multi
    return


def plot_results_pannel(fit_param, diagnostic_param, fit_diag_param_vals, fig, fig_params, errorRanges, vmin, vmax, cmap, bestModel, xRanges):
    print 'Preparing ' + all_parameter_names_dict[fit_param] + ' vs. ' + all_parameter_names_dict[diagnostic_param] + '...'
    fig.subplots_adjust(hspace=0.4)
    ax = fig.add_subplot(fig_params[0],fig_params[1],fig_params[2])
    ax.fill([errorRanges[0], errorRanges[1], errorRanges[1], errorRanges[0]], [0, 0, vars(bestModel)[diagnostic_param] * 1.2, vars(bestModel)[diagnostic_param] * 1.2], 'b', alpha=0.1)
    ax.scatter(fit_diag_param_vals[fit_param], fit_diag_param_vals[diagnostic_param], c = fit_diag_param_vals[diagnostic_param], vmin=vmin, vmax=vmax, cmap = cmap)
    ax.set_xlabel(all_parameter_names_dict[fit_param], fontsize=11)
    ax.set_ylabel(all_parameter_names_dict[diagnostic_param], fontsize=11)
    ax.set_title('Best model: ' + all_parameter_names_dict[fit_param] + ' = ' + str(vars(bestModel)[fit_param]), fontsize=12)
    ax.axvline(x=errorRanges[0], c = 'r')
    ax.axvline(x=errorRanges[1], c = 'r')
    ax.axvline(x=vars(bestModel)[fit_param], c = 'black', alpha=0.8, linewidth=1.5)
    ax.set_xlim(xRanges[fit_param][0], xRanges[fit_param][1])
    ax.set_ylim(0, vars(bestModel)[diagnostic_param] * 2)
    ax.grid(True)



def plotResults(multi=False, pdf = False):
    #Parameters vs fitness plot
    if not multi: printHeader()
    print 'Preparing parameters vs. fitness plot...'
    print
    if pdf or multi: fig = plt.figure(figsize=(20,13))
    else: fig = plt.figure(figsize=(20,10))
    bestModel = models[findBestModel()]
    fileName = maindir + '/' + object +'/pfw_' + object + '.ini'
    xRanges = readIni(fileName, parameters = True)
    errorRanges = findErrorRanges()
    #    cmap = get_cmap('RdYlGn')
    cmap = get_cmap('gist_rainbow') #colormap
    vmin, vmax = bestModel.fitness/3., bestModel.fitness*2.2  #used to normalize colors, adjust when using other colormap
    Teff, logg, mdot, vinf, beta, NHe, vtur, vrot, fitness, vmacro, nitrogen, carbon, oxygen, phosphorus, silicon = [], [], [], [], [], [], [], [], [], [], [], [], [], [], []
    for i in range(len(models)):
        Teff.append(models[i].Teff)
        logg.append(models[i].logg)
        mdot.append(models[i].mdot)
        vinf.append(models[i].vinf)
        beta.append(models[i].beta)
        NHe.append(models[i].NHe)
        vtur.append(models[i].vtur)
        vrot.append(models[i].vrot)
        fitness.append(models[i].fitness)
        vmacro.append(models[i].vmacro)
#        if models[i].nitrogen:
        nitrogen.append(models[i].nitrogen)
        carbon.append(models[i].carbon)
        oxygen.append(models[i].oxygen)
        phosphorus.append(models[i].phosphorus)
        silicon.append(models[i].silicon)
    print 'Preparing Teff vs. fitness plot...'
    fig.subplots_adjust(hspace=0.4)
    ax = fig.add_subplot(341)
    ax.fill([errorRanges[0][0], errorRanges[0][1], errorRanges[0][1], errorRanges[0][0]], [0, 0, bestModel.fitness * 1.2, bestModel.fitness * 1.2], 'b', alpha=0.1)
    ax.scatter(Teff, fitness, c = fitness, vmin=vmin, vmax=vmax, cmap = cmap)
    ax.set_xlabel('Teff', fontsize=11)
    ax.set_ylabel('Fitness', fontsize=11)
    ax.set_title('Best model: Teff = ' + str(bestModel.Teff), fontsize=12)
    ax.axvline(x=errorRanges[0][0], c = 'r')
    ax.axvline(x=errorRanges[0][1], c = 'r')
    ax.axvline(x=bestModel.Teff, c = 'black', alpha=0.8, linewidth=1.5)
    ax.set_xlim(xRanges[0][0], xRanges[0][1])
    ax.set_ylim(0, bestModel.fitness * 2)
    ax.grid(True)
    print 'Preparing log g vs. fitness plot...'
    fig.subplots_adjust(hspace=0.4)
    ax = fig.add_subplot(342)
    ax.scatter(logg, fitness, c = fitness, vmin=vmin, vmax=vmax, cmap = cmap)
    ax.set_xlabel('log g', fontsize=11)
    ax.set_ylabel('Fitness', fontsize=11)
    ax.set_title('Best model: log g = ' + str(bestModel.logg), fontsize=12)
    ax.axvline(x=errorRanges[1][0], c = 'r')
    ax.axvline(x=errorRanges[1][1], c = 'r')
    ax.axvline(x=bestModel.logg, c = 'black', alpha=0.8, linewidth=1.5)
    ax.set_xlim(xRanges[1][0], xRanges[1][1])
    ax.set_ylim(0, bestModel.fitness * 2)
    ax.fill([errorRanges[1][0], errorRanges[1][1], errorRanges[1][1], errorRanges[1][0]], [0, 0, bestModel.fitness * 2, bestModel.fitness * 2], 'b', alpha=0.1)
    ax.grid(True)
    print 'Preparing log Mdot vs. fitness plot...'
    fig.subplots_adjust(hspace=0.4)
    ax = fig.add_subplot(343)
    ax.scatter(mdot, fitness, c = fitness, vmin=vmin, vmax=vmax, cmap = cmap)
    ax.set_xlabel('log Mdot', fontsize=11)
    ax.set_ylabel('Fitness', fontsize=11)
    ax.set_title('Best model: log Mdot = ' + str(bestModel.mdot), fontsize=12)
    ax.axvline(x=errorRanges[2][0], c = 'r')
    ax.axvline(x=errorRanges[2][1], c = 'r')
    ax.axvline(x=bestModel.mdot, c = 'black', alpha=0.8, linewidth=1.5)
    ax.set_xlim(xRanges[2][0], xRanges[2][1])
    ax.set_ylim(0, bestModel.fitness * 2)
    ax.fill([errorRanges[2][0], errorRanges[2][1], errorRanges[2][1], errorRanges[2][0]], [0, 0, bestModel.fitness * 2, bestModel.fitness * 2], 'b', alpha=0.1)
    ax.grid(True)
    print 'Preparing beta vs. fitness plot...'
    fig.subplots_adjust(hspace=0.4)
    ax = fig.add_subplot(344)
    ax.scatter(beta, fitness, c = fitness, vmin=vmin, vmax=vmax, cmap = cmap)
    ax.set_xlabel('beta', fontsize=11)
    ax.set_ylabel('Fitness', fontsize=11)
    ax.set_title('Best model: beta = ' + str(bestModel.beta), fontsize=12)
    ax.axvline(x=errorRanges[4][0], c = 'r')
    ax.axvline(x=errorRanges[4][1], c = 'r')
    ax.axvline(x=bestModel.beta, c = 'black', alpha=0.8, linewidth=1.5)
    ax.set_xlim(xRanges[4][0], xRanges[4][1])
    ax.set_ylim(0, bestModel.fitness * 2)
    ax.fill([errorRanges[4][0], errorRanges[4][1], errorRanges[4][1], errorRanges[4][0]], [0, 0, bestModel.fitness * 2, bestModel.fitness * 2], 'b', alpha=0.1)
    ax.grid(True)
    print 'Preparing vinf vs. fitness plot...'
    fig.subplots_adjust(hspace=0.4)
    ax = fig.add_subplot(345)
    ax.scatter(vinf, fitness, c = fitness, vmin=vmin, vmax=vmax, cmap = cmap)
    ax.set_xlabel('vinf', fontsize=11)
    ax.set_ylabel('Fitness', fontsize=11)
    ax.set_title('Best model: vinf = ' + str(bestModel.vinf), fontsize=12)
    ax.axvline(x=errorRanges[3][0], c = 'r')
    ax.axvline(x=errorRanges[3][1], c = 'r')
    ax.axvline(x=bestModel.vinf, c = 'black', alpha=0.8, linewidth=1.5)
    ax.set_xlim(xRanges[3][0], xRanges[3][1])
    ax.set_ylim(0, bestModel.fitness * 2)
    ax.fill([errorRanges[3][0], errorRanges[3][1], errorRanges[3][1], errorRanges[3][0]], [0, 0, bestModel.fitness * 2, bestModel.fitness * 2], 'b', alpha=0.1)
    ax.grid(True)
    print 'Preparing nHe/nH vs. fitness plot...'
    fig.subplots_adjust(hspace=0.4)
    ax = fig.add_subplot(346)
    ax.scatter(NHe, fitness, c = fitness, vmin=vmin, vmax=vmax, cmap = cmap)
    ax.set_xlabel('nHe/nH', fontsize=11)
    ax.set_ylabel('Fitness', fontsize=11)
    ax.set_title('Best model: nHe/nH = ' + str(bestModel.NHe), fontsize=12)
    ax.axvline(x=errorRanges[5][0], c = 'r')
    ax.axvline(x=errorRanges[5][1], c = 'r')
    ax.axvline(x=bestModel.NHe, c = 'black', alpha=0.8, linewidth=1.5)
    ax.set_xlim(xRanges[5][0], xRanges[5][1])
    ax.set_ylim(0, bestModel.fitness * 2)
    ax.fill([errorRanges[5][0], errorRanges[5][1], errorRanges[5][1], errorRanges[5][0]], [0, 0, bestModel.fitness * 2, bestModel.fitness * 2], 'b', alpha=0.1)
    ax.grid(True)
    print 'Preparing v_tur vs. fitness plot...'
    fig.subplots_adjust(hspace=0.4)
    ax = fig.add_subplot(347)
    ax.scatter(vtur, fitness, c = fitness, vmin=vmin, vmax=vmax, cmap = cmap)
    ax.set_xlabel('v_tur', fontsize=11)
    ax.set_ylabel('Fitness', fontsize=11)
    ax.set_title('Best model: v_tur = ' + str(bestModel.vtur), fontsize=12)
    ax.axvline(x=errorRanges[6][0], c = 'r')
    ax.axvline(x=errorRanges[6][1], c = 'r')
    ax.axvline(x=bestModel.vtur, c = 'black', alpha=0.8, linewidth=1.5)
    ax.set_xlim(xRanges[6][0], xRanges[6][1])
    ax.set_ylim(0, bestModel.fitness * 2)
    ax.fill([errorRanges[6][0], errorRanges[6][1], errorRanges[6][1], errorRanges[6][0]], [0, 0, bestModel.fitness * 2, bestModel.fitness * 2], 'b', alpha=0.1)
    ax.grid(True)
    print 'Preparing v_rot vs. fitness plot...'
    fig.subplots_adjust(hspace=0.4)
    ax = fig.add_subplot(348)
    ax.scatter(vrot, fitness, c = fitness, vmin=vmin, vmax=vmax, cmap = cmap)
    ax.set_xlabel('v sin(i)', fontsize=11)
    ax.set_ylabel('Fitness', fontsize=11)
    ax.set_title('Best model: v sin(i) = ' + str(bestModel.vrot), fontsize=12)
    ax.axvline(x=errorRanges[7][0], c = 'r')
    ax.axvline(x=errorRanges[7][1], c = 'r')
    ax.axvline(x=bestModel.vrot, c = 'black', alpha=0.8, linewidth=1.5)
    ax.set_xlim(xRanges[7][0], xRanges[7][1])
    ax.set_ylim(0, bestModel.fitness * 2)
    ax.fill([errorRanges[7][0], errorRanges[7][1], errorRanges[7][1], errorRanges[7][0]], [0, 0, bestModel.fitness * 2, bestModel.fitness * 2], 'b', alpha=0.1)
    ax.grid(True)
    if not (vmacro[0]) == -1:
        print 'Preparing v_macro vs. fitness plot...'
        fig.subplots_adjust(hspace=0.4)
        ax = fig.add_subplot(3,4,13)
        ax.scatter(vmacro, fitness, c = fitness, vmin=vmin, vmax=vmax, cmap = cmap)
        ax.set_xlabel('v_macro', fontsize=11)
        ax.set_ylabel('Fitness', fontsize=11)
        ax.set_title('Best model: v_macro = ' + str(bestModel.vmacro), fontsize=12)
        ax.axvline(x=errorRanges[8][0], c = 'r')
        ax.axvline(x=errorRanges[8][1], c = 'r')
        ax.axvline(x=bestModel.vmacro, c = 'black', alpha=0.8, linewidth=1.5)
        ax.set_xlim(xRanges[8][0], xRanges[8][1])
        ax.set_ylim(0, bestModel.fitness * 2)
        ax.fill([errorRanges[8][0], errorRanges[8][1], errorRanges[8][1], errorRanges[8][0]], [0, 0, bestModel.fitness * 2, bestModel.fitness * 2], 'b', alpha=0.1)
        ax.grid(True)
    if not (len(nitrogen)) == 0:
        print 'Preparing nitrogen vs. fitness plot...'
        fig.subplots_adjust(hspace=0.4)
        ax = fig.add_subplot(349)
        ax.scatter(nitrogen, fitness, c = fitness, vmin=vmin, vmax=vmax, cmap = cmap)
        ax.set_xlabel('Nitrogen', fontsize=11)
        ax.set_ylabel('Fitness', fontsize=11)
        ax.set_title('Best model: Nitrogen = ' + str(bestModel.nitrogen), fontsize=12)
        ax.axvline(x=errorRanges[9][0], c = 'r')
        ax.axvline(x=errorRanges[9][1], c = 'r')
        ax.axvline(x=bestModel.nitrogen, c = 'black', alpha=0.8, linewidth=1.5)
        ax.set_xlim(xRanges[9][0], xRanges[9][1])
        ax.set_ylim(0, bestModel.fitness * 2)
        ax.fill([errorRanges[9][0], errorRanges[9][1], errorRanges[9][1], errorRanges[9][0]], [0, 0, bestModel.fitness * 2, bestModel.fitness * 2], 'b', alpha=0.1)
        ax.grid(True)

    if not (len(carbon)) == 0:
        print 'Preparing carbon vs. fitness plot...'
        fig.subplots_adjust(hspace=0.4)
        ax = fig.add_subplot(3,4,10)
        ax.scatter(carbon, fitness, c = fitness, vmin=vmin, vmax=vmax, cmap = cmap)
        ax.set_xlabel('Carbon', fontsize=11)
        ax.set_ylabel('Fitness', fontsize=11)
        ax.set_title('Best model: Carbon = ' + str(bestModel.carbon), fontsize=12)
        ax.axvline(x=errorRanges[10][0], c = 'r')
        ax.axvline(x=errorRanges[10][1], c = 'r')
        ax.axvline(x=bestModel.carbon, c = 'black', alpha=0.8, linewidth=1.5)
        ax.set_xlim(xRanges[10][0], xRanges[10][1])
        ax.set_ylim(0, bestModel.fitness * 2)
        ax.fill([errorRanges[10][0], errorRanges[10][1], errorRanges[10][1], errorRanges[10][0]], [0, 0, bestModel.fitness * 2, bestModel.fitness * 2], 'b', alpha=0.1)
        ax.grid(True)

    if not (len(oxygen)) == 0:
        print 'Preparing oxygen vs. fitness plot...'
        fig.subplots_adjust(hspace=0.4)
        ax = fig.add_subplot(3,4,11)
        ax.scatter(oxygen, fitness, c = fitness, vmin=vmin, vmax=vmax, cmap = cmap)
        ax.set_xlabel('Oxygen', fontsize=11)
        ax.set_ylabel('Fitness', fontsize=11)
        ax.set_title('Best model: Oxygen = ' + str(bestModel.oxygen), fontsize=12)
        ax.axvline(x=errorRanges[11][0], c = 'r')
        ax.axvline(x=errorRanges[11][1], c = 'r')
        ax.axvline(x=bestModel.oxygen, c = 'black', alpha=0.8, linewidth=1.5)
        ax.set_xlim(xRanges[11][0], xRanges[11][1])
        ax.set_ylim(0, bestModel.fitness * 2)
        ax.fill([errorRanges[11][0], errorRanges[11][1], errorRanges[11][1], errorRanges[11][0]], [0, 0, bestModel.fitness * 2, bestModel.fitness * 2], 'b', alpha=0.1)
        ax.grid(True)



#    if not (len(phosphorus)) == 0:
#        print 'Preparing phosphorus vs. fitness plot...'
#        fig.subplots_adjust(hspace=0.4)
#        ax = fig.add_subplot(3,4,13)
#        ax.scatter(phosphorus, fitness, c = fitness, vmin=vmin, vmax=vmax, cmap = cmap)
#        ax.set_xlabel('Phosphorus', fontsize=11)
#        ax.set_ylabel('Fitness', fontsize=11)
#        ax.set_title('Best model: Phosphorus = ' + str(bestModel.phosphorus), fontsize=12)
#        ax.axvline(x=errorRanges[12][0], c = 'r')
#        ax.axvline(x=errorRanges[12][1], c = 'r')
#        ax.axvline(x=bestModel.phosphorus, c = 'black', alpha=0.8, linewidth=1.5)
#        ax.set_xlim(xRanges[12][0], xRanges[12][1])
#        ax.set_ylim(0, bestModel.fitness * 2)
#        ax.fill([errorRanges[12][0], errorRanges[12][1], errorRanges[12][1], errorRanges[12][0]], [0, 0, bestModel.fitness * 2, bestModel.fitness * 2], 'b', alpha=0.1)
#        ax.grid(True)


    if not (len(silicon)) == 0:
        print 'Preparing silicon vs. fitness plot...'
        fig.subplots_adjust(hspace=0.4)
        ax = fig.add_subplot(3,4,12)
        ax.scatter(silicon, fitness, c = fitness, vmin=vmin, vmax=vmax, cmap = cmap)
        ax.set_xlabel('Silicon', fontsize=11)
        ax.set_ylabel('Fitness', fontsize=11)
        ax.set_title('Best model: Silicon = ' + str(bestModel.silicon), fontsize=12)
        ax.axvline(x=errorRanges[13][0], c = 'r')
        ax.axvline(x=errorRanges[13][1], c = 'r')
        ax.axvline(x=bestModel.silicon, c = 'black', alpha=0.8, linewidth=1.5)
        ax.set_xlim(xRanges[13][0], xRanges[13][1])
        ax.set_ylim(0, bestModel.fitness * 2)
        ax.fill([errorRanges[13][0], errorRanges[13][1], errorRanges[13][1], errorRanges[13][0]], [0, 0, bestModel.fitness * 2, bestModel.fitness * 2], 'b', alpha=0.1)
        ax.grid(True)

    print
    plt.suptitle('Parameters vs Fitness for '+object,fontsize=20)
    if not multi:
        if pdf:
            print 'Making pdf, this may take a minute...'
            fileName = maindir+'/'+object+'/plots/parameters_'+object+'.pdf'
            plt.plot()
            plt.savefig(fileName, orientation='landscape', format='pdf')
            print
            print 'Plot saved as \''+ fileName +'\''
            raw_input('Press Enter to continue... ')
        else:
            plt.show()
    else:
        print 'Making Parameters vs. fitness plot (might take a minute)...'
        multi.savefig(orientation='landscape')
        return multi
    return


def plotResultsChi2(multi=False, pdf = False):
    #Parameters vs chi2 plot
    if not multi: printHeader()
    print 'Preparing parameters vs. chi2 plot...'
    print
    if pdf or multi: fig = plt.figure(figsize=(20,13))
    else: fig = plt.figure(figsize=(20,10))
    bestModel = models[findBestModel()]
    fileName = maindir + '/' + object +'/pfw_' + object + '.ini'
    xRanges = readIni(fileName, parameters = True)
    errorRanges = findErrorRanges()
    #    cmap = get_cmap('RdYlGn')
    cmap = get_cmap('gist_rainbow') #colormap
    vmin, vmax = bestModel.chi2/3., bestModel.chi2*2.2  #used to normalize colors, adjust when using other colormap
    Teff, logg, mdot, vinf, beta, NHe, vtur, vrot, chi2, vmacro, nitrogen, carbon,oxygen, phosphorus, silicon = [], [], [], [], [], [], [], [], [], [], [], [], [], [], []
    for i in range(len(models)):
        Teff.append(models[i].Teff)
        logg.append(models[i].logg)
        mdot.append(models[i].mdot)
        vinf.append(models[i].vinf)
        beta.append(models[i].beta)
        NHe.append(models[i].NHe)
        vtur.append(models[i].vtur)
        vrot.append(models[i].vrot)
        chi2.append(models[i].chi2)
        vmacro.append(models[i].vmacro)
#        if models[i].nitrogen:
        nitrogen.append(models[i].nitrogen)
        carbon.append(models[i].carbon)
        oxygen.append(models[i].oxygen)
        phosphorus.append(models[i].phosphorus)
        silicon.append(models[i].silicon)

    print 'Preparing Teff vs. chi2 plot...'
    fig.subplots_adjust(hspace=0.4)
    ax = fig.add_subplot(341)
    ax.fill([errorRanges[0][0], errorRanges[0][1], errorRanges[0][1], errorRanges[0][0]], [0, 0, bestModel.chi2 * 1.2, bestModel.chi2 * 1.2], 'b', alpha=0.1)
    ax.scatter(Teff, chi2, c = chi2, vmin=vmin, vmax=vmax, cmap = cmap)
    ax.set_xlabel('Teff', fontsize=11)
    ax.set_ylabel('Chi2', fontsize=11)
    ax.set_title('Best model: Teff = ' + str(bestModel.Teff), fontsize=12)
    ax.axvline(x=errorRanges[0][0], c = 'r')
    ax.axvline(x=errorRanges[0][1], c = 'r')
    ax.axvline(x=bestModel.Teff, c = 'black', alpha=0.8, linewidth=1.5)
    ax.set_xlim(xRanges[0][0], xRanges[0][1])
    ax.set_ylim(0, bestModel.chi2 * 2)
    ax.grid(True)
    print 'Preparing log g vs. chi2 plot...'
    fig.subplots_adjust(hspace=0.4)
    ax = fig.add_subplot(342)
    ax.scatter(logg, chi2, c = chi2, vmin=vmin, vmax=vmax, cmap = cmap)
    ax.set_xlabel('log g', fontsize=11)
    ax.set_ylabel('Chi2', fontsize=11)
    ax.set_title('Best model: log g = ' + str(bestModel.logg), fontsize=12)
    ax.axvline(x=errorRanges[1][0], c = 'r')
    ax.axvline(x=errorRanges[1][1], c = 'r')
    ax.axvline(x=bestModel.logg, c = 'black', alpha=0.8, linewidth=1.5)
    ax.set_xlim(xRanges[1][0], xRanges[1][1])
    ax.set_ylim(0, bestModel.chi2 * 2)
    ax.fill([errorRanges[1][0], errorRanges[1][1], errorRanges[1][1], errorRanges[1][0]], [0, 0, bestModel.chi2 * 2, bestModel.chi2 * 2], 'b', alpha=0.1)
    ax.grid(True)
    print 'Preparing log Mdot vs. chi2 plot...'
    fig.subplots_adjust(hspace=0.4)
    ax = fig.add_subplot(343)
    ax.scatter(mdot, chi2, c = chi2, vmin=vmin, vmax=vmax, cmap = cmap)
    ax.set_xlabel('log Mdot', fontsize=11)
    ax.set_ylabel('Chi2', fontsize=11)
    ax.set_title('Best model: log Mdot = ' + str(bestModel.mdot), fontsize=12)
    ax.axvline(x=errorRanges[2][0], c = 'r')
    ax.axvline(x=errorRanges[2][1], c = 'r')
    ax.axvline(x=bestModel.mdot, c = 'black', alpha=0.8, linewidth=1.5)
    ax.set_xlim(xRanges[2][0], xRanges[2][1])
    ax.set_ylim(0, bestModel.chi2 * 2)
    ax.fill([errorRanges[2][0], errorRanges[2][1], errorRanges[2][1], errorRanges[2][0]], [0, 0, bestModel.chi2 * 2, bestModel.chi2 * 2], 'b', alpha=0.1)
    ax.grid(True)
    print 'Preparing beta vs. chi2 plot...'
    fig.subplots_adjust(hspace=0.4)
    ax = fig.add_subplot(344)
    ax.scatter(beta, chi2, c = chi2, vmin=vmin, vmax=vmax, cmap = cmap)
    ax.set_xlabel('beta', fontsize=11)
    ax.set_ylabel('Chi2', fontsize=11)
    ax.set_title('Best model: beta = ' + str(bestModel.beta), fontsize=12)
    ax.axvline(x=errorRanges[4][0], c = 'r')
    ax.axvline(x=errorRanges[4][1], c = 'r')
    ax.axvline(x=bestModel.beta, c = 'black', alpha=0.8, linewidth=1.5)
    ax.set_xlim(xRanges[4][0], xRanges[4][1])
    ax.set_ylim(0, bestModel.chi2 * 2)
    ax.fill([errorRanges[4][0], errorRanges[4][1], errorRanges[4][1], errorRanges[4][0]], [0, 0, bestModel.chi2 * 2, bestModel.chi2 * 2], 'b', alpha=0.1)
    ax.grid(True)
    print 'Preparing vinf vs. chi2 plot...'
    fig.subplots_adjust(hspace=0.4)
    ax = fig.add_subplot(345)
    ax.scatter(vinf, chi2, c = chi2, vmin=vmin, vmax=vmax, cmap = cmap)
    ax.set_xlabel('vinf', fontsize=11)
    ax.set_ylabel('Chi2', fontsize=11)
    ax.set_title('Best model: vinf = ' + str(bestModel.vinf), fontsize=12)
    ax.axvline(x=errorRanges[3][0], c = 'r')
    ax.axvline(x=errorRanges[3][1], c = 'r')
    ax.axvline(x=bestModel.vinf, c = 'black', alpha=0.8, linewidth=1.5)
    ax.set_xlim(xRanges[3][0], xRanges[3][1])
    ax.set_ylim(0, bestModel.chi2 * 2)
    ax.fill([errorRanges[3][0], errorRanges[3][1], errorRanges[3][1], errorRanges[3][0]], [0, 0, bestModel.chi2 * 2, bestModel.chi2 * 2], 'b', alpha=0.1)
    ax.grid(True)
    print 'Preparing nHe/nH vs. chi2 plot...'
    fig.subplots_adjust(hspace=0.4)
    ax = fig.add_subplot(346)
    ax.scatter(NHe, chi2, c = chi2, vmin=vmin, vmax=vmax, cmap = cmap)
    ax.set_xlabel('nHe/nH', fontsize=11)
    ax.set_ylabel('Chi2', fontsize=11)
    ax.set_title('Best model: nHe/nH = ' + str(bestModel.NHe), fontsize=12)
    ax.axvline(x=errorRanges[5][0], c = 'r')
    ax.axvline(x=errorRanges[5][1], c = 'r')
    ax.axvline(x=bestModel.NHe, c = 'black', alpha=0.8, linewidth=1.5)
    ax.set_xlim(xRanges[5][0], xRanges[5][1])
    ax.set_ylim(0, bestModel.chi2 * 2)
    ax.fill([errorRanges[5][0], errorRanges[5][1], errorRanges[5][1], errorRanges[5][0]], [0, 0, bestModel.chi2 * 2, bestModel.chi2 * 2], 'b', alpha=0.1)
    ax.grid(True)
    print 'Preparing v_tur vs. chi2 plot...'
    fig.subplots_adjust(hspace=0.4)
    ax = fig.add_subplot(347)
    ax.scatter(vtur, chi2, c = chi2, vmin=vmin, vmax=vmax, cmap = cmap)
    ax.set_xlabel('v_tur', fontsize=11)
    ax.set_ylabel('Chi2', fontsize=11)
    ax.set_title('Best model: v_tur = ' + str(bestModel.vtur), fontsize=12)
    ax.axvline(x=errorRanges[6][0], c = 'r')
    ax.axvline(x=errorRanges[6][1], c = 'r')
    ax.axvline(x=bestModel.vtur, c = 'black', alpha=0.8, linewidth=1.5)
    ax.set_xlim(xRanges[6][0], xRanges[6][1])
    ax.set_ylim(0, bestModel.chi2 * 2)
    ax.fill([errorRanges[6][0], errorRanges[6][1], errorRanges[6][1], errorRanges[6][0]], [0, 0, bestModel.chi2 * 2, bestModel.chi2 * 2], 'b', alpha=0.1)
    ax.grid(True)
    print 'Preparing v_rot vs. chi2 plot...'
    fig.subplots_adjust(hspace=0.4)
    ax = fig.add_subplot(348)
    ax.scatter(vrot, chi2, c = chi2, vmin=vmin, vmax=vmax, cmap = cmap)
    ax.set_xlabel('v sin(i)', fontsize=11)
    ax.set_ylabel('Chi2', fontsize=11)
    ax.set_title('Best model: v sin(i) = ' + str(bestModel.vrot), fontsize=12)
    ax.axvline(x=errorRanges[7][0], c = 'r')
    ax.axvline(x=errorRanges[7][1], c = 'r')
    ax.axvline(x=bestModel.vrot, c = 'black', alpha=0.8, linewidth=1.5)
    ax.set_xlim(xRanges[7][0], xRanges[7][1])
    ax.set_ylim(0, bestModel.chi2 * 2)
    ax.fill([errorRanges[7][0], errorRanges[7][1], errorRanges[7][1], errorRanges[7][0]], [0, 0, bestModel.chi2 * 2, bestModel.chi2 * 2], 'b', alpha=0.1)
    ax.grid(True)
    if not (vmacro[0]) == -1:
        print 'Preparing v_macro vs. chi2 plot...'
        fig.subplots_adjust(hspace=0.4)
        ax = fig.add_subplot(3,4,13)
        ax.scatter(vmacro, chi2, c = chi2, vmin=vmin, vmax=vmax, cmap = cmap)
        ax.set_xlabel('v_macro', fontsize=11)
        ax.set_ylabel('Chi2', fontsize=11)
        ax.set_title('Best model: v_macro = ' + str(bestModel.vmacro), fontsize=12)
        ax.axvline(x=errorRanges[8][0], c = 'r')
        ax.axvline(x=errorRanges[8][1], c = 'r')
        ax.axvline(x=bestModel.vmacro, c = 'black', alpha=0.8, linewidth=1.5)
        ax.set_xlim(xRanges[8][0], xRanges[8][1])
        ax.set_ylim(0, bestModel.chi2 * 2)
        ax.fill([errorRanges[8][0], errorRanges[8][1], errorRanges[8][1], errorRanges[8][0]], [0, 0, bestModel.chi2 * 2, bestModel.chi2 * 2], 'b', alpha=0.1)
        ax.grid(True)
    if not (len(nitrogen)) == 0:
        print 'Preparing nitrogen vs. chi2 plot...'
        fig.subplots_adjust(hspace=0.4)
        ax = fig.add_subplot(349)
        ax.scatter(nitrogen, chi2, c = chi2, vmin=vmin, vmax=vmax, cmap = cmap)
        ax.set_xlabel('Nitrogen', fontsize=11)
        ax.set_ylabel('Chi2', fontsize=11)
        ax.set_title('Best model: Nitrogen = ' + str(bestModel.nitrogen), fontsize=12)
        ax.axvline(x=errorRanges[9][0], c = 'r')
        ax.axvline(x=errorRanges[9][1], c = 'r')
        ax.axvline(x=bestModel.nitrogen, c = 'black', alpha=0.8, linewidth=1.5)
        ax.set_xlim(xRanges[9][0], xRanges[9][1])
        ax.set_ylim(0, bestModel.chi2 * 2)
        ax.fill([errorRanges[9][0], errorRanges[9][1], errorRanges[9][1], errorRanges[9][0]], [0, 0, bestModel.chi2 * 2, bestModel.chi2 * 2], 'b', alpha=0.1)
        ax.grid(True)

    if not (len(carbon)) == 0:
        print 'Preparing carbon vs. chi2 plot...'
        fig.subplots_adjust(hspace=0.4)
        ax = fig.add_subplot(3,4,10)
        ax.scatter(carbon, chi2, c = chi2, vmin=vmin, vmax=vmax, cmap = cmap)
        ax.set_xlabel('Carbon', fontsize=11)
        ax.set_ylabel('Chi2', fontsize=11)
        ax.set_title('Best model: Carbon = ' + str(bestModel.carbon), fontsize=12)
        ax.axvline(x=errorRanges[10][0], c = 'r')
        ax.axvline(x=errorRanges[10][1], c = 'r')
        ax.axvline(x=bestModel.carbon, c = 'black', alpha=0.8, linewidth=1.5)
        ax.set_xlim(xRanges[10][0], xRanges[10][1])
        ax.set_ylim(0, bestModel.chi2 * 2)
        ax.fill([errorRanges[10][0], errorRanges[10][1], errorRanges[10][1], errorRanges[10][0]], [0, 0, bestModel.chi2 * 2, bestModel.chi2 * 2], 'b', alpha=0.1)
        ax.grid(True)

    if not (len(oxygen)) == 0:
        print 'Preparing oxygen vs. chi2 plot...'
        fig.subplots_adjust(hspace=0.4)
        ax = fig.add_subplot(3,4,11)
        ax.scatter(oxygen, chi2, c = chi2, vmin=vmin, vmax=vmax, cmap = cmap)
        ax.set_xlabel('Oxygen', fontsize=11)
        ax.set_ylabel('Chi2', fontsize=11)
        ax.set_title('Best model: Oxygen = ' + str(bestModel.oxygen), fontsize=12)
        ax.axvline(x=errorRanges[11][0], c = 'r')
        ax.axvline(x=errorRanges[11][1], c = 'r')
        ax.axvline(x=bestModel.oxygen, c = 'black', alpha=0.8, linewidth=1.5)
        ax.set_xlim(xRanges[11][0], xRanges[11][1])
        ax.set_ylim(0, bestModel.chi2 * 2)
        ax.fill([errorRanges[11][0], errorRanges[11][1], errorRanges[11][1], errorRanges[11][0]], [0, 0, bestModel.chi2 * 2, bestModel.chi2 * 2], 'b', alpha=0.1)
        ax.grid(True)



#    if not (len(phosphorus)) == 0:
#        print 'Preparing phosphorus vs. chi2 plot...'
#        fig.subplots_adjust(hspace=0.4)
#        ax = fig.add_subplot(3,4,13)
#        ax.scatter(phosphorus, chi2, c = chi2, vmin=vmin, vmax=vmax, cmap = cmap)
#        ax.set_xlabel('Phosphorus', fontsize=11)
#        ax.set_ylabel('Chi2', fontsize=11)
#        ax.set_title('Best model: Phosphorus = ' + str(bestModel.phosphorus), fontsize=12)
#        ax.axvline(x=errorRanges[12][0], c = 'r')
#        ax.axvline(x=errorRanges[12][1], c = 'r')
#        ax.axvline(x=bestModel.phosphorus, c = 'black', alpha=0.8, linewidth=1.5)
#        ax.set_xlim(xRanges[12][0], xRanges[12][1])
#        ax.set_ylim(0, bestModel.chi2 * 2)
#        ax.fill([errorRanges[12][0], errorRanges[12][1], errorRanges[12][1], errorRanges[12][0]], [0, 0, bestModel.chi2 * 2, bestModel.chi2 * 2], 'b', alpha=0.1)
#        ax.grid(True)


    if not (len(silicon)) == 0:
        print 'Preparing silicon vs. chi2 plot...'
        fig.subplots_adjust(hspace=0.4)
        ax = fig.add_subplot(3,4,12)
        ax.scatter(silicon, chi2, c = chi2, vmin=vmin, vmax=vmax, cmap = cmap)
        ax.set_xlabel('Silicon', fontsize=11)
        ax.set_ylabel('Chi2', fontsize=11)
        ax.set_title('Best model: Silicon = ' + str(bestModel.silicon), fontsize=12)
        ax.axvline(x=errorRanges[13][0], c = 'r')
        ax.axvline(x=errorRanges[13][1], c = 'r')
        ax.axvline(x=bestModel.silicon, c = 'black', alpha=0.8, linewidth=1.5)
        ax.set_xlim(xRanges[13][0], xRanges[13][1])
        ax.set_ylim(0, bestModel.chi2 * 2)
        ax.fill([errorRanges[13][0], errorRanges[13][1], errorRanges[13][1], errorRanges[13][0]], [0, 0, bestModel.chi2 * 2, bestModel.chi2 * 2], 'b', alpha=0.1)
        ax.grid(True)

    print
    plt.suptitle('Parameters vs Chi2 for '+object,fontsize=20)
    if not multi:
        if pdf:
            print 'Making png, this may take a minute...'
            fileName = maindir+'/'+object+'/plots/parameters_'+object+'_chi2.png'
            plt.plot()
            plt.savefig(fileName, orientation='landscape', format='png')
            print
            print 'Plot saved as \''+ fileName +'\''
            raw_input('Press Enter to continue... ')
        else:
            plt.show()
    else:
        print 'Making Parameters vs. chi2 plot (might take a minute)...'
        multi.savefig(orientation='landscape')
        return multi
    return


def plotResultsP(multi=False, pdf = False):
    #Parameters vs P plot
    if not multi: printHeader()
    print 'Preparing parameters vs. P plot...'
    print
    if pdf or multi: fig = plt.figure(figsize=(20,13))
    else: fig = plt.figure(figsize=(20,10))
    bestModel = models[findBestModel()]
    fileName = maindir + '/' + object +'/pfw_' + object + '.ini'
    xRanges = readIni(fileName, parameters = True)
    errorRanges = findErrorRanges()
    #    cmap = get_cmap('RdYlGn')
    cmap = get_cmap('gist_rainbow') #colormap
    vmin, vmax = bestModel.P/3., bestModel.P*2.2  #used to normalize colors, adjust when using other colormap
    Teff, logg, mdot, vinf, beta, NHe, vtur, vrot, P, vmacro, nitrogen, carbon,oxygen, phosphorus, silicon = [], [], [], [], [], [], [], [], [], [], [], [], [], [], []
    for i in range(len(models)):
        Teff.append(models[i].Teff)
        logg.append(models[i].logg)
        mdot.append(models[i].mdot)
        vinf.append(models[i].vinf)
        beta.append(models[i].beta)
        NHe.append(models[i].NHe)
        vtur.append(models[i].vtur)
        vrot.append(models[i].vrot)
        P.append(models[i].P)
        vmacro.append(models[i].vmacro)
#        if models[i].nitrogen:
        nitrogen.append(models[i].nitrogen)
        carbon.append(models[i].carbon)
        oxygen.append(models[i].oxygen)
        phosphorus.append(models[i].phosphorus)
        silicon.append(models[i].silicon)

    print 'Preparing Teff vs. P plot...'
    fig.subplots_adjust(hspace=0.4)
    ax = fig.add_subplot(341)
    ax.fill([errorRanges[0][0], errorRanges[0][1], errorRanges[0][1], errorRanges[0][0]], [0, 0, bestModel.P * 1.2, bestModel.P * 1.2], 'b', alpha=0.1)
    ax.scatter(Teff, P, c = P, vmin=vmin, vmax=vmax, cmap = cmap)
    ax.set_xlabel('Teff', fontsize=11)
    ax.set_ylabel('P', fontsize=11)
    ax.set_title('Best model: Teff = ' + str(bestModel.Teff), fontsize=12)
    ax.axvline(x=errorRanges[0][0], c = 'r')
    ax.axvline(x=errorRanges[0][1], c = 'r')
    ax.axvline(x=bestModel.Teff, c = 'black', alpha=0.8, linewidth=1.5)
    ax.set_xlim(xRanges[0][0], xRanges[0][1])
    ax.set_ylim(0, bestModel.P * 2)
    ax.grid(True)
    print 'Preparing log g vs. P plot...'
    fig.subplots_adjust(hspace=0.4)
    ax = fig.add_subplot(342)
    ax.scatter(logg, P, c = P, vmin=vmin, vmax=vmax, cmap = cmap)
    ax.set_xlabel('log g', fontsize=11)
    ax.set_ylabel('P', fontsize=11)
    ax.set_title('Best model: log g = ' + str(bestModel.logg), fontsize=12)
    ax.axvline(x=errorRanges[1][0], c = 'r')
    ax.axvline(x=errorRanges[1][1], c = 'r')
    ax.axvline(x=bestModel.logg, c = 'black', alpha=0.8, linewidth=1.5)
    ax.set_xlim(xRanges[1][0], xRanges[1][1])
    ax.set_ylim(0, bestModel.P * 2)
    ax.fill([errorRanges[1][0], errorRanges[1][1], errorRanges[1][1], errorRanges[1][0]], [0, 0, bestModel.P * 2, bestModel.P * 2], 'b', alpha=0.1)
    ax.grid(True)
    print 'Preparing log Mdot vs. P plot...'
    fig.subplots_adjust(hspace=0.4)
    ax = fig.add_subplot(343)
    ax.scatter(mdot, P, c = P, vmin=vmin, vmax=vmax, cmap = cmap)
    ax.set_xlabel('log Mdot', fontsize=11)
    ax.set_ylabel('P', fontsize=11)
    ax.set_title('Best model: log Mdot = ' + str(bestModel.mdot), fontsize=12)
    ax.axvline(x=errorRanges[2][0], c = 'r')
    ax.axvline(x=errorRanges[2][1], c = 'r')
    ax.axvline(x=bestModel.mdot, c = 'black', alpha=0.8, linewidth=1.5)
    ax.set_xlim(xRanges[2][0], xRanges[2][1])
    ax.set_ylim(0, bestModel.P * 2)
    ax.fill([errorRanges[2][0], errorRanges[2][1], errorRanges[2][1], errorRanges[2][0]], [0, 0, bestModel.P * 2, bestModel.P * 2], 'b', alpha=0.1)
    ax.grid(True)
    print 'Preparing beta vs. P plot...'
    fig.subplots_adjust(hspace=0.4)
    ax = fig.add_subplot(344)
    ax.scatter(beta, P, c = P, vmin=vmin, vmax=vmax, cmap = cmap)
    ax.set_xlabel('beta', fontsize=11)
    ax.set_ylabel('P', fontsize=11)
    ax.set_title('Best model: beta = ' + str(bestModel.beta), fontsize=12)
    ax.axvline(x=errorRanges[4][0], c = 'r')
    ax.axvline(x=errorRanges[4][1], c = 'r')
    ax.axvline(x=bestModel.beta, c = 'black', alpha=0.8, linewidth=1.5)
    ax.set_xlim(xRanges[4][0], xRanges[4][1])
    ax.set_ylim(0, bestModel.P * 2)
    ax.fill([errorRanges[4][0], errorRanges[4][1], errorRanges[4][1], errorRanges[4][0]], [0, 0, bestModel.P * 2, bestModel.P * 2], 'b', alpha=0.1)
    ax.grid(True)
    print 'Preparing vinf vs. P plot...'
    fig.subplots_adjust(hspace=0.4)
    ax = fig.add_subplot(345)
    ax.scatter(vinf, P, c = P, vmin=vmin, vmax=vmax, cmap = cmap)
    ax.set_xlabel('vinf', fontsize=11)
    ax.set_ylabel('P', fontsize=11)
    ax.set_title('Best model: vinf = ' + str(bestModel.vinf), fontsize=12)
    ax.axvline(x=errorRanges[3][0], c = 'r')
    ax.axvline(x=errorRanges[3][1], c = 'r')
    ax.axvline(x=bestModel.vinf, c = 'black', alpha=0.8, linewidth=1.5)
    ax.set_xlim(xRanges[3][0], xRanges[3][1])
    ax.set_ylim(0, bestModel.P * 2)
    ax.fill([errorRanges[3][0], errorRanges[3][1], errorRanges[3][1], errorRanges[3][0]], [0, 0, bestModel.P * 2, bestModel.P * 2], 'b', alpha=0.1)
    ax.grid(True)
    print 'Preparing nHe/nH vs. P plot...'
    fig.subplots_adjust(hspace=0.4)
    ax = fig.add_subplot(346)
    ax.scatter(NHe, P, c = P, vmin=vmin, vmax=vmax, cmap = cmap)
    ax.set_xlabel('nHe/nH', fontsize=11)
    ax.set_ylabel('P', fontsize=11)
    ax.set_title('Best model: nHe/nH = ' + str(bestModel.NHe), fontsize=12)
    ax.axvline(x=errorRanges[5][0], c = 'r')
    ax.axvline(x=errorRanges[5][1], c = 'r')
    ax.axvline(x=bestModel.NHe, c = 'black', alpha=0.8, linewidth=1.5)
    ax.set_xlim(xRanges[5][0], xRanges[5][1])
    ax.set_ylim(0, bestModel.P * 2)
    ax.fill([errorRanges[5][0], errorRanges[5][1], errorRanges[5][1], errorRanges[5][0]], [0, 0, bestModel.P * 2, bestModel.P * 2], 'b', alpha=0.1)
    ax.grid(True)
    print 'Preparing v_tur vs. P plot...'
    fig.subplots_adjust(hspace=0.4)
    ax = fig.add_subplot(347)
    ax.scatter(vtur, P, c = P, vmin=vmin, vmax=vmax, cmap = cmap)
    ax.set_xlabel('v_tur', fontsize=11)
    ax.set_ylabel('P', fontsize=11)
    ax.set_title('Best model: v_tur = ' + str(bestModel.vtur), fontsize=12)
    ax.axvline(x=errorRanges[6][0], c = 'r')
    ax.axvline(x=errorRanges[6][1], c = 'r')
    ax.axvline(x=bestModel.vtur, c = 'black', alpha=0.8, linewidth=1.5)
    ax.set_xlim(xRanges[6][0], xRanges[6][1])
    ax.set_ylim(0, bestModel.P * 2)
    ax.fill([errorRanges[6][0], errorRanges[6][1], errorRanges[6][1], errorRanges[6][0]], [0, 0, bestModel.P * 2, bestModel.P * 2], 'b', alpha=0.1)
    ax.grid(True)
    print 'Preparing v_rot vs. P plot...'
    fig.subplots_adjust(hspace=0.4)
    ax = fig.add_subplot(348)
    ax.scatter(vrot, P, c = P, vmin=vmin, vmax=vmax, cmap = cmap)
    ax.set_xlabel('v sin(i)', fontsize=11)
    ax.set_ylabel('P', fontsize=11)
    ax.set_title('Best model: v sin(i) = ' + str(bestModel.vrot), fontsize=12)
    ax.axvline(x=errorRanges[7][0], c = 'r')
    ax.axvline(x=errorRanges[7][1], c = 'r')
    ax.axvline(x=bestModel.vrot, c = 'black', alpha=0.8, linewidth=1.5)
    ax.set_xlim(xRanges[7][0], xRanges[7][1])
    ax.set_ylim(0, bestModel.P * 2)
    ax.fill([errorRanges[7][0], errorRanges[7][1], errorRanges[7][1], errorRanges[7][0]], [0, 0, bestModel.P * 2, bestModel.P * 2], 'b', alpha=0.1)
    ax.grid(True)
    if not (vmacro[0]) == -1:
        print 'Preparing v_macro vs. P plot...'
        fig.subplots_adjust(hspace=0.4)
        ax = fig.add_subplot(3,4,13)
        ax.scatter(vmacro, P, c = P, vmin=vmin, vmax=vmax, cmap = cmap)
        ax.set_xlabel('v_macro', fontsize=11)
        ax.set_ylabel('P', fontsize=11)
        ax.set_title('Best model: v_macro = ' + str(bestModel.vmacro), fontsize=12)
        ax.axvline(x=errorRanges[8][0], c = 'r')
        ax.axvline(x=errorRanges[8][1], c = 'r')
        ax.axvline(x=bestModel.vmacro, c = 'black', alpha=0.8, linewidth=1.5)
        ax.set_xlim(xRanges[8][0], xRanges[8][1])
        ax.set_ylim(0, bestModel.P * 2)
        ax.fill([errorRanges[8][0], errorRanges[8][1], errorRanges[8][1], errorRanges[8][0]], [0, 0, bestModel.P * 2, bestModel.P * 2], 'b', alpha=0.1)
        ax.grid(True)
    if not (len(nitrogen)) == 0:
        print 'Preparing nitrogen vs. P plot...'
        fig.subplots_adjust(hspace=0.4)
        ax = fig.add_subplot(349)
        ax.scatter(nitrogen, P, c = P, vmin=vmin, vmax=vmax, cmap = cmap)
        ax.set_xlabel('Nitrogen', fontsize=11)
        ax.set_ylabel('P', fontsize=11)
        ax.set_title('Best model: Nitrogen = ' + str(bestModel.nitrogen), fontsize=12)
        ax.axvline(x=errorRanges[9][0], c = 'r')
        ax.axvline(x=errorRanges[9][1], c = 'r')
        ax.axvline(x=bestModel.nitrogen, c = 'black', alpha=0.8, linewidth=1.5)
        ax.set_xlim(xRanges[9][0], xRanges[9][1])
        ax.set_ylim(0, bestModel.P * 2)
        ax.fill([errorRanges[9][0], errorRanges[9][1], errorRanges[9][1], errorRanges[9][0]], [0, 0, bestModel.P * 2, bestModel.P * 2], 'b', alpha=0.1)
        ax.grid(True)

    if not (len(carbon)) == 0:
        print 'Preparing carbon vs. P plot...'
        fig.subplots_adjust(hspace=0.4)
        ax = fig.add_subplot(3,4,10)
        ax.scatter(carbon, P, c = P, vmin=vmin, vmax=vmax, cmap = cmap)
        ax.set_xlabel('Carbon', fontsize=11)
        ax.set_ylabel('P', fontsize=11)
        ax.set_title('Best model: Carbon = ' + str(bestModel.carbon), fontsize=12)
        ax.axvline(x=errorRanges[10][0], c = 'r')
        ax.axvline(x=errorRanges[10][1], c = 'r')
        ax.axvline(x=bestModel.carbon, c = 'black', alpha=0.8, linewidth=1.5)
        ax.set_xlim(xRanges[10][0], xRanges[10][1])
        ax.set_ylim(0, bestModel.P * 2)
        ax.fill([errorRanges[10][0], errorRanges[10][1], errorRanges[10][1], errorRanges[10][0]], [0, 0, bestModel.P * 2, bestModel.P * 2], 'b', alpha=0.1)
        ax.grid(True)

    if not (len(oxygen)) == 0:
        print 'Preparing oxygen vs. P plot...'
        fig.subplots_adjust(hspace=0.4)
        ax = fig.add_subplot(3,4,11)
        ax.scatter(oxygen, P, c = P, vmin=vmin, vmax=vmax, cmap = cmap)
        ax.set_xlabel('Oxygen', fontsize=11)
        ax.set_ylabel('P', fontsize=11)
        ax.set_title('Best model: Oxygen = ' + str(bestModel.oxygen), fontsize=12)
        ax.axvline(x=errorRanges[11][0], c = 'r')
        ax.axvline(x=errorRanges[11][1], c = 'r')
        ax.axvline(x=bestModel.oxygen, c = 'black', alpha=0.8, linewidth=1.5)
        ax.set_xlim(xRanges[11][0], xRanges[11][1])
        ax.set_ylim(0, bestModel.P * 2)
        ax.fill([errorRanges[11][0], errorRanges[11][1], errorRanges[11][1], errorRanges[11][0]], [0, 0, bestModel.P * 2, bestModel.P * 2], 'b', alpha=0.1)
        ax.grid(True)



#    if not (len(phosphorus)) == 0:
#        print 'Preparing phosphorus vs. P plot...'
#        fig.subplots_adjust(hspace=0.4)
#        ax = fig.add_subplot(3,4,13)
#        ax.scatter(phosphorus, P, c = P, vmin=vmin, vmax=vmax, cmap = cmap)
#        ax.set_xlabel('Phosphorus', fontsize=11)
#        ax.set_ylabel('P', fontsize=11)
#        ax.set_title('Best model: Phosphorus = ' + str(bestModel.phosphorus), fontsize=12)
#        ax.axvline(x=errorRanges[12][0], c = 'r')
#        ax.axvline(x=errorRanges[12][1], c = 'r')
#        ax.axvline(x=bestModel.phosphorus, c = 'black', alpha=0.8, linewidth=1.5)
#        ax.set_xlim(xRanges[12][0], xRanges[12][1])
#        ax.set_ylim(0, bestModel.P * 2)
#        ax.fill([errorRanges[12][0], errorRanges[12][1], errorRanges[12][1], errorRanges[12][0]], [0, 0, bestModel.P * 2, bestModel.P * 2], 'b', alpha=0.1)
#        ax.grid(True)


    if not (len(silicon)) == 0:
        print 'Preparing silicon vs. P plot...'
        fig.subplots_adjust(hspace=0.4)
        ax = fig.add_subplot(3,4,12)
        ax.scatter(silicon, P, c = P, vmin=vmin, vmax=vmax, cmap = cmap)
        ax.set_xlabel('Silicon', fontsize=11)
        ax.set_ylabel('P', fontsize=11)
        ax.set_title('Best model: Silicon = ' + str(bestModel.silicon), fontsize=12)
        ax.axvline(x=errorRanges[13][0], c = 'r')
        ax.axvline(x=errorRanges[13][1], c = 'r')
        ax.axvline(x=bestModel.silicon, c = 'black', alpha=0.8, linewidth=1.5)
        ax.set_xlim(xRanges[13][0], xRanges[13][1])
        ax.set_ylim(0, bestModel.P * 2)
        ax.fill([errorRanges[13][0], errorRanges[13][1], errorRanges[13][1], errorRanges[13][0]], [0, 0, bestModel.P * 2, bestModel.P * 2], 'b', alpha=0.1)
        ax.grid(True)

    print
    plt.suptitle('Parameters vs P for '+object,fontsize=20)
    if not multi:
        if pdf:
            print 'Making pdf, this may take a minute...'
            fileName = maindir+'/'+object+'/plots/parameters_'+object+'_P.png'
            plt.plot()
            plt.savefig(fileName, orientation='landscape', format='png')
            print
            print 'Plot saved as \''+ fileName +'\''
            raw_input('Press Enter to continue... ')
        else:
            plt.show()
    else:
        print 'Making Parameters vs. P plot (might take a minute)...'
        multi.savefig(orientation='landscape')
        return multi
    return



def plotBestModel(multi = False, im_num = False, path = '.'):
    if not multi: printHeader()
    bestModel = models[findBestModel()]
    print
    print 'Making best model overview...'
    fig = plt.figure(figsize=(13,20))
    ax = fig.add_subplot(2,2,1)

    errorRanges = findErrorRanges_new()

    ax.text(0.5, 0.90, 'Best Model Parameters', ha = 'center', va = 'center', transform = ax.transAxes)
    for i,j in enumerate(['Teff', 'logg', 'mdot', 'beta', 'vinf', 'NHe', 'vtur', 'vmacro', 'vrot', 'radius', 'dmom', 'luminosity', 'carbon', 'oxygen', 'nitrogen', 'silicon']):
        ax.text(0.1, 0.80 - (i*0.05), all_parameter_names_dict[j], ha = 'left', transform = ax.transAxes)
        ax.text(0.35, 0.80 - (i*0.05), str(round(vars(bestModel)[j],3)), ha = 'left', transform = ax.transAxes)
        ax.text(0.6, 0.80 - (i*0.05), '[' + str(round(errorRanges[j][0],3)) + ', ' + str(round(errorRanges[j][1],3)) + ']', ha = 'left', transform = ax.transAxes)

    ax.set_xticks([])
    ax.set_yticks([])


    converg_params = {}
    converg_params['generation'] = []
    converg_params['number'] = []
    converg_params['fitness'] = []
    converg_params['chi2'] = []
    for i in range(len(models)):
        converg_params['generation'].append(vars(models[i])['generation'])
        converg_params['number'].append(vars(models[i])['number'])
        converg_params['fitness'].append(vars(models[i])['fitness'])
        converg_params['chi2'].append(vars(models[i])['chi2'])
    gens = max(converg_params['generation'])
    individuals = max(converg_params['number'])

    fileName = maindir + '/' + object +'/pfw_' + object + '.ini'
    xRanges = readIni_new(fileName, parameters = True)

    fit_params_used = [i for i in fit_parameters if xRanges[i][2] > 0]

    ax2 = fig.add_subplot(2,2,2)
    ax2.text(1.7, 0.90, 'GA Run Information', ha = 'center', va = 'center', transform = ax.transAxes)

    ax2.text(1.3, 0.80, 'Generations', ha = 'left', transform = ax.transAxes)
    ax2.text(1.3, 0.75, 'Population Size', ha = 'left', transform = ax.transAxes)
    ax2.text(1.3, 0.70, '# of Parameters fit', ha = 'left', transform = ax.transAxes)
    ax2.text(1.3, 0.65, 'Run ID', ha = 'left', transform = ax.transAxes)
    ax2.text(1.3, 0.60, 'Best Fittness', ha = 'left', transform = ax.transAxes)
    ax2.text(1.3, 0.55, 'Best Chi2', ha = 'left', transform = ax.transAxes)

    ax2.text(1.8, 0.80, gens, ha = 'left', transform = ax.transAxes)
    ax2.text(1.8, 0.75, individuals, ha = 'left', transform = ax.transAxes)
    ax2.text(1.8, 0.70, len(fit_params_used), ha = 'left', transform = ax.transAxes)
    ax2.text(1.8, 0.65, object, ha = 'left', transform = ax.transAxes)
    ax2.text(1.8, 0.60, max(converg_params['fitness']), ha = 'left', transform = ax.transAxes)
    ax2.text(1.8, 0.55, min(converg_params['chi2']), ha = 'left', transform = ax.transAxes)

    ax2.set_xticks([])
    ax2.set_yticks([])
    print gens
    print individuals
    print len(fit_params_used)


    '''
    ax.text(0.1, 0.80, 'T_eff:', ha = 'left', transform = ax.transAxes)
    ax.text(0.1, 0.75, 'log g:', ha = 'left', transform = ax.transAxes)
    ax.text(0.1, 0.70, 'log M_dot:', ha = 'left', transform = ax.transAxes)
    ax.text(0.1, 0.65, 'beta:', ha = 'left', transform = ax.transAxes)
    ax.text(0.1, 0.60, 'v_inf:', ha = 'left', transform = ax.transAxes)
    ax.text(0.1, 0.55, 'nHe/nH:', ha = 'left', transform = ax.transAxes)
    ax.text(0.1, 0.50, 'v_tur:', ha = 'left', transform = ax.transAxes)
    ax.text(0.1, 0.45, 'v_macro:', ha = 'left', transform = ax.transAxes)
    ax.text(0.1, 0.40, 'v sin(i):', ha = 'left', transform = ax.transAxes)
    ax.text(0.1, 0.35, 'Radius:', ha = 'left', transform = ax.transAxes)
    ax.text(0.1, 0.30, 'd_mom:', ha = 'left', transform = ax.transAxes)
    ax.text(0.1, 0.25, 'Luminosity:', ha = 'left', transform = ax.transAxes)
    ax.text(0.1, 0.20, 'Carbon:', ha = 'left', transform = ax.transAxes)
    ax.text(0.1, 0.15, 'Oxygen:', ha = 'left', transform = ax.transAxes)
    ax.text(0.1, 0.10, 'Nitrogen:', ha = 'left', transform = ax.transAxes)
    ax.text(0.1, 0.05, 'Silicon:', ha = 'left', transform = ax.transAxes)


    ax.text(0.35, 0.80, str(bestModel.Teff), ha = 'left', transform = ax.transAxes)
    ax.text(0.35, 0.75, str(bestModel.logg), ha = 'left', transform = ax.transAxes)
    ax.text(0.35, 0.70, str(bestModel.mdot), ha = 'left', transform = ax.transAxes)
    ax.text(0.35, 0.65, str(bestModel.beta), ha = 'left', transform = ax.transAxes)
    ax.text(0.35, 0.60, str(bestModel.vinf), ha = 'left', transform = ax.transAxes)
    ax.text(0.35, 0.55, str(round(bestModel.NHe, 3)), ha = 'left', transform = ax.transAxes)
    ax.text(0.35, 0.50, str(bestModel.vtur), ha = 'left', transform = ax.transAxes)
    ax.text(0.35, 0.45, str(bestModel.vmacro), ha = 'left', transform = ax.transAxes)
    ax.text(0.35, 0.40, str(bestModel.vrot), ha = 'left', transform = ax.transAxes)
    ax.text(0.35, 0.35, str(round(bestModel.radius, 3)), ha = 'left', transform = ax.transAxes)
    '''

    if multi:
        if im_num:
            fileName = path + 'temp' + str(im_num).zfill(2) + '.png'
            plt.savefig(fileName, orientation='portrait', format = 'png', dpi=300)
            im = Image.open(fileName)
            im = im.convert('RGB')
            im.save(path + 'temp' + str(im_num).zfill(2)+ '.pdf')
            im_num += 1
            return im_num
        else:
            multi.savefig(orientation='portrait')
            return multi
    else: return



def findModel():
    while True:
        printHeader()
        print 'Model lookup'
        print ' 1. Find by parameters'
        print ' 2. Find by minimum fitness'
        print ' 3. Find by model ID'
        print ' 0. Return to analysis menu'
        print
        print
        choice = raw_input('Choose an option: ')
        if choice is '1': findModelParameters()
        if choice is '2': findModelFitness()
        if choice is '3':
            number = int(raw_input('Enter model number: '))
            generation = int(raw_input('Enter model generation: '))
            result = [findModelID(number,generation)]
            printSearchResults(result)
        if choice is '0': break
    return
    return

def findModelParameters():
    print
    print 'Enter the parameters for the model search.'
    print 'Leave blank if you don\'t want to search for the parameter.'
    print
    teff = raw_input('Enter Teff: ')
    logg = raw_input('Enter log g: ')
    mdot = raw_input('Enter log mdot: ')
    vinf = raw_input('Enter v_inf: ')
    beta = raw_input('Enter beta: ')
    NHe = raw_input('Enter NHe: ')
    vtur = raw_input('Enter v_tur: ')
    vrot = raw_input('Enter v_rot: ')
    if teff is '': teff = 0
    if logg is '': logg = 0
    if mdot is '': mdot = 0
    if vinf is '': vinf = 0
    if beta is '': beta = 0
    if NHe is '': NHe = 0
    if vtur is '': vtur = 0
    if vrot is '': vrot = 0
    searchParameters = [teff,logg,mdot,vinf,beta,NHe,vtur,vrot]
    result = []
    for i in range(len(models)):
        if models[i].Teff == float(teff) or teff is 0:
            if models[i].logg == float(logg) or logg is 0:
                if models[i].mdot == float(mdot) or mdot is 0:
                    if models[i].vinf == float(vinf) or vinf is 0:
                        if models[i].beta == float(beta) or beta is 0:
                            if models[i].NHe == float(NHe) or NHe is 0:
                                if models[i].vtur == float(vtur) or vtur is 0:
                                    if models[i].vrot == float(vrot) or vrot is 0:
                                        result.append(models[i])
    printSearchResults(result)
    return


def findModelFitness():
    print
    bestModel = models[findBestModel()]
    while True:
        try:
            fitness = float(raw_input('Enter minimum value of fitness (best model = '+str(round(bestModel.fitness,2)) +'): '))
            break
        except:
            print 'Illegal input, try again...'

    result = []
    for i in range(len(models)):
        if models[i].fitness >= fitness: result.append(models[i])
    printSearchResults(result)
    return


def printSearchResults(result):
    printHeader()
    print 'Model search results'
    print
    print '#\tGen\t\tTeff\tlogg\tmdot\tvinf\tbeta\tNHe\tvtur\tvrot\tfitness'
    print '------------------------------------------------------------------------------------------------'
    for i in range(len(result)):
        print result[i].number,'\t',result[i].generation,'\t\t',str(int(result[i].Teff)),'\t',str(round(result[i].logg,2)),'\t',str(round(result[i].mdot,2)),'\t',str(round(result[i].vinf,1)),'\t',str(round(result[i].beta,2)),'\t', str(round(result[i].NHe,3)),'\t',str(int(result[i].vtur)),'\t',str(int(result[i].vrot)),'\t',str(round(result[i].fitness,3)) + '\t' + result[i].ID
    print
    print 'Found', len(result), 'results.'
    raw_input('Press Enter to continue.')
    return


def findErrorRanges2_new(currentRanges):
    minimum, maximum = [], []
    fileName = maindir + '/' + object +'/pfw_' + object + '.ini'
    xRange = readIni(fileName, parameters = True)
    for i in range(len(xRange)):
        minimum.append(xRange[i][1])
        maximum.append(xRange[i][0])
    bestModel = models[findBestModel()]
    lowerP = []
    P = []
    result = cp.deepcopy(currentRanges)
    for i in range(len(models)):
        if models[i].P < minimumP:
            lowerP.append(models[i])
            P.append(models[i].P)
    print max(P)
    # T boundaries
    for i in list(set(all_parameters)-set(diagnostic_parameters)):
        lowmodels= []
        highmodels = []
        for model in lowerP:
            if vars(model)[i] < vars(bestModel)[i] and vars(model)[i] < currentRanges[i][0]:
                lowmodels.append(model)
            if vars(model)[i] > vars(bestModel)[i] and vars(model)[i] > currentRanges[i][1]:
                highmodels.append(model)
        if len(lowmodels) != 0:
            result[i][0] = vars(getMaxP(lowmodels))[i]
        else:
            result[i][0] = -1 * (9 * (i == 'mdot') + 1)
        if len(highmodels) != 0:
            result[i][1] = vars(getMaxP(highmodels))[i]
        else:
            result[i][1] = 1e32
    return result

def findErrorRanges2(currentRanges):
#    if models[0].nitrogen:
#        k = 1
#    else:
#        k = 0
    minimum, maximum = [], []
    fileName = maindir + '/' + object +'/pfw_' + object + '.ini'
    xRange = readIni(fileName, parameters = True)
    for i in range(len(xRange)):
        minimum.append(xRange[i][1])
        maximum.append(xRange[i][0])
    bestModel = models[findBestModel()]
    lowerP = []
    P = []
    for i in range(len(models)):
        if models[i].P < minimumP:
            lowerP.append(models[i])
            P.append(models[i].P)
    print max(P)
    # T boundaries
    lowTmodels= []
    highTmodels = []
    for model in lowerP:
        if model.Teff < bestModel.Teff and model.Teff < currentRanges[0][0]:
            lowTmodels.append(model)
        if model.Teff > bestModel.Teff and model.Teff > currentRanges[0][1]:
            highTmodels.append(model)
    if len(lowTmodels) != 0:
        minimum[0] = getMaxP(lowTmodels).Teff
    else:
        minimum[0] = -1
    if len(highTmodels) != 0:
        maximum[0] = getMaxP(highTmodels).Teff
    else:
        maximum[0] = 1e32
    # logg boundaries
    lowloggmodels= []
    highloggmodels = []
    for model in lowerP:
        if model.logg < bestModel.logg and model.logg < currentRanges[1][0]:
            lowloggmodels.append(model)
        if model.logg > bestModel.logg and model.logg > currentRanges[1][1]:
            highloggmodels.append(model)
    if len(lowloggmodels) != 0:
        minimum[1] = getMaxP(lowloggmodels).logg
    else:
        minimum[1] = -1
    if len(highloggmodels) != 0:
        maximum[1] = getMaxP(highloggmodels).logg
    else:
        maximum[1] = 1e32
    # mdot
    lowmdotmodels= []
    highmdotmodels = []
    for model in lowerP:
        if model.mdot < bestModel.mdot and model.mdot < currentRanges[2][0]:
            lowmdotmodels.append(model)
        if model.mdot > bestModel.mdot and model.mdot > currentRanges[2][1]:
            highmdotmodels.append(model)
    if len(lowmdotmodels) != 0:
        minimum[2] = getMaxP(lowmdotmodels).mdot
    else:
        minimum[2] = -10
    if len(highmdotmodels) != 0:
        maximum[2] = getMaxP(highmdotmodels).mdot
    else:
        maximum[2] = 1e32
    # beta boundaries
    lowbetamodels= []
    highbetamodels = []
    for model in lowerP:
        if model.beta < bestModel.beta and model.beta < currentRanges[4][0]:
            lowbetamodels.append(model)
        if model.beta > bestModel.beta and model.beta > currentRanges[4][1]:
            highbetamodels.append(model)
    if len(lowbetamodels) != 0:
        minimum[4] = getMaxP(lowbetamodels).beta
    else:
        minimum[4] = -1
    if len(highbetamodels) != 0:
        maximum[4] = getMaxP(highbetamodels).beta
    else:
        maximum[4] = 1e32
    # vinf boundaries
    lowvinfmodels= []
    highvinfmodels = []
    for model in lowerP:
        if model.vinf < bestModel.vinf and model.vinf < currentRanges[3][0]:
            lowvinfmodels.append(model)
        if model.vinf > bestModel.vinf and model.vinf > currentRanges[3][1]:
            highvinfmodels.append(model)
    if len(lowvinfmodels) != 0:
        minimum[3] = getMaxP(lowvinfmodels).vinf
    else:
        minimum[3] = -1
    if len(highvinfmodels) != 0:
        maximum[3] = getMaxP(highvinfmodels).vinf
    else:
        maximum[3] = 1e32
    # NHe boundaries
    lowNHemodels= []
    highNHemodels = []
    for model in lowerP:
        if model.NHe < bestModel.NHe and model.NHe < currentRanges[5][0]:
            lowNHemodels.append(model)
        if model.NHe > bestModel.NHe and model.NHe > currentRanges[5][1]:
            highNHemodels.append(model)
    if len(lowNHemodels) != 0:
        minimum[5] = getMaxP(lowNHemodels).NHe
    else:
        minimum[5] = -1
    if len(highNHemodels) != 0:
        maximum[5] = getMaxP(highNHemodels).NHe
    else:
        maximum[5] = 1e32
    # vtur boundaries
    lowvturmodels= []
    highvturmodels = []
    for model in lowerP:
        if model.vtur < bestModel.vtur and model.vtur < currentRanges[6][0]:
            lowvturmodels.append(model)
        if model.vtur > bestModel.vtur and model.vtur > currentRanges[6][1]:
            highvturmodels.append(model)
    if len(lowvturmodels) != 0:
        minimum[6] = getMaxP(lowvturmodels).vtur
    else:
        minimum[6] = -1
    if len(highvturmodels) != 0:
        maximum[6] = getMaxP(highvturmodels).vtur
    else:
        maximum[6] = 1e32
    # vrot boundaries
    lowvrotmodels= []
    highvrotmodels = []
    for model in lowerP:
        if model.vrot < bestModel.vrot and model.vrot < currentRanges[7][0]:
            lowvrotmodels.append(model)
        if model.vrot > bestModel.vrot and model.vrot > currentRanges[7][1]:
            highvrotmodels.append(model)
    if len(lowvrotmodels) != 0:
        minimum[7] = getMaxP(lowvrotmodels).vrot
    else:
        minimum[7] = -1
    if len(highvrotmodels) != 0:
        maximum[7] = getMaxP(highvrotmodels).vrot
    else:
        maximum[7] = 1e32
    # vmacro boundaries
    lowvmacromodels= []
    highvmacromodels = []
    for model in lowerP:
        if model.vmacro < bestModel.vmacro and model.vmacro < currentRanges[8][0]:
            lowvmacromodels.append(model)
        if model.vmacro > bestModel.vmacro and model.vmacro > currentRanges[8][1]:
            highvmacromodels.append(model)
    if len(lowvmacromodels) != 0:
        minimum[8] = getMaxP(lowvmacromodels).vmacro
    else:
        minimum[8] = -1
    if len(highvmacromodels) != 0:
        maximum[8] = getMaxP(highvmacromodels).vmacro
    else:
        maximum[8] = 1e32
    # nitrogen boundaries
    if models[0].nitrogen:
        lownitrogenmodels= []
        highnitrogenmodels = []
        for model in lowerP:
            if model.nitrogen < bestModel.nitrogen and model.nitrogen < currentRanges[9][0]:
                lownitrogenmodels.append(model)
            if model.nitrogen > bestModel.nitrogen and model.nitrogen > currentRanges[9][1]:
                highnitrogenmodels.append(model)
        if len(lownitrogenmodels) != 0:
            minimum[9] = getMaxP(lownitrogenmodels).nitrogen
        else:
            minimum[9] = -1
        if len(highnitrogenmodels) != 0:
            maximum[9] = getMaxP(highnitrogenmodels).nitrogen
        else:
            maximum[9] = 1e32
    # carbon boundaries
    if models[0].carbon:
        lowcarbonmodels= []
        highcarbonmodels = []
        for model in lowerP:
            if model.carbon < bestModel.carbon and model.carbon < currentRanges[10][0]:
                lowcarbonmodels.append(model)
            if model.carbon > bestModel.carbon and model.carbon > currentRanges[10][1]:
                highcarbonmodels.append(model)
        if len(lowcarbonmodels) != 0:
            minimum[10] = getMaxP(lowcarbonmodels).carbon
        else:
            minimum[10] = -1
        if len(highcarbonmodels) != 0:
            maximum[10] = getMaxP(highcarbonmodels).carbon
        else:
            maximum[10] = 1e32
    # oxygen boundaries
    if models[0].oxygen:
        lowoxygenmodels= []
        highoxygenmodels = []
        for model in lowerP:
            if model.oxygen < bestModel.oxygen and model.oxygen < currentRanges[11][0]:
                lowoxygenmodels.append(model)
            if model.oxygen > bestModel.oxygen and model.oxygen > currentRanges[11][1]:
                highoxygenmodels.append(model)
        if len(lowoxygenmodels) != 0:
            minimum[11] = getMaxP(lowoxygenmodels).oxygen
        else:
            minimum[11] = -1
        if len(highoxygenmodels) != 0:
            maximum[11] = getMaxP(highoxygenmodels).oxygen
        else:
            maximum[11] = 1e32
    # phosphorus boundaries
    if models[0].phosphorus:
        lowphosphorusmodels= []
        highphosphorusmodels = []
        for model in lowerP:
            if model.phosphorus < bestModel.phosphorus and model.phosphorus < currentRanges[12][0]:
                lowphosphorusmodels.append(model)
            if model.phosphorus > bestModel.phosphorus and model.phosphorus > currentRanges[12][1]:
                highphosphorusmodels.append(model)
        if len(lowphosphorusmodels) != 0:
            minimum[12] = getMaxP(lowphosphorusmodels).phosphorus
        else:
            minimum[12] = -1
        if len(highphosphorusmodels) != 0:
            maximum[12] = getMaxP(highphosphorusmodels).phosphorus
        else:
            maximum[12] = 1e32
    # silicon boundaries
    if models[0].silicon:
        lowsiliconmodels= []
        highsiliconmodels = []
        for model in lowerP:
            if model.silicon < bestModel.silicon and model.silicon < currentRanges[13][0]:
                lowsiliconmodels.append(model)
            if model.silicon > bestModel.silicon and model.silicon > currentRanges[13][1]:
                highsiliconmodels.append(model)
        if len(lowsiliconmodels) != 0:
            minimum[13] = getMaxP(lowsiliconmodels).silicon
        else:
            minimum[13] = -1
        if len(highsiliconmodels) != 0:
            maximum[13] = getMaxP(highsiliconmodels).silicon
        else:
            maximum[13] = 1e32
    # luminosity boundaries
    lowluminositymodels= []
    highluminositymodels = []
    for model in lowerP:
        if model.luminosity < bestModel.luminosity and model.luminosity < currentRanges[14][0]:
            lowluminositymodels.append(model)
        if model.luminosity > bestModel.luminosity and model.luminosity > currentRanges[14][1]:
            highluminositymodels.append(model)
    if len(lowluminositymodels) != 0:
        minimum[14] = getMaxP(lowluminositymodels).luminosity
    else:
        minimum[14] = -1
    if len(highluminositymodels) != 0:
        maximum[14] = getMaxP(highluminositymodels).luminosity
    else:
        maximum[14] = 1e32
    # dmom boundaries
    lowdmommodels= []
    highdmommodels = []
    for model in lowerP:
        if model.dmom < bestModel.dmom and model.dmom < currentRanges[15][0]:
            lowdmommodels.append(model)
        if model.dmom > bestModel.dmom and model.dmom > currentRanges[15][1]:
            highdmommodels.append(model)
    if len(lowdmommodels) != 0:
        minimum[15] = getMaxP(lowdmommodels).dmom
    else:
        minimum[15] = -1
    if len(highdmommodels) != 0:
        maximum[15] = getMaxP(highdmommodels).dmom
    else:
        maximum[15] = 1e32
    # mass boundaries
    lowmassmodels= []
    highmassmodels = []
    for model in lowerP:
        if model.mass < bestModel.mass and model.mass < currentRanges[16][0]:
            lowmassmodels.append(model)
        if model.mass > bestModel.mass and model.mass > currentRanges[16][1]:
            highmassmodels.append(model)
    if len(lowmassmodels) != 0:
        minimum[16] = getMaxP(lowmassmodels).mass
    else:
        minimum[16] = -1
    if len(highmassmodels) != 0:
        maximum[16] = getMaxP(highmassmodels).mass
    else:
        maximum[16] = 1e32
    # radius boundaries
    lowradiusmodels= []
    highradiusmodels = []
    for model in lowerP:
        if model.radius < bestModel.radius and model.radius < currentRanges[17][0]:
            lowradiusmodels.append(model)
        if model.radius > bestModel.radius and model.radius > currentRanges[17][1]:
            highradiusmodels.append(model)
    if len(lowradiusmodels) != 0:
        minimum[17] = getMaxP(lowradiusmodels).radius
    else:
        minimum[17] = -1
    if len(highradiusmodels) != 0:
        maximum[17] = getMaxP(highradiusmodels).radius
    else:
        maximum[17] = 1e32
    result=[]
    for i in range(len(xRange)):
        #if i in [0,1,2,4,5,6,7]:
        #    minimum[i], maximum[i] = findBorderValue(minimum[i], maximum[i], step[i], i, xRange[i])
        result.append([minimum[i],maximum[i]])
    return result




def getMaxP(modelList):
    currentMax = 0
    result = modelList[0]
    for i in range(len(modelList)):
        if modelList[i].P > currentMax:
            result = modelList[i]
            currentMax = result.P
    print "max P found ", currentMax
    return result


def findErrorRanges_new():
    result = []
    fileName = maindir + '/' + object +'/pfw_' + object + '.ini'
    xRange = readIni_new(fileName, parameters = True)
    currentRanges = {}
    used_parameters = [i for i in all_parameters if i not in diagnostic_parameters]
    for i in used_parameters:
        currentRanges[i] = [xRange[i][1], xRange[i][0]]

    currentRanges['vinf'][0] = 1e6

    for i in range(len(models)):
        if models[i].P > minimumP: # Model is within error
            for j in used_parameters:
                currentRanges[j] = [min([vars(models[i])[j], currentRanges[j][0]]), max([vars(models[i])[j], currentRanges[j][1]])]

    result = findErrorRanges2_new(currentRanges)
    return result


def findErrorRanges():
    result = []
    fileName = maindir + '/' + object +'/pfw_' + object + '.ini'
    xRange = readIni(fileName, parameters = True)
    print xRange
    minimum, maximum, step = [],[],[]
    for i in range(len(xRange)):
        minimum.append(xRange[i][1])
        maximum.append(xRange[i][0])
        step.append(xRange[i][2])
    minimum[3] = 1e6
#    if models[0].nitrogen:
#        k = 1
#    else:
#        k = 0
    for i in range(len(models)):
        if models[i].P > minimumP: # Model is within error
            if models[i].Teff > maximum[0]: maximum[0] = models[i].Teff
            if models[i].Teff < minimum[0]: minimum[0] = models[i].Teff
            if models[i].logg > maximum[1]: maximum[1] = models[i].logg
            if models[i].logg < minimum[1]: minimum[1] = models[i].logg
            if models[i].mdot > maximum[2]: maximum[2] = models[i].mdot
            if models[i].mdot < minimum[2]: minimum[2] = models[i].mdot
            if models[i].beta > maximum[4]: maximum[4] = models[i].beta
            if models[i].beta < minimum[4]: minimum[4] = models[i].beta
            if models[i].vinf > maximum[3]: maximum[3] = models[i].vinf
            if models[i].vinf < minimum[3]: minimum[3] = models[i].vinf
            if models[i].NHe > maximum[5]: maximum[5] = models[i].NHe
            if models[i].NHe < minimum[5]: minimum[5] = models[i].NHe
            if models[i].vtur > maximum[6]: maximum[6] = models[i].vtur
            if models[i].vtur < minimum[6]: minimum[6] = models[i].vtur
            if models[i].vrot > maximum[7]: maximum[7] = models[i].vrot
            if models[i].vrot < minimum[7]: minimum[7] = models[i].vrot
            if models[i].vmacro > maximum[8]: maximum[8] = models[i].vmacro
            if models[i].vmacro < minimum[8]: minimum[8] = models[i].vmacro
#            if models[i].nitrogen:
            if models[i].nitrogen > maximum[9]: maximum[9] = models[i].nitrogen
            if models[i].nitrogen < minimum[9]: minimum[9] = models[i].nitrogen
            if models[i].carbon > maximum[10]: maximum[10] = models[i].carbon
            if models[i].carbon < minimum[10]: minimum[10] = models[i].carbon
            if models[i].oxygen > maximum[11]: maximum[11] = models[i].oxygen
            if models[i].oxygen < minimum[11]: minimum[11] = models[i].oxygen
            if models[i].phosphorus > maximum[12]: maximum[12] = models[i].phosphorus
            if models[i].phosphorus < minimum[12]: minimum[12] = models[i].phosphorus
            if models[i].silicon > maximum[13]: maximum[13] = models[i].silicon
            if models[i].silicon < minimum[13]: minimum[13] = models[i].silicon
            if models[i].luminosity < minimum[14]: minimum[14] = models[i].luminosity
            if models[i].luminosity > maximum[14]: maximum[14] = models[i].luminosity
            if models[i].dmom < minimum[15]: minimum[15] = models[i].dmom
            if models[i].dmom > maximum[15]: maximum[15] = models[i].dmom
            if models[i].mass < minimum[16]: minimum[16] = models[i].mass
            if models[i].mass > maximum[16]: maximum[16] = models[i].mass
            if models[i].radius < minimum[17]: minimum[17] = models[i].radius
            if models[i].radius > maximum[17]: maximum[17] = models[i].radius
    for i in range(len(xRange)):
        #if i in [0,1,2,4,5,6,7]:
        #    minimum[i], maximum[i] = findBorderValue(minimum[i], maximum[i], step[i], i, xRange[i])
        result.append([minimum[i],maximum[i]])
    print result
    raw_input()
    result = findErrorRanges2(result)
    return result

def findBorderValue(minimum, maximum, step, i, xRange):
    found = False
    while not found:
        if minimum != xRange[0]:
            minimum = minimum - step
            if checkIfExplored(minimum, i):
                found = True
        else: found = True
    found = False
    while not found:
        if maximum != xRange[1]:
            maximum = maximum + step
            if checkIfExplored(maximum, i):
                found = True
        else: found = True
    return minimum, maximum

def checkIfExplored(value, i):
    print "checking " + str(i) + " for " + str(value)
    theseModels = []
    result = False
    if i == 0:
        for j in range(len(models)):
            if round(models[j].Teff,3) == round(value,3):
                theseModels.append(models[j])
    if i == 1:
        for j in range(len(models)):
            if round(models[j].logg,3) == round(value,3):
                theseModels.append(models[j])
    if i == 2:
        for j in range(len(models)):
            if round(models[j].mdot,3) == round(value,3):
                theseModels.append(models[j])
    if i == 4:
        for j in range(len(models)):
            if round(models[j].beta,3) == round(value,3):
                theseModels.append(models[j])
    if i == 5:
        for j in range(len(models)):
            if round(models[j].NHe,3) == round(value,3):
                theseModels.append(models[j])
    if i == 6:
        for j in range(len(models)):
            if round(models[j].vtur,3) == round(value,3):
                theseModels.append(models[j])
    if i == 7:
        for j in range(len(models)):
            if round(models[j].vrot,3) == round(value,3):
                theseModels.append(models[j])
    print len(theseModels)
    bestModel = models[findBestModel()]
    for mod in theseModels:
        if mod.fitness/bestModel.fitness > 0.5:
            result = True
    return result


def calculateP(normalize = False):
    degreesFreedom = countDataPoints() - numberOfParameters
    if normalize:
        scaling = findScaling(degreesFreedom)
    else: scaling = 1
    for i in range(len(models)):
        models[i].chi2 = (models[i].chi2 * degreesFreedom) / scaling
        models[i].P = stats.distributions.chi2.sf(models[i].chi2, degreesFreedom)
    return


def findScaling(degreesFreedom):
    bestModel = models[findBestModel()]
    result = bestModel.chi2
    return result


def countDataPoints():
    result = 0
    normFile = readNorm(maindir+'/'+object+'/'+object+'.norm')
    for i in range(len(lines)):
        for j in range(len(normFile[0])):
            if normFile[0][j] > lines[i].WLleft and normFile[0][j] < lines[i].WLright:
                result += 1
    return result


#SUPPORT FUNCTIONS

def checkFile(file,directory):
    # Check if a given file or directory is present
    fileFound= False
    for fileName in os.listdir(directory):
        if fnmatch.fnmatch(fileName, file):
            fileFound= True
    return fileFound

def restart():
    # Restarts the script
    print 'Restarting...'
    sys.argv = ['GA_Analysis_'+GAversion+'.py']
    os.chdir(maindir)
    python = sys.executable
    os.execl(python, python, * sys.argv)

def printHeader():
    os.system("clear")
    print '              *******************'
    print '              *** GA_ANALYSIS ***'
    print '              *******************'
    print
    print '  GA_Analysis version: ' + version
    print '  GA Version: ' + GAversion
    print
    print '  Object = ' + object
    print


#RUN IT
startUp()
