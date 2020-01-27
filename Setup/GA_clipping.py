#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Jan  9 17:36:48 2020

@author: calum
"""

import numpy as np
import re
import sys
from collections import OrderedDict
import matplotlib.pyplot as plt


object_name = sys.argv[1]
norm_file = object_name + '/' + object_name + '.norm'
setup_lines = object_name + '/' + object_name + '_lines.setup'
output_directory = '../Inputs/' + object_name + '/'



def read_setup_lines(setup_lines):
    with open(setup_lines, 'r') as f:
        line_data = f.readlines()
        print(line_data)
        for i in line_data:
            if i[0][0] == '#':
                line_data.remove(i)

        lines = {}

        for i in line_data:
            (line_id, res, leftwvl, leftoff, rightwvl, rightoff, weight) = re.split(r'\s+', i[:-1])
            lines[(line_id)] = {'res':res, 'leftwvl':leftwvl, 'leftoff':leftoff, 'rightwvl':rightwvl, 'rightoff':rightoff, 'weight':weight}

    return(lines)

lines = read_setup_lines(setup_lines)

def clipping(norm_file, lines):
    """
    Function used to clip regions of contamination in spectral line profile. Remove any regions of spectrum in the line that are clearly not due to the
    process creating the line, for example; a line blend (which we aren't fitting), cosmic, nebular or telluric contamination etc.
    """
    norm = np.genfromtxt(norm_file)
    wave_norm = norm[:,0]
    flux_norm = norm[:,1]
    std_norm = norm[:,2]

    clip_bounds = {}

    for line_id in lines:
        clip = True
        while clip == True:
            f, ax = plt.subplots()
            ax.set_xlim((float(lines[line_id]['leftwvl']), float(lines[line_id]['rightwvl'])))
            ax.set_ylim((0.6, 1.25))
            ax.set_title('Select left fitting boundary (press Enter when ready)')
            ax.plot(wave_norm,flux_norm)
            f.canvas.mpl_connect('button_press_event', clicker)
            f.canvas.mpl_connect('key_press_event', onkey)
            f.canvas.mpl_connect('key_press_event', onkey_quit)
            plt.show()
            leftbound = click_value[0]

            if click_value[0] == 0:
                clip = False
                break

            f, ax = plt.subplots()
            ax.set_xlim((float(lines[line_id]['leftwvl']), float(lines[line_id]['rightwvl'])))
            ax.set_ylim((0.6, 1.25))
            ax.set_title('Select right fitting boundary (press Enter when ready)')
            ax.plot(wave_norm,flux_norm)
            f.canvas.mpl_connect('button_press_event', clicker)
            f.canvas.mpl_connect('key_press_event', onkey)
            f.canvas.mpl_connect('key_press_event', onkey_quit)
            plt.show()
            rightbound = click_value[0]

            clip_bounds[(line_id)] = {'leftbound':leftbound, 'rightbound':rightbound}


            ind = [i for i in range(len(wave_norm)) if wave_norm[i] < leftbound or wave_norm[i] > rightbound]

            flux_norm = flux_norm[ind]
            wave_norm = wave_norm[ind]
            std_norm = std_norm[ind]

            if click_value[0] == 0:
                clip = False

    head = len(wave_norm)

    norm_file = np.column_stack((wave_norm, flux_norm, std_norm))
    np.savetxt(output_directory + object_name + '.norm', norm_file, header='#'+str(head)+' #0', comments='')

    f,ax = plt.subplots()
    ax.set_xlim((float(lines[line_id]['leftwvl']), float(lines[line_id]['rightwvl'])))
    ax.set_ylim((0.6, 1.25))
    ax.plot(wave_norm, flux_norm)
    plt.show()



def clicker(event):
    global click_value
    if event.inaxes:
        click_value = [event.xdata, event.ydata]
        plt.axvline(x=event.xdata, linestyle='-', color='red')
        print(click_value)
        plt.draw()

def onkey(event):
    if event.key == 'enter':
        plt.close()

def onkey_quit(event):
    if event.key == 'q':
        click_value[0] = 0
        plt.close()

clipping(norm_file, lines)
