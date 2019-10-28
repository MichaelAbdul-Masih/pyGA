import numpy as np
import math
from collections import OrderedDict
from schwimmbad import MPIPool
import sys

if sys.version_info > (3,):
    long = int


def crossover_raw(pop_pre_crossover, fitness, prob_type = 'rank'):
    if prob_type == 'rank':
        pop_size = len(fitness)
        ind = np.argsort(fitness)[::-1]
        ranks = [np.where(ind == i)[0][0] for i in range(len(ind))]
        probabilities = 2. * (pop_size + 1) - 2*(ranks)
        probabilities = probabilities / np.sum(probabilities)
    elif prob_type == 'fitness':
        probabilities = fitness / np.sum(fitness)

    chromosome_length = len(pop_pre_crossover[0])

    indicies = range(len(pop_pre_crossover))

    combinations = []
    for i in range(int(len(pop_pre_crossover) / 2) + 1):
        parent1_ind = np.random.choice(len(probabilities), 1, p = probabilities)[0]
        temp_indicies = list(indicies)
        temp_indicies.pop(parent1_ind)
        temp_fitnesses = fitness[temp_indicies] / np.sum(fitness[temp_indicies])
        parent2_ind = np.random.choice(len(probabilities), 1, p = probabilities)[0]
        combinations.append([pop_pre_crossover[parent1_ind], pop_pre_crossover[parent2_ind]])

    crossover_chance = np.random.rand(len(combinations), 2)
    crossover_location = np.random.choice(chromosome_length, (len(combinations), 2))
    new_population = []
    for i in range(len(combinations)):
        if crossover_chance[i][0] <= 0.85:
            if crossover_chance[i][1] <= 0.85/2:
                child1 = combinations[i][0][:crossover_location[i][0]] + combinations[i][1][crossover_location[i][0]:]
                child2 = combinations[i][1][:crossover_location[i][0]] + combinations[i][0][crossover_location[i][0]:]
            else:
                loc1 = min(crossover_location[i])
                loc2 = max(crossover_location[i])
                child1 = combinations[i][0][:loc1] + combinations[i][1][loc1:loc2] + combinations[i][0][loc2:]
                child2 = combinations[i][1][:loc1] + combinations[i][0][loc1:loc2] + combinations[i][1][loc2:]
        else:
            child1 = combinations[i][0]
            child2 = combinations[i][1]
        new_population.append(child1)
        new_population.append(child2)
    return new_population[:len(pop_pre_crossover)]


def crossover_and_mutate_raw(population_raw, fitness, mutation_rate):
    crossover_pop = crossover_raw(population_raw, fitness)
    mutated_pop = mutation_raw_all(crossover_pop, mutation_rate)
    creep_pop = creep_mutation_raw_all(mutated_pop, mutation_rate)
    return creep_pop


def crossover_and_mutate(population_raw, fitness, precision):
    pop = np.round(population_raw, precision)
    pop_pre_crossover = [''.join([str(i).split('.')[-1].ljust(precision, '0') for i in j]) for j in pop]
    crossover_pop = crossover_raw(pop_pre_crossover, fitness)
    mutated_pop = mutation_raw_all(crossover_pop)
    new_population = [[float('0.'+x[i:i+precision]) for i in range(0, len(x), precision)] for x in mutated_pop]
    return np.array(new_population)


def mutation_raw_all(crossover_pop, mutation_rate = 0.25):
    chromosome_length = len(crossover_pop[0])

    mutation_chance = np.random.rand(len(crossover_pop), chromosome_length)
    mutation_replacement = np.random.choice(10, (len(crossover_pop), chromosome_length))
    mutation_replacement_str = np.array(mutation_replacement, dtype='str')
    cross_array = [list(i) for i in  crossover_pop]
    cross_array = np.array(cross_array, dtype='str')
    cross_array[mutation_chance < mutation_rate] = mutation_replacement_str[mutation_chance < mutation_rate]
    crossover_pop = np.array([''.join(i) for i in cross_array])

    return crossover_pop


def creep_mutation_raw_all(mutated_pop, mutation_rate = 0.25):
    chromosome_length = len(mutated_pop[0])
    creep_pop = [i for i in  mutated_pop]
    creep_pop = np.array(creep_pop, dtype='str')
    creep_chance = np.random.rand(len(mutated_pop), chromosome_length)
    creep_adjustment = np.random.choice(2, (len(mutated_pop), chromosome_length)) * 2 - 1
    for individual_number in range(len(mutated_pop)):
        individual_longint = long(creep_pop[individual_number])
        for locus in range(chromosome_length):
            if creep_chance[individual_number][locus] <= mutation_rate:
                individual_longint += long(creep_adjustment[individual_number][locus] * 10 ** (chromosome_length - locus-1))
        if individual_longint >= long(10 ** (chromosome_length)): individual_longint -= long(10 ** (chromosome_length))
        elif individual_longint < 0: individual_longint += long(10 ** (chromosome_length))
        creep_pop[individual_number] = str(individual_longint).zfill(chromosome_length)
        #creep_pop[individual_number] = str(abs(individual_longint)).zfill(chromosome_length)[-1*chromosome_length:]
    return creep_pop


def adjust_mutation_rate(mutation_rate, fitnesses, fit_metric_min=0.05, fit_metric_max=0.25, mut_rate_min = .0005,
mut_rate_max = .25, delta=1.5):
    fitness_metric = abs(np.max(fitnesses) - np.median(fitnesses)) / (np.max(fitnesses) + np.median(fitnesses))

    if fitness_metric <= fit_metric_min:
        mutation_rate = min(mut_rate_max, mutation_rate * delta)
    elif fitness_metric >= fit_metric_max:
        mutation_rate = max(mut_rate_min, mutation_rate / delta)

    return mutation_rate

def create_chromosome(parameters, population_size):
    keys = list(parameters.keys())
    precisions = [parameters[i].precision for i in keys]
    chromosome_length = np.sum(precisions)
    nucleotides = np.array(np.random.choice(10, (population_size, chromosome_length)), dtype='str')
    chromosomes = [''.join(i) for i in nucleotides]
    return chromosomes


def translate_chromosome(parameters, chromosome):
    keys = list(parameters.keys())
    values_dict = OrderedDict()
    for i in keys:
        precision = parameters[i].precision
        param_min = parameters[i].min
        param_max = parameters[i].max
        param_range = param_max - param_min
        value = float('0.' + chromosome[:precision]) * param_range + param_min
        order = int(math.ceil(np.log10(abs(param_max))))
        value = round(value, precision - order)
        chromosome = chromosome[precision:]
        values_dict[parameters[i].name] = value
    return values_dict


def batch_translate_chromosomes(parameters, chromosomes, generation):
    dicts = []
    for ind, chromosome in enumerate(chromosomes):
        values_dict = translate_chromosome(parameters, chromosome)
        values_dict['run_id'] = str(generation).zfill(4) + '_' + str(ind).zfill(4)
        dicts.append(values_dict)
    return dicts

class Parameters(OrderedDict):
    """docstring for Parameters."""
    def __init__(self):
        super(Parameters, self).__init__(self)

    def add(self, name, lower_bound, upper_bound, precision):
        self[name] = Parameter(name, lower_bound, upper_bound, precision)


class Parameter(object):
    """docstring for Parameter."""
    def __init__(self, name, lower_bound, upper_bound, precision):
        self.name = name
        self.min = lower_bound
        self.max = upper_bound
        self.precision = precision
