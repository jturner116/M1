# Traveling salesperson problem

import sys
import argparse
from dataclasses import dataclass
import math
import numpy as np

@dataclass
class City:
    id: int
    x: int
    y: int

# read from file

def readfile(filename):
    with open(filename) as f:
        lines = f.readlines()
    
    num_cities = int(lines[0])
    cities = []

    for line in lines[1:]:
        id, x, y = line.split()
        cities.append(City(int(id), int(x), int(y)))
    return num_cities, cities

def find_dist(path: list, cities: list):
    
    total_dist = 0
    total_dist =+ math.sqrt((cities[path[0]].x - 0)**2 + (cities[path[0]].y - 0)**2)
    for i in range(1, len(path)):
        prev_city = cities[path[i-1]] 
        current_city = cities[path[i]] 
        total_dist += math.sqrt((current_city.x - prev_city.x)**2 + 
                                (current_city.y - prev_city.y)**2)
    last_city = cities[path[-1]]
    total_dist += math.sqrt((last_city.x - 0)**2 + 
                            (last_city.y - 0)**2)
    return total_dist

def best_neighbor(path: list, cities: list):
    best_dist = find_dist(path, cities)
    best_path = path
    # 2-swap method for finding best neighbor
    for i in range(1, len(path)-1):
        for j in range(i+1, len(path)):
            new_path = path.copy()
            new_path[i], new_path[j] = new_path[j], new_path[i]
            print(f'2-swap path: {new_path}')
            new_dist = find_dist(new_path, cities)
            if new_dist < best_dist:
                best_dist = new_dist
                best_path = new_path
    # 2-opt method for finding best neighbor 
    for i in range(1, len(path)-1):
        for j in range(i+1, len(path)):
            new_path = path.copy()
            new_path[i:j] =  new_path[i:j][::-1]
            print(f'2-opt path: {new_path}')
            new_dist = find_dist(new_path, cities)
            if new_dist < best_dist:
                best_dist = new_dist
                best_path = new_path

    return best_path, best_dist

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Traveling salesperson problem')
    parser.add_argument('filename', help='file containing the graph')

    args = parser.parse_args()

    # read from file
    num_cities, cities = readfile(args.filename)

    total_dist = 0

    rand_city_graph = np.random.permutation(num_cities)
    best_path, best_dist = best_neighbor(rand_city_graph, cities)

    print(f'Random path: {rand_city_graph}')
    print(f'Random dist: {find_dist(rand_city_graph, cities)}')
    print(f'Best path: {best_path}')
    print(f'Best dist: {best_dist}')