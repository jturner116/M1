# Traveling salesperson problem
# Jonah Turner
# TPA11 IAFA
import argparse
from dataclasses import dataclass
import math
import numpy as np
from time import time


@dataclass
class City:
    id: int
    x: int
    y: int


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
    total_dist = +math.sqrt((cities[path[0]].x - 0) ** 2 + (cities[path[0]].y - 0) ** 2)
    for i in range(1, len(path)):
        prev_city = cities[path[i - 1]]
        current_city = cities[path[i]]
        total_dist += math.sqrt(
            (current_city.x - prev_city.x) ** 2 + (current_city.y - prev_city.y) ** 2
        )
    last_city = cities[path[-1]]
    total_dist += math.sqrt((last_city.x - 0) ** 2 + (last_city.y - 0) ** 2)
    return total_dist


def best_neighbor(path: list, cities: list):
    best_dist = find_dist(path, cities)
    best_path = path
    # 2-swap method for finding best neighbor
    for i in range(1, len(path) - 1):
        for j in range(i + 1, len(path)):
            new_path = path.copy()
            new_path[i], new_path[j] = new_path[j], new_path[i]
            new_dist = find_dist(new_path, cities)
            if new_dist < best_dist:
                best_dist = new_dist
                best_path = new_path
    # 2-opt method for finding best neighbor
    for i in range(1, len(path) - 1):
        for j in range(i + 1, len(path)):
            new_path = path.copy()
            new_path[i:j] = new_path[i:j][::-1]
            new_dist = find_dist(new_path, cities)
            if new_dist < best_dist:
                best_dist = new_dist
                best_path = new_path

    return best_path, best_dist


def steepest_hill_climbing(path: list, cities: list, max_iter=10):
    best_dist = find_dist(path, cities)
    best_path = path
    temp_iter = max_iter
    print(f"SHC Max Iter: {max_iter}")
    while temp_iter > 0:
        temp_iter -= 1
        new_path, new_dist = best_neighbor(best_path, cities)
        if new_dist < best_dist:
            best_dist = new_dist
            best_path = new_path
        else:
            break
    return best_path, best_dist, max_iter - temp_iter


def iter_shc(path: list, cities: list, max_essais=100):
    best_dist = find_dist(path, cities)
    best_path = path
    essai_trouve = 0

    for i in range(max_essais):
        new_path = np.random.permutation(path)
        new_path, new_dist, _ = steepest_hill_climbing(new_path, cities, max_essais)
        if new_dist < best_dist:
            essai_trouve = i
            best_dist = new_dist
            best_path = new_path
    return best_path, best_dist, essai_trouve


def generate_2opt_neighbors(path, cities, tabou_list):
    neighbors = []
    for i in range(1, len(path) - 2):
        for j in range(i + 1, len(path)):
            if j - i == 1:
                continue  # Skip adjacent edges as it doesn't change the path
            new_path = path[:i] + path[i:j][::-1] + path[j:]
            if new_path not in tabou_list:
                new_dist = find_dist(new_path, cities)
                neighbors.append((new_path, new_dist))
    return neighbors


def tabu_search(path: list, cities: list, max_iter=20, tabou_size=10):
    best_dist = find_dist(path, cities)
    best_path = path
    tabou_list = [path]
    for i in range(max_iter):
        neighbors = generate_2opt_neighbors(best_path, cities, tabou_list)
        if not neighbors:  # Check if neighbors list is empty
            break
        best_neighbor = min(neighbors, key=lambda x: x[1])
        best_path = best_neighbor[0]
        best_dist = best_neighbor[1]
        tabou_list.append(best_path)
        if len(tabou_list) > tabou_size:
            tabou_list.pop(0)
    return best_path, best_dist


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Traveling salesperson problem")
    parser.add_argument("filename", help="file containing the graph")
    parser.add_argument("max_essais", help="maximum number of iterations", default=10)
    parser.add_argument(
        "tabou_iters", help="number of iterations for tabou search", default=200
    )

    args = parser.parse_args()

    # read from file
    num_cities, cities = readfile(args.filename)
    start = time()
    max = int(args.max_essais)
    tabou_iters = int(args.tabou_iters)

    total_dist = 0

    rand_city_graph = np.random.permutation(num_cities).tolist()
    best_path, best_dist = best_neighbor(rand_city_graph, cities)
    print(f"Random path: {rand_city_graph}")
    print(f"Random dist: {find_dist(rand_city_graph, cities)}")
    print("----------------------")

    shc_path, shc_dist, iters_used = steepest_hill_climbing(
        rand_city_graph, cities, max
    )
    print(f"SHC Best neighbor path: {shc_path}")
    print(f"SHC Best neighbor dist: {shc_dist}")
    print(f"SHC Iterations used: {iters_used}")

    iter_shc_path, iter_shc_dist, iter_shc_essai = iter_shc(
        rand_city_graph, cities, max
    )

    print("----------------------")
    print(f"Iter SHC Best neighbor path: {iter_shc_path}")
    print(f"Iter SHC Best neighbor dist: {iter_shc_dist}")
    print(f"Iter SHC Essai found: {iter_shc_essai}")
    print("----------------------")
    print("Tabou search:")
    tabou_path, tabou_dist = tabu_search(rand_city_graph, cities, tabou_iters, 200)
    print(f"Tabou path: {tabou_path}")
    print(f"Tabou dist: {tabou_dist}")
    print(f"Total time: {time() - start}")
