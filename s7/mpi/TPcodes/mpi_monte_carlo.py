from mpi4py import MPI
import numpy as np
import time
import random

comm = MPI.COMM_WORLD
rank = comm.Get_rank()
size = comm.Get_size()

NB = 10000001
inside = np.zeros(1, dtype='i')
random.seed(rank)
if rank == 0:
    start = time.time()
for _ in range(NB//size):
    x = random.random()
    y = random.random()
    if x*x + y*y <= 1:
        inside[0] +=1

print("Rank: ", rank, " inside: ", inside)

insides = np.zeros(size, dtype='i')

comm.Gather(inside, insides, root=0)

if rank == 0:
    end = time.time()
    print("Pi =", 4 * sum(insides)/NB, "in ", end-start, 'seconds')