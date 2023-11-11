from mpi4py import MPI
import numpy as np

comm = MPI.COMM_WORLD

rank = comm.Get_rank()

# np.random.seed(0)

numbers = np.random.randint(100, size=10, dtype='i')

max = np.max(numbers)
min = np.min(numbers)

print("From process of rank", rank, "the max is", max, "and the min is", min)