import sys
from mpi4py import MPI
import numpy as np

comm = MPI.COMM_WORLD
rank = comm.Get_rank()
size = comm.Get_size()


def cumul(a, b):
    return np.array(sum(range(a, b)), dtype='i')

number = int(sys.argv[1])
if rank == 0:
    total_sum = np.zeros(1, dtype='i')
else:
    total_sum = None

low_bound = number // size * rank
high_bound = number // size * (rank + 1)

partial_sum = cumul(low_bound, high_bound)

comm.Reduce(partial_sum, total_sum, op=MPI.SUM, root=0)
print("Total sum on ", rank, " = ", total_sum)