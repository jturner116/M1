# python3 ./number.py 42

from mpi4py import MPI
comm = MPI.COMM_WORLD
import numpy as np

rank = comm.Get_rank()

vec = np.zeros(1, dtype="i")

if rank == 0:
    vec[0] = 42
    print("From process of rank 0 the passnumber is", vec[0])
else:
    print("From process of rank", rank, "the passnumber is", vec[0])

comm.Bcast(vec, root=0)

print("After collective in process of rank", rank, "the passnumber is", vec[0])

