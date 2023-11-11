from mpi4py import MPI
import numpy as np

comm = MPI.COMM_WORLD
rank = comm.Get_rank()
size = comm.Get_size()

SIZE=12

if rank == 0:
    np.random.seed(42)
    tab = np.random.randint(100, size = SIZE, dtype='i')
else:
    tab = None

local_tab = np.zeros(4, dtype='i')
comm.Scatter(tab, local_tab, root=0)

c_max = np.zeros(1, dtype='i')
c_pos = np.zeros(1, dtype='i')
c_max[0] = np.max(local_tab)
c_pos[0] = np.argmax(local_tab) + rank * 4
print("Rank: ", rank, " max: ", c_max, " pos: ", c_pos)

max_pos = np.zeros(size, dtype='i')
max_val = np.zeros(size, dtype='i')

comm.Gather(c_max, max_val, root=0)
comm.Gather(c_pos, max_pos, root=0)

if rank == 0:
    pos = np.argmax(max_val)
    print("Max pos: ", max_pos[pos])
    print("Max val: ", max_val)

