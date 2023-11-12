# Sequential version
# python3 n-bodies-base.py 12 1000

import sys
import math
import random
import matplotlib.pyplot as plt
import time
import numpy as np
from mpi4py import MPI

ID, POSX, POSY, SPEEDX, SPEEDY, WEIGHT = range(6)

solarmass=1.98892e30
def circlev(rx, ry):
    r2=math.sqrt(rx*rx+ry*ry)
    numerator=(6.67e-11)*1e6*solarmass
    return math.sqrt(numerator/r2)
# from http://physics.princeton.edu/~fpretori/Nbody/code.htm
def create_item(id, positionx, positiony, speedx, speedy, weight):
    if positionx == 0 and positiony == 0:  # the center of the world, very heavy one...
        speedx = 0
        speedy = 0
    else:
        if speedx==0 and speedy==0:            # initial values
            magv=circlev(positionx, positiony)
            absangle = math.atan(math.fabs(positiony/positionx))
            thetav= math.pi/2-absangle
            phiv = random.uniform(0,1)*math.pi
            speedx = -1*math.copysign(1, positiony)*math.cos(thetav)*magv
            speedy = math.copysign(1, positionx)*math.sin(thetav)*magv
            #Orient a random 2D circular orbit
            if (random.uniform(0,1) <=.5):
                speedx=-speedx
                speedy=-speedy
    return np.array([id, positionx, positiony, speedx, speedy, weight], dtype='f')

def str_item(item):
    return "ID="+str(item[ID])+" POS=("+str(item[POSX])+","+str(item[POSY])+") SPEED=("+str(item[SPEEDX])+","+str(item[SPEEDY])+") WEIGHT="+str(item[WEIGHT])

def display(m, l):
    for i in range(len(l)):
        print("PROC"+str(rank)+":"+m+"-"+str_item(l[i]))

def displayPlot(d):
    plt.gcf().clear()            # to remove to see the traces of the particules...
    plt.axis((-1e17,1e17,-1e17,1e17))
    xx = [ d[i][POSX]  for i in range(len(d)) ]
    yy = [ d[i][POSY]  for i in range(len(d)) ]
    plt.plot(xx, yy, 'ro')
    plt.draw()
    plt.pause(0.00001)            # in order to see something otherwise too fast...


def interaction(i, j):
    dist = math.sqrt( (j[POSX]-i[POSX])*(j[POSX]-i[POSX]) +  (j[POSY]-i[POSY])*(j[POSY]-i[POSY]) )
    if dist == 0:
        return np.zeros(2)
    g = 6.673e-11
    factor = g * i[WEIGHT] * j[WEIGHT] / (dist*dist+3e4*3e4)
    return np.array([factor*(j[POSX]-i[POSX])/dist, factor*(j[POSY]-i[POSY])/dist])

def update(d, f):
    dt = 1e11
    vx = d[SPEEDX] + dt * f[0]/d[WEIGHT]
    vy = d[SPEEDY] + dt * f[1]/d[WEIGHT]
    px = d[POSX] + dt * vx
    py = d[POSY] + dt * vy
    return create_item(d[ID], positionx=px, positiony=py, speedx=vx, speedy=vy, weight=d[WEIGHT])

def signature(world):
    s = 0
    for d in world:
        s+=d[POSX]+d[POSY]
    return s

def init_world(n):
    data = [ create_item(id=i, positionx=1e18*math.exp(-1.8)*(.5-random.uniform(0,1)), positiony=1e18*math.exp(-1.8)*(.5-random.uniform(0,1)), speedx=0, speedy=0, weight=(random.uniform(0,1)*solarmass*10+1e20)) for i in range(n-1)]
    data.append( create_item(id=nbbodies-1, positionx=0, positiony=0, speedx=0, speedy=0, weight=1e6*solarmass))
    return np.array(data)

nbbodies = int(sys.argv[1])
NBSTEPS = int(sys.argv[2])
DISPLAY = len(sys.argv) != 4

# Modify only starting here (and in the imports)
comm = MPI.COMM_WORLD
rank = comm.Get_rank()
size = comm.Get_size()

random.seed(0)

if rank == 0:
    data = init_world(nbbodies)  # Assuming nbbodies is predefined
else:
    data = None

# Determine the local size for each process
local_nbbodies = nbbodies // size + (nbbodies % size > rank)
local_data = np.empty((local_nbbodies, 2), dtype='f')

# Scatter data to all processes
comm.Scatter(data, local_data, root=0)

start_time = time.time()

for t in range(NBSTEPS):
    # Initialize force array locally
    local_force = np.zeros((local_nbbodies, 2))

    # Gather all data to each process (needed for force calculation)
    all_data = np.empty((nbbodies, 2), dtype='f')
    comm.Allgather(local_data, all_data)

    # Calculate forces locally
    for i in range(local_nbbodies):
        for j in range(nbbodies):
            local_force[i] += interaction(all_data[i], all_data[j])

    # Update local data
    for i in range(local_nbbodies):
        local_data[i] = update(local_data[i], local_force[i])

# Gather updated data back to root
final_data = None
if rank == 0:
    final_data = np.empty((nbbodies, 2), dtype='f')

comm.Gather(local_data, final_data, root=0)

if rank == 0:
    duration = time.time() - start_time
    print("Duration: ", duration)
    print("Signature of the world:")
    print(signature(final_data))  # Assuming signature is a defined function




