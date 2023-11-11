# python3 stretching-base.py
import numpy as np
import matplotlib.image as mpimg
import matplotlib.pyplot as plt 
import matplotlib.cm as cm
from mpi4py import MPI

# MPI initialization
comm = MPI.COMM_WORLD
rank = comm.Get_rank()
size = comm.Get_size()


M = 255

# First method for stretching contrast
def f_one(x,n):
        if x==0:
                return 0
        return int(M**(1-n) * (x**n))

# Second method for stretching contrast
def f_two(x,n):
        if x==0:
                return 0
        return int((M**((n-1)/n)) * (x**(1/n)))

# Converts an image to grayscale
def rgb2gray(rgb):
        return np.dot(rgb[...,:3], [0.299, 0.587, 0.114])

# Loads an image on disk named "image.png" and convert it to greyscale, and shows it
def readImage():
        img = mpimg.imread('image.png')
        plt.imshow(img)
        print("Press 'q' to continue")
        plt.show()
        grey = rgb2gray(img)
        pixels, nblines, nbcolumns = (np.ravel(grey)*255).astype(np.int32), len(grey), len(grey[0])
        return pixels, nblines, nbcolumns

# Saves the image in "image-grey2-stretched.png" and shows it
def saveImage(newP, nblines, nbcolumns):
        newimg = newP.reshape((nblines, nbcolumns))
        plt.imshow(newimg, cmap = cm.Greys_r)
        print("Press 'q' to continue")
        plt.show()
        mpimg.imsave('image-grey2-stretched.png', newimg, cmap = cm.Greys_r )

print("Starting stretching...")

# load the image
if rank == 0:
        pixels, nblines, nbcolumns = readImage()
        print("Pixels shape: ", pixels.shape)
        nblines = np.array([nblines], dtype=np.int32)
        nbcolumns = np.array([nbcolumns], dtype=np.int32)
        print("nblines: ", nblines)
else:
    pixels = None
    nblines = np.zeros(1, dtype=np.int32)
    nbcolumns = np.zeros(1, dtype=np.int32)

# broadcast the image size using comm.Bcast
comm.Bcast(nblines, root=0)
comm.Bcast(nbcolumns, root=0)

nblines = nblines[0] // size
nbcolumns = nbcolumns[0]

local_pixels = np.zeros(nblines * nbcolumns, dtype=np.int32)

# scatter the image
comm.Scatter(pixels, local_pixels, root=0)

print("Rank", rank, "has", local_pixels.size)
# compute min and max of pixels
pix_min = np.min(local_pixels)
pix_max = np.max(local_pixels)


# compute alpha, the parameter for f_* functions
local_alpha = 1+(pix_max - pix_min) / M
print("Rank", rank, "has alpha =", local_alpha)
print(local_pixels.shape)
# stretch contrast for all pixels. f_one and f_two are the two different methods
for i in range(0,len(local_pixels)):
        if rank % 2 == 0:
                local_pixels[i] = f_one(local_pixels[i], local_alpha)
        else:
	        local_pixels[i] = f_two(local_pixels[i], local_alpha)


#gather the image
comm.Gather(local_pixels, pixels, root=0)

# save the image
if rank == 0:
        saveImage(pixels, nblines, nbcolumns)
print("Stretching done...")


