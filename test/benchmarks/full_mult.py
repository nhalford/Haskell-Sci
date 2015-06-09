import numpy as np

m1 = np.loadtxt("rand1.txt")
m2 = np.loadtxt("rand2.txt")

p = np.dot(m1,m2)

print p
