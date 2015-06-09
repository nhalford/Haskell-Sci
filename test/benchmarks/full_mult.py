import numpy as np

m1 = np.zeros((100,100),int)
np.fill_diagonal(m1,range(1,101))

m2 = np.zeros((100,100),int)
np.fill_diagonal(m2,range(101,201))

p = m1 * m2

print p
