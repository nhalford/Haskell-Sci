import numpy as np
from scipy.sparse import dia_matrix

m1 = dia_matrix((range(1,101),[0]),shape=(100,100))
m2 = dia_matrix((range(101,201),[0]),shape=(100,100))

p = np.dot(m1,m2)

print p
