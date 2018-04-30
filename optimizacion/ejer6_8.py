import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D

#ejes
[x0,x1]=np.meshgrid(np.linspace(0,50,num=100),np.linspace(-50,50,num=100))

def grafica (c=0,b=10):
	fig = plt.figure()
	ax = fig.add_subplot(111, projection='3d')
	z=c+b*x0
	ax.plot_surface(x0,x1,z)
	plt.xlabel('x0')
	plt.ylabel('x1')
	plt.title('Para c=' + str(c))
	plt.show()
