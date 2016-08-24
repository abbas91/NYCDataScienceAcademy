"""
A simple example of an animated plot
"""
import numpy as np
import math
from mpl_toolkits.mplot3d import Axes3D
from matplotlib import cm
import matplotlib.pyplot as plt
import matplotlib.animation as animation

def func(x,y):
    return (y**2+x**2) + 65

def func_grad(x,y):
    return (2*x, 2*y)


def init():
    c='r'
    ax.plot_surface(X, Y, Z, rstride=1, cstride=1, 
                    cmap=cm.coolwarm, linewidth=0.1, alpha=0.3)
    ax.set_zlim(-5, 120)
    ax.scatter(-5, 5, func(-5, 5), c=c, marker='o' )

    lst = []
    theta = np.linspace(0, 2*math.pi, 100)
    for i in range(15):
        xt = -5; yt = 5; eta=0.18; 
        for j in range(i):
            gx, gy = func_grad(xt, yt)
            xt = xt - eta*gx
            yt = yt - eta*gy

        r = np.sqrt(func(xt, yt)-65)
        ax.plot(r*np.sin(theta), r*np.cos(theta), 0, c='green', alpha=0.3)
        #ax.plot(r*np.sin(theta[-15:-10]), r*np.cos(theta[-15:-10]), 0, c='Darkred', alpha=0.5)
    

    r = np.sqrt((5**2)*2)
    rng=20
    theta = np.linspace((-1./4-1./rng)*math.pi, (-1./4+1./rng)*math.pi, 100)
    ax.scatter(-5, 5, 0, c='blue', color='blue',  marker='o', s =20, alpha=1)
    ax.plot(r*np.sin(theta), r*np.cos(theta), 0, c='blue', alpha=0.6)






    return ax

def animate(i):
    if i!=0:
        c='r'; xt = -5; yt = 5; eta=0.18
        x_last = xt; y_last = yt;
        for j in range(i-1):
            gx, gy = func_grad(xt, yt)
            xt = xt - eta*gx
            yt = yt - eta*gy

        x_last = xt; y_last = yt;
        gx, gy = func_grad(xt, yt)
        xt = xt - eta*gx
        yt = yt - eta*gy
        r = np.sqrt(func(xt, yt)-65)
        ax.scatter(xt, yt, func(xt, yt), c=c, marker='o' )
        ax.scatter(xt, yt, 0, c='blue', color='blue',  marker='o', s =8, alpha=1)
        ax.plot([x_last, xt], [y_last, yt], [0, 0], c = 'blue', alpha=0.6)
        rng=30
        theta = np.linspace((-1./4-1./rng)*math.pi, (-1./4+1./rng)*math.pi, 100)
        ax.plot(r*np.sin(theta), r*np.cos(theta), 0, c='blue', alpha=0.6)
        return ax
    else:
        return None



if __name__ == "__main__":
    
    fig  = plt.figure(figsize=(12,7))
    ax = fig.gca(projection='3d', elev=40., azim= -25)
    X, Y = np.meshgrid(np.arange(-6, 6, 0.25), np.arange(-6, 6, 0.25))
    Z = func(X,Y)

    ani = animation.FuncAnimation(fig, animate, np.arange(15), init_func=init,
                              interval=2500, blit=False, repeat=False)


    plt.show()
