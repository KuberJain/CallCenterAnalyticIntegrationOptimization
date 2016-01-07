# Required Libraries
from matplotlib import pyplot as plt
import numpy as np
plt.style.use('ggplot')

# Visualization Functions
def lineplot(x, y, title, xlabel, ylabel, line_width):
    plt.plot(x, y, linestyle='-', linewidth=line_width,
                 color='#199DD9')
    plt.xlabel(xlabel)
    plt.ylabel(ylabel)
    plt.title(title)
    plt.legend()
    plt.tight_layout()
    plt.show()
    return None

def barplot(x,y, title, xlabel, ylabel, bar_width):
    index = np.arange(len(x))
    plt.bar(index, y, width=bar_width, color='#199DD9')
    plt.xlabel(xlabel)
    plt.ylabel(ylabel)
    plt.title(title)
    plt.xticks(index + bar_width/2, x, rotation='vertical')
    plt.legend()
    plt.tight_layout()
    plt.show()
    return None

def sidebysidebarplot(x,y,z, title, xlabel, ylabel, zlabel, trendline, bar_width):
    index = np.arange(len(x))
    plt.bar(index-0.15-bar_width/2, y, width=bar_width, color='#A6A8AB', label = ylabel)
    plt.bar(index-0.15+bar_width/2, z, width=bar_width, color='#199DD9', label = zlabel)
    plt.axvline(x=x.tolist().index(trendline), linewidth=2, color='red')
    plt.xlabel(xlabel)
    plt.ylabel('Percentage')
    plt.title(title)
    plt.xticks(index, x)
    plt.legend()
    plt.tight_layout()
    plt.show()
    return None