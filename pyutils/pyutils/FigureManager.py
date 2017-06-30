#http://camillescott.github.io/blog/context-managers-for-ipython.html
from __future__ import print_function
import matplotlib.pyplot as plt

class FigManager(object):
    """
       fn (string)= root filename of hardcopy
       exts (list) = list of desired hardcopy types
       show (bool) = True if interactive graphic wanted
    """
    def __init__(self, fn='', exts=['pdf', 'png'],
                show=False, nrows=1, ncols=1,
                figsize=(18,12), tight_layout=True,
                **fig_kwds):

        self.fig, self.ax = plt.subplots(nrows=nrows,
                                         ncols=ncols,
                                         figsize=figsize,
                                         tight_layout=tight_layout,
                                         **fig_kwds)

        self.fn = fn
        self.exts = exts
        self.show = show

    def __enter__(self):

        return self.fig, self.ax

    def __exit__(self, exc_t, exc_v, trace_b):

        if exc_t is not None:
            print('ERROR', exc_t, exc_v, trace_b)

        if self.fn:
            print('saving figure', repr(self.fig))
            for ext in self.exts:
                full_name='{}.{}'.format(self.fn, ext)
                print('saving plotfile: ',full_name)
                self.fig.savefig(full_name)

        if self.show:
            print('showing figure', repr(self.fig))
            plt.show(self.fig)

        print('closing figure', repr(self.fig))
        self.fig.delaxes(self.ax)
        plt.close(self.fig)
        del self.ax
        del self.fig
        
