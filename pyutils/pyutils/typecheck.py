import numpy as np

def is_iter(obj):
    """
    return true if iterable and not a string
    """
    return hasattr(obj, '__iter__') and not isinstance(obj, str)
 
def all_scalar(*args):
    """
      return true if every argument is a scalar
    """
    allscalar=True
    for item in args:
        allscalar = allscalar & np.isscalar(item)
    return allscalar
