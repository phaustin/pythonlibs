#http://stackoverflow.com/questions/1606436/adding-docstrings-to-namedtuples-in-python
from collections import namedtuple

def my_namedtuple(typename, field_names, verbose=False,
                 rename=False, docstring=''):
    '''Returns a new subclass of namedtuple with the supplied
       docstring appended to the default one.

    >>> Point = my_namedtuple('Point', 'x, y', docstring='A point in 2D space')
    >>> print Point.__doc__
    Point(x, y):  A point in 2D space
    '''
    # create a base class and concatenate its docstring and the one passed
    _base = namedtuple(typename, field_names, verbose, rename)
    _docstring = ''.join([_base.__doc__, ':  ', docstring])

    # fill in template to create a no-op subclass with the combined docstring
    template = '''class subclass(_base):
        %(_docstring)r
        pass\n''' % locals()

    # execute code string in a temporary namespace
    namespace = dict(_base=_base, _docstring=_docstring)
    try:
        exec template in namespace
    except SyntaxError, e:
        raise SyntaxError(e.message + ':\n' + template)

    return namespace['subclass']  # subclass object created
    
