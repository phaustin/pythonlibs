from distutils.core import setup
from distutils.extension import Extension
from Cython.Distutils import build_ext
import numpy

setup(name        = 'bright',
      ext_modules = [Extension('bright', ['bright.pyx'],
                               include_dirs = [numpy.get_include(),'.'],
                               extra_compile_args=['-O3'],
                               library_dirs=['.','/usr/local/lib/gcc/x86_64-apple-darwin13.4.0/4.9.2'],
                               libraries=['fbright','gfortran'])],
      cmdclass    = {'build_ext': build_ext},
      )

