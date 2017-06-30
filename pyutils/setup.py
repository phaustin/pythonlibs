#!/usr/bin/env python

# Setup script for PyPI; use CMakeFile.txt to build extension modules

from setuptools import setup



setup(
    name='pyutils',
    version=__version__,
    packages=['pyutils'],
    classifiers=[
        'License :: OSI Approved :: BSD License'
    ],
    keywords='C++11, Python bindings',
    long_description="""pybind11""")

