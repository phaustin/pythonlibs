#!/usr/bin/env python

# Setup script for PyPI; use CMakeFile.txt to build extension modules

from setuptools import setup

setup(
    name='diskinventory',
    version= 0.1,
    packages=['diskinventory'],
    classifiers=[
        'License :: OSI Approved :: BSD License'
    ],
    keywords='C++11, Python bindings',
    long_description="""pybind11""")
