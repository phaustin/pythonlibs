#!/usr/bin/env python

# Setup script for PyPI; use CMakeFile.txt to build extension modules

from setuptools import setup


setup(
    name='pyutils',
    packages=['pyutils'],
    classifiers=[
        'License :: OSI Approved :: BSD License'
    ],
    entry_points={
          'console_scripts': [
              'get_space = pyutils.get_space:main',
              'killprocs = pyutils.killprocs:main',
              'run_shrink = pyutils.run_shrink:main',
              'crossref = pyutils.crossref:main'
          ]
    },
    install_requires=['psutil','numpy'],
    keywords='small utilities',
    long_description="""utilities""")
