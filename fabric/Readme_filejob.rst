1.  make two sql databases for files on grex and roc::

      write_filelist.py /global/scratch/vladpopa/data_analysis/OUT_3D grexfiles.db --remotehost grexhome
      write_filelist.py /tera/vpopa/data_analysis rocfiles.db --user phil --address roc.eos.ubc.ca

    (to access roc first need to do::

      ssh-agent bash
      ssh-agent .ssh/keyfile

1.  Once grexfiles.db and rocfiles.db are created, do::

      python compare_files.py

    which will read the databases, compile dictionaries with all the .nc files (roc)
    and .bin3D files (grex) and find filenames that are on both machines.  Using
    this list of files a "delete_grex.sh" script is output that would delete
    all the grex files that have been converted to netcdf files on roc

