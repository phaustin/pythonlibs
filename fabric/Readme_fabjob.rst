Working with fabric
-------------------


fabfile.py::

  fabric phil@dhcp-128-189-68-219% fab -l
  Available commands:

      ls_scratcho

  fabric phil@dhcp-128-189-68-219% fab ls_scratch
  [grexhome] Executing task 'ls_scratch'
  [grexhome] run: ls -lR /home/paustin/repos/dotfiles
  [grexhome] out: 	 Loading cmake 2.8.11 
  [grexhome] out: 
  [grexhome] out: 	Loading module: hdf5-1.8.11-intel
  [grexhome] out: 
  [grexhome] out: 
  [grexhome] out: 
  [grexhome] out: 	Loading module: netcdf/4.2.1.1-intel
  [grexhome] out: 
  [grexhome] out: 
  [grexhome] out: 
  [grexhome] out: 	 Loading cmake 2.8.11 
  [grexhome] out: 
  [grexhome] out: 	Loading module: netcdf/4.2.1.1-intel
  [grexhome] out: 
  [grexhome] out: 
  [grexhome] out: 
  [grexhome] out: /home/paustin/repos/dotfiles:
  [grexhome] out: total 52
  [grexhome] out: -rw-rw-r-- 1 paustin paustin  9993 Feb 19 13:54 dot_abbrev_defs
  [grexhome] out: -rw-rw-r-- 1 paustin paustin  1309 Feb 19 13:54 dot_alias
  [grexhome] out: -rw-rw-r-- 1 paustin paustin  2477 Feb 19 13:54 dot_bashrc
  [grexhome] out: -rw-rw-r-- 1 paustin paustin 13088 Feb 19 13:54 dot_emacs
  [grexhome] out: -rw-rw-r-- 1 paustin paustin   943 Feb 19 13:54 dotgitconfig
  [grexhome] out: -rw-rw-r-- 1 paustin paustin   997 Feb 19 13:54 dot_gitconfig
  [grexhome] out: -rw-rw-r-- 1 paustin paustin   682 Feb 19 13:54 dot_hgrc
  [grexhome] out: -rw-rw-r-- 1 paustin paustin    67 Feb 19 13:54 dot_xpdfrc
  [grexhome] out: 


  Done.
  Disconnecting from paustin@grex.westgrid.ca... done.

  
fabjob.py::

  fabric phil@dhcp-128-189-68-219% python fabjob.py
  writing script to /var/folders/xw/gs1bbq9n6213d1mf4v8qhsnm0000gn/T/tmpXWnzjX
  [grexhome] Executing task 'grex_call'
  [grexhome] run: echo "sentry";echo $HOME;echo "sentry"
  [grexhome] Executing task 'grex_call'
  [grexhome] run: mkdir -p /home/paustin/scriptdir
  /var/folders/xw/gs1bbq9n6213d1mf4v8qhsnm0000gn/T/tmpOWsxhv
  /var/folders/xw/gs1bbq9n6213d1mf4v8qhsnm0000gn/T/tmp5lTgnA
  [grexhome] Executing task 'grex_call'
  [grexhome] run: bash /home/paustin/scriptdir/scriptfile
  date                                          directory links                                     name    owner  permission      size theGroup
  0    1392868476                       /home/paustin/repos/dotfiles     1                          dot_abbrev_defs  paustin  -rw-rw-r--      9993  paustin
  1    1392868476                       /home/paustin/repos/dotfiles     1                                dot_alias  paustin  -rw-rw-r--      1309  paustin
  2    1392868476                       /home/paustin/repos/dotfiles     1                               dot_bashrc  paustin  -rw-rw-r--      2477  paustin
  3    1392868476                       /home/paustin/repos/dotfiles     1                                dot_emacs  paustin  -rw-rw-r--     13088  paustin
  4    1392868476                       /home/paustin/repos/dotfiles     1                             dotgitconfig  paustin  -rw-rw-r--       943  paustin
  5    1392868476                       /home/paustin/repos/dotfiles     1                            dot_gitconfig  paustin  -rw-rw-r--       997  paustin
  6    1392868476                       /home/paustin/repos/dotfiles     1                                 dot_hgrc  paustin  -rw-rw-r--       682  paustin
  7    1392868476                       /home/paustin/repos/dotfiles     1                               dot_xpdfrc  paustin  -rw-rw-r--        67  paustin
  8    1392352789                      /home/paustin/repos/sam_devel     1                                    Build  paustin  -rwxrwxr-x      2694  paustin
  9    1392274276                      /home/paustin/repos/sam_devel     1                                  #Build#  paustin  -rwxrwxr-x      2685  paustin
  10   1392273255                      /home/paustin/repos/sam_devel     1                                 CaseName  paustin  -rw-rw-r--         8  paustin
  11   1392354450                      /home/paustin/repos/sam_devel     1                           CMakeLists.txt  paustin  -rw-rw-r--      4883  paustin
  12   1392354450                      /home/paustin/repos/sam_devel     1                          CMakeLists.txt~  paustin  -rw-rw-r--      3370  paustin
  13   1388840687                      /home/paustin/repos/sam_devel     1                        load_namelist.f90  paustin  -rw-rw-r--      2331  paustin
  14   1392273255                      /home/paustin/repos/sam_devel     1                                 M
