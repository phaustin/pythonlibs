#!/usr/bin/env python
"""
  run the a bash script on a remote using fabric,
  the script runs the command:

  ls -R -l -Q --time-style=full-iso --time=status targetdir

  and saves the output into a dataframe with columns

  columnNames=['permission','links','owner','theGroup','size','date','directory','name']

  examples:

using .ssh/config  (works on grex)

    write_filelist.py /global/scratch/vladpopa/data_analysis/OUT_3D grexfiles.db --remotehost grexhome

using ssh-agent  (roc requires ssh-agent)
    write_filelist.py /tera/vpopa/data_analysis rocfiles.db --user phil --address roc.eos.ubc.ca    
  
"""
from tempfile import NamedTemporaryFile as mkfile
from fabric.api import env, run, execute, hide, put, cd
from fabric.decorators import runs_once
import textwrap,subprocess,shlex
import os, cStringIO
from parse_ls import read_ls
import dataset
import argparse

import os,errno

def silent_remove(filename):
    print("{:s} will be destroyed if it exists".format(filename))
    try:
        os.remove(filename)
    except OSError as e:
        if e.errno != errno.ENOENT: # errno.ENOENT = no such file or directory
            raise # re-raise exception if a different error occured

def remote_wrapper(command):
    """
      return a function that, when called,
      runs a shell command using fabric.api.run
      and returns the output
    """
    def remote_call():
        #run this on remote
        output=run(command)
        return output
    return remote_call

def command(the_command):
    """
       execute a local command, capturing the output of
       stdout and stderr 
    """
    try:
        with mkfile(delete=False) as  stdout:
            with mkfile(delete=False) as  stderr:
                subprocess.check_call(shlex.split(the_command),stderr=stderr,stdout=stdout)
    except subprocess.CalledProcessError:
        print "received output, check stdout and stderr files"
    return (stdout.name,stderr.name)
            
def write_database(filename,dframe):
    dbstring='sqlite:///{:s}'.format(filename)
    silent_remove(filename)
    db = dataset.connect(dbstring)
    the_table = db.create_table('files')
    for i,item in dframe.iterrows():
        the_table.insert(dict(item))    
    return db

if __name__=="__main__":
    #write_filelist.py  grexhome /home/paustin/repos/dotfiles dotfiles.db
    linebreaks=argparse.RawTextHelpFormatter
    descrip=textwrap.dedent(globals()['__doc__'])
    parser = argparse.ArgumentParser(formatter_class=linebreaks,description=descrip)
    parser.add_argument('remotedir', nargs=1,type=str,help='directory to be listed')
    parser.add_argument('database', nargs=1,type=str,help='database name (with or without path)')
    parser.add_argument('--remotehost', nargs='?', type=str,help='ssh name for remote machine')
    parser.add_argument('--user', nargs='?', type=str,help='ssh user')
    parser.add_argument('--address', nargs='?', type=str,help='ssh address')
    args=parser.parse_args()

    remotedir=args.remotedir[0]
    database=args.database[0]


    do_user=False
    if args.remotehost:
        env.use_ssh_config = True
        homedir=os.getenv('HOME')
        address=args.remotehost
        env.ssh_config_path='%s/.ssh/config' % homedir
    else:
        env.use_ssh_config = False
        do_user=True
        address=args.address
        env.user=args.user
        print "setting user=%s, host=%s" % (env.user,address)

    print "logging onto %s: " % address
    env.hosts = [address]

    import re

    #
    # this re will extract the home directory
    #
    get_payload=re.compile(".*sentry(.*)sentry.*",re.DOTALL)


    #
    # here is the script we want to execute on remote
    #
    the_script=\
    """
    echo "sentry"
    ls -R -l -Q --time-style=full-iso --time=status {target:s}
    echo "sentry"
    """
    #/global/scratch/vladpopa/data_analysis/OUT_3D
    #/tera/vpopa/data_analysis/OUT_3D
    #
    # write the script to a tmpfile
    #
    the_script=textwrap.dedent(the_script)
    the_script=the_script.strip()
    the_script=the_script.format(target=remotedir)
    with mkfile(delete=False) as scriptfile:
        print "writing script to %s" % scriptfile.name
        scriptfile.write(the_script)
        scriptfile.write('\n')
    #
    # find the HOME directory on remote
    #
    the_command='echo "sentry";echo $HOME;echo "sentry"'
    run_command=remote_wrapper(the_command)
    with hide('output'):
        test=execute(run_command)
        find_home=get_payload.match(test[address])
        remote_home=find_home.groups(1)[0].strip()
    print "succeeded in finding homedir=",remote_home
    #
    # make a directory to contain the scriptfile on remote
    #
    the_command='mkdir -p %s/scriptdir' % remote_home
    run_command=remote_wrapper(the_command)
    with hide('output'):
        test=execute(run_command)
    #
    #
    # scp the file to remote
    #
    if do_user:
        the_command="scp {scriptfile:s} {user:s}@{host:s}:{home:s}/scriptdir/scriptfile".\
            format(scriptfile=scriptfile.name,user=env.user,host=address,home=remote_home)
    else:
        the_command="scp {scriptfile:s} {host:s}:{home:s}/scriptdir/scriptfile".\
          format(scriptfile=scriptfile.name,host=address,home=remote_home)
    stdout,stderr=command(the_command)
    print stdout
    print stderr
    #
    # run the script we scp'd to scriptfile  on remote using bash
    #
    the_command='bash %s/scriptdir/scriptfile' % remote_home
    run_command=remote_wrapper(the_command)
    with hide('output'):
        test=execute(run_command)
        file_list=get_payload.match(test[address])
        file_list=file_list.groups(1)[0].strip()
        listfile=cStringIO.StringIO(file_list)
        df=read_ls(listfile)
        #print df.to_string()
        write_database(database,df)
    
            
            
    
    
