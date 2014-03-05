#!/usr/bin/env python
from fabric.api import env, run, execute, hide, put, cd
import os

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


if __name__=="__main__":

    env.hosts = ['roc.eos.ubc.ca']
    env.user = 'phil'
    #env.use_ssh_config = True
    env.password="fdLfums!"
    homedir=os.getenv('HOME')
    env.ssh_config_path='%s/.ssh/config' % homedir
    print "config is at: ",env.ssh_config_path

    the_command='echo "sentry";echo $HOME;echo "sentry"'
    run_command=remote_wrapper(the_command)
    test=execute(run_command)
    print test
    

