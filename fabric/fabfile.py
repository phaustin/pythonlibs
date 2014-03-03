#
#  fab -l
#  fab ls_scratch
#
from fabric.api import env, run
env.use_ssh_config = True
env.ssh_config_path='/Users/phil/.ssh/config'

env.hosts = ['grexhome']

def ls_scratch():
    output=run('ls -lR /home/paustin/repos/dotfiles')


