"""
module for various subprocess commands
"""
#http://article.gmane.org/gmane.comp.python.general/487577/match=getstatusoutput+subprocess
def command(command):
     """
     usage: status,output=command(commandstring)
     """ 
     from subprocess import Popen, PIPE, STDOUT
     p = Popen(command, stdout=PIPE, stderr=STDOUT, shell=True)
     s = p.stdout.read()
     return p.wait(), s
 
