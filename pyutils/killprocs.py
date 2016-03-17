#https://pythonhosted.org/psutil/
import psutil
from pyutils.helper_funs import make_tuple
import sys

def on_terminate(proc):
    print("process {} terminated with exit code {}".format(proc, proc.returncode))


if __name__ == "__main__":

    keepit={}
    keys=['time','name','cmdline','proc']
    for proc in psutil.process_iter():
        try:
            the_dict=dict(zip(keys,(proc.create_time(),proc.exe(),proc.cmdline(),proc)))
            keepit[proc.pid] = make_tuple(the_dict)
        except (psutil.ZombieProcess, psutil.AccessDenied,psutil.NoSuchProcess):
            pass


    proclist=[]
    #
    # don't kill this process or the emacs python parser
    #
    for the_tup in keepit.values():
        if the_tup.name.find('mini3') > -1 and the_tup.cmdline[-1].find('killprocs') == -1 and \
            the_tup.cmdline[-1].find('elpy') == -1:
            proclist.append(the_tup)

    proconly = [item.proc for item in proclist]
    [proc.terminate() for proc in proconly]

    gone, alive = psutil.wait_procs(proconly, timeout=3, callback=on_terminate)

    for p in alive:
        p.kill()
