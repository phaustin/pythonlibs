#https://pythonhosted.org/psutil/
import psutil


keepit={}
for proc in psutil.process_iter():
    try:
        try:
            fullname = proc.exe()
            pinfo = proc
            keepit[proc.pid] = (proc.create_time(),proc.exe(),proc.cmdline(),proc)
        except Exception as e:
            print("Exception: ",e)
    except psutil.NoSuchProcess:
        pass
    else:
        pass

def on_terminate(proc):
    print("process {} terminated with exit code {}".format(proc, proc.returncode))

proclist=[]    
for create,name,cmdline,proc in keepit.values():
    if name.find('mini3') > -1:
        proclist.append((create,name,cmdline,proc))

proclist.sort()        
for item in proclist:
    print(item)
        
# for p in proclist:
#     p.terminate()
    
# gone, alive = psutil.wait_procs(proclist, timeout=3, callback=on_terminate)
    
# for p in alive:
#     p.kill()
