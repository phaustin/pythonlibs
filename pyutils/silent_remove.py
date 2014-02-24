import os,errno
def silent_remove(filename):
    print("{:s} will be destroyed if it exists".format(filename))
    try:
        os.remove(filename)
    except OSError as e:
        if e.errno != errno.ENOENT: # errno.ENOENT = no such file or directory
            raise # re-raise exception if a different error occured
