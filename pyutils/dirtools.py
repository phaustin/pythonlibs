import os,errno
def silent_remove(filename):
    """remove a directory without failure
       if directory doesn't exist
    """
    print("{:s} will be destroyed if it exists".format(filename))
    try:
        os.remove(filename)
    except OSError as e:
        if e.errno != errno.ENOENT: # errno.ENOENT = no such file or directory
            raise # re-raise exception if a different error occured

def new_dir(the_dir):
    """create a directory without failure if
       directory already exists
    """
    try:
        os.makedirs(the_dir)
    except OSError, e:
        if e.errno == errno.EEXIST:
            pass  #not a problem if file exists

#http://stackoverflow.com/questions/10840533/most-pythonic-way-to-delete-a-file-which-may-not-exist
# import contextlib
# with contextlib.suppress(FileNotFoundError):
#     os.remove(filename)

#https://docs.python.org/3/library/pathlib.html
#http://stackoverflow.com/questions/23924223/python-shutil-rmtree-doesnt-work-with-windows-library
#https://docs.python.org/3.5/library/shutil.html

# import os, stat
# import shutil

# def remove_readonly(func, path, _):
#     "Clear the readonly bit and reattempt the removal"
#     os.chmod(path, stat.S_IWRITE)
#     func(path)

# shutil.rmtree(directory, onerror=remove_readonly)
