#!/home/phil/anaconda3/bin/python
import site,os,scandir
import stat

for subdir,dirs,files in scandir.walk('.'):
    for the_file in files:
        full_name="{}/{}".format(subdir,the_file)
        try:
            fileStats=os.stat(full_name)
            #os.chmod(full_name,fileStats.st_mode | stat.S_IROTH | stat.S_IRGRP)
            print(full_name)
        except FileNotFoundError:
            pass
        print('subdir: ',subdir)
