#!/home/phil/anaconda3/bin/python
import site,os,scandir
import stat

for subdir,dirs,files in scandir.walk('.'):
    for the_file in files:
        full_name="{}/{}".format(subdir,the_file)
        try:
            fileStats=os.stat(full_name)
            os.chmod(full_name,fileStats.st_mode | stat.S_IROTH | stat.S_IRGRP)
        except (FileNotFoundError,PermissionError):
            print("permission denied for ",full_name)
    print("working on: ",subdir)
    fileStats=os.stat(subdir)
    try:
        os.chmod(subdir,fileStats.st_mode | stat.S_IXOTH |  stat.S_IROTH | stat.S_IXGRP |  stat.S_IRGRP)
    except PermissonError:
        print("permission denied for ",subdir)

            
    ## the_file=dir_dict
    ## if the_file['isDir']:
    ##     os.chmod(the_file['fullname'],st.S_IXUSR | st.S_IWUSR | st.S_IRUSR)
    ## else:
    ##     os.chmod(the_file['fullname'], st.S_IWUSR | st.S_IRUSR)
