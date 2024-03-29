#walk a directory tree, saving statistics about
#each file and directory
#then print the sorted list
#fast walker at http://effbot.org/librarybook/os-path.htm example 4

import os,stat,time,datetime

print("caught walker I")

class DirectoryStatWalker(object):
    """return {'fullname':fullname, 'fullStats':fullStats,
      'directory':self.directory,'filename':the_file,
      'isDir':stat.S_ISDIR(mode),'modTime':modtime}
    """

    def __init__(self, directory):
        self.stack = [directory]
        self.files = []
        self.index = 0

    def __getitem__(self, index):
        while 1:
            try:
                the_file = self.files[self.index]
                self.index = self.index + 1
            except IndexError:
                # pop next directory from stack
                self.directory = self.stack.pop()
                self.files = os.listdir(self.directory)
                self.index = 0
            else:
                # got a filename or directory
                fullname = os.path.join(self.directory,\
                                    the_file)
                fullStats = os.stat(fullname)
                mode = fullStats[stat.ST_MODE]
                #push the directory on the stack
                if stat.S_ISDIR(mode) and not stat.S_ISLNK(mode):
                    self.stack.append(fullname)
                #return either the file or directory
                year,month,day,hour,minute,second=time.localtime(fullStats[stat.ST_MTIME])[:6]
                modtime=datetime.datetime(year,month,day,hour,minute,second)
                return {'fullname':fullname, 'fullStats':fullStats,
                        'directory':self.directory,'filename':the_file,
                        'isDir':stat.S_ISDIR(mode),'modTime':modtime}

if __name__== "__main__":
    keepfiles=[]
    for fileDict in DirectoryStatWalker("/home/phil/teaching/atsc301"):
        keepfiles.append(fileDict)
    for item in keepfiles:
        if item['isDir']:
            print(item['fullname'])
    

        




    
        
    
                         
