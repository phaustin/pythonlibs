import argparse
import os,stat,time,datetime

class walkTree:
    """return {'fullname':fullname, 'fullStats':fullStats,
      'directory':self.directory,'filename':file,
      'isDir':stat.S_ISDIR(mode),'modTime':modtime}
    """

    def __init__(self, directory):
        self.stack = [directory]
        self.files = []
        self.index = 0

    def __getitem__(self, index):
        while 1:
            try:
                file = self.files[self.index]
                self.index = self.index + 1
            except IndexError:
                # pop next directory from stack
                self.directory = self.stack.pop()
                self.files = os.listdir(self.directory)
                self.index = 0
            else:
                # got a filename or directory
                fullname = os.path.join(self.directory, file)
                try:
                    fullStats = os.stat(fullname)
                except OSError:
                    continue
                mode = fullStats[stat.ST_MODE]
                #push the directory on the stack
                if stat.S_ISDIR(mode) and not stat.S_ISLNK(mode):
                    self.stack.append(fullname)
                #return either the file or directory
                year,month,day,hour,minute,second=time.localtime(fullStats[stat.ST_MTIME])[:6]
                modtime=datetime.datetime(year,month,day,hour,minute,second)
                return {'fullname':fullname, 'fullStats':fullStats,
                        'directory':self.directory,'filename':file,
                        'isDir':stat.S_ISDIR(mode),'modTime':modtime}

if __name__== "__main__":

    import os.path

    parser = argparse.ArgumentParser(description=
          "recursively find file properties")
    parser.add_argument('topdir', nargs=1, type=str,
                        help='top directory')
    args=parser.parse_args()
    topdir=args.topdir[0]

    for fileDict in DirectoryStatWalker(topdir):
        print(fileDict)
