from __future__ import with_statement
from hashlib import md5

def md5sum(filename, buf_size=8192):
    m = md5()
    # the with statement makes sure the file will be closed 
    with open(filename) as f:
        # We read the file in small chunk until EOF
        data = f.read(buf_size)
        while data:
            # We had data to the md5 hash
            m.update(data)
            data = f.read(buf_size)
    # We return the md5 hash in hexadecimal format
    return m.hexdigest()

if __name__ == '__main__':
    import sys
    print md5sum(sys.argv[1])
