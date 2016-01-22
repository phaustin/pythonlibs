def istext(filename):
    s=open(filename,'rb').read(512)
    try:
        s.decode('ascii')
        out=True
    except UnicodeDecodeError:
        out=False
    return out

