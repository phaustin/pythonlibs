import logging


def make_logger(name='logger',  reuse=False):
    logging.basicConfig(level=logging.INFO)
    filename = '{}.txt'.format(name)
    formatter = logging.Formatter('{message:s}', style='{')
    fileout = logging.FileHandler(filename=filename, mode='w')
    mylog = logging.getLogger(name)
    mylog.addHandler(fileout)
    console = logging.StreamHandler()
    console.setLevel(logging.INFO)
    console.setFormatter(formatter)
    mylog.addHandler(console)
    mylog.propagate = True
    return mylog
