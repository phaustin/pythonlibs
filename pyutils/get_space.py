"""
get paths
"""
from pathlib import Path

def calc_gigs(dir_path,file_rex):
    filelist=Path(dir_path).glob(file_rex)
    tot_size=0
    for item in filelist:
        tot_size += item.stat().st_size
    return tot_size*1.e-9

def main():
    import argparse
    linebreaks=argparse.RawTextHelpFormatter
    descrip=__doc__.lstrip()
    parser = argparse.ArgumentParser(formatter_class=linebreaks,description=descrip)
    parser.add_argument('dir_path',nargs='?',type=str,default='.',help='directory with files')
    parser.add_argument('file_rex',nargs='?',type=str,default='*.h5',help='*h5')
    args=parser.parse_args()
    print(f'calling with directory {args.dir_path} and regex {args.file_rex}')
    the_size=calc_gigs(args.dir_path,args.file_rex)
    print(f'file size is {the_size:6.2f} gigabytes')

    

if __name__ == "__main__":
    main()









