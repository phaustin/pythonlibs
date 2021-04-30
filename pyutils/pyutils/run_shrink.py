"""
shrink an image
"""
import argparse
from .img_to_md import shrinkit
from pathlib import Path

def make_parser():
    """
    set up the command line arguments needed to call the program
    """
    linebreaks = argparse.RawTextHelpFormatter
    parser = argparse.ArgumentParser(
        formatter_class=linebreaks, description=__doc__.lstrip())
    parser.add_argument('infile', type=str, help='path to input image')
    parser.add_argument('outpath', type=str, help='path to output folder')
    parser.add_argument('scale_factor', type=float, help='fraction to scale image (float)')
    return parser

#
# if args is None then parse_args operates on
# contents of sys.argv by default
#

def main(args=None):
    parser = make_parser()
    args=parser.parse_args()
    infile=Path(args.infile).resolve()
    outpath=Path(args.outpath).resolve()
    shrinkit(infile,outpath,args.scale_factor)

if __name__ == "__main__":
     main()
