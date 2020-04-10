"""
Read in a netcdf file and copy over all attributes and variables
with compression turned on
"""

from netCDF4 import Dataset
from pathlib import Path
import numpy as np


with Dataset(theFile,'r') as old_nc:
    with Dataset(newFileName,'w') as new_nc:
        theDims=old_nc.dimensions
        for key,value in theDims.items():
            inDim=len(value)
            newNC.createDimension(key,inDim)
        for varName,theVar in oldNC.variables.items():
            newVar=newNC.createVariable(varName,theVar.dtype,\
                                theVar.dimensions,zlib=True)
            newVar[...]=theVar[...]

if __name__ == "__main__":
    import argparse
    linebreaks=argparse.RawTextHelpFormatter
    descrip=__doc__.lstrip()
    parser = argparse.ArgumentParser(formatter_class=linebreaks,description=descrip)
    parser.add_argument('nc_file',type=str,help='uncompressed netcdf file')
    args=parser.parse_args()
    in_path = Path(args.nc_file)
    full_path = in_path.resolve()
    file_dir = full_path.parent
    filename = f'{in_file.stem}_compress.nc'
    out_path = file_dir / Path(filename)
    print(out_path)






            
