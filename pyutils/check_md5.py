#!/usr/bin/env python3
from __future__ import division
import mmap
from hashlib import md5
from contextlib import closing

def find_md5(the_map,seek_point,buf_length):
    the_map.seek(seek_point)
    data=the_map.read(buf_length)
    m = md5()
    m.update(data)
    return m.hexdigest()

def check_md5(filename,buf_length):
    try:
        with open(filename,"rb") as infile:
            with closing(mmap.mmap(infile.fileno(),0,access=mmap.ACCESS_READ)) as the_map:
                file_size=the_map.size()
                if file_size > 3*buf_length:
                    start_seek=0
                    mid_seek=int(file_size/2)
                    end_seek=file_size - buf_length
                else:
                    start_seek=0
                    mid_seek=0
                    end_seek=0
                start_hex=find_md5(the_map,start_seek,buf_length)
                mid_hex=find_md5(the_map,mid_seek,buf_length)
                end_hex=find_md5(the_map,end_seek,buf_length)
    except:
        file_size,start_hex,mid_hex,end_hex=(-1,'bad','bad','bad')
    return (file_size,start_hex,mid_hex,end_hex)

if __name__ == '__main__':
    the_file="/backupspace/newroc_users/nchaparr/nchaparr_owl/thesis/LESRun/Nov302013/data/runs/sam_case6/OUT_3D/NCHAPP1_testing_doscamiopdata_24_0000000780.bin3D"
    buf_length=int(1.e5)
    print(check_md5(the_file,buf_length))
    
        
        
