#!/usr/bin/env python3
import mmap
import math
from hashlib import md5
from contextlib import closing
import sys, traceback


def find_md5(the_map, seek_point, buf_length):
    the_map.seek(seek_point)
    data = the_map.read(buf_length)
    m = md5()
    m.update(data)
    return m.hexdigest()


def check_md5(filename, buffer_length=1.e4):
    """
      legacy buf_length was int(1.e5)
    """
    buf_length = int(buffer_length)
    try:
        with open(filename, "rb") as infile:
            with closing(mmap.mmap(infile.fileno(),
                                   0,
                                   access=mmap.ACCESS_READ)) as the_map:
                file_size = the_map.size()
                if file_size > 3 * buf_length:
                    start_seek = 0
                    mid_seek = int(math.floor(file_size / 2))
                    end_seek = file_size - buf_length
                else:
                    start_seek = 0
                    mid_seek = None
                    end_seek = None
                    mid_hex = 'NA'
                    end_hex = 'NA'
                start_hex = find_md5(the_map, start_seek, buf_length)
                if mid_seek:
                    mid_hex = find_md5(the_map, mid_seek, buf_length)
                if end_seek:
                    end_hex = find_md5(the_map, end_seek, buf_length)
    except Exception as e:
        print(e)
        (type_, value_, traceback_) = sys.exc_info()
        print("type_ =", type_)
        print("value_ =", value_)
        traceback.print_tb(traceback_)
        file_size, start_hex, mid_hex, end_hex = (-1, 'bad', 'bad', 'bad')
    the_hash = ';'.join((start_hex, mid_hex, end_hex))
    return the_hash


if __name__ == '__main__':
    import sys
    if len(sys.argv) < 2:
        the_file = "/backupspace/newroc_users/nchaparr/nchaparr_owl/thesis/LESRun/Nov302013/data/runs/sam_case6/OUT_3D/NCHAPP1_testing_doscamiopdata_24_0000000780.bin3D"
    else:
        the_file = sys.argv[1]
    buf_length = int(1.e5)
    print(check_md5(the_file, buf_length))
