"""
calculate transfer rate given a series of
date; du -d 1 -k
results
"""
import dateutil
import sys
import numpy as np
out = """
/media/sea8a/data phil@pip% date; du -d 1
Sun Mar 27 10:59:06 PDT 2016
2325009472	./newtera
2325012920	.
/media/sea8a/data phil@pip% date; du -d 1
Sun Mar 27 10:59:16 PDT 2016
2325476428	./newtera
2325479876	.
/media/sea8a/data phil@pip% date; du -d 1
Sun Mar 27 10:59:56 PDT 2016
2327353960	./newtera
2327357408	.
/media/sea8a/data phil@pip% date; du -d 1
Sun Mar 27 11:09:30 PDT 2016
2354294812	./newtera
2354298260	.
Sun Mar 27 21:00:36 PDT 2016
4046648480	./newtera
4046651928	.
"""

import re
whitespace = re.compile('\s+')
the_lines = out.split('\n')
sizes = []
times = []
for count, line in enumerate(the_lines):
    try:
        out = dateutil.parser.parse(line)
        the_size = int(whitespace.split(the_lines[count + 1])[0])
        sizes.append(the_size)
        times.append(out)
    except (ValueError, OverflowError):
        print('failed: "+++{}--- '.format(line))
        pass
sizes = np.array(sizes)
times = np.array(times)
size_diff = np.diff(sizes)
time_diff = np.diff(times)
for the_size, the_time in zip(size_diff, time_diff):
    rate = the_size/the_time.seconds*1.e-3
    print("{} bytes in {} seconds for {} Mbytes/s".format(the_size, the_time,rate))
