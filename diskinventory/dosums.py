import re
space=re.compile('\s+')
with open('final_list.txt','r') as f:
    the_sum=0.
    for line in f.readlines():
        line=line.strip()
        name,size=space.split(line)
        the_sum+=float(size)
print(the_sum)
