import pandas as pd
keep_dict={}

var='/newtera/tera/vpopa/sam_checkpoints/ENT_S6_IDEAL_GREX/OUT_STAT'    
var='/newtera/tera/vpopa/sam_checkpoints/ENT_S6_IDEAL_HD_GREX/OUT_3D'
var = '/newtera/tera/vpopa/sam_sandbox/OUT_3D'

for key,value in vpopa_size:
    if key.find(var) > -1:
        keep_dict[key] = value

name_dict={}
for key,value in vpopa_name:
    if key.find(var) > -1:
        name_dict[key] = value

full_dict={}        
for the_key in name_dict.keys():
    f1=pd.DataFrame(name_dict[the_key])
    f2=pd.DataFrame(keep_dict[the_key])
    full_dict[the_key]=pd.concat([f1,f2],axis=1)

for the_key,value in full_dict.items():
    print('++++++++++ {} ++++++++++++'.format(the_key))
    print(value)
    
for item in full_dict[var]['name']:
    print(item)


