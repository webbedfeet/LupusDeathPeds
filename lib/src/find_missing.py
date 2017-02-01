## Finding which files are missing from the analysis
import os
from glob import glob

dirs = ['adult','adult_10','inception']

out = {}

def find_missing(dir):
    goldstd = ['Rfile'+str(i)+'.rda' for i in range(1,62)]
    rdas = [os.path.split(x)[1] for x in glob(os.path.join(dir,'*.rda'))]
    miss_rda = set(goldstd)-set(rdas)
    indx = [int(x.replace('Rfile','').replace('.rda','')) for x in miss_rda]
    indx.sort()
    return indx

for d in dirs:
    out[d] = find_missing(d)

print out

