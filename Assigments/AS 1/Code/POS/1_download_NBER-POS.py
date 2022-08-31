# I adapt the code presented in: 
# https://opensourceoptions.com/blog/use-python-to-download-multiple-files-or-urls-in-parallel/
# for downloading the Raw data from CMS

#libraries
import requests
import time
from multiprocessing import cpu_count
from multiprocessing.pool import ThreadPool
import numpy as np

################## Functions #########################

# download a URL
def download_url(args):
    t0 = time.time()
    url, fn = args[0], args[1]
    try:
        r = requests.get(url)
        with open(fn, 'wb') as f:
            f.write(r.content)
        return(url, time.time() - t0)
    except Exception as e:
        print('Exception in download_url():', e)

# handle the parallel download
def download_parallel(args):
    cpus = cpu_count()
    results = ThreadPool(cpus - 1).imap_unordered(download_url, args)
    for result in results:
        print('url:', result[0], 'time (s):', result[1])

############### Create the lists ####################

# Create a list with the URL to download from NBER

nber = 'http://www.nber.org/pos/'
years = list(range(1993, 2017+1))
urls = [nber + str(i) + '/pos' + str(i) + '.dta.zip' for i in years]

# Create a list for the local path to download the files for windows
local = r'C:\Users\16083\Documents\GitHub\Econ771\Assigments\AS 1\Data\POS'
fns = [local + '\\pos' + str(i) + '.dta.zip' for i in years] 

# Create a list for the local path to download the files for Mac
#local = r'/Users/nix/Documents/GitHub/Econ771/Assigments/AS 1/Data'
#fns2 = [local + '/pos' + str(i) + '.dta.zip' for i in years] 


#zip both in one argument, neccesary for the next step
inputs = zip(urls, fns)

#Call the function and download the data bases
download_parallel(inputs)