#!/usr/bin/python

from os.path import join
from time import sleep
import requests
from ratelimit import rate_limited

OUR_KEY = '155-9a3123798006a07e9a054657ba047c68'
HEADERS = {'X-API-Key': OUR_KEY}
API = 'http://2016sv.icfpcontest.org/api'

@rate_limited(1)
def hello():
    url = join(API, 'hello')
    r = requests.get(url, headers=HEADERS)
    print r.content

@rate_limited(1)
def blob_lookup(hsh):
    url = join(API, 'blob', hsh)
    r = requests.get(url, headers=HEADERS)
    return r

@rate_limited(1)
def get_status():
    url = join(API, 'snapshot', 'list')
    r = requests.get(url, headers=HEADERS)
    snapshots = r.json()
    print snapshots
    hsh = snapshots["snapshots"][-1]["snapshot_hash"]
    print hsh
    sleep(1)
    return blob_lookup(hsh).json()

@rate_limited(1)
def download_problems():
    status = get_status()
    for problem in status["problems"]:
        id = problem["problem_id"]
        with open("problems/json/problem_{}.json".format(id), 'w') as f:
            f.write(str(problem))
        hsh = problem["problem_spec_hash"]
        sleep(1)
        print "Downloading problem {}".format(id)
        blob = blob_lookup(hsh).content
        with open("problems/problem_{}.txt".format(id), 'w') as f:
            f.write(str(blob))


if __name__ == "__main__":
    #hello()
    #get_status()
    download_problems()

