#!/usr/bin/python

import sys
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

@rate_limited(1)
def submit_solution(id, fname):
    solution = open(fname).read()
    payload = {'problem_id': id, 'solution_spec': solution}
    url = join(API, 'solution', 'submit')
    r = requests.post(url, headers=HEADERS, data=payload)
    print r.text
    if r.status_code == 200:
        j = r.json()
        if j["ok"] == True and j["resemblance"] > 0.9:
            with open("done.txt", "a") as f:
                f.write(str(j["problem_id"]) + "\n")
            print "marking as done"

def usage():
    print "Available commands: hello; status; download; submit problem_id solution.txt"
    sys.exit(1)

if __name__ == "__main__":
    if len(sys.argv) < 2:
        usage()
    command = sys.argv[1]
    if command == 'hello':
        hello()
    elif command == 'status':
        print get_status()
    elif command == 'download':
        download_problems()
    elif command == 'submit':
        submit_solution(sys.argv[2], sys.argv[3])
    else:
        usage()


