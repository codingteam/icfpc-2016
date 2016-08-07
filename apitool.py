#!/usr/bin/python

import sys
from os.path import join, exists, getsize
from time import sleep
import requests
from ratelimit import rate_limited
import re

OUR_KEY = '155-9a3123798006a07e9a054657ba047c68'
HEADERS = {'X-API-Key': OUR_KEY}
API = 'http://2016sv.icfpcontest.org/api'
number_re = re.compile("[0-9]+")

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
        fname = "problems/json/problem_{}.json".format(id)
        if exists(fname):
            print "{}: file exists, do not download".format(id)
            continue
        with open(fname, 'w') as f:
            f.write(str(problem))
        hsh = problem["problem_spec_hash"]
        sleep(1)
        print "Downloading problem {}".format(id)
        blob = blob_lookup(hsh).content
        with open("problems/problem_{}.txt".format(id), 'w') as f:
            f.write(str(blob))

@rate_limited(1)
def submit_solution(id, fname):
    if not exists(fname):
        print "{}: file does not exist".format(fname)
        return False
    solution = open(fname).read()
    if len(solution) > 4998:
        print "{}: solution too large, skipping".format(id)
        return True
    payload = {'problem_id': id, 'solution_spec': solution}
    url = join(API, 'solution', 'submit')
    r = requests.post(url, headers=HEADERS, data=payload)
    print r.text
    j = r.json()
    if j["ok"] == False and "solution to an own problem" in j["error"]:
        with open("own.txt", "a") as f:
            f.write(id + "\n")
        print "marking as own"
        return False
    if r.status_code == 200:
        if j["ok"] == True:
            print "marking as done"
            with open("done.txt", "a") as f:
                f.write(str(j["problem_id"]) + "\n")
            with open("approximate.txt", "a") as f:
                f.write("{}\t{}\n".format(j["problem_id"], j["resemblance"]))
        return True
    return False

@rate_limited(1)
def submit_all():
    doneTxt = open('done.txt').readlines()
    doneTxt = [s.strip() for s in doneTxt]
    ownTxt = open('own.txt').readlines()
    ownTxt = [s.strip() for s in ownTxt]
    while True:
        try:
            line = raw_input()
        except EOFError:
            break
        else:
            if number_re.match(line):
                id = line
            else:
                id = number_re.search(line).group(0)
            fname = "solutions/problem_{}.txt".format(id)
            if id in doneTxt:
                print "{}: Already submited".format(id)
            elif id in ownTxt:
                print "{}: Own problem".format(id)
            else:
                if exists(fname) and getsize(fname) > 4998:
                    print "Solution for problem {} too large, skipping.".format(id)
                if exists(fname) and getsize(fname) < 46:
                    print "Solution for problem {} too small, skipping.".format(id)
                elif exists(fname):
                    print "Submit " + fname
                    r = submit_solution(id, fname)
                    if not r:
                        print ">>>> ERROR!"
                else:
                    print "{}: file does not exist".format(fname)

def usage():
    print "Available commands: hello; status; download; submit problem_id solution.txt; submitall"
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
    elif command == 'submitall':
        submit_all()
    else:
        usage()


