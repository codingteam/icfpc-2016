#!/usr/bin/python

import requests

OUR_KEY = '155-9a3123798006a07e9a054657ba047c68'

def hello():
    url = 'http://2016sv.icfpcontest.org/api/hello'
    headers = {'X-API-Key': OUR_KEY}
    r = requests.get(url, headers=headers)
    print r.content

if __name__ == "__main__":
    hello()
