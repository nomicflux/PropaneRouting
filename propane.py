import requests as req
import json
from random import randint, random
from datetime import datetime
from time import sleep
from multiprocessing import Pool
from requests.auth import HTTPBasicAuth

tank_ids = [9,10,11,12,13]

lower = {9: -32,
    10: -32,
    11: -64,
    12: -16,
    13: -16}

upper = {9: 16,
    10: 8,
    11: 64,
    12: 8,
    13: 4}

skip = {9: 7, 10: 6, 11: 7, 12: 4, 13: 6}

url = "http://127.0.0.1:8080/auth/readings"

def clamp(val, mn, mx):
  if val > mx:
    val = mx
  elif val < mn:
    val = mn

  return val

def synth_tank(id_):
  reading = 3072
  obj = {'tank': id_}
  skipping = skip[id_]
  lower_bd = lower[id_]
  upper_bd = upper[id_]
  while True:
    send = randint(0,7)
    if send < skipping:
      if 1.0 / (float(reading) + 1.0) > 4.0 * random():
        reading = 3072
      else:
        reading += randint(lower_bd, upper_bd)
      reading = clamp(reading, 0, 4096)
      obj['value'] = reading
      obj['sensorsent'] = datetime.strftime(datetime.utcnow(),
        "%Y-%m-%dT%H:%M:%S.%fZ")
      print(obj['sensorsent'], reading, url)
      req.post(url, json=obj, auth=HTTPBasicAuth('vendor', 'password'))
    sleep(0.5)
  print("{} finished".format(id_))

if __name__ == "__main__":
  with Pool(5) as p:
    p.map(synth_tank, tank_ids)
