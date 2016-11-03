#!/usr/bin/env python

CMD = "./cormoran.native"

import subprocess
import time

best_param = {
    "abp" : ("bddoct", 0),
    "concloop" : ("oct", 6),
    "dekker" : ("oct", 2),
    "kessel" : ("bddoct", 0),
    "peterson" : ("oct", 2),
    "queue" : ("oct", 1),
}

def run_test(test_name):
    domain, wdelay = best_param[test_name]
    file_name = "tests/safe/benchmarks/%s.test" % test_name
    print("Running test", test_name)
    print("domain %s, %d exact steps before widening" % (domain, wdelay))
    t1 = time.time()
    result = subprocess.check_output([CMD, "-d", domain, "-w", str(wdelay), file_name])
    t2 = time.time()
    print(result.decode().strip())
    print("Time %f\n" % (t2 - t1))

if __name__ == "__main__":
   for test_name in best_param:
       run_test(test_name)
