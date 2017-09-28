#!/usr/bin/env python

import subprocess
import time
import sys

CMD = "./cormoran.native"

best_interleaving_param = {
    # test_name : (inner, wdelay)
    "abp" : ("bddoct", 0),
    "concloop" : ("oct", 6),
    "dekker" : ("oct", 2),
    "kessel" : ("bddoct", 0),
    "peterson" : ("bddoct", 0),
    "queue" : ("oct", 1),
}

# TODO: on dekker, one fence can be removed. Modular analysis goes from ~20s to
# ~30s with the same parameters, and interleaving analysis seems to suffer
# a lot more. Check this.

# TODO: find the right params for kessel.
best_modular_param = {
    # test_name : (inner, outer, control, wdelay, xdelay)
    "abp" : ("bddpolka", "mark", "from-labels", 0, 999),
    "concloop" : ("bddoct", "mark-smart", "from-labels", 6, 999),
    "dekker" : ("bddpolka", "mark-smart", "from-labels", 0, 2),
    # "kessel" : ("bddoct", 0),
    "peterson" : ("bddoct", "mark", "from-labels", 999, 999),
    "queue" : ("bddpolka", "mark-smart", "from-labels", 2, 1),
}

def get_interleaving_param(test_name):
    inner, wdelay = best_interleaving_param[test_name]
    return [
        "--method", "interleaving",
        "--inner", inner,
        "--outer", "mark",
        "--state-widening-delay", str(wdelay),
    ]

def get_modular_param(test_name):
    inner, outer, control, wdelay, xdelay = best_modular_param[test_name]
    return [
        "--method", "modular",
        "--inner", inner,
        "--outer", outer,
        "--control", control,
        "--state-widening-delay", str(wdelay),
        "--intf-widening-delay", str(xdelay),
    ]

if len(sys.argv) > 1 and sys.argv[1] == "modular":
    PARAM = best_modular_param
    GET_PARAM = get_modular_param
else:
    PARAM = best_interleaving_param
    GET_PARAM = get_interleaving_param

def run_test(test_name):
    params = GET_PARAM(test_name)
    file_name = "tests/safe/benchmarks/%s.test" % test_name
    print("Running test", test_name)
    t1 = time.time()
    full_command = [CMD, file_name] + params
    print(" ".join(full_command))
    result = subprocess.check_output(full_command)
    t2 = time.time()
    print(result.decode().strip())
    print("Time %f\n" % (t2 - t1))

if __name__ == "__main__":
   for test_name in PARAM:
       run_test(test_name)
