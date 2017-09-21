#! /usr/bin/env python
import sys

HELP_MSG = """
Usage : ./gen_peterson.py <n>

Generates a program composed of 2 threads that do n lock/unlock iterations of
Peterson algorithm.
""".strip()

HEADER_FMT = """
{{
{properties}
}}

bool flag_0, flag_1, turn;

true
"""

ITERATION_T0_FMT = """
flag_0 = true
mfence

turn = false
mfence

f = flag_1
label ineedthisone_{iteration_index}
t = turn

while (f && not t) {{
    f = flag_1
    t = turn
}}

label critical_section_{iteration_index}
flag_0 = false
"""

ITERATION_T1_FMT = """
flag_1 = true
mfence

turn = true
mfence

f = flag_0
label ineedthisone_{iteration_index}
t = turn

while (f && t) {{
    f = flag_0
    t = turn
}}

label critical_section_{iteration_index}
flag_1 = false
"""

PROPERTY_FMT = "\
    @ (0:critical_section_{iteration_0}, 1:critical_section_{iteration_1}) false\
"

def properties(iteration_number):
    return "\n".join(
        PROPERTY_FMT.format(iteration_0=i, iteration_1 = j)
        for i in range(iteration_number)
        for j in range(iteration_number)
        )

def thread(thread_fmt, iteration_number):
    return "\n".join(
        thread_fmt.format(iteration_index=i)
        for i in range(iteration_number)
        )

def thread_0(iteration_number):
    return thread(ITERATION_T0_FMT, iteration_number)

def thread_1(iteration_number):
    return thread(ITERATION_T1_FMT, iteration_number)

def header(iteration_number):
    return HEADER_FMT.format(properties=properties(iteration_number))

def full_program(iteration_number):
    return "\n".join((
        header(iteration_number),
        "#### THREAD 0 ####",
        thread_0(iteration_number),
        "#### THREAD 1 ####",
        thread_1(iteration_number),
        ))

if __name__ == "__main__":
    try:
        iteration_number = int(sys.argv[1])
    except (ValueError, IndexError):
        print(HELP_MSG)
    else:
        print(full_program(iteration_number))
