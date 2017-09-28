#! /usr/bin/env python
import sys
import itertools

HELP_MSG = """
Usage : ./gen_round_robin.py <n>

Generates a program composed of n threads that pass themselves a token in
a round-robin scheme.""".strip()

HEADER_FMT = """
{{
{properties}
}}

int flag;

flag = 0
"""

PROPERTY_FMT = "\
    @ ({thread_i}:critical_section, {thread_j}:critical_section) false\
"

def properties(thread_number):
    return "\n".join(
        PROPERTY_FMT.format(thread_i=i, thread_j=j)
        for i in range(thread_number)
        for j in range(thread_number)
        if i < j
        )

def header(thread_number):
    return HEADER_FMT.format(properties=properties(thread_number))

THREAD_FMT = """\
#### THREAD {thread_id} ####
while true {{
    while flag < {thread_id} || flag > {thread_id} {{ }}
    label critical_section
    flag = {next_thread}
    label crit_end
}}
"""

def threads(thread_number):
    return "\n".join(
        itertools.chain(
            (THREAD_FMT.format(thread_id=i, next_thread=i+1)
                for i in range(thread_number - 1)),
            (THREAD_FMT.format(thread_id=thread_number-1, next_thread=0),)
            )
        )

def full_program(thread_number):
    return "\n".join((
        header(thread_number),
        threads(thread_number),
    ))

if __name__ == "__main__":
    try:
        thread_number = int(sys.argv[1])
    except (ValueError, IndexError):
        print(HELP_MSG)
    else:
        print(full_program(thread_number))
