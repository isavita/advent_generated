#!/usr/bin/env bash
python3 - << 'PY'
total_code = 0
total_memory = 0
with open('input.txt','r',encoding='utf-8') as f:
    for line in f:
        s = line.rstrip('\n')
        total_code += len(s)
        total_memory += len(eval(s))
print(total_code - total_memory)
PY