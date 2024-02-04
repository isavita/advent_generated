
import os
import sys
from typing import List

class Monkey:
    def __init__(self):
        self.items = []
        self.operation = None
        self.div = 0
        self.next = [0, 0]
        self.inspections = 0

def parse(s: str) -> Monkey:
    m = Monkey()
    lines = s.split("\n")
    for item in lines[1].split(": ")[1].split(", "):
        m.items.append(int(to_int(item)))
    f = lines[2].split("= ")[1].split()
    if f[1] == "+":
        if f[2] == "old":
            m.operation = lambda old: old + old
        else:
            m.operation = lambda old: old + int(f[2])
    elif f[1] == "*":
        if f[2] == "old":
            m.operation = lambda old: old * old
        else:
            m.operation = lambda old: old * int(f[2])
    m.div = int(lines[3].split(" divisible by ")[1])
    m.next[0] = int(lines[4].split(" throw to monkey ")[1])
    m.next[1] = int(lines[5].split(" throw to monkey ")[1])
    return m

def monkey_business(monkeys: List[Monkey], rounds: int, worry: bool) -> int:
    div = 1
    for m in monkeys:
        div *= m.div

    for _ in range(rounds):
        for m in monkeys:
            while len(m.items) > 0:
                m.inspections += 1
                item = m.operation(m.items[0])
                if worry:
                    item %= div
                else:
                    item //= 3
                if item % m.div == 0:
                    monkeys[m.next[0]].items.append(item)
                else:
                    monkeys[m.next[1]].items.append(item)
                m.items = m.items[1:]
    
    inspections = [m.inspections for m in monkeys]
    inspections.sort(reverse=True)
    return inspections[0] * inspections[1]

def to_int(s: str) -> int:
    return int(s)

def read_all(path: str) -> str:
    with open(path, "r") as file:
        return file.read()

if __name__ == "__main__":
    monkeys = []
    s = read_all("input.txt")
    for m in s.split("\n\n"):
        monkeys.append(parse(m))
    print(monkey_business(monkeys, 20, False))
