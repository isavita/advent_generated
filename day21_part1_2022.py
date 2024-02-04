
import sys

def calculate(monkey, jobs, results):
    if monkey in results:
        return results[monkey]

    if monkey not in jobs:
        raise Exception("Monkey not found: " + monkey)

    job = jobs[monkey]

    try:
        num = int(job)
        results[monkey] = num
        return num
    except ValueError:
        parts = job.split()
        a = calculate(parts[0], jobs, results)
        b = calculate(parts[2], jobs, results)

        if parts[1] == "+":
            result = a + b
        elif parts[1] == "-":
            result = a - b
        elif parts[1] == "*":
            result = a * b
        elif parts[1] == "/":
            result = a // b
        else:
            raise Exception("Unknown operation: " + parts[1])

        results[monkey] = result
        return result

if __name__ == "__main__":
    with open("input.txt", "r") as file:
        lines = file.readlines()

    jobs = {}
    results = {}

    for line in lines:
        parts = line.strip().split(": ")
        jobs[parts[0]] = parts[1]

    print(calculate("root", jobs, results))
