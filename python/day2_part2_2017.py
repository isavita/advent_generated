
import numpy as np

data = np.loadtxt('input.txt', dtype=int)
print(np.sum(np.max(data, axis=1) - np.min(data, axis=1)))

result = 0
for row in data:
    for i in range(len(row)):
        for j in range(i+1, len(row)):
            if row[i] % row[j] == 0:
                result += row[i] // row[j]
            elif row[j] % row[i] == 0:
                result += row[j] // row[i]
print(result)
