def spinlock(step):
    buffer = [0]
    position = 0
    
    for i in range(1, 2018):
        position = (position + step) % len(buffer)
        buffer.insert(position + 1, i)
        position = (position + 1) % len(buffer)
    
    # Find the value after 2017
    index_2017 = buffer.index(2017)
    return buffer[(index_2017 + 1) % len(buffer)]

# Puzzle input
step = 363

result = spinlock(step)
print(result)
