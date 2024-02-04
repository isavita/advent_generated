with open('input.txt', 'r') as file:
    blocked_ips = []
    for line in file:
        start, end = map(int, line.strip().split('-'))
        blocked_ips.append((start, end))
    
    blocked_ips.sort()
    
    current_ip = 0
    for start, end in blocked_ips:
        if current_ip < start:
            break
        current_ip = max(current_ip, end + 1)
    
    print(current_ip)