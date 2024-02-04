with open('input.txt', 'r') as f:
    ips = f.readlines()

def has_abba(s):
    for i in range(len(s) - 3):
        if s[i] != s[i+1] and s[i] == s[i+3] and s[i+1] == s[i+2]:
            return True
    return False

def supports_tls(ip):
    hypernet = False
    abba_outside = False
    abba_inside = False
    
    for i in range(len(ip)):
        if ip[i] == '[':
            hypernet = True
        elif ip[i] == ']':
            hypernet = False
        elif i <= len(ip) - 4:
            if has_abba(ip[i:i+4]):
                if hypernet:
                    abba_inside = True
                else:
                    abba_outside = True
                    
    return abba_outside and not abba_inside

count = 0
for ip in ips:
    if supports_tls(ip):
        count += 1

print(count)