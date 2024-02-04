with open('input.txt', 'r') as file:
    data = file.read().splitlines()

def has_abba(s):
    for i in range(len(s) - 3):
        if s[i] == s[i + 3] and s[i + 1] == s[i + 2] and s[i] != s[i + 1]:
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
            if has_abba(ip[i:i + 4]):
                if hypernet:
                    abba_inside = True
                else:
                    abba_outside = True
    return abba_outside and not abba_inside

def has_aba(s):
    aba_list = []
    for i in range(len(s) - 2):
        if s[i] == s[i + 2] and s[i] != s[i + 1]:
            aba_list.append(s[i:i+3])
    return aba_list

def supports_ssl(ip):
    hypernet_abas = []
    supernet_abas = []
    hypernet = False
    for i in range(len(ip)):
        if ip[i] == '[':
            hypernet = True
        elif ip[i] == ']':
            hypernet = False
        elif i <= len(ip) - 3:
            if hypernet:
                hypernet_abas.extend(has_aba(ip[i:i+3]))
            else:
                supernet_abas.extend(has_aba(ip[i:i+3]))
    
    for aba in supernet_abas:
        bab = aba[1] + aba[0] + aba[1]
        if bab in hypernet_abas:
            return True
    return False

tls_count = 0
ssl_count = 0

for ip in data:
    if supports_tls(ip):
        tls_count += 1
    if supports_ssl(ip):
        ssl_count += 1

print(tls_count)
print(ssl_count)