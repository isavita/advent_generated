
def readInput(filename):
    with open(filename, 'r') as file:
        return file.readline().strip()

def findNextPassword(password):
    while True:
        password = incrementPassword(password)
        if isValidPassword(password):
            break
    return password

def incrementPassword(password):
    password = list(password)
    for i in range(len(password)-1, -1, -1):
        password[i] = chr(ord(password[i]) + 1)
        if password[i] > 'z':
            password[i] = 'a'
        else:
            break
    return ''.join(password)

def isValidPassword(password):
    return hasStraight(password) and not containsInvalidLetters(password) and hasTwoPairs(password)

def hasStraight(password):
    for i in range(len(password)-2):
        if ord(password[i])+1 == ord(password[i+1]) and ord(password[i])+2 == ord(password[i+2]):
            return True
    return False

def containsInvalidLetters(password):
    return 'i' in password or 'o' in password or 'l' in password

def hasTwoPairs(password):
    count = 0
    i = 0
    while i < len(password)-1:
        if password[i] == password[i+1]:
            count += 1
            i += 2
        else:
            i += 1
    return count >= 2

currentPassword = readInput("input.txt")
firstNewPassword = findNextPassword(currentPassword)
secondNewPassword = findNextPassword(firstNewPassword)
print(secondNewPassword)
