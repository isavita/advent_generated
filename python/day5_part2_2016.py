
import hashlib

def find_password(door_id):
    password = [''] * 8
    filled_positions = 0
    found = [False] * 8
    i = 0
    
    while filled_positions < 8:
        m = hashlib.md5()
        m.update((door_id + str(i)).encode('utf-8'))
        hash_str = m.hexdigest()

        if hash_str.startswith('00000'):
            pos = hash_str[5]
            if pos.isdigit() and 0 <= int(pos) < 8:
                pos_index = int(pos)
                if not found[pos_index]:
                    found[pos_index] = True
                    password[pos_index] = hash_str[6]
                    filled_positions += 1

        i += 1

    return ''.join(password)

if __name__ == "__main__":
    with open("input.txt", 'r') as file:
        door_id = file.read().strip()
        password = find_password(door_id)
        print(password)
