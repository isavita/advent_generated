
def is_prime(n):
    for i in range(2, int(n**0.5) + 1):
        if n % i == 0:
            return False
    return True

if __name__ == "__main__":
    with open("input.txt", "r") as file:
        b = 57 * 100 + 100000
        c = b + 17000
        h = 0

        for x in range(b, c + 1, 17):
            if not is_prime(x):
                h += 1

        print(h)
