def main():
    # Initialize register5
    register5 = 0
    seen = set()
    last_unique = 0

    while True:
        register3 = register5 | 65536
        register5 = 7586220

        while True:
            register1 = register3 & 255
            register5 = (((register5 + register1) & 16777215) * 65899) & 16777215

            if register3 < 256:
                if register5 in seen:
                    # Cycle detected; last_unique holds the answer for Part Two
                    print(f"Part Two Answer: {last_unique}")
                    return
                seen.add(register5)
                last_unique = register5
                break
            else:
                register3 = register3 // 256

        # For Part One, we want the first value of register5
        # So, we can print it and break after the first iteration
        if not seen:
            # This condition will never be true since we add before
            pass

    # Note: The loop will return before reaching here

if __name__ == "__main__":
    main()
