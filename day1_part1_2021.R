depths <- scan("input.txt")
increases <- sum(diff(depths) > 0)
print(increases)