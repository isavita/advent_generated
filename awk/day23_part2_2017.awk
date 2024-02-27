
function isPrime(n) {
	for (i = 2; i * i <= n; i++) {
		if (n % i == 0) {
			return 0
		}
	}
	return 1
}

BEGIN {
	b = 57 * 100 + 100000
	c = b + 17000
	h = 0

	for (x = b; x <= c; x += 17) {
		if (!isPrime(x)) {
			h++
		}
	}

	print h
}
