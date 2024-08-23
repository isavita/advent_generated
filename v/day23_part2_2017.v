import math

fn is_prime(n int) bool {
	if n < 2 {
		return false
	}
	for i in 2 .. int(math.sqrt(n)) + 1 {
		if n % i == 0 {
			return false
		}
	}
	return true
}

fn main() {
	mut b := 57 * 100 + 100000
	mut c := b + 17000
	mut h := 0

	for b <= c {
		if !is_prime(b) {
			h++
		}
		b += 17
	}

	println(h)
}