import os

fn main() {
	data := os.read_file('input.txt') or { return }
	passphrases := data.trim_space().split('\n')
	mut valid_count := 0

	for passphrase in passphrases {
		words := passphrase.split(' ')
		mut word_set := map[string]bool{}

		mut valid := true
		for word in words {
			if word_set[word] {
				valid = false
				break
			}
			word_set[word] = true
		}

		if valid {
			valid_count++
		}
	}

	println(valid_count)
}