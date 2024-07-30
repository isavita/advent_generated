import os

fn sort_string(w string) string {
	mut chars := w.split('').map(it.str())
	chars.sort()
	return chars.join('')
}

fn main() {
	data := os.read_file('input.txt') or { return }
	passphrases := data.trim_space().split('\n')
	mut valid_count := 0

	for passphrase in passphrases {
		words := passphrase.split(' ')
		mut word_set := map[string]bool{}

		mut valid := true
		for word in words {
			sorted_word := sort_string(word)
			if sorted_word in word_set {
				valid = false
				break
			}
			word_set[sorted_word] = true
		}

		if valid {
			valid_count++
		}
	}

	println(valid_count)
}