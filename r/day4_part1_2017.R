
passphrases <- readLines("input.txt")
valid_passphrases <- 0

for (passphrase in passphrases) {
  words <- strsplit(passphrase, " ")[[1]]
  if (length(words) == length(unique(words))) {
    valid_passphrases <- valid_passphrases + 1
  }
}

print(valid_passphrases)
