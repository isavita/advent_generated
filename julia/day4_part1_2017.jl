
open("input.txt") do file
    passphrases = split(read(file, String), "\n")
    valid_count = 0

    for passphrase in passphrases
        words = split(passphrase)
        unique_words = Set{String}()

        for word in words
            if word in unique_words
                break
            else
                push!(unique_words, word)
            end
        end

        if length(unique_words) == length(words)
            valid_count += 1
        end
    end

    println(valid_count)
end
