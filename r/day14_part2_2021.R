
input <- readLines("input.txt", warn = FALSE)
template <- input[1]
rule_lines <- input[grep(" -> ", input)]

template_ints <- utf8ToInt(template) - 64
pair_counts <- matrix(0, 26, 26)
if (length(template_ints) > 1) {
    for (i in 1:(length(template_ints) - 1)) {
        pair_counts[template_ints[i], template_ints[i+1]] <- pair_counts[template_ints[i], template_ints[i+1]] + 1
    }
}

rules_df <- do.call(rbind, strsplit(rule_lines, " -> "))
if (!is.null(rules_df)) {
    rule_indices <- matrix(0, nrow(rules_df), 3)
    for (i in seq_len(nrow(rules_df))) {
        p <- utf8ToInt(rules_df[i, 1]) - 64
        ins <- utf8ToInt(rules_df[i, 2]) - 64
        rule_indices[i, ] <- c(p[1], p[2], ins)
    }
} else {
    rule_indices <- matrix(0, 0, 3)
}

for (step in 1:40) {
    new_counts <- matrix(0, 26, 26)
    for (i in seq_len(nrow(rule_indices))) {
        r <- rule_indices[i, 1]
        c <- rule_indices[i, 2]
        ins <- rule_indices[i, 3]
        cnt <- pair_counts[r, c]
        if (cnt > 0) {
            new_counts[r, ins] <- new_counts[r, ins] + cnt
            new_counts[ins, c] <- new_counts[ins, c] + cnt
        }
    }
    pair_counts <- new_counts
}

char_counts <- rowSums(pair_counts)
if (length(template_ints) > 0) {
    last_char <- template_ints[length(template_ints)]
    char_counts[last_char] <- char_counts[last_char] + 1
}

res <- char_counts[char_counts > 0]
if (length(res) > 0) {
    cat(sprintf("%.0f\n", max(res) - min(res)))
} else {
    cat("0\n")
}
