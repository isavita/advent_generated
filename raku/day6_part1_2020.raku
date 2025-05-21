
sub MAIN {
    say slurp('input.txt')
        .split("\n\n")
        .map({ .split("\n").join('').comb.unique.elems })
        .sum;
}
