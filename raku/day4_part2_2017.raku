sub MAIN() {
    say 'input.txt'.IO.slurp.lines.grep({
        my @words = $_.words;
        @words.elems == Set(@words.map({ .comb.sort.join })).elems;
    }).elems;
}