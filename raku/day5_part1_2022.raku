
sub move(@st, @steps) {
    my @stacks = @st.map: { .reverse.Array }
    for @steps {
        my ($n, $from, $to) = .words[1,3,5]>>.Int;
        $from--; $to--;
        @stacks[$to].push: @stacks[$from].pop for ^$n;
    }
    @stacks[*;*-1].join
}

sub MAIN() {
    my ($layout, $moves) = 'input.txt'.IO.slurp.split("\n\n");
    my @lines = $layout.lines;
    my @st = [[] xx (@lines[0].chars + 1) div 4];
    for @lines -> $line {
        for $line.comb.kv -> $i, $c {
            @st[($i-1) div 4].push: $c if 'A' le $c le 'Z';
        }
    }
    say move(@st, $moves.lines);
}
