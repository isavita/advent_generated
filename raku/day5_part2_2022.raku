
sub MAIN {
    my ($drawing, $moves) = slurp('input.txt').split("\n\n");

    my @stacks;
    for $drawing.lines.reverse -> $line {
        for $line.comb.kv -> $i, $c {
            @stacks[($i-1) div 4].push($c) if $c ~~ /<[A..Z]>/;
        }
    }

    for $moves.lines -> $step {
        my ($n, $from, $to) = $step.comb(/\d+/);
        @stacks[$to-1].push: |@stacks[$from-1].splice(*-$n);
    }

    put @stacks>>[*-1].join;
}
