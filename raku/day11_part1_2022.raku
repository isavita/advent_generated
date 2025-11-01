
my @monkeys = slurp('input.txt').split("\n\n").map: {
    my @l = .lines;
    my @items = @l[1].comb(/\d+/);
    my ($op,$v) = @l[2].substr(19).words[1,2];
    my $div = @l[3].comb(/\d+/)[0];
    my $t = @l[4].comb(/\d+/)[0];
    my $f = @l[5].comb(/\d+/)[0];
    { items => @items, op => $op, val => $v, div => $div, t => $t, f => $f, ins => 0 }
}

for ^20 {
    for @monkeys.kv -> $i, $m {
        while $m<items> {
            my $old = $m<items>.shift;
            ++$m<ins>;
            my $w = $old;
            my $b = $m<val> eq 'old' ?? $old !! +$m<val>;
            $w = $m<op> eq '+' ?? $w + $b !! $w * $b;
            $w = $w div 3;
            my $to = $w % $m<div> ?? $m<f> !! $m<t>;
            @monkeys[$to]<items>.push: $w;
        }
    }
}

say @monkeys.map(*<ins>).sort.reverse[^2].reduce: * * *
