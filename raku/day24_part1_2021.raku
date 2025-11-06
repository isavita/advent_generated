
sub MAIN {
    my @k; my @l; my @m;
    for 'input.txt'.IO.lines.kv -> $i,$line {
        given $i % 18 {
            when 4  { @l.push: $line.split(' ')[2].Int }
            when 5  { @k.push: $line.split(' ')[2].Int }
            when 15 { @m.push: $line.split(' ')[2].Int }
        }
    }

    my %constraints;
    my @stack;
    for @l.kv -> $i,$op {
        if $op == 1 {
            @stack.push: $i;
        } else {
            my $pop = @stack.pop;
            %constraints{$pop} = [$i, @m[$pop] + @k[$i]];
        }
    }

    my @max = 0 xx 14;
    for %constraints.kv -> $i, ($j,$diff) {
        my $v = 9;
        $v-- while $v + $diff > 9;
        @max[$i] = $v;
        @max[$j] = $v + $diff;
    }

    say [+] @max.map: * * 10 ** (13 - $++);
}
