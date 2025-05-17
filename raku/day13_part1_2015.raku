
sub MAIN {
    my %happiness;
    my %people-set;

    for 'input.txt'.IO.lines -> $line {
        my @parts = $line.split(/\s+/);
        my $p1 = @parts[0];
        my $p2 = @parts[10].chop;
        my $change = @parts[3].Int;
        if @parts[2] eq 'lose' {
            $change = -$change;
        }
        %happiness{$p1}{$p2} = $change;
        %people-set{$p1} = True;
        %people-set{$p2} = True;
    }

    my @people = %people-set.keys.List;
    my $n = @people.elems;
    my $max-happiness = -Inf;

    for @people.permutations -> @seating {
        my $current-happiness = 0;
        for 0 ..^ $n -> $i {
            my $person = @seating[$i];
            my $left = @seating[($i - 1 + $n) % $n];
            my $right = @seating[($i + 1) % $n];
            $current-happiness += %happiness{$person}{$left} + %happiness{$person}{$right};
        }
        $max-happiness = max($max-happiness, $current-happiness);
    }

    say $max-happiness;
}
