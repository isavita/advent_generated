
sub MAIN() {
    my $input-data = 'input.txt'.IO.slurp.trim;
    my @initial-cups = $input-data.comb.map(*.Int);

    my $N_moves = 100;

    my %next-cup;
    my $current-cup = @initial-cups[0];
    my $max-cup = @initial-cups.max;

    for 0 ..^ @initial-cups.end -> $i {
        %next-cup{@initial-cups[$i]} = @initial-cups[$i+1];
    }
    %next-cup{@initial-cups[*-1]} = @initial-cups[0];

    for 1 .. $N_moves -> $move {
        my $c1 = %next-cup{$current-cup};
        my $c2 = %next-cup{$c1};
        my $c3 = %next-cup{$c2};

        my %picked-up-set = set($c1, $c2, $c3);

        %next-cup{$current-cup} = %next-cup{$c3};

        my $destination = $current-cup - 1;
        $destination = $max-cup if $destination < 1;

        while %picked-up-set{$destination}:exists {
            $destination--;
            $destination = $max-cup if $destination < 1;
        }

        my $old-next-after-destination = %next-cup{$destination};
        %next-cup{$destination} = $c1;
        %next-cup{$c3} = $old-next-after-destination;

        $current-cup = %next-cup{$current-cup};
    }

    my $result-cup = %next-cup{1};
    my @result-sequence;
    while $result-cup != 1 {
        @result-sequence.push($result-cup);
        $result-cup = %next-cup{$result-cup};
    }
    say @result-sequence.join('');
}
