
sub play-game(@p1, @p2, Bool :$recursive = False) {
    my %seen;
    my @d1 = @p1;
    my @d2 = @p2;
    while @d1 && @d2 {
        if $recursive {
            my $key = @d1.join(',') ~ '|' ~ @d2.join(',');
            return 1, @d1 if %seen{$key}:exists;
            %seen{$key} = True;
        }
        my $c1 = @d1.shift;
        my $c2 = @d2.shift;
        my Int $winner;
        if $recursive && @d1 >= $c1 && @d2 >= $c2 {
            $winner = play-game(@d1[^$c1], @d2[^$c2], :recursive).head;
        } else {
            $winner = $c1 > $c2 ?? 1 !! 2;
        }
        if $winner == 1 {
            @d1.push: $c1, $c2;
        } else {
            @d2.push: $c2, $c1;
        }
    }
    return @d2 ?? (2, @d2) !! (1, @d1);
}

sub calculate-score(@deck) {
    sum @deck.reverse.kv.map: -> $i, $v { $v * ($i + 1) }
}

my ($p1, $p2) = slurp('input.txt').split("\n\n");
my @p1 = $p1.lines[1..*].map: *.Int;
my @p2 = $p2.lines[1..*].map: *.Int;

say calculate-score play-game(@p1, @p2).tail;
say calculate-score play-game(@p1, @p2, :recursive).tail;
