
sub MAIN() {
    my $fh = 'input.txt'.IO.open or die;
    my @lines = $fh.lines;
    $fh.close;

    my $polymer = @lines.shift;
    @lines.shift while @lines.elems and @lines[0].trim eq '';

    my %rules;
    for @lines -> $line {
        my ($pair, $insert) = $line.split(' -> ');
        %rules{$pair} = $insert;
    }

    my %char-counts;
    my %pair-counts;

    for $polymer.comb -> $c {
        %char-counts{$c}++;
    }

    for ^($polymer.chars - 1) -> $i {
        %pair-counts{$polymer.substr($i, 2)}++;
    }

    for ^10 {
        my %next-pair-counts;
        my %next-char-counts = %char-counts.clone;

        for %pair-counts.kv -> $pair, $count {
            my $insert-char = %rules{$pair};

            %next-char-counts{$insert-char} += $count;

            my $char1 = $pair.substr(0, 1);
            my $char2 = $pair.substr(1, 1);

            %next-pair-counts{$char1 ~ $insert-char} += $count;
            %next-pair-counts{$insert-char ~ $char2} += $count;
        }
        %pair-counts = %next-pair-counts;
        %char-counts = %next-char-counts;
    }

    my @values = %char-counts.values;
    say @values.max - @values.min;
}
