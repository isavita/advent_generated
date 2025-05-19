
sub read-blueprint(Str $filename) {
    my @lines = $filename.IO.lines.grep(*.trim.chars > 0);

    my $initial-state = @lines[0].words.tail(1).head.chomp('.');
    my $steps = @lines[1].words.tail(2).head.Int;

    my %states;
    my $i = 2;
    while $i < @lines.elems {
        my $state-name = @lines[$i].substr(*-2, 1);
        $i++;

        %states{$state-name} = %();

        for 0, 1 -> $val {
            my $write-val = @lines[$i + 1].words.tail(1).head.substr(0, 1).Int;
            my $move-dir  = @lines[$i + 2] ~~ / left / ?? -1 !! 1;
            my $next-state = @lines[$i + 3].words.tail(1).head.chomp('.');

            %states{$state-name}{$val} = ($write-val, $move-dir, $next-state);

            $i += 4;
        }
    }

    return $initial-state, $steps, %states;
}

sub run-turing-machine(Str $initial-state, Int $steps, %states) {
    my %tape;
    my $cursor = 0;
    my $state = $initial-state;

    for 1 .. $steps {
        my $current-val = %tape{$cursor} // 0;

        my ($write-val, $move-dir, $next-state) = %states{$state}{$current-val};

        %tape{$cursor} = $write-val;
        $cursor += $move-dir;
        $state = $next-state;
    }

    return %tape.values.sum;
}

sub MAIN() {
    my ($initial-state, $steps, %states) = read-blueprint('input.txt');
    my $checksum = run-turing-machine($initial-state, $steps, %states);
    say $checksum;
}
