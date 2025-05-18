
sub MAIN {
    my @starting_numbers = 'input.txt'.IO.slurp.split(',').map: *.Int;
    my %spoken;
    my Int $last_spoken;

    my Int $num_starting = @starting_numbers.elems;

    for 0 .. $num_starting - 2 -> Int $i {
        %spoken{@starting_numbers[$i]} = $i + 1;
    }

    $last_spoken = @starting_numbers[$num_starting - 1];

    for $num_starting + 1 .. 30000000 -> Int $turn {
        my Int $next_number;
        my $prev_turn = %spoken{$last_spoken};

        if $prev_turn.defined {
            $next_number = ($turn - 1) - $prev_turn;
        } else {
            $next_number = 0;
        }

        %spoken{$last_spoken} = $turn - 1;
        $last_spoken = $next_number;
    }

    say $last_spoken;
}
