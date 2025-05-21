
sub MAIN {
    my @memory-banks = "input.txt".IO.slurp.words.map(*.Int);
    my $num-banks = @memory-banks.elems;

    my %seen-configs;
    my $cycles = 0;

    loop {
        my $current-config-str = @memory-banks.join(',');

        last if %seen-configs{$current-config-str}:exists;
        %seen-configs{$current-config-str} = True;

        my ($max-val, $max-idx) = (0, -1);
        for ^$num-banks -> $i {
            if @memory-banks[$i] > $max-val {
                $max-val = @memory-banks[$i];
                $max-idx = $i;
            }
        }

        @memory-banks[$max-idx] = 0;
        while $max-val > 0 {
            $max-idx = ($max-idx + 1) % $num-banks;
            @memory-banks[$max-idx]++;
            $max-val--;
        }

        $cycles++;
    }

    say $cycles;
}
