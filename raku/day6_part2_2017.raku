
sub MAIN {
    my @banks = 'input.txt'.IO.slurp.words».Int;
    my %seen;
    my int $cycles = 0;
    my str $key;

    loop {
        $key = @banks.join(',');
        if %seen{$key}:exists {
            say $cycles;
            say $cycles - %seen{$key};
            exit 0;
        }
        %seen{$key} = $cycles++;

        my int $max = @banks.max;
        my int $i = @banks.first(* == $max, :k);
        @banks[$i] = 0;

        my int $len = @banks.elems;
        while $max-- {
            $i = ($i + 1) % $len;
            ++@banks[$i];
        }
    }
}
