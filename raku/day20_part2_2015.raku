
sub main {
    my $input-value = 'input.txt'.IO.slurp.trim.Int;
    my $target = $input-value div 11;

    my @houses = (0) xx ($target + 1);

    for 1 .. $target -> $elf {
        for 1 .. 50 -> $k {
            my $house = $elf * $k;
            if $house > $target {
                last;
            }
            @houses[$house] += $elf;
        }
    }

    for 1 .. $target -> $house-number {
        if @houses[$house-number] >= $target {
            print $house-number;
            last;
        }
    }
}

main();
