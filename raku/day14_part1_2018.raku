
sub MAIN {
    my $input = 'input.txt'.IO.slurp.Int;

    my @scoreboard = 3, 7;
    my ($elf1, $elf2) = 0, 1;

    while @scoreboard.elems < $input + 10 {
        my $new-score = @scoreboard[$elf1] + @scoreboard[$elf2];
        if $new-score >= 10 {
            @scoreboard.push($new-score div 10);
        }
        @scoreboard.push($new-score % 10);

        $elf1 = ($elf1 + @scoreboard[$elf1] + 1) % @scoreboard.elems;
        $elf2 = ($elf2 + @scoreboard[$elf2] + 1) % @scoreboard.elems;
    }

    say @scoreboard[$input .. $input + 9].join('');
}
