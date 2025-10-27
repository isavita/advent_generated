
my @prog = 'input.txt'.IO.lines.map: { .words.cache };
my int $max = 50;

sub run(int $a) {
    my int @r = $a, 0, 0, 0;
    my int $last = -1;
    my int $out  = 0;
    my int $pc   = 0;

    while $pc < @prog {
        my @i := @prog[$pc];
        given @i[0] {
            when 'cpy' { @r[@i[2].ord - 97] = @i[1] ~~ /\d/ ?? +@i[1] !! @r[@i[1].ord - 97] }
            when 'inc' { ++@r[@i[1].ord - 97] }
            when 'dec' { --@r[@i[1].ord - 97] }
            when 'jnz' {
                my int $v = @i[1] ~~ /\d/ ?? +@i[1] !! @r[@i[1].ord - 97];
                if $v ≠ 0 {
                    $pc += (@i[2] ~~ /\d/ ?? +@i[2] !! @r[@i[2].ord - 97]);
                    next
                }
            }
            when 'out' {
                my int $v = @i[1] ~~ /\d/ ?? +@i[1] !! @r[@i[1].ord - 97];
                return False unless $v == 0 || $v == 1;
                return False if $v == $last;
                $last = $v;
                return True if ++$out > $max
            }
        }
        ++$pc
    }
    False
}

say (1..*).first: &run
