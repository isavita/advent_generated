
sub MAIN {
    my @ops = &addr, &addi, &mulr, &muli, &banr, &bani, &borr, &bori, &setr, &seti, &gtir, &gtri, &gtrr, &eqir, &eqri, &eqrr;
    my @opcodes = <addr addi mulr muli banr bani borr bori setr seti gtir gtri gtrr eqir eqri eqrr>;

    my $ip-register;
    my @program;
    for 'input.txt'.IO.lines {
        when /^ '#'ip \s+ (\d+) / { $ip-register = $0.Int }
        when /^ (\w+) \s+ (\d+) \s+ (\d+) \s+ (\d+) / {
            @program.push: { :op(@ops[@opcodes.first(* eq $0, :k)]), :a($1.Int), :b($2.Int), :c($3.Int) }
        }
    }

    my @r = 1, 0, 0, 0, 0, 0;
    my int $ip = 0;
    my int $cycles = 0;
    while 0 <= $ip < +@program && $cycles < 1000 {
        @r[$ip-register] = $ip;
        my %inst := @program[$ip];
        @r[%inst<c>] = %inst<op>(@r, %inst<a>, %inst<b>);
        $ip = @r[$ip-register] + 1;
        $cycles++
    }

    my $n = @r.max;
    my $total = 0;
    $total += $_ if $n %% $_ for 1..$n;
    say $total
}

sub addr(@r, int $a, int $b) returns int { @r[$a] + @r[$b] }
sub addi(@r, int $a, int $b) returns int { @r[$a] + $b }
sub mulr(@r, int $a, int $b) returns int { @r[$a] * @r[$b] }
sub muli(@r, int $a, int $b) returns int { @r[$a] * $b }
sub banr(@r, int $a, int $b) returns int { @r[$a] +& @r[$b] }
sub bani(@r, int $a, int $b) returns int { @r[$a] +& $b }
sub borr(@r, int $a, int $b) returns int { @r[$a] +| @r[$b] }
sub bori(@r, int $a, int $b) returns int { @r[$a] +| $b }
sub setr(@r, int $a, int $b) returns int { @r[$a] }
sub seti(@r, int $a, int $b) returns int { $a }
sub gtir(@r, int $a, int $b) returns int { $a > @r[$b] ?? 1 !! 0 }
sub gtri(@r, int $a, int $b) returns int { @r[$a] > $b ?? 1 !! 0 }
sub gtrr(@r, int $a, int $b) returns int { @r[$a] > @r[$b] ?? 1 !! 0 }
sub eqir(@r, int $a, int $b) returns int { $a == @r[$b] ?? 1 !! 0 }
sub eqri(@r, int $a, int $b) returns int { @r[$a] == $b ?? 1 !! 0 }
sub eqrr(@r, int $a, int $b) returns int { @r[$a] == @r[$b] ?? 1 !! 0 }
