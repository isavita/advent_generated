
my %OP = (
    addr => { action => '+', a => 'r', b => 'r' },
    addi => { action => '+', a => 'r', b => 'v' },
    mulr => { action => '*', a => 'r', b => 'r' },
    muli => { action => '*', a => 'r', b => 'v' },
    banr => { action => '&', a => 'r', b => 'r' },
    bani => { action => '&', a => 'r', b => 'v' },
    borr => { action => '|', a => 'r', b => 'r' },
    bori => { action => '|', a => 'r', b => 'v' },
    setr => { action => 'a', a => 'r', b => 'r' },
    seti => { action => 'a', a => 'v', b => 'r' },
    gtir => { action => '>', a => 'v', b => 'r' },
    gtri => { action => '>', a => 'r', b => 'v' },
    gtrr => { action => '>', a => 'r', b => 'r' },
    eqir => { action => '=', a => 'v', b => 'r' },
    eqri => { action => '=', a => 'r', b => 'v' },
    eqrr => { action => '=', a => 'r', b => 'r' }
);

sub MAIN {
    my @lines = 'input.txt'.IO.lines;
    my $sum = 0;
    my $line-count = 0;
    while $line-count < @lines {
        if @lines[$line-count].starts-with('B') {
            my @registers = @lines[$line-count].match(/ \d+ /, :g).map: +*;
            my @instruction = @lines[$line-count+1].match(/ \d+ /, :g).map: +*;
            my @n = @lines[$line-count+2].match(/ \d+ /, :g).map: +*;

            $sum++ if test-code(@registers, @n, @instruction) >= 3;
            $line-count += 4;
        } else {
            last;
        }
    }
    say $sum;
}

sub test-code(@registers, @n, @instruction) {
    my $sum = 0;
    for %OP.values -> %op {
        my @out = run-op(%op, @registers, @instruction);
        if @n eqv @out {
            $sum++;
        }
    }
    $sum
}

sub run-op(%op, @registers, @instruction) {
    my @r = @registers.clone;
    my $A = %op<a> eq 'r' ?? @r[@instruction[1]] !! @instruction[1];
    my $B = %op<b> eq 'r' ?? @r[@instruction[2]] !! @instruction[2];
    @r[@instruction[3]] = do given %op<action> {
        when '+' { $A + $B }
        when '*' { $A * $B }
        when '&' { $A +& $B }
        when '|' { $A +| $B }
        when 'a' { $A }
        when '>' { $A > $B ?? 1 !! 0 }
        when '=' { $A eq $B ?? 1 !! 0 }
    }
    @r
}
