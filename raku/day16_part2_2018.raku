
my @opcodes = (
    { name => 'addr', action => '+', a => 'r', b => 'r', matchCount => [] },
    { name => 'addi', action => '+', a => 'r', b => 'v', matchCount => [] },
    { name => 'mulr', action => '*', a => 'r', b => 'r', matchCount => [] },
    { name => 'muli', action => '*', a => 'r', b => 'v', matchCount => [] },
    { name => 'banr', action => '&', a => 'r', b => 'r', matchCount => [] },
    { name => 'bani', action => '&', a => 'r', b => 'v', matchCount => [] },
    { name => 'borr', action => '|', a => 'r', b => 'r', matchCount => [] },
    { name => 'bori', action => '|', a => 'r', b => 'v', matchCount => [] },
    { name => 'setr', action => 'a', a => 'r', b => 'r', matchCount => [] },
    { name => 'seti', action => 'a', a => 'v', b => 'r', matchCount => [] },
    { name => 'gtir', action => '>', a => 'v', b => 'r', matchCount => [] },
    { name => 'gtri', action => '>', a => 'r', b => 'v', matchCount => [] },
    { name => 'gtrr', action => '>', a => 'r', b => 'r', matchCount => [] },
    { name => 'eqir', action => '=', a => 'v', b => 'r', matchCount => [] },
    { name => 'eqri', action => '=', a => 'r', b => 'v', matchCount => [] },
    { name => 'eqrr', action => '=', a => 'r', b => 'r', matchCount => [] },
);

my @lines = 'input.txt'.IO.lines;
my $sum = 0;
my $lineCount = 0;

while $lineCount < @lines.elems {
    my $line = @lines[$lineCount];
    if $line && $line.substr(0,1) eq 'B' {
        my @registers = $line.comb(/\d+/).map(*.Int);
        my @instruction = @lines[$lineCount+1].comb(/\d+/).map(*.Int);
        my @result = @lines[$lineCount+2].comb(/\d+/).map(*.Int);
        $sum += 1 if test-code(@registers, @result, @instruction) >= 3;
        $lineCount += 4;
    } else {
        last;
    }
}

my %ordered;
while %ordered.elems < 16 {
    for @opcodes -> $op {
        if $op<matchCount>.elems == 1 {
            my $c = $op<matchCount>[0];
            %ordered{$c} = $op;
            for @opcodes -> $op2 { remove($op2, $c); }
        }
    }
}

$lineCount += 2;
my @r = 0,0,0,0;
for $lineCount ..^ @lines.elems -> $i {
    my @instruction = @lines[$i].comb(/\d+/).map(*.Int);
    run-op(%ordered{@instruction[0]}, @r, @instruction);
}
put @r[0];

sub remove($op, $c) {
    $op<matchCount>.splice($op<matchCount>.first($c, :k),1) if $c ∈ $op<matchCount>;
}

sub add($op, $c) {
    $op<matchCount>.push($c) unless $c ∈ $op<matchCount>;
}

sub test-code(@registers, @result, @instruction) {
    my $count = 0;
    for @opcodes -> $op {
        if run-op($op, @registers.clone, @instruction) ~~ @result {
            add($op, @instruction[0]);
            $count++;
        }
    }
    $count;
}

sub run-op($op, @reg, @ins) {
    my $A = $op<a> eq 'r' ?? @reg[@ins[1]] !! @ins[1];
    my $B = $op<b> eq 'r' ?? @reg[@ins[2]] !! @ins[2];
    given $op<action> {
        when '+' { @reg[@ins[3]] = $A + $B }
        when '*' { @reg[@ins[3]] = $A * $B }
        when '&' { @reg[@ins[3]] = $A +& $B }
        when '|' { @reg[@ins[3]] = $A +| $B }
        when 'a' { @reg[@ins[3]] = $A }
        when '>' { @reg[@ins[3]] = $A > $B ?? 1 !! 0 }
        when '=' { @reg[@ins[3]] = $A == $B ?? 1 !! 0 }
    }
    @reg;
}
