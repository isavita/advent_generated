
sub get-value($arg, %r) { $arg ~~ /^\-?\d+$/ ?? +$arg !! %r{$arg} // 0 }

my @inst = 'input.txt'.IO.lines;
my %r0 = p => 0;
my %r1 = p => 1;
my @q0;
my @q1;
my $sent = 0;
my int $i0 = 0;
my int $i1 = 0;
my int $d0 = 0;
my int $d1 = 0;

while !($d0 && $d1) {
    $d0 = $d1 = 1;

    while $i0 < @inst {
        my ($cmd,$x,$y) = @inst[$i0].split(' ');
        given $cmd {
            when 'snd' { @q1.push: get-value($x,%r0) }
            when 'set' { %r0{$x} = get-value($y,%r0) }
            when 'add' { %r0{$x} += get-value($y,%r0) }
            when 'mul' { %r0{$x} *= get-value($y,%r0) }
            when 'mod' { %r0{$x} %= get-value($y,%r0) }
            when 'rcv' { last unless @q0; %r0{$x} = @q0.shift }
            when 'jgz' { $i0 += get-value($y,%r0) - 1 if get-value($x,%r0) > 0 }
        }
        ++$i0;
        $d0 = 0;
    }

    while $i1 < @inst {
        my ($cmd,$x,$y) = @inst[$i1].split(' ');
        given $cmd {
            when 'snd' { @q0.push: get-value($x,%r1); ++$sent }
            when 'set' { %r1{$x} = get-value($y,%r1) }
            when 'add' { %r1{$x} += get-value($y,%r1) }
            when 'mul' { %r1{$x} *= get-value($y,%r1) }
            when 'mod' { %r1{$x} %= get-value($y,%r1) }
            when 'rcv' { last unless @q1; %r1{$x} = @q1.shift }
            when 'jgz' { $i1 += get-value($y,%r1) - 1 if get-value($x,%r1) > 0 }
        }
        ++$i1;
        $d1 = 0;
    }
}

put $sent;
