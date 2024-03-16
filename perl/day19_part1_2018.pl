open my $fh, '<', 'input.txt' or die "Cannot open file: $!";
my $ipBind;
my @instructions;
my @registers = (0, 0, 0, 0, 0, 0);

while (my $line = <$fh>) {
    chomp $line;
    if ($line =~ /^#ip (\d+)$/) {
        $ipBind = $1;
    } else {
        push @instructions, [split(' ', $line)];
    }
}

my $ip = 0;
while ($ip >= 0 && $ip < scalar(@instructions)) {
    $registers[$ipBind] = $ip;
    my ($opcode, $a, $b, $c) = @{$instructions[$ip]};
    if ($opcode eq 'addr') {
        $registers[$c] = $registers[$a] + $registers[$b];
    } elsif ($opcode eq 'addi') {
        $registers[$c] = $registers[$a] + $b;
    } elsif ($opcode eq 'mulr') {
        $registers[$c] = $registers[$a] * $registers[$b];
    } elsif ($opcode eq 'muli') {
        $registers[$c] = $registers[$a] * $b;
    } elsif ($opcode eq 'banr') {
        $registers[$c] = $registers[$a] & $registers[$b];
    } elsif ($opcode eq 'bani') {
        $registers[$c] = $registers[$a] & $b;
    } elsif ($opcode eq 'borr') {
        $registers[$c] = $registers[$a] | $registers[$b];
    } elsif ($opcode eq 'bori') {
        $registers[$c] = $registers[$a] | $b;
    } elsif ($opcode eq 'setr') {
        $registers[$c] = $registers[$a];
    } elsif ($opcode eq 'seti') {
        $registers[$c] = $a;
    } elsif ($opcode eq 'gtir') {
        $registers[$c] = $a > $registers[$b] ? 1 : 0;
    } elsif ($opcode eq 'gtri') {
        $registers[$c] = $registers[$a] > $b ? 1 : 0;
    } elsif ($opcode eq 'gtrr') {
        $registers[$c] = $registers[$a] > $registers[$b] ? 1 : 0;
    } elsif ($opcode eq 'eqir') {
        $registers[$c] = $a == $registers[$b] ? 1 : 0;
    } elsif ($opcode eq 'eqri') {
        $registers[$c] = $registers[$a] == $b ? 1 : 0;
    } elsif ($opcode eq 'eqrr') {
        $registers[$c] = $registers[$a] == $registers[$b] ? 1 : 0;
    }
    
    $ip = $registers[$ipBind];
    $ip++;
}

print $registers[0];