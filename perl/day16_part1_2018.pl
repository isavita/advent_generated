use strict;
use warnings;

open(my $fh, '<', 'input.txt') or die $!;
my @lines = <$fh>;
close($fh);

my @opcodes = (
    {name => "addr", action => '+', a => 'r', b => 'r', matchCount => []},
    {name => "addi", action => '+', a => 'r', b => 'v', matchCount => []},
    {name => "mulr", action => '*', a => 'r', b => 'r', matchCount => []},
    {name => "muli", action => '*', a => 'r', b => 'v', matchCount => []},
    {name => "banr", action => '&', a => 'r', b => 'r', matchCount => []},
    {name => "bani", action => '&', a => 'r', b => 'v', matchCount => []},
    {name => "borr", action => '|', a => 'r', b => 'r', matchCount => []},
    {name => "bori", action => '|', a => 'r', b => 'v', matchCount => []},
    {name => "setr", action => 'a', a => 'r', b => 'r', matchCount => []},
    {name => "seti", action => 'a', a => 'v', b => 'r', matchCount => []},
    {name => "gtir", action => '>', a => 'v', b => 'r', matchCount => []},
    {name => "gtri", action => '>', a => 'r', b => 'v', matchCount => []},
    {name => "gtrr", action => '>', a => 'r', b => 'r', matchCount => []},
    {name => "eqir", action => '=', a => 'v', b => 'r', matchCount => []},
    {name => "eqri", action => '=', a => 'r', b => 'v', matchCount => []},
    {name => "eqir", action => '=', a => 'r', b => 'r', matchCount => []}
);

my $sum = 0;
my $lineCount = 0;
while ($lineCount < scalar(@lines)) {
    if (length($lines[$lineCount]) > 0 && substr($lines[$lineCount], 0, 1) eq 'B') {
        my @split = split /[^0-9]+/, $lines[$lineCount];
        my @registers = ($split[1], $split[2], $split[3], $split[4]);

        @split = split /[^0-9]+/, $lines[$lineCount + 1];
        my @instruction = ($split[0], $split[1], $split[2], $split[3]);

        @split = split /[^0-9]+/, $lines[$lineCount + 2];
        my @n = ($split[1], $split[2], $split[3], $split[4]);

        my $tempSum = testCode(\@registers, \@n, \@instruction, \@opcodes);

        if ($tempSum >= 3) {
            $sum++;
        }

        $lineCount = $lineCount + 4;
    } else {
        last;
    }
}

print "$sum\n";

sub remove {
    my ($op, $c) = @_;
    my $i = -1;
    foreach my $j (0..$#{$op->{matchCount}}) {
        if ($c == $op->{matchCount}[$j]) {
            $i = $j;
        }
    }
    if ($i != -1) {
        splice(@{$op->{matchCount}}, $i, 1);
    }
}

sub add {
    my ($op, $c) = @_;
    foreach my $v (@{$op->{matchCount}}) {
        if ($v == $c) {
            return;
        }
    }
    push @{$op->{matchCount}}, $c;
}

sub testCode {
    my ($registers, $n, $instruction, $opcodes) = @_;
    my $sum = 0;
    foreach my $i (0..$#{$opcodes}) {
        if (match($n, runOp($opcodes->[$i], $registers, $instruction))) {
            add($opcodes->[$i], $instruction->[0]);
            $sum++;
        }
    }
    return $sum;
}

sub match {
    my ($r, $c) = @_;
    if (scalar(@$r) != scalar(@$c)) {
        return 0;
    }
    foreach my $i (0..$#$r) {
        if ($r->[$i] != $c->[$i]) {
            return 0;
        }
    }
    return 1;
}

sub runOp {
    my ($op, $registers, $instruction) = @_;
    my @registerCP = @$registers;
    my ($A, $B);

    if ($op->{a} eq 'r') {
        $A = $registerCP[$instruction->[1]];
    } else {
        $A = $instruction->[1];
    }

    if ($op->{b} eq 'r') {
        $B = $registerCP[$instruction->[2]];
    } else {
        $B = $instruction->[2];
    }

    if ($op->{action} eq '+') {
        $registerCP[$instruction->[3]] = $A + $B;
    } elsif ($op->{action} eq '*') {
        $registerCP[$instruction->[3]] = $A * $B;
    } elsif ($op->{action} eq '&') {
        $registerCP[$instruction->[3]] = $A & $B;
    } elsif ($op->{action} eq '|') {
        $registerCP[$instruction->[3]] = $A | $B;
    } elsif ($op->{action} eq 'a') {
        $registerCP[$instruction->[3]] = $A;
    } elsif ($op->{action} eq '>') {
        $registerCP[$instruction->[3]] = $A > $B ? 1 : 0;
    } elsif ($op->{action} eq '=') {
        $registerCP[$instruction->[3]] = $A == $B ? 1 : 0;
    }

    return \@registerCP;
}

sub strToInt {
    my ($s) = @_;
    return int($s);
}

sub regSplit {
    my ($text, $delimeter) = @_;
    my @i = ($text =~ /$delimeter/g);
    my $laststart = 0;
    my @n;
    foreach my $element (@i) {
        push @n, substr($text, $laststart, $element - $laststart);
        $laststart = $element + 1;
    }
    push @n, substr($text, $laststart);
    return @n;
}