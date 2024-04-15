use strict;
use warnings;

open my $fh, '<', 'input.txt' or die "Cannot open input.txt: $!";
my @lines = <$fh>;
chomp @lines;
close $fh;

my @opcodes = (
    { name => "addr", action => '+', a => 'r', b => 'r' },
    { name => "addi", action => '+', a => 'r', b => 'v' },
    { name => "mulr", action => '*', a => 'r', b => 'r' },
    { name => "muli", action => '*', a => 'r', b => 'v' },
    { name => "banr", action => '&', a => 'r', b => 'r' },
    { name => "bani", action => '&', a => 'r', b => 'v' },
    { name => "borr", action => '|', a => 'r', b => 'r' },
    { name => "bori", action => '|', a => 'r', b => 'v' },
    { name => "setr", action => 'a', a => 'r', b => 'r' },
    { name => "seti", action => 'a', a => 'v', b => 'r' },
    { name => "gtir", action => '>', a => 'v', b => 'r' },
    { name => "gtri", action => '>', a => 'r', b => 'v' },
    { name => "gtrr", action => '>', a => 'r', b => 'r' },
    { name => "eqir", action => '=', a => 'v', b => 'r' },
    { name => "eqri", action => '=', a => 'r', b => 'v' },
    { name => "eqrr", action => '=', a => 'r', b => 'r' }
);

my $sum = 0;
my $line_count = 0;

while ($line_count < @lines) {
    if ($lines[$line_count] =~ /^B/) {
        my @registers = ($lines[$line_count] =~ /\d+/g);
        my @instruction = ($lines[$line_count + 1] =~ /\d+/g);
        my @result = ($lines[$line_count + 2] =~ /\d+/g);
        my $temp_sum = test_code(\@registers, \@result, \@instruction, \@opcodes);
        $sum++ if $temp_sum >= 3;
        $line_count += 4;
    } else {
        last;
    }
}

my %ordered_opcodes;
while (keys %ordered_opcodes < 16) {
    for my $op (@opcodes) {
        if (scalar @{$op->{match_count}} == 1) {
            my $c = $op->{match_count}[0];
            $ordered_opcodes{$c} = $op;
            for my $op_j (@opcodes) {
                remove($op_j, $c);
            }
        }
    }
}

$line_count += 2;

my @r = (0, 0, 0, 0);

while ($line_count < @lines) {
    my @instruction = ($lines[$line_count] =~ /\d+/g);
    @r = run_op($ordered_opcodes{$instruction[0]}, \@r, \@instruction);
    $line_count++;
}

print $r[0], "\n";

sub remove {
    my ($op, $c) = @_;
    @{$op->{match_count}} = grep { $_ != $c } @{$op->{match_count}};
}

sub add {
    my ($op, $c) = @_;
    push @{$op->{match_count}}, $c unless grep { $_ == $c } @{$op->{match_count}};
}

sub test_code {
    my ($registers, $result, $instruction, $opcodes) = @_;
    my $sum = 0;
    for my $op (@$opcodes) {
        if (match($result, [run_op($op, $registers, $instruction)])) {
            add($op, $instruction->[0]);
            $sum++;
        }
    }
    return $sum;
}

sub match {
    my ($r, $c) = @_;
    for (0..$#$r) {
        return 0 if $r->[$_] != $c->[$_];
    }
    return 1;
}

sub run_op {
    my ($op, $registers, $instruction) = @_;
    my @register_cp = @$registers;
    my ($A, $B);
    $A = $op->{a} eq 'r' ? $register_cp[$instruction->[1]] : $instruction->[1];
    $B = $op->{b} eq 'r' ? $register_cp[$instruction->[2]] : $instruction->[2];
    if ($op->{action} eq '+') {
        $register_cp[$instruction->[3]] = $A + $B;
    } elsif ($op->{action} eq '*') {
        $register_cp[$instruction->[3]] = $A * $B;
    } elsif ($op->{action} eq '&') {
        $register_cp[$instruction->[3]] = $A & $B;
    } elsif ($op->{action} eq '|') {
        $register_cp[$instruction->[3]] = $A | $B;
    } elsif ($op->{action} eq 'a') {
        $register_cp[$instruction->[3]] = $A;
    } elsif ($op->{action} eq '>') {
        $register_cp[$instruction->[3]] = $A > $B ? 1 : 0;
    } elsif ($op->{action} eq '=') {
        $register_cp[$instruction->[3]] = $A == $B ? 1 : 0;
    }
    return @register_cp;
}