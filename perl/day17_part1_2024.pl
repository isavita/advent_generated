
use strict;
use warnings;

open my $fh, '<', 'input.txt' or die "Cannot open input.txt: $!";

my ($A, $B, $C);
my @program;

while (my $line = <$fh>) {
    chomp $line;
    next if $line =~ /^$/;
    if ($line =~ /^Register A:\s*(\d+)/) {
        $A = $1;
    } elsif ($line =~ /^Register B:\s*(\d+)/) {
        $B = $1;
    } elsif ($line =~ /^Register C:\s*(\d+)/) {
        $C = $1;
    } elsif ($line =~ /^Program:\s*(.+)/) {
        @program = map { int } split /,\s*/, $1;
    }
}

sub getComboVal {
    my $op = shift;
    return $op <= 3 ? $op : $op == 4 ? $A : $op == 5 ? $B : $op == 6 ? $C : die "invalid combo operand";
}

my @outputVals;
my $ip = 0;
while ($ip < @program) {
    my $opcode = $program[$ip];
    my $operand = $program[$ip + 1];
    if ($opcode == 0) { # adv
        my $den = getComboVal($operand);
        $A = $den == 0 ? 0 : $A / (2 ** $den);
        $ip += 2;
    } elsif ($opcode == 1) { # bxl
        $B ^= $operand;
        $ip += 2;
    } elsif ($opcode == 2) { # bst
        $B = getComboVal($operand) % 8;
        $ip += 2;
    } elsif ($opcode == 3) { # jnz
        $ip = $A != 0 ? $operand : $ip + 2;
    } elsif ($opcode == 4) { # bxc
        $B ^= $C;
        $ip += 2;
    } elsif ($opcode == 5) { # out
        push @outputVals, getComboVal($operand) % 8;
        $ip += 2;
    } elsif ($opcode == 6) { # bdv
        my $den = getComboVal($operand);
        $B = $A / (2 ** $den);
        $ip += 2;
    } elsif ($opcode == 7) { # cdv
        my $den = getComboVal($operand);
        $C = $A / (2 ** $den);
        $ip += 2;
    } else {
        last;
    }
}

print join(",", @outputVals), "\n";
