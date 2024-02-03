
use strict;
use warnings;

open(my $fh, '<', 'input.txt') or die "Error opening file: $!";
my @instructions;

while (my $line = <$fh>) {
    chomp $line;
    push @instructions, $line;
}

close($fh);

for (my $i = 0; $i < scalar @instructions; $i++) {
    my ($op, $arg) = parseInstruction($instructions[$i]);
    if ($op eq "acc") {
        next;
    }

    my @modifiedInstructions = @instructions;
    if ($op eq "jmp") {
        $modifiedInstructions[$i] = sprintf("nop %d", $arg);
    } else {
        $modifiedInstructions[$i] = sprintf("jmp %d", $arg);
    }

    my ($accumulator, $terminated) = executeBootCode(\@modifiedInstructions);
    if ($terminated) {
        print "$accumulator\n";
        last;
    }
}

sub executeBootCode {
    my ($instructions) = @_;
    my $accumulator = 0;
    my %visited;
    my $currentInstruction = 0;

    while ($currentInstruction < scalar @$instructions) {
        if ($visited{$currentInstruction}) {
            return ($accumulator, 0);
        }

        $visited{$currentInstruction} = 1;
        my ($op, $arg) = parseInstruction($instructions->[$currentInstruction]);

        if ($op eq "acc") {
            $accumulator += $arg;
            $currentInstruction++;
        } elsif ($op eq "jmp") {
            $currentInstruction += $arg;
        } else {
            $currentInstruction++;
        }
    }

    return ($accumulator, 1);
}

sub parseInstruction {
    my ($instruction) = @_;
    my @parts = split(' ', $instruction);
    my $op = $parts[0];
    my $arg = $parts[1];
    return ($op, $arg);
}
