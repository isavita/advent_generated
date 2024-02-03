
open(my $fh, '<', 'input.txt') or die "Error opening file: $!";
my @instructions;

while (my $line = <$fh>) {
    chomp $line;
    push @instructions, $line;
}

my ($accumulator, $loop) = executeBootCode(\@instructions);
print "$accumulator\n";

sub executeBootCode {
    my ($instructions) = @_;
    my $accumulator = 0;
    my %visited;
    my $currentInstruction = 0;

    while ($currentInstruction < scalar(@$instructions)) {
        if ($visited{$currentInstruction}) {
            return ($accumulator, 1);
        }

        $visited{$currentInstruction} = 1;
        my @parts = split(' ', $instructions->[$currentInstruction]);
        my $op = $parts[0];
        my $arg = $parts[1];

        if ($op eq "acc") {
            $accumulator += $arg;
            $currentInstruction++;
        } elsif ($op eq "jmp") {
            $currentInstruction += $arg;
        } else {
            $currentInstruction++;
        }
    }

    return ($accumulator, 0);
}
