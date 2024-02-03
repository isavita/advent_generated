
use strict;
use warnings;

open my $fh, '<', 'input.txt' or die $!;
my $data = <$fh>;
close $fh;

my @strs = split(',', $data);
my @original = map { int($_) } @strs;

for my $noun (0..99) {
    for my $verb (0..99) {
        my @memory = @original;
        $memory[1] = $noun;
        $memory[2] = $verb;
        if (execute(\@memory) == 19690720) {
            print 100*$noun + $verb . "\n";
            exit;
        }
    }
}

sub execute {
    my ($memory) = @_;
    for (my $i = 0; $i < scalar(@$memory); $i += 4) {
        if ($memory->[$i] == 1) {
            $memory->[$memory->[$i+3]] = $memory->[$memory->[$i+1]] + $memory->[$memory->[$i+2]];
        }
        elsif ($memory->[$i] == 2) {
            $memory->[$memory->[$i+3]] = $memory->[$memory->[$i+1]] * $memory->[$memory->[$i+2]];
        }
        elsif ($memory->[$i] == 99) {
            return $memory->[0];
        }
    }
    return $memory->[0];
}
