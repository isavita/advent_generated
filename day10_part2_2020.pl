
use strict;
use warnings;

open my $fh, '<', 'input.txt' or die "Error opening file: $!";
my @adapters = (0);

while (my $line = <$fh>) {
    chomp $line;
    push @adapters, $line;
}

close $fh;

@adapters = sort {$a <=> $b} @adapters;
push @adapters, $adapters[-1] + 3;

printf("%d\n", countArrangements(\@adapters));

sub countArrangements {
    my ($adapters) = @_;
    my %ways = (0 => 1);

    for (my $i = 1; $i < scalar(@$adapters); $i++) {
        my $currentJoltage = $adapters->[$i];
        foreach my $diff (1, 2, 3) {
            $ways{$currentJoltage} += $ways{$currentJoltage - $diff} || 0;
        }
    }

    return $ways{$adapters->[-1]};
}
