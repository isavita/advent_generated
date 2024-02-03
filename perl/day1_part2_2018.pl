
use strict;
use warnings;

open my $fh, '<', 'input.txt' or die "Error reading file: $!";
my @frequencyChanges = map { chomp; $_ } <$fh>;
close $fh;

my %frequencies;
my $currentFrequency = 0;
$frequencies{$currentFrequency} = 1;

my $found = 0;
while (!$found) {
    foreach my $change (@frequencyChanges) {
        my $frequencyDelta = $change;
        $currentFrequency += $frequencyDelta;
        if ($frequencies{$currentFrequency}) {
            print "$currentFrequency\n";
            $found = 1;
            last;
        }
        $frequencies{$currentFrequency} = 1;
    }
}
