
use strict;
use warnings;

# Read input from file
open my $fh, '<', 'input.txt' or die $!;
my $line = <$fh>;
close $fh;

my @numbers = split(' ', $line);

my ($sum, $index) = parseTree(\@numbers, 0);
print "$sum\n";

sub parseTree {
    my ($data, $index) = @_;
    my $childCount = $data->[$index];
    my $metaCount = $data->[$index + 1];
    $index += 2;

    my $sum = 0;
    for (my $i = 0; $i < $childCount; $i++) {
        my ($childSum, $newIndex) = parseTree($data, $index);
        $sum += $childSum;
        $index = $newIndex;
    }

    for (my $i = 0; $i < $metaCount; $i++) {
        $sum += $data->[$index + $i];
    }
    $index += $metaCount;

    return ($sum, $index);
}
