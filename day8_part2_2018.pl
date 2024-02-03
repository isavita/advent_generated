
use strict;
use warnings;

# Read input from file
open my $fh, '<', 'input.txt' or die $!;
my $line = <$fh>;
close $fh;

my @numbers = split(' ', $line);

# Parse the tree and calculate the value of the root node
my ($value, $index) = parseTree(\@numbers, 0);
print "$value\n";

sub parseTree {
    my ($data, $index) = @_;
    my $childCount = $data->[$index];
    my $metaCount = $data->[$index + 1];
    $index += 2;

    my @childValues;
    for (my $i = 0; $i < $childCount; $i++) {
        my ($childValue, $newIndex) = parseTree($data, $index);
        $index = $newIndex;
        push @childValues, $childValue;
    }

    my $value = 0;
    if ($childCount == 0) {
        for (my $i = 0; $i < $metaCount; $i++) {
            $value += $data->[$index + $i];
        }
    } else {
        for (my $i = 0; $i < $metaCount; $i++) {
            my $metadata = $data->[$index + $i];
            if ($metadata <= $childCount && $metadata > 0) {
                $value += $childValues[$metadata - 1];
            }
        }
    }
    $index += $metaCount;

    return ($value, $index);
}
