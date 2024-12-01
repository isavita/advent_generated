
use strict;
use warnings;

# Read input from file
open(my $fh, '<', 'input.txt') or die "Could not open file: $!";
my @left;
my @right;
while (<$fh>) {
    chomp;
    my ($l, $r) = split /\s+/;
    push @left, $l;
    push @right, $r;
}
close $fh;

# Sort the lists
@left = sort { $a <=> $b } @left;
@right = sort { $a <=> $b } @right;

# Calculate the total distance
my $total_distance = 0;
for (my $i = 0; $i < @left; $i++) {
    $total_distance += abs($left[$i] - $right[$i]);
}

# Print the result
print "Total distance: $total_distance\n";
