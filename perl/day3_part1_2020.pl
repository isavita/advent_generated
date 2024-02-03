
use strict;
use warnings;

open(my $fh, '<', 'input.txt') or die $!;
my @forest;
while (my $line = <$fh>) {
    chomp $line;
    push @forest, $line;
}
close($fh);

my $trees = countTrees(\@forest, 3, 1);
print "$trees\n";

sub countTrees {
    my ($forest, $right, $down) = @_;
    my $trees = 0;
    my $x = 0;
    my $width = length($forest->[0]);

    for (my $y = 0; $y < scalar(@$forest); $y += $down) {
        if (substr($forest->[$y], $x % $width, 1) eq '#') {
            $trees++;
        }
        $x += $right;
    }

    return $trees;
}
