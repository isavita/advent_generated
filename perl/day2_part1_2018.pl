
use strict;
use warnings;

open(my $fh, '<', 'input.txt') or die $!;
my ($twoCount, $threeCount) = (0, 0);

while (my $id = <$fh>) {
    chomp $id;
    my ($twos, $threes) = countTwosAndThrees($id);
    $twoCount++ if $twos;
    $threeCount++ if $threes;
}

my $checksum = $twoCount * $threeCount;
print "$checksum\n";

sub countTwosAndThrees {
    my ($id) = @_;
    my %charCount;
    foreach my $char (split //, $id) {
        $charCount{$char}++;
    }

    my ($hasTwos, $hasThrees) = (0, 0);
    foreach my $count (values %charCount) {
        $hasTwos = 1 if $count == 2;
        $hasThrees = 1 if $count == 3;
    }
    return ($hasTwos, $hasThrees);
}
