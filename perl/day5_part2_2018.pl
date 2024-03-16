use strict;
use warnings;
use List::Util qw(min);

open my $fh, '<', 'input.txt' or die $!;
my $polymer = <$fh>;
chomp $polymer;
close $fh;

sub react {
    my $polymer = shift;
    my $reactionOccurred = 1;
    while ($reactionOccurred) {
        $reactionOccurred = 0;
        for (my $i = 0; $i < length($polymer) - 1; $i++) {
            if (substr($polymer, $i, 1) ne substr($polymer, $i + 1, 1) && uc(substr($polymer, $i, 1)) eq uc(substr($polymer, $i + 1, 1))) {
                substr($polymer, $i, 2, '');
                $reactionOccurred = 1;
            }
        }
    }
    return $polymer;
}

my $minLength = length($polymer);
for my $unit ('a' .. 'z') {
    my $tempPolymer = $polymer;
    $tempPolymer =~ s/$unit//ig;
    my $reactedPolymer = react($tempPolymer);
    $minLength = min($minLength, length($reactedPolymer));
}

print $minLength . "\n";