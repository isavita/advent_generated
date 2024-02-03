
use strict;
use warnings;

open(my $fh, '<', 'input.txt') or die "Error opening file: $!";
my $totalScore = 0;

while (my $line = <$fh>) {
    chomp $line;
    my $opponent = substr($line, 0, 1);
    my $roundEnd = substr($line, 2, 1);

    my $yourMove = ' ';
    if ($roundEnd eq 'X') {
        $yourMove = $opponent eq 'A' ? 'Z' : ($opponent eq 'B' ? 'X' : 'Y');
    } elsif ($roundEnd eq 'Y') {
        $yourMove = $opponent eq 'A' ? 'X' : ($opponent eq 'B' ? 'Y' : 'Z');
    } else {
        $yourMove = $opponent eq 'A' ? 'Y' : ($opponent eq 'B' ? 'Z' : 'X');
    }

    my $score = 0;
    if ($yourMove eq 'X') {
        $score = 1;
    } elsif ($yourMove eq 'Y') {
        $score = 2;
    } elsif ($yourMove eq 'Z') {
        $score = 3;
    }

    if (($opponent eq 'A' && $yourMove eq 'Y') || ($opponent eq 'B' && $yourMove eq 'Z') || ($opponent eq 'C' && $yourMove eq 'X')) {
        $score += 6;
    } elsif (($opponent eq 'A' && $yourMove eq 'X') || ($opponent eq 'B' && $yourMove eq 'Y') || ($opponent eq 'C' && $yourMove eq 'Z')) {
        $score += 3;
    }

    $totalScore += $score;
}

close($fh) or die "Error closing file: $!";
print "$totalScore\n";
