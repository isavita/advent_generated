use strict;
use warnings;

my $total_score = 0;

open my $file, '<', 'input.txt' or die "Error opening file: $!";

while (my $line = <$file>) {
    chomp $line;
    my ($opponent, $your_move) = split ' ', $line;

    my $score = 0;
    if ($your_move eq 'X') {
        $score = 1;
    } elsif ($your_move eq 'Y') {
        $score = 2;
    } elsif ($your_move eq 'Z') {
        $score = 3;
    }

    if (($opponent eq 'A' && $your_move eq 'Y') || ($opponent eq 'B' && $your_move eq 'Z') || ($opponent eq 'C' && $your_move eq 'X')) {
        $score += 6;
    } elsif (($opponent eq 'A' && $your_move eq 'X') || ($opponent eq 'B' && $your_move eq 'Y') || ($opponent eq 'C' && $your_move eq 'Z')) {
        $score += 3;
    }

    $total_score += $score;
}

close $file;

print "$total_score\n";