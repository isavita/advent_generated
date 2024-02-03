
use strict;
use warnings;

open(my $fh, '<', 'input.txt') or die "Could not open file 'input.txt' $!";
my $score = 0;
my $depth = 0;
my $inGarbage = 0;
my $cancelNext = 0;

while (my $line = <$fh>) {
    chomp $line;
    foreach my $ch (split //, $line) {
        if ($cancelNext) {
            $cancelNext = 0;
            next;
        }

        if ($inGarbage) {
            if ($ch eq '!') {
                $cancelNext = 1;
            } elsif ($ch eq '>') {
                $inGarbage = 0;
            }
        } else {
            if ($ch eq '{') {
                $depth++;
            } elsif ($ch eq '}') {
                $score += $depth;
                $depth--;
            } elsif ($ch eq '<') {
                $inGarbage = 1;
            }
        }
    }
}

print "$score\n";
