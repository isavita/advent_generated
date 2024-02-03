
open(my $fh, '<', 'input.txt') or die "Cannot open file: $!";
my $score = 0;
my $depth = 0;
my $inGarbage = 0;
my $cancelNext = 0;
my $garbageCount = 0;

while (my $line = <$fh>) {
    chomp $line;
    my @chars = split('', $line);
    foreach my $ch (@chars) {
        if ($cancelNext) {
            $cancelNext = 0;
            next;
        }

        if ($inGarbage) {
            if ($ch eq '!') {
                $cancelNext = 1;
            } elsif ($ch eq '>') {
                $inGarbage = 0;
            } else {
                $garbageCount++;
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

print "$garbageCount\n";
