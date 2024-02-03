
use strict;
use warnings;

open(my $fh, '<', "input.txt") or die $!;
my $tlsCount = 0;

while (my $line = <$fh>) {
    if (supportsTLS($line)) {
        $tlsCount++;
    }
}

print $tlsCount;

sub supportsTLS {
    my $ip = shift;
    my @bracketContents = $ip =~ /\[[a-z]+\]/g;

    foreach my $bracketContent (@bracketContents) {
        if (containsABBA($bracketContent)) {
            return 0;
        }
    }

    $ip =~ s/\[[a-z]+\]/-/g;
    return containsABBA($ip);
}

sub containsABBA {
    my $s = shift;

    for (my $i = 0; $i < length($s) - 3; $i++) {
        if (substr($s, $i, 1) ne substr($s, $i + 1, 1) && substr($s, $i, 1) eq substr($s, $i + 3, 1) && substr($s, $i + 1, 1) eq substr($s, $i + 2, 1)) {
            return 1;
        }
    }

    return 0;
}
