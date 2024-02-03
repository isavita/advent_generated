
use strict;
use warnings;

open(my $fh, '<', 'input.txt') or die $!;

my $sslCount = 0;
while (my $line = <$fh>) {
    if (supportsSSL($line)) {
        $sslCount++;
    }
}

print "$sslCount\n";

sub supportsSSL {
    my ($ip) = @_;

    my @bracketContents = $ip =~ /\[[a-z]+\]/g;

    $ip =~ s/\[[a-z]+\]/-/g;
    my @abas = findABAs($ip);
    foreach my $aba (@abas) {
        my $bab = substr($aba, 1, 1) . substr($aba, 0, 1) . substr($aba, 1, 1);
        foreach my $bracketContent (@bracketContents) {
            if ($bracketContent =~ /$bab/) {
                return 1;
            }
        }
    }

    return 0;
}

sub findABAs {
    my ($s) = @_;
    my @abas;

    for (my $i = 0; $i < length($s) - 2; $i++) {
        if (substr($s, $i, 1) ne substr($s, $i + 1, 1) && substr($s, $i, 1) eq substr($s, $i + 2, 1)) {
            push @abas, substr($s, $i, 3);
        }
    }

    return @abas;
}
