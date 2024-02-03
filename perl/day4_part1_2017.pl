
use strict;
use warnings;

open my $fh, '<', "input.txt" or die "Cannot open file: $!";
my @passphrases = map { chomp; $_ } <$fh>;
close $fh;

my $validCount = 0;

foreach my $passphrase (@passphrases) {
    my @words = split(' ', $passphrase);
    my %wordSet;

    my $valid = 1;
    foreach my $word (@words) {
        if ($wordSet{$word}) {
            $valid = 0;
            last;
        }
        $wordSet{$word} = 1;
    }

    if ($valid) {
        $validCount++;
    }
}

print "$validCount\n";
