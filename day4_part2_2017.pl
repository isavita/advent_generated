
use strict;
use warnings;
use List::Util qw(uniqstr);

open(my $fh, '<', 'input.txt') or die "Could not open file 'input.txt' $!";
my @passphrases = <$fh>;
close $fh;

my $validCount = 0;

foreach my $passphrase (@passphrases) {
    my @words = split(' ', $passphrase);
    my %wordSet;

    my $valid = 1;
    foreach my $word (@words) {
        my $sortedWord = join('', sort(split('', $word)));
        if ($wordSet{$sortedWord}) {
            $valid = 0;
            last;
        }
        $wordSet{$sortedWord} = 1;
    }

    if ($valid) {
        $validCount++;
    }
}

print "$validCount\n";
