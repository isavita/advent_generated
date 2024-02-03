
use strict;
use warnings;

open(my $fh, "<", "input.txt") or die "Cannot open file: $!";
my $input = do { local $/; <$fh> };
close($fh);

my $nice = 0;
my $disallowPattern = qr/(ab|cd|pq|xy)/;
my @lines = split("\n", $input);
foreach my $line (@lines) {
    my $vowels = 0;
    foreach my $char (split("", $line)) {
        if ($char =~ /[aeiou]/) {
            $vowels++;
        }
    }
    my $hasDouble = 0;
    my @chars = split("", $line);
    for (my $i = 0; $i < scalar(@chars) - 1; $i++) {
        if ($chars[$i] eq $chars[$i + 1]) {
            $hasDouble = 1;
            last;
        }
    }
    if ($vowels >= 3 && $line !~ /$disallowPattern/ && $hasDouble) {
        $nice++;
    }
}

print "$nice\n";
