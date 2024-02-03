
use strict;
use warnings;

my $s = readAll("input.txt");
print firstNUnique($s, 4) . "\n";

sub firstNUnique {
    my ($s, $n) = @_;
    for (my $i = $n; $i < length($s); $i++) {
        my $b = substr($s, $i - $n, $n);
        my @chars = split //, $b;
        my %seen;
        foreach my $char (@chars) {
            $seen{$char} = 1;
        }
        if (scalar @chars == scalar keys %seen) {
            return $i;
        }
    }
    return -1;
}

sub readAll {
    my ($path) = @_;
    open(my $fh, '<', $path) or die "Cannot open file: $!";
    my $content = do { local $/; <$fh> };
    close($fh);
    return $content;
}
