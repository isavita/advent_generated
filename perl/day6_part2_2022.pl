
use strict;
use warnings;

sub readAll {
    my $path = shift;
    open my $fh, '<', $path or die $!;
    my $content = do { local $/; <$fh> };
    close $fh;
    return $content;
}

sub SetOf {
    my @b = @_;
    my %m;
    foreach my $c (@b) {
        $m{$c} = 1;
    }
    @b = keys %m;
    return @b;
}

my $s = readAll("input.txt");
print firstNUnique($s, 14);

sub firstNUnique {
    my ($s, $n) = @_;
    for (my $i = $n; $i < length($s); $i++) {
        my @b = split //, substr($s, $i - $n, $n);
        if (scalar @b == scalar SetOf(@b)) {
            return $i;
        }
    }
    return -1;
}
