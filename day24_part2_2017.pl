
use strict;
use warnings;

my @components;
my @used;
my $maxStrength = 0;
my $maxLength = 0;

sub findStrongestLongestBridge {
    my ($components, $used, $port, $strength, $length) = @_;

    if ($length > $maxLength || ($length == $maxLength && $strength > $maxStrength)) {
        $maxStrength = $strength;
        $maxLength = $length;
    }

    for my $i (0 .. $#{$components}) {
        my $c = $components->[$i];
        if ($used->[$i]) {
            next;
        }

        if ($c->{a} == $port || $c->{b} == $port) {
            $used->[$i] = 1;
            my $nextPort = $c->{a};
            if ($c->{a} == $port) {
                $nextPort = $c->{b};
            }
            findStrongestLongestBridge($components, $used, $nextPort, $strength + $c->{a} + $c->{b}, $length + 1);
            $used->[$i] = 0;
        }
    }
}

open(my $fh, '<', 'input.txt') or die $!;
while (my $line = <$fh>) {
    chomp $line;
    my @ports = split('/', $line);
    my $a = $ports[0];
    my $b = $ports[1];
    push @components, {a => $a, b => $b};
}
close($fh);

@used = (0) x scalar @components;
findStrongestLongestBridge(\@components, \@used, 0, 0, 0);

print "$maxStrength\n";
