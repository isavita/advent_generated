
use strict;
use warnings;

open(my $fh, '<', 'input.txt') or die "Error opening file: $!";
my $input = do { local $/; <$fh> };
close($fh);

my %wireToRule;

foreach my $inst (split("\n", $input)) {
    my @parts = split(" -> ", $inst);
    $wireToRule{$parts[1]} = $parts[0];
}

my $aSignal = memoDFS(\%wireToRule, "a", {});

print $aSignal . "\n";

sub memoDFS {
    my ($graph, $entry, $memo) = @_;

    if (exists $memo->{$entry}) {
        return $memo->{$entry};
    }

    if ($entry =~ /[0-9]/) {
        return toInt($entry);
    }

    my $sourceRule = $graph->{$entry};
    my @parts = split(" ", $sourceRule);

    my $result;
    if (@parts == 1) {
        $result = memoDFS($graph, $parts[0], $memo);
    } elsif ($parts[0] eq "NOT") {
        my $start = memoDFS($graph, $parts[1], $memo);
        $result = 65535 ^ $start;
    } elsif ($parts[1] eq "AND") {
        $result = memoDFS($graph, $parts[0], $memo) & memoDFS($graph, $parts[2], $memo);
    } elsif ($parts[1] eq "OR") {
        $result = memoDFS($graph, $parts[0], $memo) | memoDFS($graph, $parts[2], $memo);
    } elsif ($parts[1] eq "LSHIFT") {
        $result = memoDFS($graph, $parts[0], $memo) << memoDFS($graph, $parts[2], $memo);
    } elsif ($parts[1] eq "RSHIFT") {
        $result = memoDFS($graph, $parts[0], $memo) >> memoDFS($graph, $parts[2], $memo);
    }

    $memo->{$entry} = $result;
    return $result;
}

sub toInt {
    my ($s) = @_;
    my $n = int($s);
    return $n;
}
