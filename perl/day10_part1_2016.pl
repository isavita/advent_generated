
use strict;
use warnings;

my %bots;
my $valueRegex = qr/value (\d+) goes to (bot \d+)/;
my $givesRegex = qr/(bot \d+) gives low to (bot \d+|output \d+) and high to (bot \d+|output \d+)/;

open(my $fh, "<", "input.txt") or die "Cannot open input.txt: $!";
while (my $line = <$fh>) {
    chomp $line;
    if ($line =~ $valueRegex) {
        my ($value, $botID) = ($1, $2);
        $bots{$botID} //= { chips => [] };
        push @{$bots{$botID}->{chips}}, $value;
    } elsif ($line =~ $givesRegex) {
        my ($botID, $lowTo, $highTo) = ($1, $2, $3);
        $bots{$botID} //= { chips => [] };
        $bots{$botID}->{lowTo} = $lowTo;
        $bots{$botID}->{highTo} = $highTo;
    }
}

while (1) {
    my $action = 0;
    foreach my $botID (keys %bots) {
        my $b = $bots{$botID};
        if (@{$b->{chips}} == 2) {
            $action = 1;
            my ($low, $high) = minMax($b->{chips}[0], $b->{chips}[1]);
            if ($low == 17 && $high == 61) {
                print "$botID\n";
                exit;
            }
            $b->{chips} = [];

            giveChip(\%bots, $b->{lowTo}, $low);
            giveChip(\%bots, $b->{highTo}, $high);
        }
    }
    last unless $action;
}

sub giveChip {
    my ($bots, $target, $value) = @_;
    $bots->{$target} //= { chips => [] };
    push @{$bots->{$target}->{chips}}, $value;
}

sub minMax {
    my ($a, $b) = @_;
    return $a < $b ? ($a, $b) : ($b, $a);
}
