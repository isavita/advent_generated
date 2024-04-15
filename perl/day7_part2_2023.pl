use strict;
use warnings;

my %valueDict = ('J' => 1, '2' => 2, '3' => 3, '4' => 4, '5' => 5, '6' => 6, '7' => 7, '8' => 8, '9' => 9, 'T' => 10, 'Q' => 11, 'K' => 12, 'A' => 13);

open my $fh, '<', 'input.txt' or die "Could not open file 'input.txt': $!";
my @lines = <$fh>;
close $fh;

my @hands;
foreach my $line (@lines) {
    chomp $line;
    next if $line eq '';
    my ($cards) = $line =~ /([2-9AKQJT]+)/;
    my ($bid) = $line =~ /\s(\d+)/;
    push @hands, {cards => $cards, bid => $bid};
}

my @matches = ([], [], [], [], [], [], []);
foreach my $hand (@hands) {
    my %count;
    $count{$_}++ foreach split //, $hand->{cards};
    if ($count{'J'}) {
        my $highV = 0;
        my $highKey = 'J';
        foreach my $y (keys %count) {
            next if $y eq 'J';
            if ($count{$y} > $highV || ($count{$y} == $highV && $valueDict{$y} > $valueDict{$highKey})) {
                $highKey = $y;
                $highV = $count{$y};
            }
        }
        if ($highKey ne 'J') {
            $count{$highKey} += $count{'J'};
            delete $count{'J'};
        }
    }
    my $value = 1;
    $value *= $_ foreach values %count;
    if ($value == 1) {
        push @{$matches[6]}, $hand;
    } elsif ($value == 2) {
        push @{$matches[5]}, $hand;
    } elsif ($value == 3) {
        push @{$matches[3]}, $hand;
    } elsif ($value == 4) {
        if (scalar keys %count == 2) {
            push @{$matches[1]}, $hand;
        } else {
            push @{$matches[4]}, $hand;
        }
    } elsif ($value == 5) {
        push @{$matches[0]}, $hand;
    } elsif ($value == 6) {
        push @{$matches[2]}, $hand;
    } else {
        print "oh no\n";
    }
}

my @convertedMatches;
foreach my $x (@matches) {
    my @temp;
    foreach my $i (@$x) {
        my $y = $i->{cards};
        $y =~ s/A/E/g;
        $y =~ s/T/A/g;
        $y =~ s/J/1/g;
        $y =~ s/Q/C/g;
        $y =~ s/K/D/g;
        my $val = hex($y);
        push @temp, [$val, $i->{bid}];
    }
    @temp = sort { $b->[0] <=> $a->[0] } @temp;
    push @convertedMatches, @temp;
}

my $total = 0;
for (my $x = 0; $x < @convertedMatches; $x++) {
    $total += $convertedMatches[$x][1] * (@convertedMatches - $x);
}

print "$total\n";