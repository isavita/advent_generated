
use strict;
use warnings;

my @cards;
my $input = do {
    local $/;
    open my $fh, "<", "input.txt" or die "Error reading file: $!";
    <$fh>;
};

my @lines = split /\n/, $input;

foreach my $line (@lines) {
    next if $line eq "";
    my $card = lexLineIntoCard($line);
    push @cards, $card;
}

sub getPointsForCard {
    my ($card) = @_;
    my $points = 0;
    foreach my $given (keys %{$card->{givens}}) {
        if (exists $card->{winnings}->{$given}) {
            $points += $card->{givens}->{$given} * $card->{winnings}->{$given};
        }
    }
    return $points;
}

sub lexLineIntoCard {
    my ($line) = @_;
    my ($cardDataStr) = $line =~ /: (.*)/;
    my @cardData = split / \| /, $cardDataStr;

    my %winnings;
    while ($cardData[0] =~ /(\d+)/g) {
        $winnings{$1}++;
    }

    my %givens;
    while ($cardData[1] =~ /(\d+)/g) {
        $givens{$1}++;
    }

    return {
        winnings   => \%winnings,
        givens     => \%givens,
        totalCount => 1,
    };
}

foreach my $i (0 .. $#cards) {
    my $points = getPointsForCard($cards[$i]);
    foreach my $j (1 .. $points) {
        $cards[$i + $j]{totalCount} += 1 * $cards[$i]{totalCount};
    }
}

my $totalCards = 0;
foreach my $card (@cards) {
    $totalCards += $card->{totalCount};
}

print "$totalCards\n";
