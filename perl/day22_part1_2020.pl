
use strict;
use warnings;

open(my $fh, '<', 'input.txt') or die $!;
my (@player1Deck, @player2Deck);
my $currentDeck = \@player1Deck;

while (my $line = <$fh>) {
    chomp $line;
    if ($line eq "") {
        $currentDeck = \@player2Deck;
        next;
    }
    if ($line =~ /Player/) {
        next;
    }
    my $card = int($line);
    push @$currentDeck, $card;
}

while (@player1Deck && @player2Deck) {
    my $card1 = shift @player1Deck;
    my $card2 = shift @player2Deck;
    if ($card1 > $card2) {
        push @player1Deck, $card1, $card2;
    } else {
        push @player2Deck, $card2, $card1;
    }
}

my @winningDeck = @player1Deck ? @player1Deck : @player2Deck;
my $score = 0;
for my $i (0..$#winningDeck) {
    $score += $winningDeck[$i] * (scalar(@winningDeck) - $i);
}
print "$score\n";
