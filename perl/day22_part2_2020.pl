
#!/usr/bin/perl
use strict;
use warnings;
use List::Util qw(max);

# Read input from file
open my $fh, '<', 'input.txt' or die "Cannot open input.txt: $!";
my @input = <$fh>;
close $fh;

# Parse input into player decks
my (@player1, @player2);
my $current_player = \@player1;
for my $line (@input) {
    chomp $line;
    if ($line =~ /Player 1:/) {
        $current_player = \@player1;
    } elsif ($line =~ /Player 2:/) {
        $current_player = \@player2;
    } elsif ($line =~ /^\d+$/) {
        push @$current_player, $line;
    }
}

# Play recursive combat
my ($winner, $winning_deck) = play_recursive_combat(\@player1, \@player2);

# Calculate score
my $score = calculate_score($winning_deck);
print "Winning player's score: $score\n";

sub play_recursive_combat {
    my ($deck1, $deck2, $game_num) = @_;
    $game_num //= 1;
    my %previous_rounds;

    while (@$deck1 && @$deck2) {
        # Check for infinite game prevention
        my $round_key = join(',', @$deck1) . '|' . join(',', @$deck2);
        if ($previous_rounds{$round_key}++) {
            return (1, $deck1);  # Player 1 wins
        }

        # Draw top cards
        my $card1 = shift @$deck1;
        my $card2 = shift @$deck2;

        my $round_winner;
        if (@$deck1 >= $card1 && @$deck2 >= $card2) {
            # Recursive combat
            my @sub_deck1 = @$deck1[0 .. $card1 - 1];
            my @sub_deck2 = @$deck2[0 .. $card2 - 1];
            ($round_winner) = play_recursive_combat(\@sub_deck1, \@sub_deck2, $game_num + 1);
        } else {
            # Regular combat
            $round_winner = $card1 > $card2 ? 1 : 2;
        }

        # Add cards to winner's deck
        if ($round_winner == 1) {
            push @$deck1, $card1, $card2;
        } else {
            push @$deck2, $card2, $card1;
        }
    }

    # Determine overall winner
    return @$deck1 ? (1, $deck1) : (2, $deck2);
}

sub calculate_score {
    my ($deck) = @_;
    my $score = 0;
    my $multiplier = scalar @$deck;
    
    for my $card (@$deck) {
        $score += $card * $multiplier;
        $multiplier--;
    }
    
    return $score;
}
