
use strict;
use warnings;
use List::Util qw(uniq);

open(my $fh, '<', 'input.txt') or die $!;
my @scores;
while (my $line = <$fh>) {
    chomp $line;
    my ($score, $incomplete) = checkAndCompleteLine($line);
    push @scores, $score if $incomplete;
}
close($fh);

@scores = sort {$a <=> $b} @scores;
my $middleScore = $scores[int(@scores/2)];
print "$middleScore\n";

sub checkAndCompleteLine {
    my ($line) = @_;
    my %pairings = (')' => '(', ']' => '[', '}' => '{', '>' => '<');
    my %scoreValues = (')' => 1, ']' => 2, '}' => 3, '>' => 4);
    my $opening = "([{<";
    my $closing = ")]}>";
    my @stack;

    foreach my $char (split //, $line) {
        if (index($opening, $char) != -1) {
            push @stack, $char;
        } elsif (index($closing, $char) != -1) {
            if (@stack == 0 || $stack[-1] ne $pairings{$char}) {
                return (0, 0); # corrupted line
            }
            pop @stack;
        }
    }

    if (@stack == 0) {
        return (0, 0); # line is not incomplete
    }

    my $score = 0;
    foreach my $i (reverse 0..$#stack) {
        $score *= 5;
        $score += $scoreValues{getClosingChar($stack[$i])};
    }
    return ($score, 1);
}

sub getClosingChar {
    my ($openingChar) = @_;
    return ')' if $openingChar eq '(';
    return ']' if $openingChar eq '[';
    return '}' if $openingChar eq '{';
    return '>' if $openingChar eq '<';
    return ' ';
}
