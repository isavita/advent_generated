
use strict;
use warnings;

open(my $fh, '<', 'input.txt') or die $!;

my $totalScore = 0;
while (my $line = <$fh>) {
    chomp $line;
    my ($score, $corrupted) = checkLine($line);
    if ($corrupted) {
        $totalScore += $score;
    }
}

close($fh);

print "$totalScore\n";

sub checkLine {
    my ($line) = @_;
    my %pairings = (')' => '(', ']' => '[', '}' => '{', '>' => '<');
    my %scores = (')' => 3, ']' => 57, '}' => 1197, '>' => 25137);
    my @stack = ();

    foreach my $char (split //, $line) {
        if ($char eq '(' || $char eq '[' || $char eq '{' || $char eq '<') {
            push @stack, $char;
        } elsif ($char eq ')' || $char eq ']' || $char eq '}' || $char eq '>') {
            if (@stack == 0 || $stack[$#stack] ne $pairings{$char}) {
                return ($scores{$char}, 1); # corrupted line
            }
            pop @stack; # pop from stack
        }
    }
    return (0, 0); # line is not corrupted
}
