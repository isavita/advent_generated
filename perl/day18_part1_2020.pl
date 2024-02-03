
use strict;
use warnings;

open(my $fh, "<", "input.txt") or die "Error opening file: $!";
my $sum = 0;

while (my $expression = <$fh>) {
    chomp $expression;
    my $result = evaluate($expression);
    $sum += $result;
}

print "$sum\n";

sub evaluate {
    my ($expression) = @_;
    my @tokens = tokenize($expression);
    return evaluateTokens(\@tokens);
}

sub tokenize {
    my ($expression) = @_;
    $expression =~ s/\(/( /g;
    $expression =~ s/\)/ )/g;
    return split(/\s+/, $expression);
}

sub evaluateTokens {
    my ($tokens) = @_;
    my @ops = ();
    my @vals = ();

    for (my $i = 0; $i < scalar(@$tokens); $i++) {
        my $token = $tokens->[$i];
        if ($token eq "(") {
            push @ops, $token;
        } elsif ($token eq "+" || $token eq "*") {
            while (scalar(@ops) > 0 && $ops[-1] ne "(") {
                push @vals, applyOp(pop(@ops), pop(@vals), pop(@vals));
            }
            push @ops, $token;
        } elsif ($token eq ")") {
            while ($ops[-1] ne "(") {
                push @vals, applyOp(pop(@ops), pop(@vals), pop(@vals));
            }
            pop @ops; # Remove the opening '('
        } else {
            push @vals, $token;
        }
    }
    while (scalar(@ops) > 0) {
        push @vals, applyOp(pop(@ops), pop(@vals), pop(@vals));
    }
    return $vals[0];
}

sub applyOp {
    my ($op, $a, $b) = @_;
    if ($op eq "+") {
        return $a + $b;
    } elsif ($op eq "*") {
        return $a * $b;
    } else {
        die "Unknown operator: $op";
    }
}
