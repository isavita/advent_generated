use strict;
use warnings;

my $Size = 10007;
my @deck = (0..$Size-1);

open(my $fh, "<", "input.txt") or die "Cannot open file: $!";
while (my $line = <$fh>) {
    chomp $line;

    if ($line eq "deal into new stack") {
        @deck = dealIntoNewStack(\@deck);
        next;
    }

    if ($line =~ /^cut (-?\d+)$/) {
        my $n = $1;
        @deck = cutN(\@deck, $n);
        next;
    }

    if ($line =~ /deal with increment (\d+)$/) {
        my $n = $1;
        @deck = dealWithIncrement(\@deck, $n);
        next;
    }
}

print find2019(\@deck) . "\n";

sub dealIntoNewStack {
    my ($deck) = @_;
    for (my $i = 0; $i < $Size/2; $i++) {
        ($deck->[$i], $deck->[$Size-$i-1]) = ($deck->[$Size-$i-1], $deck->[$i]);
    }
    return @$deck;
}

sub cutN {
    my ($deck, $n) = @_;
    if ($n >= 0) {
        return (@$deck[$n..$#$deck], @$deck[0..$n-1]);
    } else {
        return (@$deck[$n+$#deck+1..$#$deck], @$deck[0..$#deck+$n]);
    }
}

sub dealWithIncrement {
    my ($deck, $n) = @_;
    my @newDeck;
    for (my $i = 0; $i < $Size; $i++) {
        $newDeck[($i*$n)%$Size] = $deck->[$i];
    }
    return @newDeck;
}

sub find2019 {
    my ($deck) = @_;
    for (my $i = 0; $i < $Size; $i++) {
        if ($deck->[$i] == 2019) {
            return $i;
        }
    }
    return -1;
}