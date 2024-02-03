
use strict;
use warnings;

open(my $fh, '<', 'input.txt') or die $!;
my @messages;
while (my $line = <$fh>) {
    chomp $line;
    push @messages, $line;
}
close($fh);

my $originalMessage = getOriginalMessage(\@messages);
print "$originalMessage\n";

sub getOriginalMessage {
    my ($messages) = @_;
    if (@$messages == 0) {
        return "";
    }
    my $messageLength = length($messages->[0]);
    my @count;
    for (my $i = 0; $i < $messageLength; $i++) {
        $count[$i] = {};
    }

    foreach my $message (@$messages) {
        my @chars = split //, $message;
        for (my $j = 0; $j < @chars; $j++) {
            $count[$j]{$chars[$j]}++;
        }
    }

    my $originalMessage = "";
    foreach my $charCount (@count) {
        $originalMessage .= getLeastCommonChar($charCount);
    }

    return $originalMessage;
}

sub getLeastCommonChar {
    my ($count) = @_;
    my $minChar;
    my $minCount = ~0 >> 1;
    foreach my $char (keys %$count) {
        if ($count->{$char} < $minCount) {
            $minCount = $count->{$char};
            $minChar = $char;
        }
    }
    return $minChar;
}
