
use strict;
use warnings;

open(my $fh, '<', 'input.txt') or die $!;
my @messages;
while (my $line = <$fh>) {
    chomp $line;
    push @messages, $line;
}
close($fh);

my $correctedMessage = getCorrectedMessage(\@messages);
print "$correctedMessage\n";

sub getCorrectedMessage {
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
        my @chars = split(//, $message);
        for (my $j = 0; $j < scalar(@chars); $j++) {
            $count[$j]{$chars[$j]}++;
        }
    }

    my $correctedMessage = "";
    foreach my $charCount (@count) {
        $correctedMessage .= getMostCommonChar($charCount);
    }

    return $correctedMessage;
}

sub getMostCommonChar {
    my ($count) = @_;
    my $maxChar;
    my $maxCount = 0;
    foreach my $char (keys %$count) {
        if ($count->{$char} > $maxCount) {
            $maxCount = $count->{$char};
            $maxChar = $char;
        }
    }
    return $maxChar;
}
