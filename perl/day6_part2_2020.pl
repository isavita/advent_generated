
use strict;
use warnings;

open my $fh, '<', 'input.txt' or die "Error opening file: $!";
my $totalCount = 0;
my %groupAnswers;
my $groupSize = 0;

while (my $line = <$fh>) {
    chomp $line;
    if ($line eq "") {
        foreach my $count (values %groupAnswers) {
            if ($count == $groupSize) {
                $totalCount++;
            }
        }
        %groupAnswers = ();
        $groupSize = 0;
    } else {
        $groupSize++;
        foreach my $question (split //, $line) {
            $groupAnswers{$question}++;
        }
    }
}

foreach my $count (values %groupAnswers) {
    if ($count == $groupSize) {
        $totalCount++;
    }
}

print "$totalCount\n";
