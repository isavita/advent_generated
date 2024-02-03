
open(my $fh, '<', 'input.txt') or die "Error opening file: $!";
my $totalCount = 0;
my %groupAnswers;

while (my $line = <$fh>) {
    chomp $line;
    if ($line eq "") {
        $totalCount += scalar keys %groupAnswers;
        %groupAnswers = ();
    } else {
        foreach my $question (split //, $line) {
            $groupAnswers{$question} = 1;
        }
    }
}

$totalCount += scalar keys %groupAnswers;
print "$totalCount\n";
