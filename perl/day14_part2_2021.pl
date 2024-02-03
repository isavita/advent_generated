
use strict;
use warnings;

open my $fh, '<', 'input.txt' or die $!;
my $template = <$fh>;
chomp $template;

my %rules;
while (my $line = <$fh>) {
    chomp $line;
    next if $line eq "";
    my ($key, $value) = split / -> /, $line;
    $rules{$key} = $value;
}
close $fh;

my %pairCounts;
for (my $i = 0; $i < length($template) - 1; $i++) {
    $pairCounts{substr($template, $i, 2)}++;
}

for (my $step = 0; $step < 40; $step++) {
    my %newPairCounts;
    while (my ($pair, $count) = each %pairCounts) {
        if (exists $rules{$pair}) {
            my $insert = $rules{$pair};
            $newPairCounts{substr($pair, 0, 1) . $insert} += $count;
            $newPairCounts{$insert . substr($pair, 1, 1)} += $count;
        } else {
            $newPairCounts{$pair} += $count;
        }
    }
    %pairCounts = %newPairCounts;
}

my %elementCounts;
while (my ($pair, $count) = each %pairCounts) {
    $elementCounts{substr($pair, 0, 1)} += $count;
}
$elementCounts{substr($template, -1)}++;

my ($maxCount, $minCount) = (0, 1<<63 - 1);
while (my ($key, $count) = each %elementCounts) {
    $maxCount = $count if $count > $maxCount;
    $minCount = $count if $count < $minCount;
}

print $maxCount - $minCount . "\n";
