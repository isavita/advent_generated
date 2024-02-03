
use strict;
use warnings;

open(my $fh, '<', 'input.txt') or die "Cannot open file: $!";
my @replacements;
my $molecule;

while (my $line = <$fh>) {
    chomp $line;
    next if $line eq "";
    if ($line =~ / => /) {
        push @replacements, $line;
    } else {
        $molecule = $line;
    }
}

my %molecules;
foreach my $replacement (@replacements) {
    my @parts = split / => /, $replacement;
    for (my $i = 0; $i < length($molecule); $i++) {
        if (index($molecule, $parts[0], $i) == $i) {
            my $newMolecule = substr($molecule, 0, $i) . $parts[1] . substr($molecule, $i + length($parts[0]));
            $molecules{$newMolecule} = 1;
        }
    }
}

print scalar keys %molecules;
