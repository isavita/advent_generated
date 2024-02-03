
open(my $fh, '<', 'input.txt') or die "Error opening file: $!";
my @passports;
my $passport = "";

while (my $line = <$fh>) {
    chomp($line);
    if ($line eq "") {
        push @passports, $passport;
        $passport = "";
    } else {
        $passport .= " " . $line;
    }
}
if ($passport ne "") {
    push @passports, $passport;
}

my $validPassports = 0;
my @requiredFields = ("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid");

foreach my $p (@passports) {
    if (isValid($p, \@requiredFields)) {
        $validPassports++;
    }
}

print "$validPassports\n";

sub isValid {
    my ($passport, $requiredFields) = @_;
    foreach my $field (@$requiredFields) {
        if ($passport !~ /$field:/) {
            return 0;
        }
    }
    return 1;
}
