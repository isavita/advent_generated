
use strict;
use warnings;

open(my $fh, '<', 'input.txt') or die "Error opening file: $!";
my @passports;
my $passport = "";

while (my $line = <$fh>) {
    chomp $line;
    if ($line eq "") {
        push @passports, $passport =~ s/^\s+//r;
        $passport = "";
    } else {
        $passport .= " $line";
    }
}
if ($passport ne "") {
    push @passports, $passport =~ s/^\s+//r;
}

my $validPassports = 0;

foreach my $p (@passports) {
    if (isValidPassport($p)) {
        $validPassports++;
    }
}

print "$validPassports\n";

sub isValidPassport {
    my $passport = shift;
    my @fields = split(' ', $passport);
    my %fieldMap;
    foreach my $field (@fields) {
        my @parts = split(':', $field);
        $fieldMap{$parts[0]} = $parts[1];
    }

    return validateByr($fieldMap{"byr"}) &&
        validateIyr($fieldMap{"iyr"}) &&
        validateEyr($fieldMap{"eyr"}) &&
        validateHgt($fieldMap{"hgt"}) &&
        validateHcl($fieldMap{"hcl"}) &&
        validateEcl($fieldMap{"ecl"}) &&
        validatePid($fieldMap{"pid"});
}

sub validateByr {
    my $value = shift;
    return validateYear($value, 1920, 2002);
}

sub validateIyr {
    my $value = shift;
    return validateYear($value, 2010, 2020);
}

sub validateEyr {
    my $value = shift;
    return validateYear($value, 2020, 2030);
}

sub validateYear {
    my ($value, $min, $max) = @_;
    my $year = int($value);
    return $year >= $min && $year <= $max;
}

sub validateHgt {
    my $value = shift;
    if ($value =~ /cm$/) {
        my $hgt = int($value =~ s/cm$//r);
        return $hgt >= 150 && $hgt <= 193;
    } elsif ($value =~ /in$/) {
        my $hgt = int($value =~ s/in$//r);
        return $hgt >= 59 && $hgt <= 76;
    }
    return 0;
}

sub validateHcl {
    my $value = shift;
    return $value =~ /^#[0-9a-f]{6}$/;
}

sub validateEcl {
    my $value = shift;
    my %validEcl = ("amb" => 1, "blu" => 1, "brn" => 1, "gry" => 1, "grn" => 1, "hzl" => 1, "oth" => 1);
    return exists $validEcl{$value};
}

sub validatePid {
    my $value = shift;
    return $value =~ /^[0-9]{9}$/;
}
