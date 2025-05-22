
sub MAIN() {
    my $file-content = 'input.txt'.IO.slurp;

    my @passports = $file-content.split("\n\n").map({ .trim.trans("\n" => " ") });

    my @required-fields = <byr iyr eyr hgt hcl ecl pid>;

    my $valid-passports = 0;
    for @passports -> $p {
        if is-valid($p, @required-fields) {
            $valid-passports++;
        }
    }

    $valid-passports.say;
}

sub is-valid(Str $passport, @required-fields --> Bool) {
    for @required-fields -> $field {
        unless $passport.contains("$field:") {
            return False;
        }
    }
    True;
}
