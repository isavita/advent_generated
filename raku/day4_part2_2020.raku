
sub MAIN {
    my $valid = 0;
    my %p;
    for 'input.txt'.IO.lines -> $line {
        if $line.trim eq '' {
            $valid++ if check-passport(%p);
            %p = ();
            next;
        }
        %p.append: $line.split(' ').map: { .split(':')[0] => .split(':')[1] }
    }
    $valid++ if check-passport(%p);
    put $valid;
}

sub check-passport(%p) {
    return False unless %p<byr iyr eyr hgt hcl ecl pid>.all.so;
    return False unless 1920 <= +%p<byr> <= 2002;
    return False unless 2010 <= +%p<iyr> <= 2020;
    return False unless 2020 <= +%p<eyr> <= 2030;
    return False unless %p<hgt> ~~ / ^ (\d+) (cm || in) $ / && do {
        $1 eq 'cm' ?? 150 <= $0 <= 193 !! 59 <= $0 <= 76
    };
    return False unless %p<hcl> ~~ / ^ '#' <[0..9a..f]> ** 6 $ /;
    return False unless %p<ecl> ∈ <amb blu brn gry grn hzl oth>;
    return False unless %p<pid> ~~ / ^ \d ** 9 $ /;
    True
}
