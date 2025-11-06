
sub is-moving-away($x-pos, $y-pos, $x-vel, $y-vel, $x-min, $x-max, $y-min, $y-max) {
    return True if $x-pos < $x-min && $x-vel < 0;
    return True if $x-pos > $x-max && $x-vel > 0;
    return True if $y-pos < $y-min && $y-vel < 0;
    False
}

my $line = slurp 'input.txt';
my ($x-range-str, $y-range-str) = $line.split(', ');
my ($x-min, $x-max) = $x-range-str.substr(15).split('..').map: *.Int;
my ($y-min, $y-max) = $y-range-str.substr(2).split('..').map: *.Int;

my $count = 0;

for -1000..1000 -> $x-vel {
    for -1000..1000 -> $y-vel {
        my ($x-pos, $y-pos) = 0, 0;
        my ($cur-x-vel, $cur-y-vel) = $x-vel, $y-vel;
        my Bool $in-target = False;
        loop {
            $x-pos += $cur-x-vel;
            $y-pos += $cur-y-vel;

            if $x-min <= $x-pos <= $x-max && $y-min <= $y-pos <= $y-max {
                $in-target = True;
                last;
            }

            last if is-moving-away($x-pos, $y-pos, $cur-x-vel, $cur-y-vel,
                                   $x-min, $x-max, $y-min, $y-max);

            $cur-x-vel -= 1 if $cur-x-vel > 0;
            $cur-x-vel += 1 if $cur-x-vel < 0;
            $cur-y-vel -= 1;
        }

        $count++ if $in-target;
    }
}

say $count;
