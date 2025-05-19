
class SnailNumber {
    has Int $.value is rw;
    has SnailNumber $.left is rw;
    has SnailNumber $.right is rw;

    method is-regular {
        !defined($!left) && !defined($!right)
    }

    method add(SnailNumber $other) {
        my $new = SnailNumber.new(:left(self.deep-copy), :right($other.deep-copy));
        $new.reduce;
    }

    method reduce {
        loop {
            my ($exploded, $lv, $rv) = self.explode(0);
            if $exploded {
                next;
            }
            unless self.split {
                last;
            }
        }
        self;
    }

    method explode(Int $depth --> List) {
        if self.is-regular {
            return (False, 0, 0);
        }

        if $depth == 4 {
            my $lv = $!left.value;
            my $rv = $!right.value;
            $!left = $!right = Nil;
            $!value = 0;
            return (True, $lv, $rv);
        }

        my ($exploded, $lv, $rv) = $!left.explode($depth + 1);
        if $exploded {
            if defined($!right) {
                $!right.add-left($rv);
            }
            return (True, $lv, 0);
        }

        ($exploded, $lv, $rv) = $!right.explode($depth + 1);
        if $exploded {
            if defined($!left) {
                $!left.add-right($lv);
            }
            return (True, 0, $rv);
        }

        return (False, 0, 0);
    }

    method add-left(Int $value) {
        if self.is-regular {
            $!value += $value;
        } else {
            $!left.add-left($value);
        }
    }

    method add-right(Int $value) {
        if self.is-regular {
            $!value += $value;
        } else {
            $!right.add-right($value);
        }
    }

    method split(--> Bool) {
        if self.is-regular {
            if defined($!value) && $!value >= 10 {
                $!left = SnailNumber.new(:value($!value div 2));
                $!right = SnailNumber.new(:value(($!value + 1) div 2));
                $!value = Nil;
                return True;
            }
            return False;
        }

        self.left.split or self.right.split;
    }

    method magnitude(--> Int) {
        if self.is-regular {
            return $!value // 0;
        }
        return 3 * $!left.magnitude + 2 * $!right.magnitude;
    }

    method deep-copy(--> SnailNumber) {
        if self.is-regular {
            return SnailNumber.new(:value($!value));
        }
        return SnailNumber.new(:left($!left.deep-copy), :right($!right.deep-copy));
    }
}

sub parse-snail-number(Str $input-str --> SnailNumber) {
    my $s = $input-str.trim;

    if $s ~~ m/^ \d+ $/ {
        return SnailNumber.new(:value($s.Int));
    }

    $s = $s.substr(1, $s.chars - 2);

    my $balance = 0;
    my $split-index = -1;
    for $s.comb.kv -> $i, $char {
        if $char eq '[' {
            $balance++;
        } elsif $char eq ']' {
            $balance--;
        } elsif $char eq ',' and $balance == 0 {
            $split-index = $i;
            last;
        }
    }

    my $left-str = $s.substr(0, $split-index);
    my $right-str = $s.substr($split-index + 1);

    my $left = parse-snail-number($left-str);
    my $right = parse-snail-number($right-str);

    return SnailNumber.new(:left($left), :right($right));
}

sub main {
    my @snail-numbers;
    for 'input.txt'.IO.lines -> $line {
        @snail-numbers.push(parse-snail-number($line.trim));
    }

    my $largest-magnitude = 0;

    for ^@snail-numbers.elems -> $i {
        for ^@snail-numbers.elems -> $j {
            if $i == $j {
                next;
            }

            my $a = @snail-numbers[$i];
            my $b = @snail-numbers[$j];

            my $sum1 = $a.add($b).magnitude;
            $largest-magnitude = max($largest-magnitude, $sum1);

            my $sum2 = $b.add($a).magnitude;
            $largest-magnitude = max($largest-magnitude, $sum2);
        }
    }

    say $largest-magnitude;
}

main;
