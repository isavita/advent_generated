
use v6;

class SnailNumber {
    has Int $.value is rw;
    has SnailNumber $.left is rw;
    has SnailNumber $.right is rw;

    method is-regular { $!left.defined.not && $!right.defined.not }

    method add(SnailNumber $other) {
        SnailNumber.new(left => self, right => $other).reduce
    }

    method reduce() {
        loop {
            my ($exploded, $lval, $rval) = self!explode(0);
            if $exploded { next }
            unless self!split() { last }
        }
        self
    }

    method !explode(Int $depth) {
        if self.is-regular { return False, 0, 0 }

        if $depth == 4 {
            my $lval = $!left.value;
            my $rval = $!right.value;
            $!left = $!right = Nil;
            $!value = 0;
            return True, $lval, $rval;
        }

        my ($exploded, $lval, $rval) = $!left!explode($depth + 1);
        if $exploded {
            if $rval > 0 { $!right!add-left($rval) }
            return True, $lval, 0;
        }

        ($exploded, $lval, $rval) = $!right!explode($depth + 1);
        if $exploded {
            if $lval > 0 { $!left!add-right($lval) }
            return True, 0, $rval;
        }

        return False, 0, 0;
    }

    method !add-left(Int $value) {
        if self.is-regular {
            $!value += $value;
        } else {
            $!left!add-left($value);
        }
    }

    method !add-right(Int $value) {
        if self.is-regular {
            $!value += $value;
        } else {
            $!right!add-right($value);
        }
    }

    method !split() {
        if self.is-regular {
            if $!value >= 10 {
                $!left = SnailNumber.new(value => $!value div 2);
                $!right = SnailNumber.new(value => ($!value + 1) div 2);
                $!value = 0;
                return True;
            }
            return False;
        }
        $!left!split() || $!right!split()
    }

    method magnitude() {
        if self.is-regular {
            return $!value;
        }
        3 * $!left.magnitude() + 2 * $!right.magnitude()
    }
}

sub parse-snail-number(Str $input-str) {
    my $s = $input-str.trim;
    unless $s.starts-with('[') {
        return SnailNumber.new(value => $s.Int);
    }

    my $balance = 0;
    my $split-index = 0;
    for 1 .. $s.chars - 2 -> $i {
        my $char = $s.substr($i, 1);
        if $char eq '[' { $balance++ }
        elsif $char eq ']' { $balance-- }
        elsif $char eq ',' {
            if $balance == 0 {
                $split-index = $i;
                last;
            }
        }
    }

    my $left-str = $s.substr(1, $split-index - 1);
    my $right-str = $s.substr($split-index + 1, $s.chars - 2 - $split-index);

    my $left = parse-snail-number($left-str);
    my $right = parse-snail-number($right-str);

    SnailNumber.new(left => $left, right => $right);
}

sub MAIN() {
    my @snail-numbers = 'input.txt'.IO.lines.map(&parse-snail-number);

    my $result = @snail-numbers[0];
    for @snail-numbers[1..*] -> $num {
        $result = $result.add($num);
    }

    say $result.magnitude;
}
