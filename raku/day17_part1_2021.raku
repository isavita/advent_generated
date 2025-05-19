
use v6;

sub MAIN {
    my $line = 'input.txt'.IO.slurp.chomp;
    my ($x-part, $y-part) = $line.split(': ')[1].split(', ');
    my ($x-min, $x-max) = $x-part.split('=')[1].split('..').map(*.Int);
    my ($y-min, $y-max) = $y-part.split('=')[1].split('..').map(*.Int);

    my $min-x-vel = 0;
    my $cur-v = 0;
    my $cur-pos = 0;
    while $cur-pos < $x-min {
        $cur-v++;
        $cur-pos += $cur-v;
    }
    $min-x-vel = $cur-v;

    my $max-y-global = -Inf;

    for $min-x-vel .. $x-max -> $x-vel {
        for $y-min .. abs($y-min) -> $y-vel {
            my $x-pos = 0;
            my $y-pos = 0;
            my $cur-x-vel = $x-vel;
            my $cur-y-vel = $y-vel;
            my $highest-y = $y-pos;
            my $hit = False;

            loop {
                $x-pos += $cur-x-vel;
                $y-pos += $cur-y-vel;
                $highest-y = ($highest-y, $y-pos).max;

                if $x-min <= $x-pos <= $x-max && $y-min <= $y-pos <= $y-max {
                    $hit = True;
                    last;
                }

                if $x-pos > $x-max || ($y-pos < $y-min && $cur-y-vel < 0) || ($cur-x-vel == 0 && $x-pos < $x-min) {
                     last;
                }

                if $cur-x-vel > 0 {
                    $cur-x-vel--;
                }
                $cur-y-vel--;
            }

            if $hit {
                $max-y-global = ($max-y-global, $highest-y).max;
            }
        }
    }
    say $max-y-global;
}
