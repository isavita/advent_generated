
sub MAIN() {
    my @numbers = 'input.txt'.IO.lines.map: *.Int;
    my $preamble = 25;

    for $preamble ..^ @numbers.elems -> $i {
        my $target = @numbers[$i];
        my @window = @numbers[$i - $preamble .. $i - 1];

        my %seen;
        for @window -> $num {
            %seen{$num} = 1;
        }

        my $found = False;
        for @window -> $num1 {
            my $num2 = $target - $num1;
            if %seen{$num2} and $num1 != $num2 {
                $found = True;
                last;
            }
        }

        unless $found {
            say $target;
            last;
        }
    }
}
