
my $content = slurp('input.txt');
my $enabled = True;
my $total = 0;
my $i = 0;
while $i < $content.chars {
    if $content.substr($i, 4) eq 'mul(' {
        my $substring = $content.substr($i);
        if $substring ~~ m/ ^ 'mul(' (\d ** 1..3) ',' (\d ** 1..3) ')' / {
            if $enabled {
                $total += $0.Int * $1.Int;
            }
            $i += $/.chars;
        } else {
            $i++;
        }
    } elsif $content.substr($i, 4) eq 'do()' {
        $enabled = True;
        $i += 4;
    } elsif $content.substr($i, 7) eq "don't()" {
        $enabled = False;
        $i += 7;
    } else {
        $i++;
    }
}
say $total;
