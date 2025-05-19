
sub main {
    my @lines = 'input.txt'.IO.lines.map(*.chomp).grep(*.chars);
    my @rules;
    my @updates;
    my $i = 0;
    while $i < @lines.elems && @lines[$i] ~~ m/ '|' / {
        @rules.push: @lines[$i].split('|').map(*.Int);
        $i++;
    }
    while $i < @lines.elems {
        @updates.push: @lines[$i].split(',').map(*.Int);
        $i++;
    }

    my $s = 0;
    for @updates -> @u {
        if is-correct(@u, @rules) {
            $s += @u[@u.elems div 2];
        }
    }
    say $s;
}

sub is-correct(@update, @rules) {
    my %pos;
    for @update.kv -> $index, $value {
        %pos{$value} = $index;
    }

    for @rules -> @rule {
        my ($x, $y) = @rule;
        if %pos{$x}:exists and %pos{$y}:exists {
            if %pos{$x} > %pos{$y} {
                return False;
            }
        }
    }
    return True;
}

main();
