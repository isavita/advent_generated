
#!/usr/bin/env raku

sub read-all(Str $path) {
    $path.IO.slurp.chomp;
}

sub main {
    my @k;
    my @l;
    my @m;

    my @lines = read-all("input.txt").split("\n");

    for @lines.kv -> $i, $line {
        my @parts = $line.words;
        given $i % 18 {
            when 4 { @l.push: @parts[2].Int }
            when 5 { @k.push: @parts[2].Int }
            when 15 { @m.push: @parts[2].Int }
        }
    }

    my %constraints;
    my @stack;
    for 0..13 -> $i {
        if @l[$i] == 1 {
            @stack.push($i);
        } else {
            my $pop = @stack.pop;
            %constraints{$pop} = [$i, @m[$pop] + @k[$i]];
        }
    }

    my @min-val = 0 xx 14;
    for 0..13 -> $i {
        if %constraints{$i}:exists {
            my ($j, $diff) = %constraints{$i}.List;
            my $vmin = max(1, 1 - $diff);

            @min-val[$i] = $vmin;
            @min-val[$j] = $vmin + $diff;
        }
    }

    say @min-val.join('').Int;
}

sub MAIN {
    main();
}
