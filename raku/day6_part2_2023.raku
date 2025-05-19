
#!/usr/bin/env raku

sub main {
    my $content = slurp('input.txt');
    my @lines = $content.split("\n");

    my $time_line = @lines[0];
    my $distance_line = @lines[1];

    my $time = $time_line.split(':')[1].comb(/\d/).join('').Int;
    my $distance = $distance_line.split(':')[1].comb(/\d/).join('').Int;

    my $discriminant = $time * $time - 4 * $distance;
    my $sqrt_discriminant = sqrt($discriminant);

    my $root1 = ($time - $sqrt_discriminant) / 2;
    my $root2 = ($time + $sqrt_discriminant) / 2;

    my $epsilon = 1e-9;
    my $lower_bound_h = ($root1 + $epsilon).ceiling.Int;
    my $upper_bound_h = ($root2 - $epsilon).floor.Int;

    my $ways_to_win = $upper_bound_h - $lower_bound_h + 1;

    say $ways_to_win;
}

main();
