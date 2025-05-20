
sub MAIN {
    my @changes = 'input.txt'.IO.lines.map(*.Int);
    my $frequency = 0;
    my %seen;
    %seen{$frequency} = True;

    loop {
        for @changes -> $change {
            $frequency += $change;
            if %seen{$frequency}:exists {
                say $frequency;
                exit;
            }
            %seen{$frequency} = True;
        }
    }
}
