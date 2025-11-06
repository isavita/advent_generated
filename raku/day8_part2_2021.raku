
sub decode-signals(Str $filename) {
    my $total = 0;
    for $filename.IO.lines {
        my ($patterns, $output) = .split(' | ');
        my @patterns = $patterns.split(' ').map: *.comb.sort.join;
        my @output   = $output.split(' ').map: *.comb.sort.join;

        my %digit;
        %digit{1} = @patterns.first: *.chars == 2;
        %digit{7} = @patterns.first: *.chars == 3;
        %digit{4} = @patterns.first: *.chars == 4;
        %digit{8} = @patterns.first: *.chars == 7;

        %digit{3} = @patterns.first: { .chars == 5 && (%digit{1}.comb ⊆ .comb) }
        %digit{9} = @patterns.first: { .chars == 6 && (%digit{4}.comb ⊆ .comb) }
        %digit{0} = @patterns.first: { .chars == 6 && $_ ne %digit{9} && (%digit{1}.comb ⊆ .comb) }
        %digit{6} = @patterns.first: { .chars == 6 && $_ ne %digit{9} && $_ ne %digit{0} }
        %digit{5} = @patterns.first: { .chars == 5 && .comb ⊆ %digit{6}.comb }
        %digit{2} = @patterns.first: { .chars == 5 && $_ ne %digit{3} && $_ ne %digit{5} }

        my %rev = %digit.invert;
        $total += +( @output.map: { %rev{$_} // 0 } ).join;
    }
    $total
}

sub MAIN() {
    say decode-signals('input.txt')
}
