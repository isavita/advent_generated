
sub MAIN() {
    my @lines = 'input.txt'.IO.lines;
    my $template = @lines.shift;
    my %rules;
    for @lines {
        next unless .chars;
        my ($pair,$ins) = .split(' -> ');
        %rules{$pair} = $ins;
    }

    my %pair-count;
    for 0..$template.chars-2 {
        my $pair = $template.substr($_,2);
        %pair-count{$pair}++;
    }

    for ^40 {
        my %new;
        for %pair-count.kv -> $pair,$cnt {
            if %rules{$pair}:exists {
                my $ins = %rules{$pair};
                %new{$pair.substr(0,1)~$ins} += $cnt;
                %new{$ins~$pair.substr(1,1)} += $cnt;
            } else {
                %new{$pair} += $cnt;
            }
        }
        %pair-count = %new;
    }

    my %element;
    for %pair-count.kv -> $pair,$cnt {
        %element{$pair.substr(0,1)} += $cnt;
    }
    %element{$template.substr(*-1)}++;

    say %element.values.max - %element.values.min;
}
