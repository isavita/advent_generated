
sub memo-dfs(%graph, $entry, %memo) {
    if %memo{$entry}:exists {
        return %memo{$entry}
    }

    if $entry ~~ /^\d+$/ {
        return $entry.Int
    }

    my $source-rule = %graph{$entry};
    my @parts = $source-rule.split(" ");

    my $result;
    given @parts.elems {
        when 1 {
            $result = memo-dfs(%graph, @parts[0], %memo)
        }
        when 2 {
            my $val = memo-dfs(%graph, @parts[1], %memo);
            $result = 65535 +^ $val;
        }
        when 3 {
            my $left = memo-dfs(%graph, @parts[0], %memo);
            my $right = memo-dfs(%graph, @parts[2], %memo);
            given @parts[1] {
                when "AND"    { $result = $left +& $right }
                when "OR"     { $result = $left +| $right }
                when "LSHIFT" { $result = $left +< $right }
                when "RSHIFT" { $result = $left +> $right }
            }
        }
    }

    %memo{$entry} = $result;
    return $result;
}

sub some-assembly-required($input) {
    my %wire-to-rule;

    for $input.lines -> $inst {
        my @parts = $inst.split(" -> ");
        %wire-to-rule{@parts[1]} = @parts[0];
    }

    my $a-signal = memo-dfs(%wire-to-rule, "a", %());

    %wire-to-rule<b> = $a-signal.Str;
    return memo-dfs(%wire-to-rule, "a", %());
}

sub MAIN {
    my $input-data = slurp "input.txt".trim;
    say some-assembly-required($input-data);
}
