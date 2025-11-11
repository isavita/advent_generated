
#!/usr/bin/env raku

my %wires;
my @gates;

for "input.txt".IO.lines {
    next if .trim eq '';
    if /^ (\w+) ':' \s* (\d+) $/ {
        %wires{~$0} = { value => +$1, initialized => True };
    } elsif /^ (\w+) \s+ (AND|OR|XOR) \s+ (\w+) \s+ '->' \s+ (\w+) $/ {
        my ($in1, $op, $in2, $out) = ~$0, ~$1, ~$2, ~$3;
        @gates.push: { input1 => $in1, op => $op, input2 => $in2, output => $out };
        %wires{$_} //= { value => 0, initialized => False } for $in1, $in2, $out;
    }
}

my $changed = True;
while $changed {
    $changed = False;
    for @gates -> %g {
        if %wires{%g<input1>}<initialized> && %wires{%g<input2>}<initialized> && !%wires{%g<output>}<initialized> {
            %wires{%g<output>}<value> = do given %g<op> {
                when 'AND' { %wires{%g<input1>}<value> +& %wires{%g<input2>}<value> }
                when 'OR'  { %wires{%g<input1>}<value> +| %wires{%g<input2>}<value> }
                when 'XOR' { %wires{%g<input1>}<value> +^ %wires{%g<input2>}<value> }
            };
            %wires{%g<output>}<initialized> = True;
            $changed = True;
        }
    }
}

say [+] %wires.keys.grep(/^z/).map: {
    my $bit = .substr(1).Int;
    %wires{$_}<initialized> && %wires{$_}<value> ?? 2 ** $bit !! 0
}
