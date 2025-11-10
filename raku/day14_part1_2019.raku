
#!/usr/bin/env raku

# Define a class to represent a chemical reaction
class Reaction {
    has $.output;
    has @.inputs;
    
    method new($output, @inputs) {
        self.bless(:$output, :@inputs);
    }
}

# Parse a chemical string like "10 A" into a hash
sub parse-chemical(Str $s) {
    my ($amount, $name) = $s.trim.split(/\s+/, 2);
    return { amount => $amount.Int, name => $name };
}

# Main function to calculate required ore
sub MAIN() {
    my $file = open "input.txt", :r or die "Cannot open input.txt: $!";
    
    # Parse reactions from file
    my %reactions = {};
    for $file.lines -> $line {
        my ($inputs-str, $output-str) = split "=>", $line, 2;
        my $output = parse-chemical($output-str.trim);
        my @inputs = $inputs-str.trim.split(',')».trim.map: { parse-chemical($_) };
        %reactions{$output<name>} = Reaction.new($output, @inputs);
    }
    $file.close;
    
    # Calculate ore needed for 1 fuel
    my %surplus = {};
    my $ore = calculate-ore("FUEL", 1, %reactions, %surplus);
    say $ore;
}

# Recursive function to calculate ore required
sub calculate-ore(Str $chem-name, Int $amount is copy, %reactions, %surplus) {
    return $amount if $chem-name eq "ORE";
    
    my $reaction = %reactions{$chem-name} or die "No reaction for $chem-name";
    
    # Use existing surplus
    if %surplus{$chem-name}:exists && %surplus{$chem-name} >= $amount {
        %surplus{$chem-name} -= $amount;
        return 0;
    }
    
    $amount -= %surplus{$chem-name} // 0;
    %surplus{$chem-name} = 0;
    
    my $times = ceiling($amount / $reaction.output<amount>);
    my $ore = 0;
    
    for $reaction.inputs -> $input {
        $ore += calculate-ore($input<name>, $input<amount> * $times, %reactions, %surplus);
    }
    
    %surplus{$chem-name} += $times * $reaction.output<amount> - $amount;
    return $ore;
}
