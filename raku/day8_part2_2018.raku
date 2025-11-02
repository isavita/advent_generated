
use v6;

sub sum-metadata(@nums, $idx is rw) {
    my $num-child = @nums[$idx++];
    my $num-metadata = @nums[$idx++];
    my $sum = 0;
    
    $sum += sum-metadata(@nums, $idx) for ^$num-child;
    $sum += @nums[$idx++] for ^$num-metadata;
    
    return $sum;
}

sub node-value(@nums, $idx is rw) {
    my $num-child = @nums[$idx++];
    my $num-metadata = @nums[$idx++];
    my @child-values;
    
    @child-values.push: node-value(@nums, $idx) for ^$num-child;
    
    my $value = 0;
    for ^$num-metadata {
        my $metadata = @nums[$idx++];
        $value += $num-child == 0 ?? $metadata !! 
                  (0 < $metadata <= $num-child ?? @child-values[$metadata-1] !! 0);
    }
    
    return $value;
}

sub MAIN {
    my $data = "input.txt".IO.slurp.trim.split(/\s+/).map(*.Int);
    my @nums = @$data;
    
    my $idx = 0;
    say sum-metadata(@nums, $idx);
    
    $idx = 0;
    say node-value(@nums, $idx);
}
