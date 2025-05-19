
#!/usr/bin/env raku
use v6;

sub dragon_curve(Str $a) {
    $a ~ '0' ~ $a.flip.trans('01' => '10')
}

sub generate_data(Str $initial_state, Int $disk_length) {
    my $data = $initial_state;
    while $data.chars < $disk_length {
        $data = dragon_curve($data);
    }
    return $data.substr(0, $disk_length);
}

sub calculate_checksum(Str $data) {
    my $checksum = $data.comb.rotor(2).map({ $_[0] eq $_[1] ?? '1' !! '0' }).join('');
    if $checksum.chars %% 2 {
        calculate_checksum($checksum);
    } else {
        $checksum;
    }
}

sub MAIN {
    my $initial_state = open('input.txt').slurp.chomp;

    my $disk_length_1 = 272;
    my $disk_data_1 = generate_data($initial_state, $disk_length_1);
    my $checksum_1 = calculate_checksum($disk_data_1);
    say $checksum_1;

    my $disk_length_2 = 35651584;
    my $disk_data_2 = generate_data($initial_state, $disk_length_2);
    my $checksum_2 = calculate_checksum($disk_data_2);
    say $checksum_2;
}
