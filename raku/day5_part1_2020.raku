
sub MAIN {
    say 'input.txt'.IO.lines.map({ $_.trans('FL' => '00', 'BR' => '11').parse-base(2) }).max;
}
