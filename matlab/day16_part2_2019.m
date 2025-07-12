
function main()

INPUT_REPEAT = 10000;
PHASES = 100;

fid = fopen('input.txt', 'r');
input_str = fgetl(fid);
fclose(fid);

input_digits = double(input_str) - double('0');

offset = str2double(input_str(1:7));

repeatedInput = repmat(input_digits, 1, INPUT_REPEAT);

for phase = 1:PHASES
    segment = repeatedInput(offset + 1 : end);
    repeatedInput(offset + 1 : end) = mod(flip(cumsum(flip(segment))), 10);
end

result_digits = repeatedInput(offset + 1 : offset + 8);
fprintf('%d', result_digits);
fprintf('\n');

end
