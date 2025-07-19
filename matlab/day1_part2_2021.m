
function main
    vals = load('input.txt');
    sums = conv(vals, [1 1 1], 'valid');
    count = sum(diff(sums) > 0);
    fprintf('%d\n', count);
end
