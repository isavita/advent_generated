
function main()
    data = fileread('input.txt');
    steps = (data == '(') - (data == ')');
    floors = cumsum(steps);
    position = find(floors == -1, 1, 'first');
    fprintf('%d\n', position);
end
