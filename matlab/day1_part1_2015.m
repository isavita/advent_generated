
function main()
    data = fileread('input.txt');
    floor = sum(data == '(') - sum(data == ')');
    disp(floor);
end
