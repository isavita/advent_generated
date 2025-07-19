
function main
    txt = fileread('input.txt');
    lines = regexp(txt,'\r?\n','split');
    lines = lines(~cellfun('isempty',lines));
    
    nice1 = 0;
    nice2 = 0;
    
    for s = lines
        str = s{1};
        
        % Part 1
        vowels = sum(ismember(str,['a','e','i','o','u']));
        double = any(str(1:end-1)==str(2:end));
        bad = ~isempty(regexp(str,'ab|cd|pq|xy','once'));
        nice1 = nice1 + (vowels>=3 && double && ~bad);
        
        % Part 2
        pair = ~isempty(regexp(str,'(..).*\1','once'));
        repeat = any(str(1:end-2)==str(3:end));
        nice2 = nice2 + (pair && repeat);
    end
    
    fprintf('%d\n%d\n',nice1,nice2);
end
