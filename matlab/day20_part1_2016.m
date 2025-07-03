
function main()
    fileId = fopen('input.txt', 'r');
    blocked_ips = [];
    while ~feof(fileId)
        line = strtrim(fgets(fileId));
        parts = strsplit(line, '-');
        start = str2double(parts{1});
        end_ip = str2double(parts{2});
        blocked_ips = [blocked_ips; [start, end_ip]];
    end
    fclose(fileId);
    
    blocked_ips = sortrows(blocked_ips);
    
    current_ip = 0;
    for i = 1:size(blocked_ips, 1)
        start = blocked_ips(i, 1);
        end_ip = blocked_ips(i, 2);
        if current_ip < start
            break
        end
        current_ip = max(current_ip, end_ip + 1);
    end
    
    fprintf('%d\n', current_ip);
end

main();
