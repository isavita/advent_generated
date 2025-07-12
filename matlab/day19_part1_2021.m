
function main()
    fid = fopen('input.txt', 'r');
    if fid ~= -1
        content = fread(fid, '*char')';
        fclose(fid);
        disp(content);
    end
end

main();
