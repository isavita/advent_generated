
function main()
    fid = fopen('input.txt', 'r');
    if fid == -1
        error('Error opening file');
    end

    dirs = struct('path', {}, 'size', {});
    files = struct('path', {}, 'size', {});
    current_path = '/';
    dir_count = 0;
    file_count = 0;

    while ~feof(fid)
        line = strtrim(fgets(fid));
        if isempty(line)
            continue;
        end

        tokens = strsplit(line, ' ');

        if strcmp(tokens{1}, '$')
            if strcmp(tokens{2}, 'cd')
                dir_name = tokens{3};
                if strcmp(dir_name, '/')
                    current_path = '/';
                elseif strcmp(dir_name, '..')
                    last_slash = find(current_path == '/', 1, 'last');
                    if ~isempty(last_slash) && last_slash ~= 1
                        current_path = current_path(1:last_slash-1);
                    else
                        current_path = '/';
                    end
                else
                    if ~strcmp(current_path, '/')
                        current_path = [current_path, '/', dir_name];
                    else
                        current_path = [current_path, dir_name];
                    end
                end

                if ~any(strcmp({dirs.path}, current_path))
                    dir_count = dir_count + 1;
                    dirs(dir_count).path = current_path;
                    dirs(dir_count).size = 0;
                end
            end
        elseif ~strcmp(tokens{1}, 'dir')
            file_size = str2double(tokens{1});
            file_name = tokens{2};
            file_path = [current_path, '/', file_name];
            if strcmp(current_path, '/')
                file_path = ['/', file_name];
            end

            file_count = file_count + 1;
            files(file_count).path = file_path;
            files(file_count).size = file_size;
        end
    end
    fclose(fid);

    for i = 1:file_count
        file_path_tokens = strsplit(files(i).path, '/');
        current_dir_path = '';
        for j = 2:length(file_path_tokens)
            if isempty(file_path_tokens{j})
                continue;
            end
            if isempty(current_dir_path)
                current_dir_path = '/';
            else
                current_dir_path = [current_dir_path, '/', file_path_tokens{j}];
            end

            dir_idx = find(strcmp({dirs.path}, current_dir_path), 1);
            if ~isempty(dir_idx)
                dirs(dir_idx).size = dirs(dir_idx).size + files(i).size;
            else
                dir_count = dir_count + 1;
                dirs(dir_count).path = current_dir_path;
                dirs(dir_count).size = files(i).size;
            end
        end
    end

    dir_sizes = zeros(1, dir_count);
    for i = 1:dir_count
        dir_sizes(i) = dirs(i).size;
    end

    sorted_sizes = sort(dir_sizes);

    total_space = 70000000;
    required_space = 30000000;

    root_size = 0;
    for i = 1:dir_count
        if strcmp(dirs(i).path, '/')
            root_size = dirs(i).size;
            break;
        end
    end

    available_space = total_space - root_size;
    needed_space = required_space - available_space;

    result = -1;
    for i = 1:length(sorted_sizes)
        if sorted_sizes(i) >= needed_space
            result = sorted_sizes(i);
            break;
        end
    end

    fprintf('%d\n', result);
end
