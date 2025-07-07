
function main()
    % Reads input from input.txt and prints the output to standard output.
    % Solves the "Print Queue" coding challenge.

    % Read the entire file content
    fileContent = fileread('input.txt');

    % Split the content into rules and updates
    parts = strsplit(fileContent, '\n\n');
    ruleLines = strsplit(parts{1}, '\n');
    updateLines = strsplit(parts{2}, '\n');

    % Parse the ordering rules into a dependency graph (adjacency list)
    % and also store the reverse dependencies for easier checking.
    dependencies = containers.Map('KeyType', 'char', 'ValueType', 'any');
    reverseDependencies = containers.Map('KeyType', 'char', 'ValueType', 'any');

    for i = 1:length(ruleLines)
        if isempty(ruleLines{i})
            continue;
        end
        ruleParts = strsplit(ruleLines{i}, '|');
        pre = ruleParts{1};
        post = ruleParts{2};

        % Add to dependencies: pre -> post
        if ~isKey(dependencies, pre)
            dependencies(pre) = {};
        end
        dependencies(pre) = [dependencies(pre), {post}];

        % Add to reverse dependencies: post -> pre
        if ~isKey(reverseDependencies, post)
            reverseDependencies(post) = {};
        end
        reverseDependencies(post) = [reverseDependencies(post), {pre}];
    end

    totalMiddlePage = 0;

    % Process each update
    for i = 1:length(updateLines)
        if isempty(updateLines{i})
            continue;
        end
        currentUpdatePages = strsplit(updateLines{i}, ',');
        currentUpdatePages = strtrim(currentUpdatePages); % Remove potential whitespace

        % Check if the current update is in the correct order
        isCorrectOrder = true;
        for j = 1:length(currentUpdatePages)
            currentPage = currentUpdatePages{j};

            % Check dependencies where currentPage is the prerequisite
            if isKey(dependencies, currentPage)
                dependentPages = dependencies(currentPage);
                for k = 1:length(dependentPages)
                    dependentPage = dependentPages{k};
                    % If the dependent page is also in the current update
                    if any(strcmp(dependentPage, currentUpdatePages))
                        % Find the index of the dependent page in the update
                        dependentPageIndex = find(strcmp(dependentPage, currentUpdatePages), 1);
                        % If the dependent page appears before the current page, it's an error
                        if dependentPageIndex < j
                            isCorrectOrder = false;
                            break;
                        end
                    end
                end
            end
            if ~isCorrectOrder
                break;
            end

            % Check reverse dependencies where currentPage is the dependent
            if isKey(reverseDependencies, currentPage)
                prerequisitePages = reverseDependencies(currentPage);
                for k = 1:length(prerequisitePages)
                    prerequisitePage = prerequisitePages{k};
                    % If the prerequisite page is also in the current update
                    if any(strcmp(prerequisitePage, currentUpdatePages))
                        % Find the index of the prerequisite page in the update
                        prerequisitePageIndex = find(strcmp(prerequisitePage, currentUpdatePages), 1);
                        % If the prerequisite page appears after the current page, it's an error
                        if prerequisitePageIndex > j
                            isCorrectOrder = false;
                            break;
                        end
                    end
                end
            end
            if ~isCorrectOrder
                break;
            end
        end

        % If the update is in the correct order, find and add its middle page
        if isCorrectOrder
            numPages = length(currentUpdatePages);
            middleIndex = ceil(numPages / 2);
            middlePage = str2double(currentUpdatePages{middleIndex});
            totalMiddlePage = totalMiddlePage + middlePage;
        end
    end

    % Print the final result
    fprintf('The sum of the middle page numbers of correctly ordered updates is: %d\n', totalMiddlePage);
end

% To run this program:
% 1. Save the code as a .m file (e.g., print_queue.m).
% 2. Create a file named input.txt in the same directory with your input data.
% 3. Open MATLAB, navigate to the directory where you saved the file.
% 4. Run the script by typing 'print_queue' in the MATLAB command window and pressing Enter.
