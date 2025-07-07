
function main()
    % Reads input from input.txt and prints the output to standard output.
    % Solves the "Print Queue" coding challenge.

    % Read the entire file content
    fileContent = fileread('input.txt');

    % Split the content into rules and updates
    parts = strsplit(fileContent, '\n\n');
    rulesStr = strsplit(parts{1}, '\n');
    updatesStr = strsplit(parts{2}, '\n');

    % Parse the rules into a dependency graph (adjacency list)
    % and also store the reverse dependencies for easier checking.
    dependencies = containers.Map('KeyType', 'char', 'ValueType', 'any');
    reverseDependencies = containers.Map('KeyType', 'char', 'ValueType', 'any');
    allPages = containers.Map('KeyType', 'char', 'ValueType', 'logical');

    for i = 1:length(rulesStr)
        if isempty(rulesStr{i})
            continue;
        end
        ruleParts = strsplit(rulesStr{i}, '|');
        pre = ruleParts{1};
        post = ruleParts{2};

        % Add to allPages to keep track of all unique page numbers mentioned in rules
        allPages(pre) = true;
        allPages(post) = true;

        % Store dependencies: pre must come before post
        if ~isKey(dependencies, pre)
            dependencies(pre) = {};
        end
        dependencies(pre) = [dependencies(pre), {post}];

        % Store reverse dependencies: post must come after pre
        if ~isKey(reverseDependencies, post)
            reverseDependencies(post) = {};
        end
        reverseDependencies(post) = [reverseDependencies(post), {pre}];
    end

    % Process each update
    correctlyOrderedSum = 0;
    incorrectlyOrderedSum = 0;

    for i = 1:length(updatesStr)
        if isempty(updatesStr{i})
            continue;
        end
        currentUpdatePages = strsplit(updatesStr{i}, ',');
        currentUpdatePages = strtrim(currentUpdatePages); % Trim whitespace

        % Filter rules to only consider pages present in the current update
        relevantDependencies = containers.Map('KeyType', 'char', 'ValueType', 'any');
        relevantReverseDependencies = containers.Map('KeyType', 'char', 'ValueType', 'any');
        updatePageSet = containers.Map('KeyType', 'char', 'ValueType', 'logical');
        for j = 1:length(currentUpdatePages)
            updatePageSet(currentUpdatePages{j}) = true;
        end

        for page = keys(dependencies)
            pageStr = page{1};
            if isKey(updatePageSet, pageStr)
                dependentPages = dependencies(pageStr);
                for k = 1:length(dependentPages)
                    dependentPage = dependentPages{k};
                    if isKey(updatePageSet, dependentPage)
                        if ~isKey(relevantDependencies, pageStr)
                            relevantDependencies(pageStr) = {};
                        end
                        relevantDependencies(pageStr) = [relevantDependencies(pageStr), {dependentPage}];
                    end
                end
            end
        end

        for page = keys(reverseDependencies)
            pageStr = page{1};
            if isKey(updatePageSet, pageStr)
                precedentPages = reverseDependencies(pageStr);
                for k = 1:length(precedentPages)
                    precedentPage = precedentPages{k};
                    if isKey(updatePageSet, precedentPage)
                        if ~isKey(relevantReverseDependencies, pageStr)
                            relevantReverseDependencies(pageStr) = {};
                        end
                        relevantReverseDependencies(pageStr) = [relevantReverseDependencies(pageStr), {precedentPage}];
                    end
                end
            end
        end

        % Check if the current update is in the correct order
        isCorrect = true;
        for j = 1:length(currentUpdatePages)
            currentPage = currentUpdatePages{j};
            % Check if any page that should come *after* currentPage is before it
            if isKey(relevantDependencies, currentPage)
                pagesAfter = relevantDependencies(currentPage);
                for k = 1:length(pagesAfter)
                    pageAfter = pagesAfter{k};
                    % Find the index of pageAfter in the current update
                    idxPageAfter = find(strcmp(currentUpdatePages, pageAfter), 1);
                    if ~isempty(idxPageAfter) && idxPageAfter < j
                        isCorrect = false;
                        break;
                    end
                end
            end
            if ~isCorrect, break; end

            % Check if any page that should come *before* currentPage is after it
            if isKey(relevantReverseDependencies, currentPage)
                pagesBefore = relevantReverseDependencies(currentPage);
                for k = 1:length(pagesBefore)
                    pageBefore = pagesBefore{k};
                    % Find the index of pageBefore in the current update
                    idxPageBefore = find(strcmp(currentUpdatePages, pageBefore), 1);
                    if ~isempty(idxPageBefore) && idxPageBefore > j
                        isCorrect = false;
                        break;
                    end
                end
            end
            if ~isCorrect, break; end
        end

        % Calculate middle page number
        numPages = length(currentUpdatePages);
        middleIndex = ceil(numPages / 2);
        middlePage = str2double(currentUpdatePages{middleIndex});

        if isCorrect
            correctlyOrderedSum = correctlyOrderedSum + middlePage;
        else
            % If not correct, sort it using topological sort (Kahn's algorithm)
            sortedUpdate = topologicalSort(currentUpdatePages, relevantDependencies, relevantReverseDependencies);
            numSortedPages = length(sortedUpdate);
            middleIndexSorted = ceil(numSortedPages / 2);
            middlePageSorted = str2double(sortedUpdate{middleIndexSorted});
            incorrectlyOrderedSum = incorrectlyOrderedSum + middlePageSorted;
        end
    end

    fprintf('Sum of middle pages of correctly ordered updates: %d\n', correctlyOrderedSum);
    fprintf('Sum of middle pages of correctly ordered incorrectly ordered updates: %d\n', incorrectlyOrderedSum);
end

function sortedPages = topologicalSort(pages, dependencies, reverseDependencies)
    % Performs a topological sort on a list of pages given dependency rules.
    % Uses Kahn's algorithm.

    % Initialize in-degrees for all pages in the current update
    inDegree = containers.Map('KeyType', 'char', 'ValueType', 'double');
    for i = 1:length(pages)
        inDegree(pages{i}) = 0;
    end

    % Calculate in-degrees based on relevant dependencies
    for page = keys(reverseDependencies)
        pageStr = page{1};
        if isKey(inDegree, pageStr) % Ensure the page is in the current update
            precedents = reverseDependencies(pageStr);
            for k = 1:length(precedents)
                precedentPage = precedents{k};
                if isKey(inDegree, precedentPage) % Ensure the precedent is in the current update
                    inDegree(pageStr) = inDegree(pageStr) + 1;
                end
            end
        end
    end

    % Initialize a queue with all nodes having an in-degree of 0
    queue = {};
    for i = 1:length(pages)
        if inDegree(pages{i}) == 0
            queue{end+1} = pages{i};
        end
    end

    sortedPages = {};

    % Process the queue
    while ~isempty(queue)
        % Dequeue a node
        currentPage = queue{1};
        queue(1) = [];

        % Add to the sorted list
        sortedPages{end+1} = currentPage;

        % Decrease in-degree of adjacent nodes
        if isKey(dependencies, currentPage)
            dependents = dependencies(currentPage);
            for k = 1:length(dependents)
                dependentPage = dependents{k};
                if isKey(inDegree, dependentPage) % Ensure the dependent is in the current update
                    inDegree(dependentPage) = inDegree(dependentPage) - 1;
                    % If in-degree becomes 0, enqueue it
                    if inDegree(dependentPage) == 0
                        queue{end+1} = dependentPage;
                    end
                end
            end
        end
    end

    % Check for cycles (though the problem implies no cycles in valid inputs)
    if length(sortedPages) ~= length(pages)
        error('Graph has a cycle, cannot perform topological sort.');
    end
end

% To run this program:
% 1. Save the code as a .m file (e.g., print_queue.m).
% 2. Create a file named input.txt in the same directory with your puzzle input.
% 3. Run the script from the MATLAB command window by typing: main
