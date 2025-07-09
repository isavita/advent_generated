
function main()
    input_str = fileread('input.txt');
    result = solve(input_str);
    fprintf('%d\n', result);
end

function [graph, starting_material] = parse_input(input_str)
    blocks = strsplit(input_str, '\n\n', 'CollapseDelimiters', false);
    starting_material = split_molecules(blocks{2});
    graph = containers.Map;
    lines = strsplit(blocks{1}, '\n');
    for i = 1:numel(lines)
        line = strtrim(lines{i});
        if isempty(line), continue; end
        parts = strsplit(line, ' => ');
        reactant = parts{1};
        product = parts{2};
        if graph.isKey(reactant)
            current_products = graph(reactant);
            graph(reactant) = [current_products, {product}];
        else
            graph(reactant) = {product};
        end
    end
end

function molecules = split_molecules(input_str)
    molecules = regexp(input_str, '[A-Z][a-z]*', 'match');
end

function steps = solve(input_str)
    [reverse_graph, starting_mols] = parse_input(input_str);
    product_to_reactant = containers.Map;
    reactants = reverse_graph.keys;
    for i = 1:numel(reactants)
        reactant = reactants{i};
        products = reverse_graph(reactant);
        for j = 1:numel(products)
            product = products{j};
            if product_to_reactant.isKey(product)
                error('Duplicate product found: %s', product);
            end
            product_to_reactant(product) = reactant;
        end
    end
    all_products_list = product_to_reactant.keys;
    mol = strjoin(starting_mols, '');
    steps = 0;
    initial_mol = mol;
    initial_steps = 0;
    while ~strcmp(mol, 'e')
        change_made = false;
        for i = 1:numel(all_products_list)
            prod = all_products_list{i};
            indices = strfind(mol, prod);
            if ~isempty(indices)
                change_made = true;
                count = numel(indices);
                steps = steps + count;
                mol = strrep(mol, prod, product_to_reactant(prod));
                break;
            end
        end
        if ~change_made
            all_products_list = all_products_list(randperm(numel(all_products_list)));
            mol = initial_mol;
            steps = initial_steps;
        end
    end
end
