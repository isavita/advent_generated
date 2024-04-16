# Node represents an object in space.
mutable struct Node
    name::String
    children::Vector{Node}
    parent::Union{Node,Nothing}
end

# findOrCreateNode finds or creates a node with the given name.
function findOrCreateNode(name::String, nodes::Dict{String,Node})
    if haskey(nodes, name)
        return nodes[name]
    end
    node = Node(name, Node[], nothing)
    nodes[name] = node
    return node
end

# buildOrbitMap builds the orbit map from the input data.
function buildOrbitMap(lines::Vector{String})
    nodes = Dict{String,Node}()
    for line in lines
        parts = split(line, ")")
        center = findOrCreateNode(String(parts[1]), nodes)
        orbiter = findOrCreateNode(String(parts[2]), nodes)
        push!(center.children, orbiter)
        orbiter.parent = center
    end
    return nodes
end

# pathToRoot creates a vector of nodes from a given node to the root.
function pathToRoot(node::Node)
    path = Node[]
    while node !== nothing
        push!(path, node)
        node = node.parent
    end
    return path
end

# findCommonAncestor finds the common ancestor of two nodes and returns the paths to it.
function findCommonAncestor(node1::Node, node2::Node)
    path1 = pathToRoot(node1)
    path2 = pathToRoot(node2)

    i = length(path1)
    j = length(path2)

    # Move upwards until the paths diverge.
    while i > 0 && j > 0 && path1[i] === path2[j]
        i -= 1
        j -= 1
    end
    return i, j
end

# Read input from file
lines = readlines("input.txt")

# Build the orbit map
orbitMap = buildOrbitMap(lines)

# Calculate the number of orbital transfers
transfersYOU, transfersSAN = findCommonAncestor(orbitMap["YOU"].parent, orbitMap["SAN"].parent)
println(transfersYOU + transfersSAN)