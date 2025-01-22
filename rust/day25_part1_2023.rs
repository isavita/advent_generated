
use std::{
    collections::{HashMap, HashSet, VecDeque},
    fs,
};

type Vertice = String;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct Edge {
    start: Vertice,
    end: Vertice,
}

type Graph = HashMap<Vertice, HashSet<Edge>>;

fn parse_input(input: &str) -> Graph {
    let mut graph: Graph = HashMap::new();
    for line in input.lines() {
        let parts: Vec<&str> = line.split(": ").collect();
        let vertice = parts[0].to_string();
        let others: Vec<&str> = parts[1].split(' ').collect();

        graph.entry(vertice.clone()).or_insert_with(HashSet::new);
        for other in others {
            let other_vertice = other.to_string();
            graph.entry(other_vertice.clone()).or_insert_with(HashSet::new);

            graph
                .get_mut(&vertice)
                .unwrap()
                .insert(Edge {
                    start: vertice.clone(),
                    end: other_vertice.clone(),
                });
            graph
                .get_mut(&other_vertice)
                .unwrap()
                .insert(Edge {
                    start: other_vertice,
                    end: vertice.clone(),
                });
        }
    }
    graph
}

fn breadth_first_search(
    graph: &Graph,
    start: &Vertice,
    goal_func: impl Fn(&Vertice) -> bool,
) -> (bool, HashMap<Vertice, Vertice>) {
    let mut frontier = VecDeque::from([start.clone()]);
    let mut reached = HashSet::from([start.clone()]);
    let mut came_from = HashMap::from([(start.clone(), start.clone())]);

    while let Some(current) = frontier.pop_front() {
        if goal_func(&current) {
            return (true, came_from);
        }

         if let Some(edges) = graph.get(&current) {
            for edge in edges {
                if !reached.contains(&edge.end) {
                    frontier.push_back(edge.end.clone());
                    reached.insert(edge.end.clone());
                    came_from.insert(edge.end.clone(), current.clone());
                }
            }
        }
    }
    (false, came_from)
}

fn reconstruct_path(start: &Vertice, end: &Vertice, came_from: &HashMap<Vertice, Vertice>) -> Vec<Vertice> {
    let mut path = Vec::new();
    let mut current = end.clone();

    while &current != start {
        path.insert(0, current.clone());
        current = came_from.get(&current).unwrap().clone();
    }

    path.insert(0, start.clone());
    path
}

fn copy_graph(graph: &Graph) -> Graph {
    graph
        .iter()
        .map(|(vertice, edges)| {
            (vertice.clone(), edges.clone())
        })
        .collect()
}

fn solve(input: &str) -> usize {
    let min_cut = 3;

    let graph = parse_input(input);

    let source = graph.keys().next().unwrap().clone();

    let mut separate_graph = None;

    for end in graph.keys() {
         if &source == end {
            continue;
        }

        let mut new_graph = copy_graph(&graph);
        for _ in 0..min_cut {
            let (found , came_from) = breadth_first_search(&new_graph, &source, |v| v == end);
            if !found{
                continue;
            }
            let path = reconstruct_path(&source, end, &came_from);
             for i in 0..path.len() - 1 {
                let start = &path[i];
                let end = &path[i+1];

                 if let Some(edges) = new_graph.get_mut(start){
                    edges.remove(&Edge{start: start.clone(), end: end.clone()});
                }
            }
        }

        let (is_valid, _) = breadth_first_search(&new_graph, &source, |v| v == end);
         if !is_valid {
            separate_graph = Some(new_graph);
            break;
        }
    }


    let separate_graph = separate_graph.unwrap();
    let (_, came_from) = breadth_first_search(&separate_graph, &source, |_| false);
    let length1 = came_from.len();
    let length2 = separate_graph.len() - length1;
    length1 * length2
}

fn main() {
    let input = fs::read_to_string("input.txt").expect("Unable to read file");
    println!("{}", solve(&input));
}
