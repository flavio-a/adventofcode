use crustofcode;
use slab_tree::*;

#[derive(Debug)]
struct File {
    size: Option<i64>,
    name: String,
}

#[allow(dead_code)]
fn print_tree(tree: &Tree<File>) -> () {
    let mut s = String::new();
    tree.write_formatted(&mut s).unwrap();
    println!("{}", s);
}

fn is_leaf<T>(node: &NodeRef<T>) -> bool {
    node.first_child().is_none()
}

fn compute_size(tree: &mut Tree<File>, id: NodeId) -> i64 {
    let node = tree.get(id).unwrap();
    if is_leaf(&node) {
        return node.data().size.unwrap();
    }
    // Internal node
    if node.data().size.is_some() {
        return node.data().size.unwrap();
    }
    let mut size = 0;
    let child_ids: Vec<NodeId> = node.children().map(|child| child.node_id()).collect();
    for child_id in child_ids.into_iter() {
        size += compute_size(tree, child_id);
    }
    let mut node_mut = tree.get_mut(id).unwrap();
    node_mut.data().size = Some(size);
    return size;
}

fn main() {
    println!("Day 7");

    let datastream = crustofcode::read_input_lines();
    let mut fs: Tree<File> = TreeBuilder::new()
        .with_capacity(datastream.len())
        .with_root(File {
            size: None,
            name: "/".to_owned(),
        })
        .build();
    let mut curr: NodeId = fs.root_id().unwrap();
    let mut lines = datastream.into_iter();
    assert_eq!(lines.next(), Some("$ cd /".to_string()));
    for line in lines {
        if line.chars().nth(0) == Some('$') {
            if &line[2..4] == "cd" {
                let name = &line[5..];
                if name == "/" {
                    curr = fs.root_id().unwrap();
                } else if name == ".." {
                    curr = fs.get(curr).unwrap().parent().unwrap().node_id();
                } else {
                    curr = fs
                        .get(curr)
                        .unwrap()
                        .children()
                        .find(|child| child.data().name == name)
                        .unwrap()
                        .node_id();
                }
            } else if &line[2..4] == "ls" {
                // Nothing, it processes it afterwards
            } else {
                panic!("Unexpexted plot twist!");
            }
        } else {
            let (name, size) = if &line[0..4] == "dir " {
                (line[4..].to_owned(), None)
            } else {
                let splits: Vec<&str> = line.split(' ').collect();
                assert_eq!(splits.len(), 2);
                (splits[1].to_string(), Some(crustofcode::str2int(splits[0])))
            };
            let mut curr_mut: NodeMut<File> = fs.get_mut(curr).unwrap();
            curr_mut.append(File {
                size: size,
                name: name,
            });
        }
    }

    let root_id = fs.root_id().unwrap();
    compute_size(&mut fs, root_id);
    // print_tree(&fs);

    // Part 1
    let root = fs.root().unwrap();
    println!(
        "{}",
        root.traverse_pre_order()
            .filter_map(|n| if is_leaf(&n) { None } else { n.data().size })
            .filter(|&v| v <= 100000)
            .sum::<i64>()
    );

    // Part 2
    let missing_space = 30000000 - (70000000 - root.data().size.unwrap());
    println!(
        "{}",
        root.traverse_pre_order()
            .filter_map(|n| if is_leaf(&n) { None } else { n.data().size })
            .filter(|&v| v >= missing_space)
            .min()
            .unwrap()
    );
}
