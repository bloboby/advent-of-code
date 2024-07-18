use std::collections::VecDeque;

#[derive(Debug)]
struct Node {
  children: Vec<Node>,
  metadata: Vec<i32>,
}

fn parse_node(input: &mut VecDeque<i32>) -> Node {
  let num_children = input.pop_front().unwrap();
  let num_metadata = input.pop_front().unwrap();

  let mut children = Vec::new();
  for _i in 0..num_children {
    children.push(parse_node(input));
  }

  let mut metadata = Vec::new();
  for _i in 0..num_metadata {
    metadata.push(input.pop_front().unwrap());
  }

  Node {
    children: children,
    metadata: metadata,
  }
}

fn get_value(node: &Node) -> i32 {
  let mut value = 0;
  let len = node.children.len();

  if node.children.is_empty() {
    for data in &node.metadata {
      value += data;
    }
    return value;
  }

  for data in &node.metadata {
    let n: usize = (*data).try_into().unwrap();
    if n == 0 || n > len {
      continue;
    }
    value += get_value(&node.children[n - 1]);
  }
  value
}

pub fn solve(contents: &String) -> i32 {
  let mut data = VecDeque::<i32>::new();
  for n in contents.trim().split(" ") {
    data.push_back(n.parse().unwrap());
  }

  let node = parse_node(&mut data);
  get_value(&node)
}
