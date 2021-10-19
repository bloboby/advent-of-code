#include <iostream>
#include <string>
#include <unordered_map>
#include <vector>

struct Node {
  int value = 0;
  int next;  // Index in |cups| of the next node.
  int pred;  // Index in |cups| of the predecessor.

  Node (int v) : value(v) {}
};

const int N = 1e6;
const int kNumMoves = 1e7;


std::vector<Node> initialise(std::string input) {
  std::vector<Node> cups;
  const int n = input.length();
  std::unordered_map<int, int> indices;

  // Initialise value and predecessor for the input nodes.
  for (int i=0; i<n; ++i) {
    int value = input[i] - '0';
    cups.push_back(Node(value));
    indices.insert({value, i});
  }
  for (Node& v : cups) {
    if (v.value == 1) continue;
    v.pred = indices[v.value - 1];
  }

  // Initialise value and predecessor for remaining nodes.
  for (int i=n+1; i<=N; ++i) {
    Node v(i);
    if (i == n+1) v.pred = indices[n];
    else v.pred = i-2;
    cups.push_back(v);
  }
  cups[indices[1]].pred = N-1;

  // Initialise pointer to the next node.
  for (int i=0; i<N; ++i)
    cups[i].next = (i+1)%N;

  return cups;
}


int main() {
  // Initialise the linked list.
  std::string input;
  std::cin >> input;
  std::vector<Node> cups = initialise(input);

  auto value = [&](int idx) { return cups[idx].value; };
  auto next = [&](int idx) { return cups[idx].next; };
  auto pred = [&](int idx) { return cups[idx].pred; };

  // Perform moves.
  int current = 0;
  for (int i=0; i<kNumMoves; ++i) {
    // Remove the next three cups.
    int first = next(current);
    int second = next(first);
    int third = next(second);
    cups[current].next = next(third);

    // Find the destination cup.
    int dest = pred(current);
    while (value(dest) == value(first)
        || value(dest) == value(second)
        || value(dest) == value(third)) {
      dest = pred(dest);
    }

    // Insert the removed cups.
    cups[third].next = next(dest);
    cups[dest].next = first;

    current = next(current);
  }

  // Find cup 1 and print the answer.
  int one = input.find("1");
  std::cout << (long long) value(next(one))
             * (long long) value(next(next(one)));
  return 0;
}

