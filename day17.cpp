#include <functional>  // greater
#include <iostream>
#include <queue>
#include <set>
#include <string>
#include <tuple>
#include <utility>  // pair
#include <vector>

using namespace std;

struct Node;
typedef pair<int, int> ii;
typedef pair<int, Node> nn;
typedef vector<vector<int>> vvi;

int kMin = 4;   // 0 for part 1
int kMax = 10;  // 3 for part 1

ii N = ii(-1, 0);
ii E = ii(0, 1);
ii S = ii(1, 0);
ii W = ii(0, -1);

struct Node {
  ii xy;
  ii dir;
  int straight;

  Node() = default;

  bool operator<(const Node& other) const {
    tuple<ii, ii, int> a{this->xy, this->dir, this->straight};
    tuple<ii, ii, int> b{other.xy, other.dir, other.straight};
    return a < b;
  }
};

ii add(ii a, ii b) { return ii(a.first + b.first, a.second + b.second); }

vector<Node> get_nbrs(const vvi& grid, const Node& node) {
  vector<Node> nbrs;
  nbrs.push_back(Node{add(node.xy, node.dir), node.dir, node.straight + 1});
  if (node.straight < kMin) return nbrs;

  if (node.dir == N || node.dir == S) {
    nbrs.push_back(Node{add(node.xy, E), E, 1});
    nbrs.push_back(Node{add(node.xy, W), W, 1});
  }
  if (node.dir == E || node.dir == W) {
    nbrs.push_back(Node{add(node.xy, N), N, 1});
    nbrs.push_back(Node{add(node.xy, S), S, 1});
  }
  return nbrs;
}

int dijkstra(const vvi& grid, const vector<nn>& start) {
  int n = grid.size();
  int m = grid[0].size();
  priority_queue<nn, vector<nn>, greater<nn>> pq(start.begin(), start.end());
  set<Node> vis;

  while (true) {
    nn curr = pq.top();
    pq.pop();
    Node node = curr.second;
    if (vis.count(node)) continue;

    int x = node.xy.first;
    int y = node.xy.second;
    if (x == n - 1 && y == m - 1 && node.straight >= kMin) return curr.first;

    for (auto nbr : get_nbrs(grid, node)) {
      int x2 = nbr.xy.first;
      int y2 = nbr.xy.second;
      if (nbr.straight > kMax) continue;
      if (x2 < 0 || y2 < 0 || x2 >= n || y2 >= m) continue;
      pq.push(make_pair(curr.first + grid[x2][y2], nbr));
    }

    vis.insert(node);
  }
}

int main() {
  vvi grid;
  string line;
  while (cin >> line) {
    vector<int> row;
    for (char c : line) row.push_back(c - '0');
    grid.push_back(row);
  }

  vector<nn> start = {make_pair(0, Node{ii(0, 0), E, 0}),
                      make_pair(0, Node{ii(0, 0), S, 0})};
  cout << dijkstra(grid, start) << endl;
  return 0;
}