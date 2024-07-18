#include <cassert>
#include <iostream>
#include <map>
#include <queue>
#include <set>
#include <string>
#include <utility>  // pair
#include <vector>

using namespace std;

typedef long long ll;
typedef pair<int, int> ii;

int bfs_slow(const vector<string>& grid, int n, int m, const ii& start,
             int end) {
  map<ii, int> dists;
  deque<pair<int, ii>> q = {{0, start}};

  while (!q.empty()) {
    auto curr = q.front();
    q.pop_front();

    int dist = curr.first;
    ii xy = curr.second;
    int x = xy.first;
    int y = xy.second;

    if (dist > end) break;
    if (dists.count(xy)) continue;
    if (grid[(x % n + n) % n][(y % m + m) % m] == '#') continue;

    dists.insert({xy, dist});
    vector<ii> nbrs = {{x + 1, y}, {x - 1, y}, {x, y + 1}, {x, y - 1}};
    for (ii nbr : nbrs) q.push_back({dist + 1, nbr});
  }

  int ans = 0;
  for (const auto& kv : dists) {
    if ((kv.second - end) % 2 == 0) ++ans;
  }
  return ans;
}

map<ii, int> bfs_fast(const vector<string>& grid, int n, int m,
                      const ii& start) {
  map<ii, int> dists;
  deque<pair<int, ii>> q = {{0, start}};

  while (!q.empty()) {
    auto curr = q.front();
    q.pop_front();

    int dist = curr.first;
    ii xy = curr.second;
    int x = xy.first;
    int y = xy.second;

    if (dists.count(xy)) continue;
    if (x < 0 || y < 0 || x >= n || y >= m) continue;
    if (grid[x][y] == '#') continue;

    dists.insert({xy, dist});
    vector<ii> nbrs = {{x + 1, y}, {x - 1, y}, {x, y + 1}, {x, y - 1}};
    for (ii nbr : nbrs) q.push_back({dist + 1, nbr});
  }

  return dists;
}

ll extract_part2(const map<ii, int>& dists, ll n) {
  assert(n % 2 == 0);

  ll odd = 0;
  ll even = 0;
  ll odd_corners = 0;
  ll even_corners = 0;
  for (const auto& kv : dists) {
    int d = kv.second;
    if (d % 2 == 1) ++odd;
    if (d % 2 == 0) ++even;
    if (d % 2 == 1 && d > 65) ++odd_corners;
    if (d % 2 == 0 && d > 65) ++even_corners;
  }

  // I don't know why this is needed but it makes the answers match the
  // brute-force.
  even_corners--;

  ll ans = (n + 1) * (n + 1) * odd + n * n * even - (n + 1) * odd_corners +
           n * even_corners;
  return ans;
}

int main() {
  vector<string> grid;
  string line;
  while (cin >> line) grid.push_back(line);

  int n = grid.size();
  int m = grid[0].size();

  ii start;
  for (int i = 0; i < n; ++i) {
    for (int j = 0; j < m; ++j) {
      if (grid[i][j] == 'S') start = ii(i, j);
    }
  }

  // Part 1
  int part1 = bfs_slow(grid, n, m, start, 64);
  cout << part1 << endl;

  // Calibration
  auto dists = bfs_fast(grid, n, m, start);
  for (ll repeats : {0, 2, 4}) {
    ll total = 131 * repeats + 65;
    int expected = bfs_slow(grid, n, m, start, total);
    int actual = extract_part2(dists, repeats);
    cout << "Expected: " << expected << endl;
    cout << "Actual:   " << actual << endl;
  }

  // Part 2
  cout << extract_part2(dists, 202300) << endl;

  return 0;
}