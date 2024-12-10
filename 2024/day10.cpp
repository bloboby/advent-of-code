#include <deque>
#include <iostream>
#include <set>
#include <string>
#include <utility>
#include <vector>

using namespace std;

typedef pair<int, int> ii;

class Day10 {
 public:
  void read() {
    string line;
    while (cin >> line) grid.push_back(line);
    n = grid.size();
    m = grid[0].size();
  }

  vector<ii> get_trailheads() const {
    vector<ii> trailheads;
    for (int i = 0; i < grid.size(); ++i) {
      for (int j = 0; j < grid[0].size(); ++j) {
        if (grid[i][j] == '0') {
          trailheads.push_back({i, j});
        }
      }
    }
    return trailheads;
  }

  int score(const ii& trailhead) const { return dfs1(trailhead, '0').size(); }

  int rating(const ii& trailhead) const { return dfs2(trailhead, '0'); }

 private:
  vector<string> grid;
  int n;
  int m;

  set<ii> dfs1(const ii& coords, char height) const {
    if (height == '9') return {coords};

    set<ii> peaks;
    int i = coords.first;
    int j = coords.second;
    vector<ii> nbrs = {{i - 1, j}, {i + 1, j}, {i, j - 1}, {i, j + 1}};

    for (const auto& nbr : nbrs) {
      int a = nbr.first;
      int b = nbr.second;
      if (a >= 0 and b >= 0 and a < n and b < m and grid[a][b] == height + 1) {
        auto nbr_peaks = dfs1({a, b}, height + 1);
        peaks.merge(nbr_peaks);
      }
    }

    return peaks;
  }

  int dfs2(const ii& coords, char height) const {
    if (height == '9') return 1;

    int sum = 0;
    int i = coords.first;
    int j = coords.second;
    vector<ii> nbrs = {{i - 1, j}, {i + 1, j}, {i, j - 1}, {i, j + 1}};

    for (const auto& nbr : nbrs) {
      int a = nbr.first;
      int b = nbr.second;
      if (a >= 0 and b >= 0 and a < n and b < m and grid[a][b] == height + 1) {
        sum += dfs2({a, b}, height + 1);
      }
    }

    return sum;
  }
};

int main() {
  auto day10 = Day10();
  day10.read();
  auto trailheads = day10.get_trailheads();

  int part1 = 0;
  int part2 = 0;
  for (const auto& th : trailheads) {
    part1 += day10.score(th);
    part2 += day10.rating(th);
  }
  cout << part1 << endl;
  cout << part2 << endl;

  return 0;
}