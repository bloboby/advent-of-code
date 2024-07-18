#include <algorithm>  // max
#include <deque>
#include <iostream>
#include <set>
#include <string>
#include <utility>  // pair
#include <vector>

using namespace std;

typedef pair<int, int> ii;
typedef pair<ii, ii> beam;  // (coord, dir)

ii N = ii(-1, 0);
ii E = ii(0, 1);
ii S = ii(1, 0);
ii W = ii(0, -1);

ii add(ii a, ii b) { return ii(a.first + b.first, a.second + b.second); }

vector<ii> next_dir(char c, ii dir) {
  bool vert = dir == N || dir == S;
  bool horz = dir == E || dir == W;

  if (c == '.' || (c == '|' && vert) || (c == '-' && horz)) return {dir};
  if (c == '|' && horz) return {N, S};
  if (c == '-' && vert) return {E, W};

  if (c == '/') {
    if (dir == N) return {E};
    if (dir == E) return {N};
    if (dir == S) return {W};
    if (dir == W) return {S};
  }

  if (c == '\\') {
    if (dir == N) return {W};
    if (dir == E) return {S};
    if (dir == S) return {E};
    if (dir == W) return {N};
  }

  cout << "ERROR" << endl;
  return {};
}

int energised(const vector<string>& grid, int n, int m, beam start) {
  set<beam> seen;
  deque<beam> beams = {start};
  while (!beams.empty()) {
    beam b = beams.front();
    beams.pop_front();

    int x = b.first.first;
    int y = b.first.second;

    if (x < 0 || y < 0 || x >= n || y >= m) continue;
    if (seen.count(b)) continue;
    seen.insert(b);

    for (auto d : next_dir(grid[x][y], b.second)) {
      beams.push_back(beam(add(b.first, d), d));
    }
  }

  set<ii> energised;
  for (beam b : seen) energised.insert(b.first);
  return energised.size();
}

int main() {
  vector<string> grid;
  string line;
  while (cin >> line) grid.push_back(line);

  int n = grid.size();
  int m = grid[0].size();

  vector<beam> starts;
  for (int i = 0; i < n; ++i) {
    starts.push_back(beam(ii(i, 0), E));
    starts.push_back(beam(ii(i, m - 1), W));
  }
  for (int j = 0; j < m; ++j) {
    starts.push_back(beam(ii(0, j), S));
    starts.push_back(beam(ii(n - 1, j), N));
  }

  int ans = 0;
  for (beam s : starts) ans = max(ans, energised(grid, n, m, s));
  cout << ans << endl;
  return 0;
}