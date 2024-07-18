#include <algorithm>  // min, max
#include <cctype>     // isdigit
#include <iostream>
#include <map>
#include <string>
#include <utility>  // pair
#include <vector>

using namespace std;

typedef pair<int, int> ii;

int main() {
  vector<string> grid;
  string line;
  while (cin >> line) {
    grid.push_back(line);
  }

  int n = grid.size();
  int m = grid[0].size();

  map<ii, vector<int>> symbols;

  for (int i = 0; i < n; ++i) {
    for (int j = 0; j < m; ++j) {
      if (isdigit(grid[i][j])) {
        int start = j;
        while (isdigit(grid[i][j])) ++j;
        int number = stoi(grid[i].substr(start, j - start));

        for (int k = max(0, i - 1); k < min(n, i + 2); ++k) {
          for (int l = max(0, start - 1); l < min(m, j + 1); ++l) {
            if (k == i && start <= l && l < j) continue;
            if (isdigit(grid[k][l]) || grid[k][l] == '.') continue;

            auto key = ii(k, l);
            if (!symbols.count(key)) symbols.insert({key, vector<int>()});
            symbols[key].push_back(number);
          }
        }
      }
    }
  }

  // Part 1
  int sum = 0;
  for (const auto& kv : symbols) {
    for (int v : kv.second) {
      sum += v;
    }
  }
  cout << sum << endl;

  // Part 2
  sum = 0;
  for (const auto& kv : symbols) {
    int i = kv.first.first;
    int j = kv.first.second;
    if (grid[i][j] != '*') continue;
    if (kv.second.size() != 2) continue;

    int ratio = 1;
    for (int v : kv.second) {
      ratio *= v;
    }
    sum += ratio;
  }
  cout << sum << endl;

  return 0;
}