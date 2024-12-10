#include <climits>
#include <deque>
#include <iostream>
#include <queue>
#include <string>
#include <utility>
#include <vector>

using namespace std;

template <typename T>
using min_queue = priority_queue<T, vector<T>, greater<T>>;

typedef long long ll;

ll triangle(ll n) { return n * (n - 1) / 2; }

// Part 1

struct Block {
  ll len;
  ll id;
};

ll part1(const string& diskmap) {
  const ll GAP = -1;

  deque<Block> dq;
  for (ll i = 0; i < diskmap.size(); ++i) {
    ll id = i % 2 == 0 ? i / 2 : GAP;
    dq.push_back(Block{diskmap[i] - '0', id});
  }

  ll i = 0;
  ll checksum = 0;

  while (!dq.empty()) {
    Block left = dq.front();
    dq.pop_front();

    if (left.id != GAP) {
      checksum += left.id * (triangle(i + left.len) - triangle(i));
      i += left.len;
      continue;
    }

    while (left.len > 0) {
      if (dq.empty()) break;

      Block right = dq.back();
      dq.pop_back();

      if (right.id == GAP) continue;

      ll len = min(left.len, right.len);
      checksum += right.id * (triangle(i + len) - triangle(i));
      i += len;
      left.len -= len;
      if (right.len > len) dq.push_back({right.len - len, right.id});
    }
  }

  return checksum;
}

// Part 2

struct File {
  ll start;
  ll len;
  ll id;
};

pair<ll, ll> find_earliest(const vector<min_queue<ll>>& gaps, ll len) {
  ll min_len = len;
  ll min_start = LLONG_MAX;

  for (ll i = len; i < gaps.size(); ++i) {
    if (gaps[i].empty()) continue;

    ll start = gaps[i].top();
    if (start < min_start) {
      min_len = i;
      min_start = start;
    }
  }

  return {min_len, min_start};
}

ll part2(const string& diskmap) {
  deque<File> files;
  vector<min_queue<ll>> gaps(10);  // len -> start

  ll start = 0;
  for (ll i = 0; i < diskmap.size(); ++i) {
    ll len = diskmap[i] - '0';
    if (i % 2 == 0) {
      files.push_front(File{start, len, i / 2});
    } else {
      gaps[len].push(start);
    }
    start += len;
  }

  ll checksum = 0;

  for (const auto& file : files) {
    auto gap = find_earliest(gaps, file.len);
    ll gap_len = gap.first;
    ll start = gap.second;

    if (start >= file.start) {
      start = file.start;
    } else {
      gaps[gap_len].pop();
      if (gap_len > file.len) {
        gaps[gap_len - file.len].push(start + file.len);
      }
    }

    checksum += file.id * (triangle(start + file.len) - triangle(start));
  }

  return checksum;
}

int main() {
  string diskmap;
  cin >> diskmap;
  cout << part1(diskmap) << endl;
  cout << part2(diskmap) << endl;
  return 0;
}