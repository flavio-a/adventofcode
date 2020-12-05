#include <iostream>
#include <fstream>
#include <vector>
#include <cassert>

using namespace std;

typedef unsigned int uint;

uint bin_follow(const string& bp, uint start_idx, uint end_idx, const char down, const char up) {
    uint l = 0, u = (1 << (end_idx - start_idx)) - 1;
    // cout << l << " " << u << endl;
    for (uint i = start_idx; i < end_idx; ++i) {
        if (bp[i] == down) {
            u = (l + u) / 2;
        }
        else if (bp[i] == up) {
            l = (l + u) / 2 + 1;
        }
        // cout << l << " " << u << endl;
    }
    assert(l == u);
    return l;
}

uint seat_id(const string& bp) {
    // cout << bp << endl;
    uint row = bin_follow(bp, 0, 7, 'F', 'B');
    uint col = bin_follow(bp, 7, 10, 'L', 'R');
    return row * 8 + col;
}

int main(int argc, char* argv[]) {
    if (argc < 2) {
        cout << "Missing filename" << endl;
        return 1;
    }
    fstream infile(argv[1], fstream::in);

    string line;
    uint max_id = 0;
    vector<bool> ids(1023, false);
    while(getline(infile, line)) {
        uint curr_id = seat_id(line);
        ids[curr_id] = true;
        max_id = max(max_id, curr_id);
    }

    cout << max_id << endl;

    for (uint i = 1; i < ids.size() - 1; ++i) {
        if (!ids[i] && ids[i - 1] && ids[i + 1]) {
            cout << i << endl;
            break;
        }
    }

    infile.close();

    return 0;
}
