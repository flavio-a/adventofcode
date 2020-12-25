#include <iostream>
#include <fstream>
#include <algorithm>

using namespace std;

typedef unsigned int uint;
typedef uint cup;

uint getDest(uint curr, uint totnum, uint a, uint b, uint c) {
    uint dest = ((curr - 2 + totnum) % totnum) + 1;
    while (dest == a || dest == b || dest == c) {
        dest = ((dest - 2 + totnum) % totnum) + 1;
    }
    return dest;
}

void printFrom1(const vector<cup> &next, bool spaced = false) {
    auto p = next[1];
    while (p != 1) {
        cout << p;
        if (spaced) cout << " ";
        p = next[p];
    }
    if (spaced) cout << "1";
    cout << endl;
}

cup doMove(vector<cup> &next, const uint cupsnum, cup curr) {
    // printFrom1(next);
    const cup a = next[curr], b = next[a], c = next[b];
    const cup newcurr = next[c];
    next[curr] = newcurr;
    const cup dest = getDest(curr, cupsnum, a, b, c);
    next[c] = next[dest];
    next[dest] = a;
    // cout << "Move: current " << curr << ", destination " << dest << endl;
    return next[curr];
}

int main(int argc, char* argv[]) {
    if (argc < 2) {
        cout << "Missing filename" << endl;
        return 1;
    }
    fstream infile(argv[1], fstream::in);

    string line;
    getline(infile, line);
    infile.close();
    vector<uint> numbers;
    numbers.resize(line.length());
    transform(line.begin(), line.end(), numbers.begin(), [](const char c){ return c - '0'; });

    // Part 1
    const uint moves1 = 100;
    vector<cup> next1(numbers.size() + 1);
    // next[i] is the label of the cup after the cup with label i
    for (int i = numbers.size() - 2; i >= 0; --i) {
        next1[numbers[i]] = numbers[i + 1];
    }
    next1[numbers[numbers.size() - 1]] = numbers[0];

    cup curr = numbers[0];
    uint move = 0;
    while (move < moves1) {
        curr = doMove(next1, numbers.size(), curr);
        ++move;
    }
    printFrom1(next1);

    // Part 2
    const uint cupsnum = 1000000;
    const uint moves2 = 10000000;
    // const uint cupsnum = 15;
    // const uint moves2 = 10;
    vector<cup> next2(cupsnum + 1);
    for (int i = numbers.size() - 2; i >= 0; --i) {
        next2[numbers[i]] = numbers[i + 1];
    }
    for (uint i = numbers.size() + 1; i < cupsnum; ++i) {
        next2[i] = i + 1;
    }
    next2[numbers[numbers.size() - 1]] = numbers.size() + 1;
    next2[cupsnum] = numbers[0];
    // printFrom1(next2, true);

    curr = numbers[0];
    move = 0;
    while (move < moves2) {
        curr = doMove(next2, cupsnum, curr);
        ++move;
    }
    // cout << next2[1] << " " << next2[next2[1]] << endl;
    cout << ((unsigned  long long int)next2[1] * (unsigned long long int)next2[next2[1]]) << endl;

    return 0;
}
