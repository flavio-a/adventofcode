CXX=g++
CXXFLAGS=-std=c++17 -Wall -pedantic -O3

.PHONY: clean

%.out: %.cpp
	$(CXX) $(CXXFLAGS) -o $@ $^

clean:
	rm *.out
