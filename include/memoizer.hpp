#ifndef MEMOIZER
#define MEMOIZER

#include <map>
#include <iostream>
using namespace std;

//specifically for memoizing encryptions from positions of the enigma, uses positons hashes to map
//lookup needs to be as fast as possible
template <class S, class T>
class Memoizer {
    private: 
    //maps from a S to a pair<T, bool>, the T is the value we want, ie an encrpytion, but the bool
    //is used to determine if the memoization is valid, that is, if the memoization is done on 
    //the current initial position of the enigma, and not a later one.
    map<const S, pair<const T, bool>> memoized;
    //switches for every new run of the enigma, so that the bool in the map does not have to be explicitly 
    //set to false, but merely compared to this flag. Any new encryption that is memoized is 
    //appended with this flag
    bool valid_flag = false; 
    //if a key is checked for memoization, a pointer to the value is stored in got
    bool     memoized_found          = false;
    bool     memoized_found_is_valid = false;
    const T* memoized_get;

    public:
    Memoizer();
    ~Memoizer();

    void initialize(const vector<S> hashes);
    bool is_memoized(const S& hash);
    const T* get();
    void memoize(const T& value);
    void advance();
};
#endif