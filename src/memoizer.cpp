#include "memoizer.hpp"

template<class S, class T>
Memoizer<S,T>::Memoizer() {
}
template<class S, class T>
Memoizer<S,T>::~Memoizer() {
    memoized.clear();
}

template <class S, class T>
bool    Memoizer<S, T>::is_memoized(const S& hash) {
    memoized_found = false;
    //map<S, pair<T, bool>>::iterator 
    auto temp = memoized.find(hash);
    if (temp != memoized.end()) {
        memoized_found          = true;
        //found an entry, but is it valid?
        if (*temp.second == valid_flag ) { 
            //yes, it is a valid memo
            memoized_get            = &(*temp); //hmm ugly
            memoized_found_is_valid = true;
            return true;
        } else {
            //the memo is invalid, return false but store the reference
            memoized_get            = &(*temp); //hmm ugly
            memoized_found_is_valid = false;
            return false;
        }
    }
    return false;
}

template <class S, class T>
void Memoizer<S,T>::initialize(const vector<S>& hashes) {
    memoized.clear();
    valid_flag = true;
    for (const S h : hashes) {
        memoized.insert(h, make_pair(0, false)); //shit needs default T constructor, which most likely is int
    }
    return;
}

//note the lack of input, returns the value found in is_memoized, for efficiency
template <class S, class T>
const T* Memoizer<S,T>::get() {
    if (memoized_found && memoized_found_is_valid) {
        return memoized_get;
    } else {
        cerr<<"ERROR: trying to get non-found or non-valid memoization";
    }
    return memoized_get; //which at this point, is invalid.
}

//assumes that the hash is not in the memoization, and that 
//is_memoized of hash has been called right before this call
template <class S, class T>
void Memoizer<S,T>::memoize(const T& value) { //TODO rremove reference?
    if (memoized_found) {
        if (!memoized_found_is_valid) {
           //change found, change what is at the memoized get pointer
           *memoized_get.first  = value;       //hmm this is a reference...
           *memoized_get.second = valid_flag; 
        } else {
            cerr<<"WARNING: trying to overwrite valid memoization, ignored\n";
        }
    } else {
        cerr<<"WARNING: trying to memoize a position that was not found, ignored\n";
    }
}

//invert valid flag, so that all currently mmoized are invalid and must be memoized again
template <class S, class T>
void Memoizer<S,T>::advance() {
    valid_flag = !valid_flag;
}