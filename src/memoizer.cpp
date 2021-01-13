#include "memoizer.hpp"

Memoizer::Memoizer(int memo_length) : memo_length{memo_length} {
}

Memoizer::~Memoizer() {
    //free all memoized
    for (pair<int, pair<shint*, bool>> mem : memoized) {
        delete mem.second.first;
    }
    memoized.clear();
}

bool    Memoizer::is_memoized(int hash) {
    memoized_found = false;
    //cout<<"requesting hash of "<<hash<<": ";
    //map<S, pair<T, bool>>::iterator 
    auto temp = memoized.find(hash);
    if (temp != memoized.end()) {
        memoized_found          = true;
        //found an entry, but is it valid?
        if ((*temp).second.second == valid_flag ) { 
            //yes, it is a valid memo
            //cout<<"VALID   MEMO ";
            
            
            memoized_get            = &((*temp).second); //hmm ugly
            memoized_found_is_valid = true;
            /*for(int i=0; i<memo_length; i++) {
                cout<<(memoized_get->first)[i]<<" ";
            }*/

            return true;
        } else {
            //the memo is invalid, return false but store the reference
            //cout<<"INVALID MEMO";
            memoized_get            = &((*temp).second); //hmm ugly
            memoized_found_is_valid = false;
            return false;
        }
    } else {
        cerr<<"WARNING: hash not found! should not be possible\n";
    }
    return false;
}

void Memoizer::initialize(int num_hashes) {
    memoized.clear();
    valid_flag = true;
    shint* temp;
    for (int i = 0; i< num_hashes; i++) {
        temp = new shint[memo_length];
        memoized.insert(make_pair(i, make_pair(temp, !valid_flag)));
    }
    return;
}

//note the lack of input, returns the value found in is_memoized, for efficiency
const shint* Memoizer::get() {
    if (memoized_found && memoized_found_is_valid) {
        return memoized_get->first;
    } else {
        cerr<<"ERROR: trying to get non-found or non-valid memoization";
    }
    return memoized_get->first; //which at this point, is invalid.
}

//assumes that the hash is not in the memoization, and that 
//is_memoized of hash has been called right before this call
void Memoizer::memoize(const shint* value) {
    if (memoized_found) {
        if (!memoized_found_is_valid) {
            //change found, change what is at the memoized get pointer
            //copy value onto memoized_get.first
            memcpy(memoized_get->first, value, memo_length * sizeof(shint));
            memoized_get->second = valid_flag; 
            /*for(int i=0; i<memo_length; i++) {
                cout<<(memoized_get->first)[i]<<" ";
            }*/
        } else {
            cerr<<"WARNING: trying to overwrite valid memoization, ignored\n";
        }
    } else {
        cerr<<"WARNING: trying to memoize a position that was not found, ignored\n";
    }
}

//invert valid flag, so that all currently mmoized are invalid and must be memoized again
void Memoizer::advance() {
    valid_flag = !valid_flag;
}