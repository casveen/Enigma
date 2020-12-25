//#include "enigma.h";
#include "bombe.hpp"   //wire, diagonal board
#include "statistics.hpp"
#include <cmath>
#include <omp.h>


void update_performance_2(double &mean, double &var, std::chrono::duration<double> measurement,
                        int &records) {
    auto   t     = measurement.count();
    double mean_p= mean;
    mean         = mean_p + (t - mean_p) / (records + 1);
    var          = (records * var + (t - mean_p) * (t - mean)) / (records + 1);
    records++;
} //XXX YIKES BAD SOLUTION!!!


//The main BOMBE, delegates the hard work to its units
Bombe::Bombe(const initializer_list<Rotor> rotors, const Reflector reflector,
             const bool use_configuration_tracker) :
    m_rotors{vector<Rotor>(rotors)},
    m_reflector{vector<Reflector>({reflector})}, m_use_configuration_tracker{use_configuration_tracker} {
    m_letters= m_rotors[0].get_wires();
    // streambuf *streambuffer= cout.rdbuf();
    // m_outstream            = ostream(streambuffer);
}

Bombe::Bombe(vector<Rotor> rotors, vector<Reflector> reflector, const bool use_configuration_tracker) :
    m_rotors(rotors), m_reflector(reflector), m_use_configuration_tracker{use_configuration_tracker} {
    m_letters= m_rotors[0].get_wires();
}

vector<int> Bombe::probable_search(const string &ciphertext, const string &crib) {
    vector<int> candidates;
    // find suitable pattern
    int  ciphertext_length= ciphertext.length(), crib_length= crib.length();
    bool suitable;
    for (int i= 0; i <= ciphertext_length - crib_length; i++) {
        // compare, if same letter on same pos we have a contradiction
        suitable= true;
        for (int j= 0; j < crib_length; j++) {
            if (ciphertext[i + j] == crib[j]) {
                suitable= false;
                break;
            }
        }
        if (suitable) {
            if (m_verbose) {
                cout << "suitable substring at " << i << ": ";
                cout << ciphertext.substr(i, crib_length) << " <-> " << crib << "\n";
            }
            candidates.push_back(i);
        }
    }
    if (candidates.size() == 0) {
        cout << "WARNING: no candidates found, exiting\n";
        return candidates;
    }
    if (m_setting.only_one_candidate) {
        candidates.erase(candidates.begin() + 1, candidates.end());
    }
    return candidates;
}
vector<struct EnigmaSetting> Bombe::analyze_unit(const string & ciphertext_substring,
                                                 const string & crib,
                                                 vector<Rotor> &rotor_configuration,
                                                 Reflector &reflector, int candidate,
                                                 int most_wired_letter) {
    BombeUnit unit(rotor_configuration, reflector, m_use_configuration_tracker);
    vector<struct EnigmaSetting> solutions;

    auto      start_unit_run= std::chrono::system_clock::now();
    unit.set_identifier(unit.get_identifier() + " position " + to_string(candidate) + " ");
    // translate settings
    unit.get_setting().performance_ring_setting_mean= m_setting.performance_ring_setting_mean;
    unit.get_setting().performance_ring_setting_var = m_setting.performance_ring_setting_var;
    unit.get_setting().records_ring_setting         = m_setting.records_ring_setting;
    unit.get_setting().only_one_candidate           = m_setting.only_one_candidate;
    unit.get_setting().max_ring_settings            = m_setting.max_ring_settings;
    unit.get_setting().starting_ring_setting        = m_setting.starting_ring_setting;
    unit.get_setting().starting_rotor_positions     = m_setting.starting_rotor_positions;
    unit.get_setting().only_one_candidate           = m_setting.only_one_candidate;
    unit.get_setting().stop_on_first_valid          = m_setting.stop_on_first_valid;
    if (m_setting.time_performance) { start_unit_run= std::chrono::system_clock::now(); }

    if (m_use_configuration_tracker) {
        solutions= unit.analyze_with_configuration_tracker(ciphertext_substring, crib, most_wired_letter, candidate);
    } else {
        solutions=
        unit.analyze(ciphertext_substring, crib, most_wired_letter, candidate);
    }
    if (m_setting.time_performance) {
        auto stop_unit_run= std::chrono::system_clock::now();
        update_performance_2(m_setting.performance_unit_run_mean, m_setting.performance_unit_run_var,
                           stop_unit_run - start_unit_run, m_setting.records_unit_run);
        // std::chrono::duration<double> measurement= (stop_unit_run - start_unit_run);
        // cout << " unit run: " << measurement.count();
    }
    // update perofrmance timing
    m_setting.performance_ring_setting_mean= unit.get_setting().performance_ring_setting_mean;
    m_setting.performance_ring_setting_var = unit.get_setting().performance_ring_setting_var;
    m_setting.records_ring_setting         = unit.get_setting().records_ring_setting;
    // int tid                                = omp_get_thread_num();
    /*cout << "\n"
         << "thread " << tid << " finished " << unit.get_identifier() << " found "
         << solutions.size() << " solutions"
         << "\n";*/
    return solutions;
}
int Bombe::find_most_wired_letter(const string &ciphertext, const string &crib) {
    // make histogram of both crib and cipher letters, find most frequent element
    int *histogram= new int[m_letters];
    for (int i= 0; i < m_letters; ++i) { histogram[i]= 0; }
    for (unsigned int i= 0; i < min(ciphertext.length(), crib.length()); ++i) {
        histogram[(int)ciphertext[i] - (int)'A']++;
        histogram[(int)crib[i] - (int)'A']++;
    }
    int max_value= 0, max_index= -1;
    for (int i= 0; i < m_letters; ++i) {
        if (histogram[i] > max_value) {
            max_value= histogram[i];
            max_index= i;
        }
    }
    delete[] histogram;
    return max_index;
}

vector<struct EnigmaSetting> Bombe::analyze(const string &ciphertext, const string &crib) {
    vector<struct EnigmaSetting> solutions;
    // find candidates
    vector<int> candidates= probable_search(ciphertext, crib);
    if (candidates.size() == 0) { return solutions; }   // no candidates
    // make units with different rotor orders, and make them analyze
    vector<vector<Rotor>> rotor_configurations= choices_of(m_rotors, m_setting.rotor_count);
    if (m_setting.only_one_configuration) {
        rotor_configurations.erase(rotor_configurations.begin() + 1, rotor_configurations.end());
    }

    //Make the engage_path
    
    

    // spawn units that analyze. For each candidate, for each configuration of rotors
    // TODO thread unsaefe!!!
    vector<Rotor> rotor_configuration;
    unsigned int  i, j, k;
    //#pragma omp parallel for collapse(3) private(rotor_configuration, i, j, k)
    for (i= 0; i < candidates.size(); ++i) { //move to inner
        for (k= 0; k < m_reflector.size(); ++k) {
            for (j= 0; j < rotor_configurations.size(); ++j) {
                // cout << i << j << k << "\n";
                int    candidate           = candidates[i];
                string ciphertext_substring= ciphertext.substr(candidate, crib.length());
                int    most_wired_letter   = find_most_wired_letter(ciphertext_substring, crib);
                // TODO common
                vector<Rotor> rotor_configuration= rotor_configurations[j];
                // TODO thread it
                vector<struct EnigmaSetting> solutions_unit=
                    analyze_unit(ciphertext_substring, crib, rotor_configuration, m_reflector[k],
                                 candidate, most_wired_letter);
                for (struct EnigmaSetting solution : solutions_unit) {
                    solutions.push_back(solution);
                }
                if (m_setting.stop_on_first_valid && solutions.size() > 0) { return solutions; }
            }
        }
    }
    return solutions;
}

struct BombeSetting &Bombe::get_setting() {
    return m_setting;
}

string Bombe::preprocess(string in) const {
    // remove everything that is not an alpha
    std::regex nonalpha("[^a-zA-Z]");
    in= regex_replace(in, nonalpha, "");
    // convert string to upper case
    std::for_each(in.begin(), in.end(), [](char &c) { c= ::toupper(c); });
    return in;
}