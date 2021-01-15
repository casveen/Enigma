// make some performance runs, recording performance for a bad crib
//#include "bombe.h"
int MAX_RING_SETTINGS= 26*26*26;
#include "bombe.hpp"
#include "enigma.hpp"
#include "rotors.cpp"   //all rotors,


struct BombeTiming benchmark_3(bool use_configuration_tracker, bool use_memoizer, string crib) {
    // make up a setting and use bombe on something completely unrelated,
    // record the performance
    Bombe bombe({I, VII, III}, UKWR, use_configuration_tracker);
    bombe.get_setting().starting_rotor_positions= "AAA";
    bombe.get_setting().starting_ring_setting   = "AAA";
    bombe.get_setting().max_ring_settings       = MAX_RING_SETTINGS;
    bombe.get_setting().only_one_candidate      = true;
    bombe.get_setting().only_one_configuration  = true;
    bombe.get_setting().stop_on_first_valid     = false;
    // bombe.get_setting().time_performance        = true;
    // bombe.get_setting().stop_on_first_valid= true;
    Enigma enigma({IV, V, VI}, UKWR);
    // enigma.set_verbose(true);
    string plaintext= "THISISAPLAINTEXTTOBEENCRYPTEDWITHTHEENIGMAANDISVERYVERYLONG";
    enigma.set_rotor_position("AAA");
    enigma.set_ring_setting("AAA");
    enigma.set_plugboard("");
    string                       ciphertext= enigma.encrypt(plaintext);
    vector<struct EnigmaSetting> solutions = bombe.analyze(ciphertext, crib);
    return bombe.get_timing();
}

void benchmark() {
    struct BombeTiming timing;
    printf("----------------------------------------------------------------\n");
    printf("|                      PARTIAL RUNS                            |\n");
    printf("----------------------------------------------------------------\n");
    printf("|  R |  C | CT | MEM | RUN TOTAL  RUN MEAN   TRACKING   RECORDS|\n");
    for(bool use_configuration_tracker : {true, false}) {
        for(bool use_memoizer : {true, false}) {
            for(string crib : {"SOMEBODY", "SOMEBODYONCETOLD", "SOMEBODYONCETOLDMETHE", "SOMEBODYONCETOLDMETHEWORLDWASGON"}) {
                timing = benchmark_3(use_configuration_tracker, use_memoizer, crib);
                printf("| %2d | %2d |  %s |  %s  | %6.2E   %6.2E   %6.2E   %7d |\n",
                       3, (int) crib.size(), use_configuration_tracker?"T":"F", use_memoizer?"T":"F",  
                       timing.total_run_time, timing.mean_run_time, timing.mean_tracking_time, timing.runs);
            }
        }           
    }
}
/*
















struct BombeSetting benchmark1(bool use_configuration_tracker) {
    // make up a setting and use bombe on something completely unrelated,
    // record the performance
    Bombe bombe({I, VII, III}, UKWR, use_configuration_tracker);
    bombe.get_setting().starting_rotor_positions= "AAA";
    bombe.get_setting().starting_ring_setting   = "AAA";
    bombe.get_setting().max_ring_settings       = MAX_RING_SETTINGS;
    bombe.get_setting().only_one_candidate      = true;
    bombe.get_setting().only_one_configuration  = true;
    bombe.get_setting().stop_on_first_valid     = false;
    // bombe.get_setting().time_performance        = true;
    // bombe.get_setting().stop_on_first_valid= true;
    Enigma enigma({IV, V, VI}, UKWR);
    // enigma.set_verbose(true);
    string plaintext= "THISISAPLAINTEXTTOBEENCRYPTEDWITHTHEENIGMAANDISVERYVERYLONG";
    enigma.set_rotor_position("AAA");
    enigma.set_ring_setting("AAA");
    enigma.set_plugboard("");
    string                       ciphertext= enigma.encrypt(plaintext);
    vector<struct EnigmaSetting> solutions = bombe.analyze(ciphertext, "SOMETEXT");
    return bombe.get_setting();
}

struct BombeSetting benchmark2(bool use_configuration_tracker) {
    // make up a setting and use bombe on something completely unrelated,
    // record the performance
    Bombe bombe({II, III, IV}, UKWR, use_configuration_tracker);
    bombe.get_setting().starting_rotor_positions= "AAA";
    bombe.get_setting().starting_ring_setting   = "AAA";
    bombe.get_setting().max_ring_settings       = MAX_RING_SETTINGS;
    bombe.get_setting().only_one_candidate      = true;
    bombe.get_setting().only_one_configuration  = true;
    bombe.get_setting().stop_on_first_valid     = false;
    // bombe.get_setting().time_performance        = true;
    // bombe.get_setting().stop_on_first_valid= true;
    Enigma enigma({IV, V, VI}, UKWR);
    // enigma.set_verbose(true);
    string plaintext= "THISISAPLAINTEXTTOBEENCRYPTEDWITHTHEENIGMAANDISVERYVERYLONG";
    enigma.set_rotor_position("AAA");
    enigma.set_ring_setting("AAA");
    enigma.set_plugboard("");
    string                       ciphertext= enigma.encrypt(plaintext);
    vector<struct EnigmaSetting> solutions = bombe.analyze(ciphertext, "SOMEUNRELATEDTEXT");
    return bombe.get_setting();
}

struct BombeSetting benchmark3(bool use_configuration_tracker) {
    // make up a setting and use bombe on something completely unrelated,
    // record the performance
    Bombe bombe({III, IV, V}, UKWR, use_configuration_tracker);
    bombe.get_setting().starting_rotor_positions= "AAA";
    bombe.get_setting().starting_ring_setting   = "AAA";
    bombe.get_setting().max_ring_settings       = MAX_RING_SETTINGS;
    bombe.get_setting().only_one_candidate      = true;
    bombe.get_setting().only_one_configuration  = true;
    bombe.get_setting().stop_on_first_valid     = false;
    // bombe.get_setting().time_performance        = true;
    // bombe.get_setting().stop_on_first_valid= true;
    Enigma enigma({IV, V, VI}, UKWR);
    // enigma.set_verbose(true);
    string plaintext= "THISISAPLAINTEXTTOBEENCRYPTEDWITHTHEENIGMAANDISVERYVERYLONG";
    enigma.set_rotor_position("AAA");
    enigma.set_ring_setting("AAA");
    enigma.set_plugboard("");
    string                       ciphertext= enigma.encrypt(plaintext);
    vector<struct EnigmaSetting> solutions=
        bombe.analyze(ciphertext, "SOMEUNRELATEDTEXTANDSOMEMORE");
    return bombe.get_setting();
}

struct BombeSetting benchmark4(bool use_configuration_tracker) {
    // make up a setting and use bombe on something completely unrelated,
    // record the performance
    Bombe bombe({IV, V, VI}, UKWR, use_configuration_tracker);
    bombe.get_setting().starting_rotor_positions= "AAA";
    bombe.get_setting().starting_ring_setting   = "AAA";
    bombe.get_setting().max_ring_settings       = MAX_RING_SETTINGS;
    bombe.get_setting().only_one_candidate      = true;
    bombe.get_setting().only_one_configuration  = true;
    bombe.get_setting().stop_on_first_valid     = false;
    // bombe.get_setting().time_performance        = true;
    // bombe.get_setting().stop_on_first_valid= true;
    Enigma enigma({IV, V, VI}, UKWR);
    // enigma.set_verbose(true);
    string plaintext= "THISISAPLAINTEXTTOBEENCRYPTEDWITHTHEENIGMAANDISVERYVERYLONG";
    enigma.set_rotor_position("AAA");
    enigma.set_ring_setting("AAA");
    enigma.set_plugboard("");
    string                       ciphertext= enigma.encrypt(plaintext);
    vector<struct EnigmaSetting> solutions=
        bombe.analyze(ciphertext, "SOMEUNRELATEDTEXTANDSOMEMOREUNRELATEDTEXTTHATISUNRELATED");
    return bombe.get_setting();
}

struct BombeSetting benchmark5(bool use_configuration_tracker) {
    // make up a setting and use bombe on something completely unrelated,
    // record the performance
    Bombe bombe({I, VII, VI, IV}, UKWR, use_configuration_tracker);
    bombe.get_setting().starting_rotor_positions= "AAAA";
    bombe.get_setting().starting_ring_setting   = "AAAA";
    bombe.get_setting().max_ring_settings       = MAX_RING_SETTINGS;
    bombe.get_setting().only_one_candidate      = true;
    bombe.get_setting().only_one_configuration  = true;
    bombe.get_setting().stop_on_first_valid     = false;
    bombe.get_setting().rotor_count             = 4;
    // bombe.get_setting().time_performance        = true;
    // bombe.get_setting().stop_on_first_valid= true;
    Enigma enigma({IV, V, VI, I}, UKWR);
    // enigma.set_verbose(true);
    string plaintext= "THISISAPLAINTEXTTOBEENCRYPTEDWITHTHEENIGMAANDISVERYVERYLONG";
    enigma.set_rotor_position("AAAA");
    enigma.set_ring_setting("AAAA");
    enigma.set_plugboard("");
    string                       ciphertext= enigma.encrypt(plaintext);
    vector<struct EnigmaSetting> solutions = bombe.analyze(ciphertext, "SOMETEXT");
    return bombe.get_setting();
}

struct BombeSetting benchmark6(bool use_configuration_tracker) {
    // make up a setting and use bombe on something completely unrelated,
    // record the performance
    Bombe bombe({I, II, III, IV}, UKWR, use_configuration_tracker);
    bombe.get_setting().starting_rotor_positions= "AAAA";
    bombe.get_setting().starting_ring_setting   = "AAAA";
    bombe.get_setting().max_ring_settings       = MAX_RING_SETTINGS;
    bombe.get_setting().only_one_candidate      = true;
    bombe.get_setting().only_one_configuration  = true;
    bombe.get_setting().stop_on_first_valid     = false;
    bombe.get_setting().rotor_count             = 4;
    // bombe.get_setting().time_performance        = true;
    // bombe.get_setting().stop_on_first_valid= true;
    Enigma enigma({IV, V, VI, I}, UKWR);
    // enigma.set_verbose(true);
    string plaintext= "THISISAPLAINTEXTTOBEENCRYPTEDWITHTHEENIGMAANDISVERYVERYLONG";
    enigma.set_rotor_position("AAAA");
    enigma.set_ring_setting("AAAA");
    enigma.set_plugboard("");
    string                       ciphertext= enigma.encrypt(plaintext);
    vector<struct EnigmaSetting> solutions = bombe.analyze(ciphertext, "SOMEUNRELATEDTEXT");
    return bombe.get_setting();
}

struct BombeSetting benchmark7(bool use_configuration_tracker) {
    // make up a setting and use bombe on something completely unrelated,
    // record the performance
    Bombe bombe({I, II, III, IV}, UKWR, use_configuration_tracker);
    bombe.get_setting().starting_rotor_positions= "AAAA";
    bombe.get_setting().starting_ring_setting   = "AAAA";
    bombe.get_setting().max_ring_settings       = MAX_RING_SETTINGS;
    bombe.get_setting().only_one_candidate      = true;
    bombe.get_setting().only_one_configuration  = true;
    bombe.get_setting().stop_on_first_valid     = false;
    bombe.get_setting().rotor_count             = 4;
    // bombe.get_setting().time_performance        = true;
    // bombe.get_setting().stop_on_first_valid= true;
    Enigma enigma({IV, V, VI, I}, UKWR);
    // enigma.set_verbose(true);
    string plaintext= "THISISAPLAINTEXTTOBEENCRYPTEDWITHTHEENIGMAANDISVERYVERYLONG";
    enigma.set_rotor_position("AAAA");
    enigma.set_ring_setting("AAAA");
    enigma.set_plugboard("");
    string                       ciphertext= enigma.encrypt(plaintext);
    vector<struct EnigmaSetting> solutions=
        bombe.analyze(ciphertext, "SOMEUNRELATEDTEXTANDDOMEMORE");
    return bombe.get_setting();
}

struct BombeSetting benchmark8(bool use_configuration_tracker) {
    // make up a setting and use bombe on something completely unrelated,
    // record the performance
    Bombe bombe({I, II, III, IV}, UKWR, use_configuration_tracker);
    bombe.get_setting().starting_rotor_positions= "AAAA";
    bombe.get_setting().starting_ring_setting   = "AAAA";
    bombe.get_setting().max_ring_settings       = MAX_RING_SETTINGS;
    bombe.get_setting().only_one_candidate      = true;
    bombe.get_setting().only_one_configuration  = true;
    bombe.get_setting().stop_on_first_valid     = false;
    bombe.get_setting().rotor_count             = 4;
    // bombe.get_setting().time_performance        = true;
    // bombe.get_setting().stop_on_first_valid= true;
    Enigma enigma({I, VII, VI, II}, UKWR);
    // enigma.set_verbose(true);
    string plaintext= "THISISAPLAINTEXTTOBEENCRYPTEDWITHTHEENIGMAANDISVERYVERYVERYLONG";
    //"SOMEUNRELATEDTEXTANDSOMEMOREUNRELATEDTEXTTHATISUNRELATED"
    enigma.set_rotor_position("AAAA");
    enigma.set_ring_setting("AAAA");
    enigma.set_plugboard("");
    string                       ciphertext= enigma.encrypt(plaintext);
    vector<struct EnigmaSetting> solutions=
        bombe.analyze(ciphertext, "SOMEUNRELATEDTEXTANDSOMEMOREUNRELATEDTEXTTHATSUNRELATED");
    return bombe.get_setting();
}

struct BombeSetting benchmark9() {
    // make up a setting and use bombe on something completely unrelated,
    // record the performance
    Bombe bombe({VI, VII, VIII, V}, UKWR, true);
    bombe.get_setting().starting_rotor_positions= "AAA";
    bombe.get_setting().starting_ring_setting   = "AAA";
    bombe.get_setting().max_ring_settings       = MAX_RING_SETTINGS;
    bombe.get_setting().only_one_candidate      = true;
    bombe.get_setting().only_one_configuration  = true;
    bombe.get_setting().stop_on_first_valid     = false;
    bombe.get_setting().rotor_count             = 3;

    // bombe.get_setting().time_performance        = true;
    // bombe.get_setting().stop_on_first_valid= true;
    Enigma enigma({VII, VI, II}, UKWR);
    // enigma.set_verbose(true);
    string plaintext= "THISISAPLAINTEXTTOBEENCRYPTEDWITHTHEENIGMAANDISVERYVERYVERYLONG";
    //"SOMEUNRELATEDTEXTANDSOMEMOREUNRELATEDTEXTTHATISUNRELATED"
    enigma.set_rotor_position("AAAA");
    enigma.set_ring_setting("AAAA");
    enigma.set_plugboard("");
    string                       ciphertext= enigma.encrypt(plaintext);
    vector<struct EnigmaSetting> solutions=
        bombe.analyze(ciphertext, "SOMEUNRELATEDTEXTANDSOMEMORE");
    return bombe.get_setting();
}
struct BombeSetting benchmark10() {
    // make up a setting and use bombe on something completely unrelated,
    // record the performance
    Bombe bombe({VI, VII, VIII, V}, UKWR, false);
    bombe.get_setting().starting_rotor_positions= "AAA";
    bombe.get_setting().starting_ring_setting   = "AAA";
    bombe.get_setting().max_ring_settings       = MAX_RING_SETTINGS;
    bombe.get_setting().only_one_candidate      = true;
    bombe.get_setting().only_one_configuration  = true;
    bombe.get_setting().stop_on_first_valid     = false;
    bombe.get_setting().rotor_count             = 3;

    // bombe.get_setting().time_performance        = true;
    // bombe.get_setting().stop_on_first_valid= true;
    Enigma enigma({VII, VI, II}, UKWR);
    // enigma.set_verbose(true);
    string plaintext= "THISISAPLAINTEXTTOBEENCRYPTEDWITHTHEENIGMAANDISVERYVERYVERYLONG";
    //"SOMEUNRELATEDTEXTANDSOMEMOREUNRELATEDTEXTTHATISUNRELATED"
    enigma.set_rotor_position("AAAA");
    enigma.set_ring_setting("AAAA");
    enigma.set_plugboard("");
    string                       ciphertext= enigma.encrypt(plaintext);
    vector<struct EnigmaSetting> solutions=
        bombe.analyze(ciphertext, "SOMEUNRELATEDTEXTANDSOMEMORE");
    return bombe.get_setting();
}

struct BombeSetting benchmark11() {
    // make up a setting and use bombe on something completely unrelated,
    // record the performance
    Bombe bombe({I, II, III, IV}, UKWR, true);
    bombe.get_setting().starting_rotor_positions= "AAAA";
    bombe.get_setting().starting_ring_setting   = "AAAA";
    bombe.get_setting().max_ring_settings       = MAX_RING_SETTINGS;
    bombe.get_setting().only_one_candidate      = true;
    bombe.get_setting().only_one_configuration  = true;
    bombe.get_setting().stop_on_first_valid     = false;
    bombe.get_setting().rotor_count             = 4;

    // bombe.get_setting().time_performance        = true;
    // bombe.get_setting().stop_on_first_valid= true;
    Enigma enigma({VII, VI, II, III}, UKWR);
    // enigma.set_verbose(true);
    string plaintext= "THISISAPLAINTEXTTOBEENCRYPTEDWITHTHEENIGMAANDISVERYVERYVERYLONG";
    //"SOMEUNRELATEDTEXTANDSOMEMOREUNRELATEDTEXTTHATISUNRELATED"
    enigma.set_rotor_position("AAAA");
    enigma.set_ring_setting("AAAA");
    enigma.set_plugboard("");
    string                       ciphertext= enigma.encrypt(plaintext);
    vector<struct EnigmaSetting> solutions = bombe.analyze(ciphertext, "UNRELATEDTEXT");
    return bombe.get_setting();
}
struct BombeSetting benchmark12() {
    // make up a setting and use bombe on something completely unrelated,
    // record the performance
    Bombe bombe({I, II, III, IV}, UKWR, false);
    bombe.get_setting().starting_rotor_positions= "AAAA";
    bombe.get_setting().starting_ring_setting   = "AAAA";
    bombe.get_setting().max_ring_settings       = MAX_RING_SETTINGS;
    bombe.get_setting().only_one_candidate      = true;
    bombe.get_setting().only_one_configuration  = true;
    bombe.get_setting().stop_on_first_valid     = false;
    bombe.get_setting().rotor_count             = 4;

    // bombe.get_setting().time_performance        = true;
    // bombe.get_setting().stop_on_first_valid= true;
    Enigma enigma({VII, VI, II, III}, UKWR);
    // enigma.set_verbose(true);
    string plaintext= "THISISAPLAINTEXTTOBEENCRYPTEDWITHTHEENIGMAANDISVERYVERYVERYLONG";
    //"SOMEUNRELATEDTEXTANDSOMEMOREUNRELATEDTEXTTHATISUNRELATED"
    enigma.set_rotor_position("AAAA");
    enigma.set_ring_setting("AAAA");
    enigma.set_plugboard("");
    string                       ciphertext= enigma.encrypt(plaintext);
    vector<struct EnigmaSetting> solutions = bombe.analyze(ciphertext, "UNRELATEDTEXT");
    return bombe.get_setting();
}

struct BombeSetting benchmark13() {
    // make up a setting and use bombe on something completely unrelated,
    // record the performance
    Bombe bombe({I, II, III, IV, V}, UKWR, false);
    bombe.get_setting().starting_rotor_positions= "AAAAA";
    bombe.get_setting().starting_ring_setting   = "AAAAA";
    bombe.get_setting().max_ring_settings       = MAX_RING_SETTINGS;
    bombe.get_setting().only_one_candidate      = true;
    bombe.get_setting().only_one_configuration  = true;
    bombe.get_setting().stop_on_first_valid     = false;
    bombe.get_setting().rotor_count             = 5;

    // bombe.get_setting().time_performance        = true;
    // bombe.get_setting().stop_on_first_valid= true;
    Enigma enigma({VII, VI, II, III, I}, UKWR);
    // enigma.set_verbose(true);
    string plaintext= "THISISAPLAINTEXTTOBEENCRYPTEDWITHTHEENIGMAANDISVERYVERYVERYLONG";
    //"SOMEUNRELATEDTEXTANDSOMEMOREUNRELATEDTEXTTHATISUNRELATED"
    enigma.set_rotor_position("AAAAA");
    enigma.set_ring_setting("AAAAA");
    enigma.set_plugboard("");
    string                       ciphertext= enigma.encrypt(plaintext);
    vector<struct EnigmaSetting> solutions = bombe.analyze(ciphertext, "UNRELATEDTEXT");
    return bombe.get_setting();
}
struct BombeSetting benchmark14() {
    // make up a setting and use bombe on something completely unrelated,
    // record the performance
    Bombe bombe({I, II, III, IV, V}, UKWR, true);
    bombe.get_setting().starting_rotor_positions= "AAAAA";
    bombe.get_setting().starting_ring_setting   = "AAAAA";
    bombe.get_setting().max_ring_settings       = MAX_RING_SETTINGS;
    bombe.get_setting().only_one_candidate      = true;
    bombe.get_setting().only_one_configuration  = true;
    bombe.get_setting().stop_on_first_valid     = false;
    bombe.get_setting().rotor_count             = 5;

    // bombe.get_setting().time_performance        = true;
    // bombe.get_setting().stop_on_first_valid= true;
    Enigma enigma({VII, VI, II, III, I}, UKWR);
    // enigma.set_verbose(true);
    string plaintext= "THISISAPLAINTEXTTOBEENCRYPTEDWITHTHEENIGMAANDISVERYVERYVERYLONG";
    //"SOMEUNRELATEDTEXTANDSOMEMOREUNRELATEDTEXTTHATISUNRELATED"
    enigma.set_rotor_position("AAAAA");
    enigma.set_ring_setting("AAAAA");
    enigma.set_plugboard("");
    string                       ciphertext= enigma.encrypt(plaintext);
    vector<struct EnigmaSetting> solutions = bombe.analyze(ciphertext, "UNRELATEDTEXT");
    return bombe.get_setting();
}
*/
/*
void benchmark_without_CT() {
    struct BombeSetting setting1CT= benchmark1(false);
    printf("| 3 ROTOR SMALL CRIB       %6.2E   %6.2E   %7d |\n",
           setting1CT.performance_ring_setting_mean, setting1CT.performance_ring_setting_var,
           setting1CT.records_ring_setting);
    struct BombeSetting setting2CT= benchmark2(false);
    printf("| 3 ROTOR MEDIUM CRIB      %6.2E   %6.2E   %7d |\n",
           setting2CT.performance_ring_setting_mean, setting2CT.performance_ring_setting_var,
           setting2CT.records_ring_setting);
    struct BombeSetting setting3CT= benchmark3(false);
    printf("| 3 ROTOR LARGE CRIB       %6.2E   %6.2E   %7d |\n",
           setting3CT.performance_ring_setting_mean, setting3CT.performance_ring_setting_var,
           setting3CT.records_ring_setting);
    struct BombeSetting setting4CT= benchmark4(false);
    printf("| 3 RTR VRY LRG CRIB       %6.2E   %6.2E   %7d |\n",
           setting4CT.performance_ring_setting_mean, setting4CT.performance_ring_setting_var,
           setting4CT.records_ring_setting);
    struct BombeSetting setting5CT= benchmark5(false);
    printf("| 4 ROTOR SMALL CRIB       %6.2E   %6.2E   %7d |\n",
           setting5CT.performance_ring_setting_mean, setting5CT.performance_ring_setting_var,
           setting5CT.records_ring_setting);
    struct BombeSetting setting6CT= benchmark6(false);
    printf("| 4 ROTOR MEDIUM CRB       %6.2E   %6.2E   %7d |\n",
           setting6CT.performance_ring_setting_mean, setting6CT.performance_ring_setting_var,
           setting6CT.records_ring_setting);
    struct BombeSetting setting7CT= benchmark7(false);
    printf("| 4 ROTOR LARGE CRIB       %6.2E   %6.2E   %7d |\n",
           setting7CT.performance_ring_setting_mean, setting7CT.performance_ring_setting_var,
           setting7CT.records_ring_setting);
    struct BombeSetting setting8CT= benchmark8(false);
    printf("| 4 RTR VR LRG CRIB        %6.2E   %6.2E   %7d |\n",
           setting8CT.performance_ring_setting_mean, setting8CT.performance_ring_setting_var,
           setting8CT.records_ring_setting);   
    struct BombeSetting setting14= benchmark13();
    printf("| 5 ROTOR SMALL CRIB       %6.2E   %6.2E   %7d |\n",
           setting14.performance_ring_setting_mean, setting14.performance_ring_setting_var,
           setting14.records_ring_setting);
    
}

void benchmark_CT() {
    struct BombeSetting setting1CT= benchmark1(true);
    printf("| 3 ROTOR SMALL CRIB W.CT. %6.2E   %6.2E   %7d |\n",
           setting1CT.performance_ring_setting_mean, setting1CT.performance_ring_setting_var,
           setting1CT.records_ring_setting);
    struct BombeSetting setting2CT= benchmark2(true);
    printf("| 3 ROTOR MEDIUM CRIB      %6.2E   %6.2E   %7d |\n",
           setting2CT.performance_ring_setting_mean, setting2CT.performance_ring_setting_var,
           setting2CT.records_ring_setting);
    struct BombeSetting setting3CT= benchmark3(true);
    printf("| 3 ROTOR LARGE CRIB W.CT. %6.2E   %6.2E   %7d |\n",
           setting3CT.performance_ring_setting_mean, setting3CT.performance_ring_setting_var,
           setting3CT.records_ring_setting);
    struct BombeSetting setting4CT= benchmark4(true);
    printf("| 3 RTR VRY LRG CRIB W.CT. %6.2E   %6.2E   %7d |\n",
           setting4CT.performance_ring_setting_mean, setting4CT.performance_ring_setting_var,
           setting4CT.records_ring_setting);
    struct BombeSetting setting5CT= benchmark5(true);
    printf("| 4 ROTOR SMALL CRIB W.CT. %6.2E   %6.2E   %7d |\n",
           setting5CT.performance_ring_setting_mean, setting5CT.performance_ring_setting_var,
           setting5CT.records_ring_setting);
    struct BombeSetting setting6CT= benchmark6(true);
    printf("| 4 ROTOR MEDIUM CRB W.CT. %6.2E   %6.2E   %7d |\n",
           setting6CT.performance_ring_setting_mean, setting6CT.performance_ring_setting_var,
           setting6CT.records_ring_setting);
    struct BombeSetting setting7CT= benchmark7(true);
    printf("| 4 ROTOR LARGE CRIB W.CT. %6.2E   %6.2E   %7d |\n",
           setting7CT.performance_ring_setting_mean, setting7CT.performance_ring_setting_var,
           setting7CT.records_ring_setting);
    struct BombeSetting setting8CT= benchmark8(true);
    printf("| 4 RTR VR LRG CRIB W.CT.  %6.2E   %6.2E   %7d |\n",
           setting8CT.performance_ring_setting_mean, setting8CT.performance_ring_setting_var,
           setting8CT.records_ring_setting);   
    struct BombeSetting setting14= benchmark14();
    printf("| 5 ROTOR SMALL CRIB W.CT  %6.2E   %6.2E   %7d |\n",
           setting14.performance_ring_setting_mean, setting14.performance_ring_setting_var,
           setting14.records_ring_setting);
    
}
*/

int main() {
    benchmark();

    /*
    struct BombeSetting setting1= benchmark1(false);
    printf("| 3 ROTOR SMALL CRIB       %6.2E   %6.2E   %7d |\n",
           setting1.performance_ring_setting_mean, setting1.performance_ring_setting_var,
           setting1.records_ring_setting);
    

    struct BombeSetting setting2= benchmark2(false);
    printf("| 3 ROTOR MEDIUM CRIB      %6.2E   %6.2E   %7d |\n",
           setting2.performance_ring_setting_mean, setting2.performance_ring_setting_var,
           setting2.records_ring_setting);
    

    struct BombeSetting setting3= benchmark3(false);
    printf("| 3 ROTOR LARGE CRIB       %6.2E   %6.2E   %7d |\n",
           setting3.performance_ring_setting_mean, setting3.performance_ring_setting_var,
           setting3.records_ring_setting);
    
    
       
    struct BombeSetting setting4= benchmark4(false);
    printf("| 3 ROTOR VERY LARGE CRIB  %6.2E   %6.2E   %7d |\n",
           setting4.performance_ring_setting_mean, setting4.performance_ring_setting_var,
           setting4.records_ring_setting);
    

    struct BombeSetting setting5= benchmark5(false);
    printf("| 4 ROTOR SMALL CRIB       %6.2E   %6.2E   %7d |\n",
           setting5.performance_ring_setting_mean, setting5.performance_ring_setting_var,
           setting5.records_ring_setting);
    

    struct BombeSetting setting6= benchmark6(false);
    printf("| 4 ROTOR MEDIUM CRIB      %6.2E   %6.2E   %7d |\n",
           setting6.performance_ring_setting_mean, setting6.performance_ring_setting_var,
           setting6.records_ring_setting);
   

    struct BombeSetting setting7= benchmark7(false);
    printf("| 4 ROTOR LARGE CRIB       %6.2E   %6.2E   %7d |\n",
           setting7.performance_ring_setting_mean, setting7.performance_ring_setting_var,
           setting7.records_ring_setting);
    

    struct BombeSetting setting8= benchmark8(false);
    printf("| 4 ROTOR VERY LARGE CRIB  %6.2E   %6.2E   %7d |\n",
           setting8.performance_ring_setting_mean, setting8.performance_ring_setting_var,
           setting8.records_ring_setting);
        
           
    struct BombeSetting setting13= benchmark13();
    printf("| 5 ROTOR SMALL CRIB       %6.2E   %6.2E   %7d |\n",
           setting13.performance_ring_setting_mean, setting13.performance_ring_setting_var,
           setting13.records_ring_setting);
    */
    printf("----------------------------------------------------------\n");
    /*
    printf("|                      ENTIRE RUNS                      |\n");
    printf("----------------------------------------------------------\n");
    printf("|                          MEAN       VAR        RECORDS |\n");
    struct BombeSetting setting9= benchmark9();
    printf("| 3 ROTOR W. CG: RS        %6.2E   %6.2E   %7d |\n",
           setting9.performance_ring_setting_mean, setting9.performance_ring_setting_var,
           setting9.records_ring_setting);
    printf("| 3 ROTOR W. CG: PER RUN   %6.2E   %6.2E   %7d |\n", setting9.performance_unit_run_mean,
           setting9.performance_unit_run_var, setting9.records_unit_run);
    struct BombeSetting setting10= benchmark10();
    printf("| 3 ROTOR W/O. CG: RS      %6.2E   %6.2E   %7d |\n",
           setting10.performance_ring_setting_mean, setting10.performance_ring_setting_var,
           setting10.records_ring_setting);
    printf("| 3 ROTOR W/O. CG: PER RUN %6.2E   %6.2E   %7d |\n",
           setting10.performance_unit_run_mean, setting10.performance_unit_run_var,
           setting10.records_unit_run); 
     struct BombeSetting setting11= benchmark11();
     printf("| 4 ROTOR W. CG: RS        %6.2E   %6.2E   %7d |\n",
            setting11.performance_ring_setting_mean, setting11.performance_ring_setting_var,
            setting11.records_ring_setting);
     printf("| 4 ROTOR W. CG: PER RUN   %6.2E   %6.2E   %7d |\n",
            setting11.performance_unit_run_mean, setting11.performance_unit_run_var,
            setting11.records_unit_run);
     struct BombeSetting setting12= benchmark12();
     printf("| 4 ROTOR W/O. CG: RS      %6.2E   %6.2E   %7d |\n",
            setting12.performance_ring_setting_mean, setting12.performance_ring_setting_var,
            setting12.records_ring_setting);
     printf("| 4 ROTOR W/O. CG: PER RUN %6.2E   %6.2E   %7d |\n",
            setting12.performance_unit_run_mean, setting12.performance_unit_run_var,
            setting12.records_unit_run);

    */
}
