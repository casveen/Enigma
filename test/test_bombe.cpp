// main() provided by Catch in file 020-TestCase-1.cpp.
#include "bombe.hpp"
#include "catch.hpp"
#include "enigma.hpp"
#include "rotors.cpp"   //all rotors,




//custom small rotors
const int SMALL_ROTOR_COUNT = 6;
const int SMALL_ROTOR_NOTCH_COUNT = 2;
Rotor C0  = Rotor(SMALL_ROTOR_COUNT, SMALL_ROTOR_NOTCH_COUNT);
Rotor C1  = Rotor(SMALL_ROTOR_COUNT, SMALL_ROTOR_NOTCH_COUNT);
Rotor C2  = Rotor(SMALL_ROTOR_COUNT, SMALL_ROTOR_NOTCH_COUNT);
Rotor C3  = Rotor(SMALL_ROTOR_COUNT, SMALL_ROTOR_NOTCH_COUNT);
Rotor C4  = Rotor(SMALL_ROTOR_COUNT, SMALL_ROTOR_NOTCH_COUNT);
Rotor C5  = Rotor(SMALL_ROTOR_COUNT, SMALL_ROTOR_NOTCH_COUNT);
Rotor C6  = Rotor(SMALL_ROTOR_COUNT, SMALL_ROTOR_NOTCH_COUNT);
Rotor C7  = Rotor(SMALL_ROTOR_COUNT, SMALL_ROTOR_NOTCH_COUNT);
Rotor C8  = Rotor(SMALL_ROTOR_COUNT, SMALL_ROTOR_NOTCH_COUNT);
Rotor C9  = Rotor(SMALL_ROTOR_COUNT, SMALL_ROTOR_NOTCH_COUNT);
Rotor C10 = Rotor(SMALL_ROTOR_COUNT, SMALL_ROTOR_NOTCH_COUNT);
Rotor C11 = Rotor(SMALL_ROTOR_COUNT, SMALL_ROTOR_NOTCH_COUNT);
Rotor C12 = Rotor(SMALL_ROTOR_COUNT, SMALL_ROTOR_NOTCH_COUNT);
Rotor C13 = Rotor(SMALL_ROTOR_COUNT, SMALL_ROTOR_NOTCH_COUNT);
Rotor C14 = Rotor(SMALL_ROTOR_COUNT, SMALL_ROTOR_NOTCH_COUNT);
Rotor C15 = Rotor(SMALL_ROTOR_COUNT, SMALL_ROTOR_NOTCH_COUNT);
Rotor C16 = Rotor(SMALL_ROTOR_COUNT, SMALL_ROTOR_NOTCH_COUNT);
Rotor C17 = Rotor(SMALL_ROTOR_COUNT, SMALL_ROTOR_NOTCH_COUNT);
Rotor C18 = Rotor(SMALL_ROTOR_COUNT, SMALL_ROTOR_NOTCH_COUNT);
Rotor C19 = Rotor(SMALL_ROTOR_COUNT, SMALL_ROTOR_NOTCH_COUNT);
Reflector CR = Reflector(SMALL_ROTOR_COUNT);









//checks if ANY solution fits the given criteria
bool is_exact_setting(Enigma &enigma, vector<struct EnigmaSetting> solutions, string ring_setting,
                      string rotor_position, string correct_plugboard_string, string ciphertext,
                      string crib, int crib_pos) {

    unsigned int solution_count= solutions.size();
    //check if any solution fits the given criteria
    for (struct EnigmaSetting solution : solutions) {
        //cout<<"RP: "<<solution.rotor_position<<" --- RS: "<<solution.ring_setting<<"\n";
        bool all_correct= true;
        enigma.set_setting(solution);
        //cout<<"\nARP: "<<enigma.get_rotor_position_as_string()<<" --- ARS: "<<enigma.get_ring_setting_as_string()<<"\n";
        string encrypted_crib= enigma.encrypt(crib);
        //cout<<encrypted_crib<<"\n";
        //cout<<ciphertext<<"\n";
        bool   check         = (encrypted_crib == ciphertext.substr(crib_pos, crib.length()));
        if (check == false && solution_count == 1) {
            cerr << "WRONG DECRYPTION: encrypt(" << crib << ")=" << encrypted_crib
                 << "=/=" << ciphertext.substr(crib_pos, crib.length()) << "\n";
        }
        all_correct= all_correct && check;

        check= (solution.rotor_position == rotor_position);
        if (check == false && solution_count == 1) {
            cerr << "WRONG ROTOR POSITION: " << rotor_position << "=/=" << solution.rotor_position
                 << "\n";
        }
        all_correct= all_correct && check;

        check= (solution.ring_setting == ring_setting);
        if (check == false && solution_count == 1) {
            cerr << "WRONG RING SETTING: " << ring_setting << "=/=" << solution.ring_setting
                 << "\n";
        }
        all_correct= all_correct && check;

        if (correct_plugboard_string != "") {
            Plugboard correct_plugboard(correct_plugboard_string, 26);
            for (int i= 0; i < 26; ++i) {
                check= (solution.plugboard.get_wiring(i) == correct_plugboard.get_wiring(i));
                if (check == false) {
                    cout<<"WRONg PLUGBPARD";
                }
                if (check == false && solution_count == 1) {
                    cerr << "WRONG PLUGBOARD: correct_plugboard[" << i
                         << "]=" << correct_plugboard.get_wiring(i)
                         << "=/=" << solution.plugboard.get_wiring(i) << "=solution_plugboard[" << i
                         << "]\n";
                }
                all_correct= all_correct && check;
            }
        }

        if (all_correct) { return true; }
        //cout<<"\n";
    }
    return false;
}

/*
SCENARIO("Using custom rotors of non-standard size") {
    const Rotor     CI  = Rotor("DFEBCA", "A", "I6");
    const Rotor     CII = Rotor("DEBFAC", "B", "II6");
    const Rotor     CIII= Rotor("DFBCAE", "C", "III6");
    const Reflector CUKW= Reflector("DCBAFE", "", "UKW6");
    Bombe           bombe({I, II, III}, UKWR);
    Enigma          enigma({I, II, III}, UKWR);
    bombe.get_setting().stop_on_first_valid     = false;
    bombe.get_setting().only_one_configuration  = true;
    bombe.get_setting().starting_ring_setting   = "AAA";
    bombe.get_setting().starting_rotor_positions= "AAA";
    bombe.get_setting().rotor_count             = 3;
    string plaintext                            = "AABBCCDEFGHIJK";
    // string crib                                 = "FEDCABBACEF";
    WHEN("Given ciphertext encrypted with ring-setting AAA, "
         "ring-position AAA and steckering") {
        enigma.set_ring_setting("ACC");
        enigma.set_rotor_position("CAB");
        enigma.set_plugboard("AD.BC");
        string ciphertext= enigma.encrypt(plaintext);
        THEN("Running bombe with a complete crib should return the above "
             "setting") {
            vector<struct EnigmaSetting> solutions= bombe.analyze(ciphertext, plaintext);
            enigma.set_setting(solutions[0]);
            REQUIRE(enigma.encrypt(ciphertext) == plaintext);
            REQUIRE(solutions[0].rotor_position == "AAA");
        }
    }
    cin.get();
}*/







SCENARIO("bombe with CT finds the configuration of an enimga", "[CTbombe]") {
    cout<<"\n\ntesting with CT\n\n";
    GIVEN("Enigma: I, II, III, UKWR. Steckered") {
        Bombe bombe({I, II, III}, UKWR, true);
        bombe.get_setting().stop_on_first_valid    = false;
        bombe.get_setting().max_ring_settings      = 26*26*26;
        bombe.get_setting().rotor_count            = 3;
        bombe.get_setting().starting_ring_setting  =  "AAA";
        bombe.get_setting().only_one_candidate     = false;
        bombe.get_setting().only_one_configuration = true;
        // struct EnigmaSetting enigma_setting;

        Enigma enigma({I, II, III}, UKWR);
        enigma.set_plugboard("AK. IE. DV. CQ. BN, MO, PJ. WR. UX");
        // enigma.set_verbose(true);
        // TODO withoun andisveryverylong behaves weird, cannot find
        string plaintext= "HELLOMYHONEYHELLO";
        WHEN("ciphertext encrypted with RS:OEI, RP:SEZ") {
            enigma.set_ring_setting("OEI");
            enigma.set_rotor_position("SEZ");
            string ciphertext= enigma.encrypt(plaintext);
            THEN("All solutions from the bombe should give the same encryption") {
                vector<struct EnigmaSetting> solutions= bombe.analyze(ciphertext, plaintext);
                //cout<<"\nfound "<<solutions.size()<<" solutions using the CT\n";
                for (EnigmaSetting setting : solutions) {
                    enigma.set_setting(setting);
                    CHECK(enigma.encrypt(ciphertext) == plaintext);
                }
            }
        }
    }
    cout << "\r";
}




















SCENARIO("bombe on concrete wikipedia example", "[bombedonitz]") {
    GIVEN("Donitz message portion, and its configuration") {
        Bombe bombe({VIII, VI, V, BETA}, THINREFLECTORC);
        // struct EnigmaSetting enigma_setting;
        bombe.get_setting().stop_on_first_valid     = true;
        bombe.get_setting().only_one_configuration  = true;
        bombe.get_setting().starting_ring_setting   = "EPEI";
        bombe.get_setting().starting_rotor_positions= "AAAA";
        bombe.get_setting().rotor_count             = 4;
        string ciphertext                           = "RBBFPMHPHGCZXTDYGAHGUFXGEWKBLKGJWLQXXT";
        string crib                                 = "FOLGENDESISTSOFORTBEKANNTZU";
        WHEN("Running bombe close to configuration") {
            THEN("Bombe should find the configuration") {
                vector<struct EnigmaSetting> solutions= bombe.analyze(ciphertext, crib);
                Enigma                       enigma({VIII, VI, V, BETA}, THINREFLECTORC);
                CHECK(is_exact_setting(enigma, solutions, "EPEL", "CDTJ",
                                       "AE.BF.CM.DQ.HU.JN.LX.PR.SZ.VW", ciphertext, crib, 0));
            }
        }
    }
}
// 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
// A  B  C  D  E  F  G  H  I  J  K  L  M  N  O  P  Q  R  S  T  U  V  W  X  Y  Z
// 4  5 12 16  0  1  6 20  8  9 10 23  2 13 14 17  3 15 25 19  7 21 22 11 24 18
// 4  5 12 16  0  1  6 20  8 13 10 23  2  9 14 17  3 15 25 19  7 21 22 11 24 18 correct

SCENARIO("bombeunit finds the configuration of an enimga", "[bombeunit]") {
    GIVEN("Enigma with rotors I, II, III, reflector UKW and a long plaintext") {
        Bombe bombe({I, II, III}, UKWR);
        // struct EnigmaSetting enigma_setting;
        bombe.get_setting().stop_on_first_valid   = true;
        bombe.get_setting().only_one_configuration= true;
        Enigma enigma({I, II, III}, UKWR);
        string initial_ring_setting  = enigma.get_ring_setting_as_string();
        string initial_rotor_position= enigma.get_rotor_position_as_string();
        enigma.set_ring_setting(initial_ring_setting);
        enigma.set_rotor_position(initial_rotor_position);
        // enigma.set_verbose(true);
        string plaintext= "THISISAPLAINTEXTTOBEENCRYPTEDWITHTHEENIGMAANDISVERYV"
                          "ERYLONGITSSUPPOSEDTOBESOLONGASTOENGAGEBOTHTHEFASTAND"
                          "MIDDLEROTOROFATHREEROTORENIGMA";

        WHEN("Given ciphertext encrypted with ring-setting AAA, "
             "ring-position AAA and no steckering") {
            enigma.set_ring_setting("AAA");
            enigma.set_rotor_position("AAA");
            enigma.set_plugboard("");
            string ciphertext                           = enigma.encrypt(plaintext);
            bombe.get_setting().starting_rotor_positions= "AAA";
            bombe.get_setting().starting_ring_setting   = "AAA";
            THEN("Running bombe with a complete crib should return the above "
                 "setting") {
                vector<struct EnigmaSetting> solutions= bombe.analyze(ciphertext, plaintext);
                CHECK(is_exact_setting(enigma, solutions, "AAA", "AAA", "", ciphertext, plaintext,
                                       0));
            }
        }

        WHEN("Given ciphertext encrypted with ring-position CAA and no "
             "steckering") {
            // there was a bug with the enigma doing two turns for the first
            // check this test would not pass with this bug
            enigma.set_ring_setting("AAA");
            enigma.set_rotor_position("CAA");
            enigma.set_plugboard("");
            string ciphertext                           = enigma.encrypt(plaintext);
            bombe.get_setting().starting_rotor_positions= "AAA";
            bombe.get_setting().starting_ring_setting   = "AAA";
            THEN("Running bombe with a complete crib should return the above "
                 "setting") {
                vector<struct EnigmaSetting> solutions= bombe.analyze(ciphertext, plaintext);
                CHECK(is_exact_setting(enigma, solutions, "AAA", "CAA", "", ciphertext, plaintext,
                                       0));
            }
        }

        WHEN("Given ciphertext encrypted with ring-setting FGH, ring-position "
             "CDE, no steckering and bombe at same ring set") {
            enigma.set_ring_setting("FGH");
            enigma.set_rotor_position("CDE");

            string ciphertext                           = enigma.encrypt(plaintext);
            bombe.get_setting().starting_rotor_positions= "AAA";
            bombe.get_setting().starting_ring_setting   = "FGH";
            THEN("Running bombe with a complete crib should return the above "
                 "setting") {
                vector<struct EnigmaSetting> solutions= bombe.analyze(ciphertext, plaintext);
                CHECK(is_exact_setting(enigma, solutions, "FGH", "CDE", "", ciphertext, plaintext,
                                       0));
            }
        }

        WHEN("Given ciphertext encrypted with ring-setting RFW, ring-position "
             "BCD, bombe in same config and steckering") {
            // there was a bug with the enigma doing two turns for the first
            // check this test would not pass with this bug
            enigma.set_ring_setting("BBA");
            enigma.set_rotor_position("MCW");
            enigma.set_plugboard("AC. BD. EG. IR. JT. QZ");
            string ciphertext= enigma.encrypt(plaintext);
            // bombe.set_rotor_position("RFW");
            bombe.get_setting().starting_rotor_positions= "AAA";
            bombe.get_setting().starting_ring_setting   = "BBA";
            THEN("Running bombe with a complete crib should return the above "
                 "setting") {
                vector<struct EnigmaSetting> solutions= bombe.analyze(ciphertext, plaintext);
                CHECK(is_exact_setting(enigma, solutions, "BBA", "MCW", "AC. BD. EG. IR. JT. QZ",
                                       ciphertext, plaintext, 0));
            }
        }

        WHEN("Given ciphertext encrypted with ring-setting AFG, rotor-position "
             "HAP, steckering and bombe starting from ring-setting AEZ") {
            enigma.set_ring_setting("AFG");
            enigma.set_rotor_position("HAP");
            enigma.set_plugboard("AK. IE. DV. CQ. BN, MO, PJ. WR. UX");
            string ciphertext= enigma.encrypt(plaintext);
            enigma.reset();
            bombe.get_setting().starting_rotor_positions= "AAA";
            bombe.get_setting().starting_ring_setting   = "AEZ";
            THEN("Running bombe with a complete crib should return the above "
                 "setting") {
                vector<struct EnigmaSetting> solutions= bombe.analyze(ciphertext, plaintext);
                CHECK(is_exact_setting(enigma, solutions, "AFG", "HAP",
                                       "AK. IE. DV. CQ. BN, MO, PJ. WR. UX", ciphertext, plaintext,
                                       0));
            }
        }
    }
}   // SCENARIO

SCENARIO("bombe finds the configuration of an enimga", "[bombe]") {
    GIVEN("Enigma: I II III IV, UKWR. Steckered") {
        Bombe bombe({I, II, III, IV}, UKWR);
        bombe.get_setting().stop_on_first_valid  = true;
        bombe.get_setting().max_ring_settings    = 3;
        bombe.get_setting().rotor_count          = 3;
        bombe.get_setting().starting_ring_setting= "GHJ";
        bombe.get_setting().only_one_candidate   = false;
        // struct EnigmaSetting enigma_setting;

        Enigma enigma({II, I, IV}, UKWR);
        enigma.set_plugboard("AK. IE. DV. CQ. BN, MO, PJ. WR. UX");
        // enigma.set_verbose(true);
        // TODO withoun andisveryverylong behaves weird, cannot find
        string plaintext= "THISISAPLAINTEXTTOBEENCRYPTEDANDISVERYVERYLONG";
        WHEN("ciphertext encrypted with RS:GHK, RP:BLO") {
            enigma.set_ring_setting("GHK");
            enigma.set_rotor_position("BLO");
            // enigma.set_plugboard("");
            string ciphertext= enigma.encrypt(plaintext);
            THEN("Running bombe with a complete crib should return the above "
                 "setting") {
                vector<struct EnigmaSetting> solutions= bombe.analyze(ciphertext, plaintext);
                CHECK(is_exact_setting(enigma, solutions, "GHK", "BLO",
                                       "AK. IE. DV. CQ. BN, MO, PJ. WR. UX", ciphertext, plaintext,
                                       0));
            }
        }
    }
    cout << "\r";
}


SCENARIO("bombe with CT finds the configuration of an enimga, few rotors large crib", "[bombe]") {
    //cout<<"\n\ntesting with CT";
    GIVEN("Enigma: I, II, III, UKWR. Steckered") {
        Bombe bombe({I, II}, UKWR, true);
        bombe.get_setting().stop_on_first_valid  = false;
        bombe.get_setting().max_ring_settings    = 26*26;
        bombe.get_setting().rotor_count          = 2;
        bombe.get_setting().starting_ring_setting= "AA";
        bombe.get_setting().only_one_candidate   = false;
        // struct EnigmaSetting enigma_setting;

        Enigma enigma({I, II}, UKWR);
        enigma.set_plugboard("AK. IE. DV. CQ. BN, MO, PJ. WR. UX");
        // enigma.set_verbose(true);
        // TODO withoun andisveryverylong behaves weird, cannot find
        string plaintext= "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA";
        WHEN("ciphertext encrypted with RS:WR, RP:DP") {
            enigma.set_ring_setting("WR");
            enigma.set_rotor_position("DP");
            string ciphertext= enigma.encrypt(plaintext);
            THEN("Running bombe with a complete crib should return the above "
                 "setting") {
                vector<struct EnigmaSetting> solutions= bombe.analyze(ciphertext, plaintext);
                //cout<<"\nfound "<<solutions.size()<<" solutions using the CT\n";
                CHECK(is_exact_setting(enigma, solutions, "WR", "DP",
                                       "AK. IE. DV. CQ. BN, MO, PJ. WR. UX", ciphertext, plaintext,
                                       0));
            }
        }
    }
    cout << "\r";
}


SCENARIO("bombe with CT finds the configuration of an enimga, many small rotors", "[bombe]") {
    //cout<<"\n\ntesting with CT";
    GIVEN("Enigma: random small motors. unsteckered") {
        
        C0.randomize(); C1.randomize(); C2.randomize(); C3.randomize(); C4.randomize();
        //C0.randomize();
        Bombe bombe({C0, C1, C2, C3, C4}, CR, true);
        bombe.get_setting().stop_on_first_valid   = false;
        bombe.get_setting().max_ring_settings     = (int) pow(SMALL_ROTOR_COUNT, 5);
        bombe.get_setting().rotor_count           = 5;
        bombe.get_setting().starting_ring_setting = "AAAAA";
        bombe.get_setting().only_one_candidate    = false;
        bombe.get_setting().only_one_configuration= true;
        // struct EnigmaSetting enigma_setting;

        Enigma enigma({C0, C1, C2, C3, C4}, CR);
        //enigma.set_plugboard("AK. IE. DV. CQ. BN, MO, PJ. WR. UX");
        // enigma.set_verbose(true);
        // TODO withoun andisveryverylong behaves weird, cannot find
        string plaintext= "ABCDEFABCDEFABCDEFABCDEF";
        WHEN("ciphertext encrypted with RS:ABCDB, RP:CDBAD") {
            enigma.set_ring_setting("ABCDB");
            enigma.set_rotor_position("CDBAD");
            string ciphertext= enigma.encrypt(plaintext);
            THEN("Running bombe with a complete crib should return the above "
                 "setting") {
                vector<struct EnigmaSetting> solutions= bombe.analyze(ciphertext, plaintext);
                //cout<<"\nfound "<<solutions.size()<<" solutions using the CT\n";
                CHECK(is_exact_setting(enigma, solutions, "ABCDB", "CDBAD",
                                       "", ciphertext, plaintext,
                                       0));
            }
        }
    }
    cout << "\r";
}

