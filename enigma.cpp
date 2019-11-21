#include "enigma.h"
using namespace std;


//WHEEL
Wheel::Wheel() {  }
Wheel::Wheel(int wires):_wires{wires} {
        num=count++;
        //allocate to wiring array
        _wiring_in= new int[_wires];
        _wiring_out=new int[_wires];
        //make a legal wiring, essentially a substitution cipher
        for(int j=0; j<_wires; j++) {
            _wiring_in[j]=j;
            _wiring_out[j]=j;
        }
    }
Wheel::~Wheel() {
    delete _wiring_in;
    delete _wiring_out;
    cout<<"wheel "<<num<<" cleaned up\n";
}
// Copy constructor, very important, assures there is no shallow copy
Wheel::Wheel(Wheel const& copy) {
    cout<<"WHEEL: copy constructor\n";
    //src::https://stackoverflow.com/questions/255612/dynamically-allocating-an-array-of-objects
    _wires     =copy._wires;
    _wiring_in =new int[_wires];
    _wiring_out=new int[_wires];
    // Don't need to worry about copying integers.
    // But if the object has a copy constructor then
    // it would also need to worry about throws from the copy constructor.
    std::copy(&copy._wiring_in[0], &copy._wiring_in[_wires], _wiring_in);
    std::copy(&copy._wiring_out[0],&copy._wiring_out[_wires],_wiring_out);
}
Wheel& Wheel::operator=(Wheel rhs) { // Pass by value (thus generating a copy)
    cout<<"WHEEL: copy assignment\n";
    rhs.swap(*this); // Now swap data with the copy.
                     // The rhs parameter will delete the array when it
                     // goes out of scope at the end of the function
    return *this;
}
void Wheel::swap(Wheel& s) noexcept {
    using std::swap;
    swap(this->_wiring_in, s._wiring_in);
    swap(this->_wiring_out,s._wiring_out);
    swap(this->_wires ,s._wires);
}
int  Wheel::get_wires()      { return _wires; }
int* Wheel::get_wiring_in()     { return _wiring_in; }
int  Wheel::get_wiring_in(int i) { return _wiring_in[i]; }
int* Wheel::get_wiring_out()     { return _wiring_out; }
int  Wheel::get_wiring_out(int i) { return _wiring_out[i]; }
void Wheel::randomize() {
    int p1, p2, t;
    //init in as 012345...
    for(int i=0; i<_wires; i++) { _wiring_in[i]=i; }
    //mix wires randomly
    for(int i=0; i<_wires*_wires; i++) {
        p1=rand()%_wires;
        p2=rand()%_wires;
        if (p1!=p2) {
            t=_wiring_in[p2];
            _wiring_in[p2]=_wiring_in[p1];
            _wiring_in[p1]=t;
        }
        else {
            i--;
        }
    }
    //now that in is randomized, make out the inverse of in
    _wiring_out=make_inverse(_wiring_in, _wires);
}
//assumes list contains all integers from 0 to n-1
int* Wheel::make_inverse(int* in, int n) {
    int* out=(int*) malloc(n*sizeof*out);
    for(int i=0; i<n; i++) {
        out[in[i]]=i;
    }
    return out;
}
void Wheel::print() {
    for (int wire=0; wire<_wires; wire++) {
        printf("%2d: %2d\n", wire, _wiring_in[wire]);
        }
    return;
}
Reflector::Reflector(): Wheel() {
    }
Reflector::Reflector(int wires): Wheel(wires) {
        for(int j=0; j<wires; j++) {
            _wiring_in[j]=j+1-2*(j%2);
        }
    }
void Reflector::randomize() {
    int w1, w2, v1, v2;
    for(int k=0; k<_wires*_wires; k++) {
        w1=rand()%_wires;
        w2=rand()%_wires;
        //cross the wires if possible, otherwise try again
        if (w1!=w2 && _wiring_in[w1]!=w2) {
            v2       =_wiring_in[w2];
            v1       =_wiring_in[w1];
            _wiring_in[w2]=v1; _wiring_in[v2]=w1;
            _wiring_in[w1]=v2; _wiring_in[v1]=w2;
        }
        else {
            k-=1;
        }
    }
}



//CARTRIDGE
Cartridge::Cartridge() {} //XXX should really not be neccesary...
Cartridge::Cartridge(int wheel_count, int wires): _wheel_count{wheel_count}, _wires{wires} {
    _positions=new int[wheel_count];
    //init positions
    reset_positions();
    //make wheels
    _wheels   =new Wheel[wheel_count];
    for(int w=0; w<wheel_count; w++) {
        cout<<"CONSTR CARTRIDGE: making a wheel\n";
        _wheels[w]=Wheel(wires);
        cout<<"CONSTR CARTRIDGE: randomizing\n";
        _wheels[w].randomize();
    }
    _reflector=new Reflector();
    *_reflector=Reflector::make_random_reflector(wires);
}
Cartridge::Cartridge(Cartridge const& copy) {
    cout<<"CARTRIDGE:copy constructor\n";
    //src::https://stackoverflow.com/questions/255612/dynamically-allocating-an-array-of-objects
    _wires             =copy._wires;
    _wheel_count       =copy._wheel_count;
    _wheels            =new Wheel[_wheel_count];
    _reflector         =copy._reflector;
    _positions         =new int[_wheel_count];
    _reflector_position=copy._reflector_position;
    // Don't need to worry about copying integers.
    // But if the object has a copy constructor then
    // it would also need to worry about throws from the copy constructor.
    _wheels=copy._wheels; //wheels know how to copy, so we good
    //XXX copy wheels
    std::copy(&copy._positions[0], &copy._positions[_wires], _positions);
}
Cartridge& Cartridge::operator=(Cartridge rhs) { // Pass by value (thus generating a copy)
    cout<<"CARTRIDGE:copy assignment\n";
    rhs.swap(*this); // Now swap data with the copy.
                     // The rhs parameter will delete the array when it
                     // goes out of scope at the end of the function
    return *this;
}
void Cartridge::swap(Cartridge& s) noexcept {
    cout<<"cartridge swapping\n";
    using std::swap;
    swap(this->_wires, s._wires);
    swap(this->_wheel_count,s._wheel_count);
    swap(this->_wheels ,s._wheels);
    swap(this->_reflector ,s._reflector);
    swap(this->_positions ,s._positions);
    swap(this->_reflector_position ,s._reflector_position);

}
Cartridge::~Cartridge() {
    delete [] _wheels;
    delete _reflector;
    cout<<"cartridge cleared\n";
}
void Cartridge::reset_positions() {
    for (int p=0; p<_wheel_count; p++) {
        _positions[p]=0;
    }
    _reflector_position=0;
}
void Cartridge::set_positions(int p) {
    reset_positions();
    turn(p);
}
void Cartridge::set_positions(int* p_in) {
    for (int p=0; p<_wheel_count; p++) {
        _positions[p]=p_in[p];
    }
}
int* Cartridge::get_positions() { return _positions; }
int  Cartridge::get_positions_as_int() {
    int turn=0;
    int mult=1;
    for (int p=0; p<_wheel_count; p++) {
        turn+=_positions[p]*mult;
        mult*=_wires;
    }
    return turn;
}
void Cartridge::turn(int t) {
    int carry=t, next;
    for (int p=0; p<_wheel_count && carry>0; p++) {
        next=_positions[p]+carry;
        _positions[p]=next%_wires;
        carry=(int) next/_wires;
    }
}
//overloaded, single turn
void Cartridge::turn() { turn(1); }
//pass integer through wires without turning
int  Cartridge::encrypt(int i) {
    //forward pass + reflector(last)
    //printf("%2d ", i);
    for (int wheel=0; wheel<_wheel_count; wheel++) {
        //printf("(%2d) ", (i+_positions[wheel])%_wires);
        i=(_wheels[wheel].get_wiring_in((i+_positions[wheel])%_wires)+_wires-_positions[wheel])%_wires;
        //printf("-> %2d ", i);
    }
    //reflector
    i=(_reflector->get_wiring_in((i+_reflector_position)%_wires)+_wires-_reflector_position)%_wires;
    //backward pass
    for (int wheel=_wheel_count-1; wheel>=0; wheel--) {
        //printf("(%2d) ", (i+_positions[wheel])%_wires);
        i=(_wheels[wheel].get_wiring_out((i+_positions[wheel])%_wires)+_wires-_positions[wheel])%_wires;
        //printf("-> %2d ", i);
    }
    //printf("\n");
    return i;
}
//print the cartridge
void   Cartridge::print() {
    printf("  ");
    for (int wheel=0; wheel<_wheel_count; wheel++) {
        printf("  W%d", wheel);
    }
    for (int wire=0; wire<_wires; wire++) {
        printf("\n%2d: ", wire);
        for (int wheel=0; wheel<_wheel_count; wheel++) {
            printf("%2d  ", _wheels[wheel].get_wiring_in(wire));
        }
    }
    printf("\n");
    return;
};
 //print positions of wheels in cartridge
void   Cartridge::print_positions() {
    for (int p=0; p<_wheel_count; p++) {
        printf("%2d ", _positions[p]);
    }
    printf("\n");}
void   Cartridge::randomize() {
    for (int w=0; w<_wheel_count; w++) {
        _wheels[w].randomize();
    }
}
//XXX cut
Wheel* Cartridge::make_random_wheels(int n, int wires) {
    cout<<"cartridge is making random wheels\n";
    Wheel* wheels=new Wheel[n];
    for(int w=0; w<n; w++) {
        //cout<<"->cartridge making some random wheels\n";
        //wheels[w]=Wheel(wires);
        //wheels[w].randomize();
        wheels[w]=*(Wheel::make_random_wheel(wires));
    }
    //cout<<"cartridge made some random wheels\n";
    return wheels;
}



//ENIGMA ENGINE
Enigma::Enigma(int wheels_number, int wires) {

    _cartridge=new Cartridge();
    cout<<"ENIGMA:const cartridge\n";
    *_cartridge=Cartridge(wheels_number, wires);
    cout<<"ENIGMA:set cartridge\n";
    _wires=wires;
    _wheels_number=wheels_number;
}
Enigma::~Enigma() {
    //delete _cartridge;
}
void Enigma::randomize() {
    _cartridge->randomize();
}
void Enigma::set_coder() {
    //set code for language
}
void Enigma::reset() {
    _cartridge->reset_positions();
}
int  Enigma::encrypt(int m) {
    int c=_cartridge->encrypt(m);
    _cartridge->turn();
    return c;
}
int* Enigma::encrypt(int* m, int n) {
    int* e=(int*) malloc(n*sizeof(e));
    for (int i=0; i<n; i++) {
        e[i]=encrypt(m[i]);
    }
    return e;
}
void Enigma::print_positions() {
    _cartridge->print_positions();
}
void Enigma::print() {
    _cartridge->print();
}



/*
int main() {
    srand(time(NULL));
    //Wheel wheel=Wheel::make_random_wheel(20);
    //wheel.print();
    Cartridge cartridge=Cartridge::make_random_cartridge(3,8);
    //cartridge.print();
    //5test pos
    /*for (int t=0; t<100; t++) {
        cartridge.print_positions();
        cartridge.turn();
    }
    for (int t=0; t<8; t++) {
        printf("%2d -> %2d\n", t, cartridge.encrypt(t));
    }
    //cartridge.randomize();
    //cartridge.print();
    return 0;
}*/

//wheel good, cart good, turn good, encrypt in cartridge good
