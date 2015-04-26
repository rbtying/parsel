#include <iostream>
#include <functional>
#include "outputs.h"

psl::Chunk<char> chr2Chunk(char chr) { return [=]{return chr;}; }




psl::Chunk<std::function<interval(psl::Chunk<interval>,psl::Chunk<fsignal>)>> applyFilterF;
psl::Chunk<std::function<psl::Signal(psl::Chunk<psl::Signal>,psl::Chunk<std::function<interval(psl::Chunk<double>,psl::Chunk<interval>)>>,psl::Chunk<double>)>> intervalMap;
psl::Chunk<std::function<double(psl::Chunk<double>,psl::Chunk<double>,psl::Chunk<double>)>> envelope;
psl::Chunk<std::function<fsignal(psl::Chunk<double>)>> leftRightFilter;
psl::Chunk<std::function<interval(psl::Chunk<interval>)>> filterInterval;
psl::Chunk<std::function<std::function<psl::Signal(psl::Chunk<psl::Signal>)>(psl::Chunk<psl::Signal>)>> thing;
psl::Chunk<std::function<std::tuple<psl::Chunk<psl::Signal>>(psl::Chunk<std::vector<psl::Chunk<char>>>)>> out;

int main(int argc, char **argv) {
applyFilterF = [&]() { return [&](psl::Chunk<interval> input, psl::Chunk<fsignal> filter) {
 return [&]() { return std::make_tuple([&]() { return psl::apply([&]() { return psl::apply(interval, psl::ft, (input()).stop, (input()).start); }, [&]() { return [&]() { return psl::apply(psl::multiply, [&]() { return psl::apply(psl::ft, input); }, filter); }; }); }); }();
}; };
intervalMap = [&]() { return [&](psl::Chunk<psl::Signal> input, psl::Chunk<std::function<interval(psl::Chunk<double>,psl::Chunk<interval>)>> f, psl::Chunk<double> width) {
 return [&]() { return psl::apply(merge, [&]() { return psl::apply(map, [&]() { return psl::apply(chop, input, width); }, f); }, width); }();
}; };
envelope = [&]() { return [&](psl::Chunk<double> t, psl::Chunk<double> decay, psl::Chunk<double> attack) {
 return [&]() {
if([&]() { return [&]() { return psl::apply(psl::lessThan, t, attack); }; }()) {
return [&]() { return [&]() { return psl::apply(psl::divide, t, attack); }; }();
}
else {
return [&]() {
if([&]() { return [&]() { return psl::apply(psl::lessThan, t, decay); }; }()) {
return [&]() { return [&]() { return psl::apply(psl::minus, [&]() { return 1.0; }, [&]() { return [&]() { return psl::apply(psl::divide, [&]() { return std::make_tuple([&]() { return [&]() { return psl::apply(psl::minus, t, attack); }; }); }, decay); }; }); }; }();
}
else {
return [&]() { return 0.0; }();
};
}();
};
}();
}; };
leftRightFilter = [&]() { return [&](psl::Chunk<double> cutoff) {
 return [&]() { return psl::apply(fsignal, [&]() { return [&](psl::Chunk<double> f) {
 return [&]() {
if([&]() { return [&]() { return psl::apply(psl::lessThan, f, cutoff); }; }()) {
return [&]() { return std::make_tuple([&]() { return 0.0; }, [&]() { return 1.0; }); }();
}
else {
return [&]() { return std::make_tuple([&]() { return 1.0; }, [&]() { return 0.0; }); }();
};
}();
}; }); }();
}; };
filterInterval = [&]() { return [&](psl::Chunk<interval> input) {
 return [&]() {
psl::Chunk<std::function<double(psl::Chunk<double>)>> cutoff;
psl::Chunk<double> thing;

cutoff = [&]() { return [&](psl::Chunk<double> t) {
 return [&]() { return [&]() { return psl::apply(psl::multiply, [&]() { return psl::apply(envelope, t, [&]() { return 10.0; }, [&]() { return 0.0; }); }, thing); }; }();
}; };
thing = [&]() {
return [&]() { return 1000.0; }();
};

return [&]() { return psl::apply(applyFilterF, input, [&]() { return psl::apply(leftRightFilter, [&]() { return psl::apply(cutoff, (input()).start); }); }); }();
}();
}; };
thing = [&]() { return [&](psl::Chunk<psl::Signal> s) {
 return [&]() {
psl::Chunk<std::function<psl::Signal(psl::Chunk<psl::Signal>)>> t;

t = [&]() { return [&](psl::Chunk<psl::Signal> s) {
 return [&]() { return [&](psl::Chunk<psl::Signal> i) {
 return s();
}; }();
}; };

return t();
}();
}; };
out = [&]() { return [&](psl::Chunk<std::vector<psl::Chunk<char>>> input) {
 return [&]() {
psl::Chunk<psl::Signal> p;
psl::Chunk<psl::Signal> m;
psl::Chunk<psl::Signal> s;

p = [&]() {
return [&]() { return psl::apply([&]() { return psl::apply(thing, s); }, s); }();
};
m = [&]() {
return [&]() { return psl::apply(signal, [&]() { return psl::apply(sampleRate, s); }, psl::sin); }();
};
s = [&]() {
return [&]() { return psl::apply(psl::intervalMap, [&]() { return psl::apply(signalFromWav, input); }, filterInterval, [&]() { return 1.0000001e-2; }); }();
};

return [&]() { return std::make_tuple([&]() { return [&]() { return psl::apply(psl::plus, m, p); }; }); }();
}();
}; };

psl::Chunk<std::vector<psl::Chunk<char>>> args[argc];
for (int i = 0; i < argc; i++) {
std::vector<psl::Chunk<char>> chk(strlen(argv[i])+1);
std::transform(argv[i], argv[i]+strlen(argv[i])+1, chk.begin(), chr2Chunk);
args[i] = [=]{ return chk; }
}

bool B = true;
while(out()(args[1])().fillBuffer(B))
B = !B;
return 0;
}