#include <Rcpp.h>
#include <cstdlib> // Needed for rand() and srand()
#include <ctime>   // Needed for time()
#include <cstdint>
using namespace Rcpp;



static inline uint64_t rotl(const uint64_t x, int k) {
  return (x << k) | (x >> (64 - k));
}

// splitmix64: expands one 64-bit seed into well-mixed state words
static inline uint64_t splitmix64(uint64_t &state) {
  uint64_t z = (state += 0x9e3779b97f4a7c15ULL);
  z = (z ^ (z >> 30)) * 0xbf58476d1ce4e5b9ULL;
  z = (z ^ (z >> 27)) * 0x94d049bb133111ebULL;
  return z ^ (z >> 31);
}

struct Xoshiro256pp {
  uint64_t s[4];
  
  explicit Xoshiro256pp(uint64_t seed) {
    for (auto &w : s) w = splitmix64(seed);
  }
  
  inline uint64_t next64() {
    const uint64_t result = rotl(s[0] + s[3], 23) + s[0];
    const uint64_t t = s[1] << 17;
    s[2] ^= s[0];
    s[3] ^= s[1];
    s[1] ^= s[2];
    s[0] ^= s[3];
    s[2] ^= t;
    s[3] = rotl(s[3], 45);
    return result;
  }
};

// [[Rcpp::export]]
int rollDice(int x) {
  
  uint64_t target = (x == 64) ? ~0ULL : (1ULL << x) - 1;
  uint64_t guesses = 0;
  int iterations = 0;
  
  
  // srand(time(0));
  do {
    int roll = static_cast<int>(unif_rand() * x);
    
    guesses |= 1ULL << roll;
    
    ++iterations;
  
  } while ((guesses != target) && (iterations < 10000));
  
       return iterations;
  
}

// [[Rcpp::export]]
IntegerVector rollDiceMany(int x, int runs, int max_iter = 10000) {
  
  // Seed once from R's RNG so set.seed() controls the result
  const uint64_t seed =
    (static_cast<uint64_t>(unif_rand() * 4294967296.0) << 32) |
    static_cast<uint64_t>(unif_rand() * 4294967296.0);
  Xoshiro256pp rng(seed);
  
  const uint64_t full = (x == 64) ? ~0ULL : (1ULL << x) - 1;
  IntegerVector out(runs);
  
  for (int r = 0; r < runs; ++r) {
    uint64_t missing = full;
    int it = 0;
    while (missing && it < max_iter) {
      const int roll = static_cast<int>(
        (static_cast<__uint128_t>(rng.next64()) * x) >> 64);
      missing &= ~(1ULL << roll);
      // missing &= ~(1ULL << static_cast<int>(unif_rand() * x));
      ++it;
    }
    out[r] = it;
  }
  return out;
}

