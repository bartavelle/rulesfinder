static const uint64_t k0 = 0xc3a5c85c97cb3127ULL;
static const uint64_t k1 = 0xb492b66fbe98f273ULL;
static const uint64_t k2 = 0x9ae16a3b2f90404fULL;
static const uint64_t k3 = 0xc949d7c7509e6557ULL;

#define UNALIGNED_LOAD64(p) (*(const uint64_t*)(p))
#define UNALIGNED_LOAD32(p) (*(const uint32_t*)(p))

inline uint64_t HashLen16(const uint64_t x1, const uint64_t x2) {
  // Murmur-inspired hashing.
  const uint64_t kMul = 0x9ddfea08eb382d69ULL;
  uint64_t a = (x1 ^ x2) * kMul;
  a ^= (a >> 47);
  uint64_t b = (x2 ^ a) * kMul;
  b ^= (b >> 47);
  b *= kMul;
  return b;
}

static uint64_t RotateByAtLeast1(uint64_t val, int shift) {
  return (val >> shift) | (val << (64 - shift));
}

static uint64_t ShiftMix(uint64_t val) {
  return val ^ (val >> 47);
}

static uint64_t HashLen0to16(const char *s, size_t len) {
	if (len > 8) {
		uint64_t a = UNALIGNED_LOAD64(s);
		uint64_t b = UNALIGNED_LOAD64(s + len - 8);
		return HashLen16(a, RotateByAtLeast1(b + len, len)) ^ b;
	}
	if (len >= 4) {
		uint64_t a = UNALIGNED_LOAD32(s);
		return HashLen16(len + (a << 3), UNALIGNED_LOAD32(s + len - 4));
	}
	if (len > 0) {
		uint8_t a = s[0];
		uint8_t b = s[len >> 1];
		uint8_t c = s[len - 1];
		uint32_t y = ((uint32_t)a) + ( ((uint32_t)b) << 8);
		uint32_t z = len + ( ((uint32_t) c) << 2);
		return ShiftMix(y * k2 ^ z * k3) * k2;
	}
	return k2;
}
