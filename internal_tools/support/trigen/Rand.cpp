#include "Rand.h"


Rand::Rand (unsigned int seed) {
	state = static_cast<unsigned long>(seed);
}

int Rand::next () {
	return static_cast<int>(((state = state * 214013L + 2531011L) >> 16) & 0x7fff);
}

float Rand::nextFloat (float lo, float hi) {
	float x = static_cast<float>(next()) / static_cast<float>(MAX);
	return (x * (hi - lo)) + lo;
}

Point Rand::nextPoint (Point lo, Point hi) {
	float x = nextFloat(lo.x, hi.x);
	float y = nextFloat(lo.y, hi.y);
	float z = nextFloat(lo.z, hi.z);
	return Point(x, y, z);
}



