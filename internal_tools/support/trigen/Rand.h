#pragma once


#include "Point.h"


class Rand {
public:
	Rand (unsigned int seed);
	int next ();
	float nextFloat (float lo, float hi); // [lo, hi)
	Point nextPoint (Point lo, Point hi); // [lo, hi)
	static int const MAX = 0x7fff;
private:
	unsigned long state;
};



