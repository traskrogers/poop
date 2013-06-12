#pragma once

#include "hc.h"

#include "Point.h"
#include "Rand.h"


class TriGen {
public:
	TriGen (Rand & rand, float sceneExtent, float triangleExtent);
	void createScene (int numTraingles);
private:
	void createTrianglePoints (Point points[3]);
	void createTriangleFace (int faceIndex, int faceList[4]);
	void createTriangle (int faceIndex, Point points[3], int faceList[4]);
	HC_KEY createTriangleCloud (int numTraingles);
	HC_KEY createRotatedInstance (HC_KEY proto);
private:
	TriGen & operator= (TriGen const &);
private:
	Rand & rand;
	int maxTrianglesPerShell;
	float sceneExtent;
	float triangleExtent;
	Point sceneMin;
	Point sceneMax;
};





