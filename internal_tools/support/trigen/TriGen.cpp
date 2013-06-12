#include "TriGen.h"

#include <algorithm>


#define countof(x) (sizeof(x) / sizeof(*x))


TriGen::TriGen (Rand & rand, float sceneExtent, float triangleExtent)
	: rand(rand)
	, maxTrianglesPerShell(10000)
	, sceneExtent(sceneExtent)
	, triangleExtent(triangleExtent)
	, sceneMin(0, 0, 0)
	, sceneMax(sceneExtent, sceneExtent, sceneExtent)
{}


void TriGen::createTrianglePoints (Point points[3]) {
	points[0] = rand.nextPoint(sceneMin, sceneMax);
	Point triMax;
	if (triangleExtent == 0.0f) {
		triMax = sceneMax;
	}
	else {
		triMax.x = std::min(points[0].x + triangleExtent, sceneExtent);
		triMax.y = std::min(points[0].y + triangleExtent, sceneExtent);
		triMax.z = std::min(points[0].z + triangleExtent, sceneExtent);
	}
	points[1] = rand.nextPoint(points[0], triMax);
	points[2] = rand.nextPoint(points[0], triMax);
}


void TriGen::createTriangleFace (int faceIndex, int faceList[4]) {
	faceList[0] = 3;
	faceList[1] = 3 * faceIndex + 0;
	faceList[2] = 3 * faceIndex + 1;
	faceList[3] = 3 * faceIndex + 2;
}


void TriGen::createTriangle (int faceIndex, Point points[3], int faceList[4]) {
	createTrianglePoints(points);
	createTriangleFace(faceIndex, faceList);
}


HC_KEY TriGen::createTriangleCloud (int numTriangles) {
	Point * points = new Point[3 * numTriangles];
	int * flist = new int[4 * numTriangles];

	for (int i = 0; i < numTriangles; ++i) {
		createTriangle(i, points + 3 * i, flist + 4 * i);
	}
	HC_KEY proto = HC_Insert_Shell(3 * numTriangles, points, 4 * numTriangles, flist);

	delete [] flist;
	delete [] points;

	return proto;
}


HC_KEY TriGen::createRotatedInstance (HC_KEY proto) {
	HC_KEY ref = HC_Reference_Geometry_By_Key(proto);
	HC_Open_Geometry(ref);{
		HC_Rotate_Object(
			rand.nextFloat(0, 360),
			rand.nextFloat(0, 360),
			rand.nextFloat(0, 360));
	}HC_Close_Geometry();
	return ref;
}


void TriGen::createScene (int numTriangles) {
	static char const * colors[] = {
		"red",
		"orange",
		"yellow",
		"green",
		"blue",
		"indigo",
		"violet",
		"black",
		"white",
	};

	int fullCloudSize = maxTrianglesPerShell;
	int partialCloudSize = numTriangles % maxTrianglesPerShell;
	int numFullClouds = numTriangles / maxTrianglesPerShell;
	int numClouds = numFullClouds + (partialCloudSize > 0 ? 1 : 0);

	HC_KEY shellCloudPrototype = HC_ERROR_KEY;
	for (int i = 0; i < numClouds; ++i) {
		HC_Open_Segment("");{
			HC_Set_Color(colors[i % countof(colors)]);
			if (i < numFullClouds) {
				if (shellCloudPrototype == HC_ERROR_KEY) {
					shellCloudPrototype = createTriangleCloud(fullCloudSize);
				}
				else {
					createRotatedInstance(shellCloudPrototype);
				}
			}
			else {
				createTriangleCloud(partialCloudSize);
			}
		}HC_Close_Segment();
	}
}












