#include <iostream>
#include <string>

#include "hc.h"
#include "HStream.h"

#include "CmdOpts.h"
#include "Rand.h"
#include "TriGen.h"



int main (int argc, char ** argv) {
	CmdOpts opts;{
		std::string errMsg;
		if (!opts.parse(argc, argv, errMsg)) {
			std::cerr << errMsg << std::endl;
			return 1;
		}
		if (opts.help) {
			CmdOpts::printHelp(argv[0], std::cout);
			return 0;
		}
	}

	HC_Open_Segment("/");{
		HC_Open_Segment("");{
			Rand rand(opts.seed);
			TriGen trigen(rand, opts.sceneExtent, opts.triangleExtent);
			trigen.createScene(opts.numTriangles);

			int flags = TK_Suppress_LOD | TK_Full_Resolution;
			HTK_Write_Stream_File(opts.filename.c_str(), flags);
		}HC_Close_Segment();
	}HC_Close_Segment();

	return 0;
}






