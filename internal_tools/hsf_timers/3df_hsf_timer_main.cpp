#include <stdio.h>

#include <hc.h>
#include <HStream.h>

#include "timer.h"

static void usage()
{
	printf(
		"\n"
		"usage: \n"
		"\n"
		"3df_hsf_timer hsffile\n"
		);
}

static bool process_args(int argc, char *argv[], const char * &filename)
{
	if (argc < 2)
	{
		printf("Error: Invalid arg count.\n");
		usage();
		return false;
	}

	filename = argv[1];

	FILE *			fp;
	
	if (fopen_s(&fp, filename, "r") != 0)
	{
		printf("Error opening file: %s\n", filename);
		return false;
	}
	fclose(fp);

	return true;
}

int main(int argc, char *argv[])
{
	const char *		filename;
	if (!process_args(argc, argv, filename))
		return -1;

	Timer				t;
	if (t.IsValid() == false)
	{
		printf("Error creating timer!\n");
		return -1;
	}

	double				ms = -1;
	TK_Status			status;

	HC_Define_System_Options("multi-threading=full");
	HC_Open_Segment("?picture"); {
		t.Start();
		status = HTK_Read_Stream_File(filename);
		ms = t.Milliseconds();
	} HC_Close_Segment();

	if (status != TK_Complete)
		fprintf(stderr, "Error occurred reading file: %s\n", filename);
	
	printf("%f", ms);
	return 0;
}

