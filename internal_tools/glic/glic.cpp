


#define _CRT_SECURE_NO_WARNINGS

#include <iostream>
#include <iomanip>
#include <ctime>

#include <fstream>
#include <sstream>
#include <string>
#include <cstring>

#include <assert.h>


// g++ -std=c++0x -Wall -Wextra glic.cpp

//E:\boost_1_50_0\boost_1_50_0

#ifdef _MSC_VER
#define USE_BOOST 1
#endif


#ifdef USE_BOOST
#	include <boost/chrono.hpp>
#	include <boost/random/random_device.hpp>
#	include <boost/random/uniform_int_distribution.hpp>

namespace GLIC {
	using boost::chrono::time_point;
	using boost::chrono::system_clock;
	using boost::chrono::duration_cast;
	using boost::chrono::hours;
	using boost::random::random_device;
	using boost::random::uniform_int_distribution;
}

#else

#	include <chrono>
#	include <random>

namespace GLIC {
	using std::chrono::time_point;
	using std::chrono::system_clock;
	using std::chrono::duration_cast;
	using std::chrono::hours;
	using std::random_device;
	using std::uniform_int_distribution;
}
#endif

using namespace GLIC;

#include <ctime>




class Base64 {
private:
	static const char * BASE64;
	static unsigned char UNBASE64[];

	static bool first_decode;
	//static const char pad = '='; // standard
	static const char pad = '#';

public:
	static size_t encode(const unsigned char * in, size_t n, unsigned char * out)
	{
		size_t j=0;

		while(n>2) {

			unsigned long	v = (in[0]<<16) | (in[1]<<8) | in[2];
			*out++ = BASE64[(v >> 18) & 63];
			*out++ = BASE64[(v >> 12) & 63];
			*out++ = BASE64[(v >> 6) & 63];
			*out++ = BASE64[v & 63];

			in+=3;
			n-=3;
			j+=4;
		}

		switch(n) {
			case 1: {
				unsigned long	v = in[0]<<4;
				*out++ = BASE64[(v >> 6) & 63];
				*out++ = BASE64[v & 63];
				*out++ = pad;
				*out++ = pad;
				j+=4;
			} break;
			case 2: {
				unsigned long	v = (in[0]<<10) | (in[1]<<2);
				*out++ = BASE64[(v >> 12) & 63];
				*out++ = BASE64[(v >> 6) & 63];
				*out++ = BASE64[v & 63];
				*out++ = pad;
				j+=4;
			} break;

			default:break;
		}

		return j;
	}

	static bool decode(const unsigned char * in, size_t n_in, unsigned char * out, size_t & n_out)
	{
		if (first_decode) { 
			first_decode = false;
			memset(UNBASE64, 0xff, 256);

			for (size_t i=0; i<64; ++i) {
				UNBASE64[(int)BASE64[i]] = i;
			}
		}

		size_t i=0;
		n_out = 0;

		while(n_in>3) {

			if(in[i+3] == pad){

				if(in[i+2] == pad){
					//two input bytes validate input
					if( UNBASE64[(int)in[i]] == 0xff || 
						UNBASE64[(int)in[i+1]] == 0xff) break;

					// xxxxxxxx0000 v = in[0]<<4;
					// xxxxxx       *out++ = BASE64[v >> 6];
					//       xx0000 *out++ = BASE64[v & 63];

					//one output byte
					*out++ = (UNBASE64[(int)in[i]]<<2) | (UNBASE64[(int)in[i+1]]>>4);
					n_out+=1;

				}else{
					//three input bytes validate input
					if( UNBASE64[(int)in[i]] == 0xff || 
						UNBASE64[(int)in[i+1]] == 0xff || 
						UNBASE64[(int)in[i+2]] == 0xff) break;

					// xxxxxxxxXXXXXXXX00 v = (in[0]<<10) | (in[1]<<2);
					// xxxxxx             *out++ = BASE64[v >> 12];
					//       xxXXXX       *out++ = BASE64[(v >> 6) & 63];
					//             XXXX00 *out++ = BASE64[v & 63];

					//two output bytes
					*out++ = (UNBASE64[(int)in[i]]<<2) | (UNBASE64[(int)in[i+1]]>>4);
					*out++ = (UNBASE64[(int)in[i+1]]<<4) | (UNBASE64[(int)in[i+2]]>>2);
					n_out+=2;
				}

				//this must be last part so force break for validation
				n_in -= 4;
				break;

			}else{
				//four input bytes validate input
				if( UNBASE64[(int)in[i]] == 0xff || 
					UNBASE64[(int)in[i+1]] == 0xff || 
					UNBASE64[(int)in[i+2]] == 0xff || 
					UNBASE64[(int)in[i+3]] == 0xff) break;

				// xxxxxxxxXXXXXXXXZZZZZZZZ v = (in[0]<<16) | (in[1]<<8) | in[2];
				// xxxxxx                   *out++ = BASE64[v >> 18];
				//       xxXXXX             *out++ = BASE64[(v >> 12) & 63];
				//             XXXXZZ       *out++ = BASE64[(v >> 6) & 63];
				//                   ZZZZZZ *out++ = BASE64[v & 63];

				//three output bytes
				*out++ = (UNBASE64[(int)in[i]]<<2) | (UNBASE64[(int)in[i+1]]>>4); 
				*out++ = (UNBASE64[(int)in[i+1]]<<4) | (UNBASE64[(int)in[i+2]]>>2);
				*out++ = (UNBASE64[(int)in[i+2]]<<6) | UNBASE64[(int)in[i+3]];
				n_out+=3;
			}

			n_in -= 4;
			in += 4;
		}

		// EXTRA BYTE ARE BOGUS OR MAY BE BAD INPUT
		if(n_in) 
			return false;

		return true;
	}
};

#define UC(_x_) (unsigned char)(_x_)

const char * Base64::BASE64 = "$abcdefghijklmnopqrstuvwxyz0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ_";
//const char * Base64::BASE64 = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"; // standard
unsigned char  Base64::UNBASE64[256];
bool Base64::first_decode = true;

class yEnc {
public:
	static size_t encode(const unsigned char * in, size_t n, unsigned char * out)
	{
		size_t i, j=0;

		for(i=0;i<n;i++) {

			unsigned char c = (unsigned char)in[i] + 42;

			if(c==0x00||c==0x0A||c==0x0D||c=='='){

				out[j]='=';
				j++;

				c+=64;
			}

			out[j] = c;
			j++;

		}

		return j;
	}

	static bool decode(const unsigned char * in, size_t n_in, char unsigned * out, size_t & n_out)
	{

		n_out=0;

		while(n_in){

			unsigned char c=(unsigned char)*in;
			in++;
			n_in--;

			if(c=='='){

				if(n_in){
					c=(unsigned char)*in++;
					n_in--;
					c -= 64;
				}else{
					return false;
				}
			}

			*out++ = c-42;
			++n_out;
		}

		return true;
	}

};

///////////////////////////////////////////////


const size_t LICENSE_TAIL_PAD_SIZE = 128;

const size_t VISUALIZE_CUSTOMER_FIELD_SIZE = 128;

const size_t EXCHANGE_CUSTOMER_KEY_FIELD_SIZE = 64;
const size_t EXCHANGE_VARIABLE_KEY_FIELD_SIZE = 256;

namespace Products {

	enum Items {
		Visualize	= (1<<0),
		Exchange	= (1<<1),
		Publish		= (1<<2),
	};

}

namespace Visualize_Features {

	enum Items {
		Mobile	= (1<<0),
	};

}


struct license_elements_t {

	struct {
		char TEE;
		char x1[3];
		char ESS;
		char x2[3];
		char THREE;
		char x3[3];
		char DEE;
		char x4[3];
		char TWO;
		char x5[3];
		char ZERO;
		char x6[3];
		char ONE;
		char x7[3];
		char TWO_2;
		char x8[3];
	} header;

	int pad_01[1];
	int days;
	int pad_02[1];
	char customer[VISUALIZE_CUSTOMER_FIELD_SIZE];
	int pad_03[1];
	int products;
	int pad_04[1];
	int visualize_3df_version;
	int pad_05[1];
	int visualize_hps_version;
	int pad_06[1];
	int visualize_features;
	int pad_07[1];
	int exchange_version;
	int pad_08[1];
	int publish_version;
	int pad_09[1];
	char exchange_customer_key[EXCHANGE_CUSTOMER_KEY_FIELD_SIZE];
	int pad_10[1];
	char exchange_variable_key[EXCHANGE_VARIABLE_KEY_FIELD_SIZE];

	struct {
		char x1[3];
		char TWO;
		char x2[3];
		char ZERO;
		char x3[3];
		char ONE;
		char x4[3];
		char TWO_2;
		char x5[3];
	} footer;

	char tail_pad[LICENSE_TAIL_PAD_SIZE];
};

const size_t LICENSE_SIZE = 1024;

union license_t {

	license_elements_t elements;

	char everything[LICENSE_SIZE];
};

static bool unified_license_decode(license_t & un_lic, const char * blob) {

	if (!blob)
		return false;

	size_t	blob_length = strlen(blob);

	size_t	l64 = 0;

	while (blob_length > 0) {

		if (blob[blob_length] != ' ' && blob[blob_length] != ':') {
			blob_length--;
			l64++;
		}
		else {
			blob_length++;
			l64--;
			break;
		}
	}

	if (l64 < LICENSE_SIZE)
		return false;

	memset(&un_lic, 0, sizeof(license_t));

	const unsigned char * enc64 = (const unsigned char *)&blob[blob_length];

	unsigned char unenc64[LICENSE_SIZE*3];
	size_t unl64;
	bool successy = Base64::decode(enc64, l64, unenc64, unl64);
	(void)successy;

	unsigned char unyenc[LICENSE_SIZE*2];
	size_t unl;
	bool success = yEnc::decode(unenc64, unl64, unyenc, unl);
	(void)success;

	if (unl < sizeof(license_elements_t))
		return false;

	memcpy(un_lic.everything, unyenc, sizeof(license_elements_t));

	if ((un_lic.elements.header.TEE	== 'T') &&
		(un_lic.elements.header.ESS	== 'S') &&
		(un_lic.elements.header.THREE == '3') &&
		(un_lic.elements.header.DEE	== 'D') &&
		(un_lic.elements.header.TWO	== '2') &&
		(un_lic.elements.header.ZERO == '0') &&
		(un_lic.elements.header.ONE	== '1') &&
		(un_lic.elements.header.TWO_2 == '2') &&
		(un_lic.elements.footer.TWO	== '2') &&
		(un_lic.elements.footer.ZERO == '0') &&
		(un_lic.elements.footer.ONE	== '1') &&
		(un_lic.elements.footer.TWO_2 == '2'))
		return true;

	return false;
}


class License {

public:
	License() {

		expires_in_days = 90;

		visualize = false;
		visualize_3df_version = 0;
		visualize_hps_version = 0;

		exchange = false;
		exchange_version = 0;
		publish_version = 0;

		assert(sizeof(license_elements_t) < LICENSE_SIZE);
	}

	~License() {

	}

	void SetExpires(int in_expires_in_days=90) {
		if (in_expires_in_days > 0)
			expires_in_days = in_expires_in_days;
		else 
			expires_in_days = 0;
	}

	void SetCustomer(const char * in_customer) {
		customer = in_customer;
	}

	void SetVisualize(int in_3df_version, int in_hps_version, int in_features=0) {

		if (in_3df_version > 0 || in_hps_version > 0)
			visualize = true;
		visualize_3df_version = in_3df_version;
		visualize_hps_version = in_hps_version;
		visualize_features = in_features;
	}

	void SetExchange(int in_exchange_version, int in_publish_version) {

		if (in_exchange_version > 0 || in_publish_version > 0)
			exchange = true;

		exchange_version = in_exchange_version;
		publish_version = in_publish_version;
	}

	void SetExchangeCustomerKey(const char * in_exchange_customer_key) {

		exchange_customer_key = in_exchange_customer_key;
		assert(exchange_customer_key.length() < EXCHANGE_CUSTOMER_KEY_FIELD_SIZE);
	}

	void SetExchangeVariableKey(const char * in_exchange_variable_key) {

		exchange_variable_key = in_exchange_variable_key;
		assert(exchange_variable_key.length() < EXCHANGE_VARIABLE_KEY_FIELD_SIZE);
	}

	void SetExchangeComment(const char * in_exchange_comment) {

		exchange_comment = in_exchange_comment;
	}

	void SetExchangeImportComment(const char * in_exchange_import_comment) {

		exchange_import_comment = in_exchange_import_comment;
	}

	void SetExchangeExportComment(const char * in_exchange_export_comment) {

		exchange_export_comment = in_exchange_export_comment;
	}

	bool Generate(std::string & generated) {

		if (customer.length() == 0)
			return false;

		if (!visualize && !exchange)
			return false;

		std::string random_chars("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890");
		random_device rng;
		uniform_int_distribution<> index_dist(0, random_chars.size() - 1);

		license_t lic;

		for(size_t i = 0; i < LICENSE_SIZE; ++i) {
			lic.everything[i] = random_chars[index_dist(rng)];
		}

		lic.everything[LICENSE_SIZE-1] = '\0';

		lic.elements.header.TEE		= 'T';
		lic.elements.header.ESS		= 'S';
		lic.elements.header.THREE	= '3';
		lic.elements.header.DEE		= 'D';
		lic.elements.header.TWO		= '2';
		lic.elements.header.ZERO	= '0';
		lic.elements.header.ONE		= '1';
		lic.elements.header.TWO_2	= '2';

		lic.elements.footer.TWO		= '2';
		lic.elements.footer.ZERO	= '0';
		lic.elements.footer.ONE		= '1';
		lic.elements.footer.TWO_2	= '2';

		generated = "Expires: ";
		if (expires_in_days == 0) {
			generated += "Never";
			lic.elements.days = -1;
		}
		else {
			
			time_point<system_clock> now = system_clock::now();

			int expire_hours_from_epoch = duration_cast<hours>(now.time_since_epoch()).count() + (expires_in_days * 24);

			lic.elements.days = expire_hours_from_epoch;

			time_point<system_clock> expire_date = (now + hours(expire_hours_from_epoch)) - now.time_since_epoch();

			std::time_t expire_date_time_t = system_clock::to_time_t(expire_date);

			//std::stringstream ss;
			//ss << std::put_time(std::localtime(&expire_date_time_t), "%m/%d/%Y");
			//generated += ss.str();

			char outstr[200];
			struct tm *tmp = localtime(&expire_date_time_t);
			strftime(outstr, sizeof(outstr), "%m/%d/%Y", tmp);
			generated += outstr;
		}


		size_t customer_field_length = strlen(customer.c_str())+1;
		if (customer_field_length > VISUALIZE_CUSTOMER_FIELD_SIZE)
			customer_field_length = VISUALIZE_CUSTOMER_FIELD_SIZE;
		memcpy(lic.elements.customer, customer.c_str(), customer_field_length);
		lic.elements.customer[customer_field_length] = '\0';

		generated += "\nCustomer: " + customer;

		int products = 0;

		generated += "\nProducts:";
			
		if (visualize) {
			generated += " Visualize";
			products |= Products::Visualize;
		}

		if (exchange) {
			if (exchange_version > 0) {
				generated += " Exchange";
				products |= Products::Exchange;
			}
			if (publish_version > 0) {
				generated += " Publish";
				products |= Products::Publish;
			}
		}

		lic.elements.products = products;

		lic.elements.visualize_3df_version = 0;
		lic.elements.visualize_hps_version = 0;
		lic.elements.visualize_features = 0;

		if (visualize) {
			generated += "\nVisualize:";

			if (visualize_3df_version > 0) {
				std::stringstream ss;
				ss << " 3DF_VERSION_" << std::dec << visualize_3df_version;
				generated += ss.str();
				lic.elements.visualize_3df_version = visualize_3df_version;
			}

			if (visualize_hps_version > 0) {
				std::stringstream ss;
				ss << " HPS_VERSION_" << std::dec << visualize_hps_version;
				generated += ss.str();
				lic.elements.visualize_hps_version = visualize_hps_version;
			}

			if (visualize_features != 0) {

				if (visualize_features & Visualize_Features::Mobile) {
					generated += " Mobile";
				}

				lic.elements.visualize_features = visualize_features;
			}
		}

		lic.elements.exchange_version = 0;
		lic.elements.publish_version = 0;

		if (exchange) {

			generated += "\nExchange:";

			if (exchange_version > 0) {
				std::stringstream ss;
				ss << " EXCHANGE_VERSION_" << std::dec << exchange_version;
				generated += ss.str();
				lic.elements.exchange_version = exchange_version;
			}

			if (publish_version > 0) {
				std::stringstream ss;
				ss << " PUBLISH_VERSION_" << std::dec << publish_version;
				generated += ss.str();
				lic.elements.publish_version = publish_version;
			}

			if (exchange_comment.length() > 0) {
				generated += " ";
				generated += exchange_comment;
			}

			if (exchange_import_comment.length() > 0) {
				generated += "\nExchange Import:";
				generated += " ";
				generated += exchange_import_comment;
			}

			if (exchange_export_comment.length() > 0) {
				generated += "\nExchange Export:";
				generated += " ";
				generated += exchange_export_comment;
			}

			size_t exchange_custumer_key_length = strlen(exchange_customer_key.c_str())+1;
			if (exchange_custumer_key_length > EXCHANGE_CUSTOMER_KEY_FIELD_SIZE)
				exchange_custumer_key_length = EXCHANGE_CUSTOMER_KEY_FIELD_SIZE;
			memcpy(lic.elements.exchange_customer_key, exchange_customer_key.c_str(), exchange_custumer_key_length);
			lic.elements.exchange_customer_key[exchange_custumer_key_length] = '\0';


			size_t exchange_variable_key_length = strlen(exchange_variable_key.c_str())+1;
			if (exchange_variable_key_length > EXCHANGE_VARIABLE_KEY_FIELD_SIZE)
				exchange_variable_key_length = EXCHANGE_VARIABLE_KEY_FIELD_SIZE;
			memcpy(lic.elements.exchange_variable_key, exchange_variable_key.c_str(), exchange_variable_key_length);
			lic.elements.exchange_variable_key[exchange_variable_key_length] = '\0';
		}

		unsigned char yenc[LICENSE_SIZE*2];

		size_t l = yEnc::encode((unsigned char*)lic.everything, LICENSE_SIZE, yenc);

		unsigned char enc64[LICENSE_SIZE*3];

		size_t l64 = Base64::encode(yenc, l, enc64);

 		while (enc64[l64-1] == '#') 
 			l64--;

		enc64[l64] = '\0';

		generated += "\nKey: ";

		generated += std::string((char*)enc64);

		generated += "\n";

		////////////////////

		license_t un_lic;

		if (unified_license_decode(un_lic, generated.c_str())) {

			assert(un_lic.elements.header.TEE	== 'T');
			assert(un_lic.elements.header.ESS	== 'S');
			assert(un_lic.elements.header.THREE	== '3');
			assert(un_lic.elements.header.DEE	== 'D');
			assert(un_lic.elements.header.TWO	== '2');
			assert(un_lic.elements.header.ZERO	== '0');
			assert(un_lic.elements.header.ONE	== '1');
			assert(un_lic.elements.header.TWO_2 == '2');

			assert(un_lic.elements.footer.TWO	== '2');
			assert(un_lic.elements.footer.ZERO	== '0');
			assert(un_lic.elements.footer.ONE	== '1');
			assert(un_lic.elements.footer.TWO_2 == '2');

			assert(lic.elements.days == un_lic.elements.days);
			assert(memcmp(lic.elements.customer, un_lic.elements.customer, VISUALIZE_CUSTOMER_FIELD_SIZE) == 0);

			assert(lic.elements.products == un_lic.elements.products);
			assert(lic.elements.visualize_3df_version == un_lic.elements.visualize_3df_version);
			assert(lic.elements.visualize_hps_version == un_lic.elements.visualize_hps_version);

			assert(lic.elements.exchange_version == un_lic.elements.exchange_version);
			assert(lic.elements.publish_version == un_lic.elements.publish_version);
			assert(memcmp(lic.elements.exchange_customer_key, un_lic.elements.exchange_customer_key, EXCHANGE_CUSTOMER_KEY_FIELD_SIZE) == 0);
			assert(memcmp(lic.elements.exchange_variable_key, un_lic.elements.exchange_variable_key, EXCHANGE_VARIABLE_KEY_FIELD_SIZE) == 0);

#if 0
			if (lic.elements.days == -1) {
				generated += "Never";
			} else {
				time_point<system_clock> now = system_clock::now();
				time_point<system_clock> expire_date = (now + hours(un_lic.elements.days)) - now.time_since_epoch();

				std::time_t expire_date_time_t = system_clock::to_time_t(expire_date);

				std::stringstream ss;
				ss << std::put_time(std::localtime(&expire_date_time_t), "%m/%d/%Y");

				generated += ss.str();
			}
#endif
			return true;
		}

		return false;
	}

private:
	int			expires_in_days;
	std::string	customer;

	bool		visualize;
	int			visualize_3df_version;
	int			visualize_hps_version;
	int			visualize_features;

	bool		exchange;

	int			exchange_version;
	int			publish_version;

	std::string	exchange_customer_key;
	std::string	exchange_variable_key;
	std::string	exchange_comment;
	std::string	exchange_import_comment;
	std::string	exchange_export_comment;
};


#include "getopts.c"

//--expires 30 --customer "Techsoft3D INC" --hps_version 1 --3df_version 20 --mobile --exchange_version 5 --publish_version 2 
//--exchange_comment "Expires 31/12/2037" --exchange_import_comment "CATIA V5, PRC" --exchange_export_comment "ACIS" --exchange_customer_key 659333969a3d922f66c9361eb750 
//--exchange_variable_key 01fd78f36c8e0a16fe475e3b9210bcc74f51a0e7dc47f05d54fe58b814907db490c5addd73e6262dd710a428cbb9fd9dbf8f83a4c7d5ce9ef9d4a0b3b394a04de6ea8999a5e2d121fa64dd3af1a268de0d6758600c82f61378dd98637227bd3571623c2d6f0d6c

int main(int argc, char **argv)
{
	License lic;

	struct options opts[] = {
		{ 1,	"expires",					"Expires in N days",		"", 1 },
		{ 2,	"customer",					"Customer name",			"", 1 },
		{ 3,	"hps_version",				"HPS version",				"", 1 },
		{ 4,	"3df_version",				"3DF version",				"", 1 },
		{ 5,	"mobile",					"Visualize mobile",			"", 0 },
		{ 6,	"exchange_version",			"Exchange version",			"", 1 },
		{ 7,	"publish_version",			"Publish version",			"", 1 },
		{ 8,	"exchange_customer_key",	"Exchange customer key",	"", 1 },
		{ 9,	"exchange_variable_key",	"Exchange variable key",	"", 1 },
		{ 10,	"exchange_comment",			"Exchange comment",			"", 1 },
		{ 11,	"exchange_import_comment",	"Exchange import comment",	"", 1 },
		{ 12,	"exchange_export_comment",	"Exchange export comment",	"", 1 },
		{ 0, 0, 0, 0, 0 }
	};

	int version_hps = 0;
	int version_3df = 0;
	int visualize_features = 0;

	int version_exchange = 0;
	int version_publish = 0;

	char *	args;
	int	c;

	while ((c = getopts(argc, argv, opts, &args)) != 0) {

		switch (c) {

		case 1: { // expires
			int days = atoi(args);
			lic.SetExpires(days);
		} break;

		case 2: { // customer
			lic.SetCustomer(args);
		} break;

		case 3: { // hps_version
			version_hps = atoi(args);
		} break;

		case 4: { // 3df_version
			version_3df = atoi(args);
		} break;

		case 5: { // mobile
			visualize_features |= Visualize_Features::Mobile;
		} break;

		case 6: { // exchange_version
			version_exchange = atoi(args);
		} break;

		case 7: { // publish_version
			version_publish = atoi(args);
		} break;

		case 8: { // exchange_customer_key
			lic.SetExchangeCustomerKey(args);
		} break;

		case 9: { // exchange_variable_key
			lic.SetExchangeVariableKey(args);
		} break;

		case 10: { // exchange_comment
			lic.SetExchangeComment(args);
		} break;

		case 11: { // exchange_import_comment
			lic.SetExchangeImportComment(args);
		} break;

		case 12: { // exchange_export_comment
			lic.SetExchangeExportComment(args);
		} break;

		default: break;
		}

		if (args)
			free(args);
	}

	lic.SetVisualize(version_3df, version_hps, visualize_features);

	lic.SetExchange(version_exchange, version_publish);


 	//int s1 = sizeof(license_t);
 	//int s2 = sizeof(license_elements_t);

	std::string generated;
	if (!lic.Generate(generated)) {
		std::cout << "Generate failed\n" << std::endl;
		return EXIT_FAILURE;
	}

	std::cout << generated << std::endl;

	license_t un_lic;

	if (!unified_license_decode(un_lic, generated.c_str())) {
		std::cout << "\nLicense invalid\n" << std::endl;
		return EXIT_FAILURE;
	}

// 	boost::chrono::time_point<boost::chrono::system_clock> now;
// 	now = boost::chrono::system_clock::now();
// 	std::time_t now_c = boost::chrono::system_clock::to_time_t(now - boost::chrono::hours(24));
// 	std::cout << "One day ago, the time was " << std::put_time(std::localtime(&now_c), "%m/%d/%Y") << '\n';

	return EXIT_SUCCESS;
}

#if 0

IMPORT:
	HOOPS Exchange	KE_FUN_EXCHANGE_ADVANCED: 812
	Autodesk Inventor	KE_INVENTOR_R: 226
	CATIA V4	KE_CATIAV4_R: 201
	CATIA V5	KE_CATIAV5_R: 202
	CGR	KE_CGR_R: 217
	I-deas	KE_IDEAS_R: 204
	IFC	KE_IFC_R: 229
	IGES	KE_IGES_R: 209
	JT	KE_JT_R: 211
	Parasolid	KE_PARASOLID_R: 208
	PRC	KE_PRC_R: 222
	Pro/Engineer	KE_PROENGINEER_R: 205
	Siemens PLM Solutions NX	KE_UNIGRAPHICS_R: 206
	Solid Edge	KE_SE_R: 227
	SolidWorks	KE_SLW_R: 216
	STEP	KE_STEP_R: 210
	Stereo Lithography (STL)	KE_STL_R: 219
	Universal 3D (U3D)	KE_U3D_R: 230
	VDA-FS	KE_VDA_R: 213
	VRML	KE_WRL_R: 221
	XVL	KE_XVL_R: 220
	3D Studio, COLLADA, KMZ, OBJ	KE_DCC_R: 228
	CADDS 5	KE_CADDS_R: 200
	OneSpace Designer	KE_SOLDES_R: 224


EXPORT:
	ACIS	KE_ACIS_W: 407
	IGES	KE_IGES_W: 409
	Parasolid	KE_PARASOLID_W: 408
	STEP	KE_STEP_W: 410
	Stereo Lithography	KE_STL_W 411
	Universal 3D (U3D)	

#endif
