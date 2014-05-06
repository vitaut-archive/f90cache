/*
  simple front-end functions to mdfour code
 */

#include <stdio.h>

#include "f90cache.h"

static struct mdfour md;

void hash_buffer(const char *s, int len)
{
    mdfour_update(&md, (unsigned char *)s, len);
}

void hash_start(void)
{
    mdfour_begin(&md);
}

void hash_string(const char *s)
{
    hash_buffer(s, strlen(s));
}

void hash_int(int x)
{
    hash_buffer((char *)&x, sizeof(x));
}

/* add contents of a file to the hash */
void hash_file(const char *fname)
{
    char buf[1024];
    int fd, n;

    fd = open(fname, O_RDONLY|O_BINARY);
    if (fd == -1) {
	fprintf(stderr,"%s: Failed to open %s\n", MYNAME, fname);
	fc_log("Failed to open %s\n", fname);
	fatal("hash_file");
    }

    while ((n = read(fd, buf, sizeof(buf))) > 0) {
	hash_buffer(buf, n);
    }
    close(fd);
}

/* add (part or all) contents of a Fortran precompiled module file
   to the hash.
   Warning: the implementation strongly depends on the compiler used
   and on its version */
void hash_module_file( const char *fname, int f90_compiler_type,
                       int gnu_minor_version )
{
    char buf[32768];
    int fd, n, i;
    int special_tag_found = 0;

    fd = open(fname, O_RDONLY|O_BINARY);
    if (fd == -1) {
	fprintf(stderr,"%s: Failed to open %s\n", MYNAME, fname);
	fc_log("Failed to open %s\n", fname);
	fatal("hash_module_file");
    }

    if (f90_compiler_type == GNU_GFC) {
        if (gnu_minor_version < 9) {
            /* GNU-gfortran up to 4.8 */
            /* the module file embbeds a MD5 sum */
            /* the substring "MD5:" must to be found; the 32 following
               chars are added to the hash;
               caution: filename must not include the "MD5:" substring!
             */
	    while ((n = read(fd, buf, sizeof(buf))) > 0) {
	        for (i=1;i<n-2;i++) {
		    if (strncmp(buf+i,"MD5:",4) == 0) {
		        special_tag_found = 1;
		        break;
		    }
	        }
	        i = i + 4;
	        hash_buffer(buf+i, 32);
	        break;
	    }
	    if (!special_tag_found) {
	        fc_log("Tag 'MD5:' not found in module file %s\n", fname);
	        fatal("hash_module_file");
	    }
        } else {
            /* GNU-gfortran 4.9 */
            /* the module file is gzipped, without any MD5 sum embedded */
            /* all the module file is added to the hash  */
            while ((n = read(fd, buf, sizeof(buf))) > 0) {
                hash_buffer(buf, n);
            }
        }
    } else if (f90_compiler_type == FSF_G95) {
	/* FSF-g95 */
	/* all the module file, except the first line, is hashed */
	/* the first newline (char 10) must to be found;
	   30 is the supposed minimum length of the first line
	 */
	while ((n = read(fd, buf, sizeof(buf))) > 0) {
	    if (special_tag_found) {
		hash_buffer(buf, n);
	    } else {
		for (i=30;i<n;i++) {
		    if (buf[i] == 10) {
			special_tag_found = 1;
			break;
		    }
		}
		hash_buffer(buf+i, n-i);
	    }
	}
	if (!special_tag_found) {
	    fc_log("Newline not found in module file %s\n", fname);
	    fatal("hash_module_file");
	}
    } else if (f90_compiler_type == INTEL_IFC) {
	/* INTEL-ifort */
	/* all the module file, after the byte number 52, is hashed */
	/* bytes 1 to 52 contains some information about the date
			 and perhaps the name of the source file */
	if ((n = read(fd, buf, sizeof(buf))) > 0) {
	    hash_buffer(buf+53, n-53);
	}
	while ((n = read(fd, buf, sizeof(buf))) > 0) {
	    hash_buffer(buf, n);
	}
    } else if (f90_compiler_type == OPEN64_F95) {
	/* OPEN64-openf95 */
	/* all the module file is added to the hash */
	while ((n = read(fd, buf, sizeof(buf))) > 0) {
	    hash_buffer(buf, n);
	}
    } else if (f90_compiler_type == SUN_F95) {
	/* Solaris Studio sunf95 */
	/* all the module file is added to the hash  */
	while ((n = read(fd, buf, sizeof(buf))) > 0) {
	    hash_buffer(buf, n);
	}
    } else {
	fprintf(stderr,"f90cache: hash_module_file: Bad compiler type\n");
	fatal("hash_module_file");
    }

    close(fd);
}

/* return the hash result as a static string */
char *hash_result(void)
{
    unsigned char sum[16];
    static char ret[53];
    int i;

    hash_buffer(NULL, 0);
    mdfour_result(&md, sum);

    for (i=0;i<16;i++) {
	sprintf(&ret[i*2], "%02x", (unsigned)sum[i]);
    }
    sprintf(&ret[i*2], "-%u", (unsigned)md.totalN);

    return ret;
}
