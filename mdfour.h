/*
   Unix SMB/Netbios implementation.
   Version 1.9.
   a implementation of MD4 designed for use in the SMB authentication protocol
   Copyright (C) Andrew Tridgell 1997-1998.
*/

struct mdfour {
    uint32 A, B, C, D;
    uint32 totalN;
    unsigned char tail[64];
    unsigned tail_len;
};

void mdfour_begin(struct mdfour *md);
void mdfour_update(struct mdfour *md, const unsigned char *in, int n);
void mdfour_result(struct mdfour *md, unsigned char *out);
void mdfour(unsigned char *out, const unsigned char *in, int n);

