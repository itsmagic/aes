import math

sbox = [0x63, 0x7c, 0x77, 0x7b, 0xf2, 0x6b, 0x6f, 0xc5, 0x30, 0x01, 0x67, 0x2b, 0xfe, 0xd7, 0xab, 0x76,
        0xca, 0x82, 0xc9, 0x7d, 0xfa, 0x59, 0x47, 0xf0, 0xad, 0xd4, 0xa2, 0xaf, 0x9c, 0xa4, 0x72, 0xc0,
        0xb7, 0xfd, 0x93, 0x26, 0x36, 0x3f, 0xf7, 0xcc, 0x34, 0xa5, 0xe5, 0xf1, 0x71, 0xd8, 0x31, 0x15,
        0x04, 0xc7, 0x23, 0xc3, 0x18, 0x96, 0x05, 0x9a, 0x07, 0x12, 0x80, 0xe2, 0xeb, 0x27, 0xb2, 0x75,
        0x09, 0x83, 0x2c, 0x1a, 0x1b, 0x6e, 0x5a, 0xa0, 0x52, 0x3b, 0xd6, 0xb3, 0x29, 0xe3, 0x2f, 0x84,
        0x53, 0xd1, 0x00, 0xed, 0x20, 0xfc, 0xb1, 0x5b, 0x6a, 0xcb, 0xbe, 0x39, 0x4a, 0x4c, 0x58, 0xcf,
        0xd0, 0xef, 0xaa, 0xfb, 0x43, 0x4d, 0x33, 0x85, 0x45, 0xf9, 0x02, 0x7f, 0x50, 0x3c, 0x9f, 0xa8,
        0x51, 0xa3, 0x40, 0x8f, 0x92, 0x9d, 0x38, 0xf5, 0xbc, 0xb6, 0xda, 0x21, 0x10, 0xff, 0xf3, 0xd2,
        0xcd, 0x0c, 0x13, 0xec, 0x5f, 0x97, 0x44, 0x17, 0xc4, 0xa7, 0x7e, 0x3d, 0x64, 0x5d, 0x19, 0x73,
        0x60, 0x81, 0x4f, 0xdc, 0x22, 0x2a, 0x90, 0x88, 0x46, 0xee, 0xb8, 0x14, 0xde, 0x5e, 0x0b, 0xdb,
        0xe0, 0x32, 0x3a, 0x0a, 0x49, 0x06, 0x24, 0x5c, 0xc2, 0xd3, 0xac, 0x62, 0x91, 0x95, 0xe4, 0x79,
        0xe7, 0xc8, 0x37, 0x6d, 0x8d, 0xd5, 0x4e, 0xa9, 0x6c, 0x56, 0xf4, 0xea, 0x65, 0x7a, 0xae, 0x08,
        0xba, 0x78, 0x25, 0x2e, 0x1c, 0xa6, 0xb4, 0xc6, 0xe8, 0xdd, 0x74, 0x1f, 0x4b, 0xbd, 0x8b, 0x8a,
        0x70, 0x3e, 0xb5, 0x66, 0x48, 0x03, 0xf6, 0x0e, 0x61, 0x35, 0x57, 0xb9, 0x86, 0xc1, 0x1d, 0x9e,
        0xe1, 0xf8, 0x98, 0x11, 0x69, 0xd9, 0x8e, 0x94, 0x9b, 0x1e, 0x87, 0xe9, 0xce, 0x55, 0x28, 0xdf,
        0x8c, 0xa1, 0x89, 0x0d, 0xbf, 0xe6, 0x42, 0x68, 0x41, 0x99, 0x2d, 0x0f, 0xb0, 0x54, 0xbb, 0x16]

rcon = [[0x01, 0x02, 0x04, 0x08, 0x10, 0x20, 0x40, 0x80, 0x1b, 0x36],
        [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00],
        [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00],
        [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]]


def gf256_mul(a, b):
    p = 0
    for bit in range(8):
        if b & 0x01:
            p ^= a
        carry = a & 0x80  # detect if x^8 term is about to be generated
        a = (a << 1) & 0xFF
        if carry:
            a ^= 0x1B  # replace x^8 with x^4 + x^3 + x + 1
        b >>= 1
    return p


def gf256_mul_generate():
    gf_mul_table = [[] for i in range(256)]
    for i in range(256):
        for j in range(256):
            mul = gf256_mul(i, j)
            gf_mul_table[i].append(mul)
    return gf_mul_table


def sub_bytes(state):
    for i in range(len(state)):
        for j in range(len(state[i])):
            state[i][j] = sbox[state[i][j]]
    return state


def shift_rows(state):
    for i in range(1, len(state)):
        state[i] = state[i][i::] + state[i][0:i]
    return state


gf_mul = gf256_mul_generate()


def mix_column(state):
    temp = [[0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0]]
    for i in range(len(state)):
        temp[0][i] = gf_mul[0x02][state[0][i]] ^ gf_mul[0x03][state[1][i]] ^ state[2][i] ^ state[3][i]
        temp[1][i] = state[0][i] ^ gf_mul[0x02][state[1][i]] ^ gf_mul[0x03][state[2][i]] ^ state[3][i]
        temp[2][i] = state[0][i] ^ state[1][i] ^ gf_mul[0x02][state[2][i]] ^ gf_mul[0x03][state[3][i]]
        temp[3][i] = gf_mul[0x03][state[0][i]] ^ state[1][i] ^ state[2][i] ^ gf_mul[0x02][state[3][i]]
    return temp


def add_round_key(state, key, round):
    for i in range(len(state)):
        for j in range(len(state[i])):
            state[i][j] ^= key[i][4*round + j]
    return state




def key_expansion(key, nb, nk, nr):
    # nb - number of state column (4)
    # nk - length of key in 32 bit word (4, 6, 8)
    # nr - number of rounds (10, 12, 14)

    key_schedule = [[] for i in range(nb)]
    for r in range(nb):
        for c in range(nk):
            key_schedule[r].append(key[r + 4 * c])

    for c in range(nk, nb * (nr + 1)):  # col - column number
        if c % nk == 0:
            tmp = []
            tmp.append(sbox[key_schedule[1][c - 1]])
            tmp.append(sbox[key_schedule[2][c - 1]])
            tmp.append(sbox[key_schedule[3][c - 1]])
            tmp.append(sbox[key_schedule[0][c - 1]])

            for r in range(nb):
                k = (key_schedule[r][c - nk]) ^ (tmp[r]) ^ (rcon[r][int(c / nk - 1)])
                key_schedule[r].append(k)

        else:
            if (nk > 6) and (c % nk == 4):
                for r in range(nb):
                    k = key_schedule[r][c - nk] ^ sbox[key_schedule[r][c - 1]]
                    key_schedule[r].append(k)
            else:
                for r in range(nb):
                    k = key_schedule[r][c - nk] ^ key_schedule[r][c - 1]
                    key_schedule[r].append(k)

    for c in range(nb * (nr + 1)):
        tmp = 0
        for r in range(nb):
            tmp += key_schedule[r][c] << (8 * (nb - 1 - r))
        print format(c, '2d'), ' | ', format(tmp, '08X')

    print 'Key rounding finished.'
    return key_schedule

def print_state(state, round):
    print 'round ', round
    for i in range(len(state)):
        print format(state[i][0], '02X'), ' ', format(state[i][1], '02X'), ' ', format(state[i][2], '02X'), ' ', format(state[i][3], '02X')
    return 0


def encrypt(data, key):
    encrypt_data = []        # data after encryption
    nb = 4                   # nb - number of state column (4), defining by standard
    if len(key) == 16:       # AES128
        nk = 4               # nk - length of key in 32 bit word (4, 6, 8), depends from key length
        nr = 10              # nr - number of rounds (10, 12, 14), depends from AES type
    elif len(key) == 24:     # AES192
        nk = 6
        nr = 12
    elif len(key) == 32:     # AES256
        nk = 8
        nr = 14
    else:
        print 'Wrong key length'
        return 0

    # generate key schedule for all rounds
    key_schedule = key_expansion(key, nb, nk, nr)

    if len(data)%16 != 0:
        print 'Wrong data size. Length of data must be multiple by 16'
        return 0

    for n in range(len(data)/16):
        state = [[] for i in range(nb)]
        for i in range(nb):
            for j in range(nb):
                state[i].append(data[n*16 + i + 4 * j])

        # round 0
        state = add_round_key(state, key_schedule, 0)
        print_state(state, 0)

        # round 1 to nr-1
        for r in range(1, nr):
            state = sub_bytes(state)
            state = shift_rows(state)
            state = mix_column(state)
            state = add_round_key(state, key_schedule, r)
            print_state(state, r)

        # round nr
        state = sub_bytes(state)
        state = shift_rows(state)
        state = add_round_key(state, key_schedule, nr)
        print_state(state, nr)

        for i in range(nb):
            for j in range(nb):
                encrypt_data.append(state[j][i])
    return encrypt_data


def encrypt_128aes():
    # AES128 example from standart fips-197
    data = [0x00, 0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88, 0x99, 0xaa, 0xbb, 0xcc, 0xdd, 0xee, 0xff]
    key  = [0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f]

    enc_data_aes128 = [0x69, 0xc4, 0xe0, 0xd8, 0x6a, 0x7b, 0x04, 0x30, 0xd8, 0xcd, 0xb7, 0x80, 0x70, 0xb4, 0xc5, 0x5a]

    enc_data = encrypt(data, key)

    # compare results with reference of standard
    for i in range(len(enc_data)):
        print (format(enc_data[i], '02X')),
    print   # new line
    for i in range(len(enc_data)):
        print (format(enc_data_aes128[i], '02X')),
    print   # new line
    for i in range(len(enc_data)):
        if enc_data_aes128[i] != enc_data[i]:
            print 'Error occure!'
    print 'Data encrypted ok!'

    return 0

    encrypt(data, key)

    return 0


def encrypt_192aes():

    # AES192 example from standart fips-197
    data = [0x00, 0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88, 0x99, 0xaa, 0xbb, 0xcc, 0xdd, 0xee, 0xff]
    key  = [0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f,
            0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17]

    enc_data_aes192 = [0xdd, 0xa9, 0x7c, 0xa4, 0x86, 0x4c, 0xdf, 0xe0, 0x6e, 0xaf, 0x70, 0xa0, 0xec, 0x0d, 0x71, 0x91]

    enc_data = encrypt(data, key)

    # compare results with reference of standard
    for i in range(len(enc_data)):
        print (format(enc_data[i], '02X')),
    print   # new line
    for i in range(len(enc_data)):
        print (format(enc_data_aes192[i], '02X')),
    print   # new line
    for i in range(len(enc_data)):
        if enc_data_aes192[i] != enc_data[i]:
            print 'Error occure!'
    print 'Data encrypted ok!'

    return 0


def encrypt_256aes():

    # AES256 example from standart fips-197
    data = [0x00, 0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88, 0x99, 0xaa, 0xbb, 0xcc, 0xdd, 0xee, 0xff]
    key  = [0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f,
            0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f]

    enc_data_aes256 = [0x8e, 0xa2, 0xb7, 0xca, 0x51, 0x67, 0x45, 0xbf, 0xea, 0xfc, 0x49, 0x90, 0x4b, 0x49, 0x60, 0x89]

    enc_data = encrypt(data, key)

    # compare results with reference of standard
    for i in range(len(enc_data)):
        print (format(enc_data[i], '02X')),
    print   # new line
    for i in range(len(enc_data)):
        print (format(enc_data_aes256[i], '02X')),
    print   # new line
    for i in range(len(enc_data)):
        if enc_data_aes256[i] != enc_data[i]:
            print 'Error occure!'
    print 'Data encrypted ok!'

    return 0


encrypt_128aes()
# encrypt_192aes()
# encrypt_256aes()


#    for i in range(len(state)):
 #       print format(state[i][0], '02X'), ' ', format(state[i][1], '02X'), ' ', format(state[i][2], '02X'), ' ', format(state[i][3], '02X')

# print "gf256_mul(0x53, 0xCA) = 0x", format(gf256_mul(0x53, 0xCA), '02X')
# print "gf256_mul(0x69, 0x96) = 0x", format(gf256_mul(0x57, 0x83), '02X')
# print "gf256_mul(0x69, 0x96) = 0x", format(gf256_mul(0x57, 0x13), '02X')
# statet = [[0xd4, 0xe0, 0xb8, 0x1e], [0xbf, 0xb4, 0x41, 0x27], [0x5d, 0x52, 0x11, 0x98], [ 0x30, 0xae, 0xf1, 0xe5]]
# statenew = mix_column(statet)
# for i in range(len(state)):
#     print format(statenew[i][0], '02X'), ' ', format(statenew[i][1], '02X'), ' ', format(statenew[i][2], '02X'), ' ', format(statenew[i][3], '02X')

