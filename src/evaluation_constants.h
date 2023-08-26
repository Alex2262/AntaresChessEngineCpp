//
// Created by Alexander Tian on 6/26/23.
//

#ifndef ALTAIRCHESSENGINE_EVALUATION_CONSTANTS_H
#define ALTAIRCHESSENGINE_EVALUATION_CONSTANTS_H

#include "evaluation.h"
#include "types.h"

constexpr int GAME_PHASE_SCORES[6] = {0, 1, 1, 2, 4, 0};

constexpr int MVV_LVA_VALUES[6] = {  87, 390, 429, 561,1234,   0};

constexpr SCORE_TYPE CANONICAL_PIECE_VALUES[6] = {100, 310, 340, 500, 900, 0};

constexpr SCORE_TYPE MAX_MINOR_PIECE_VALUE = CANONICAL_PIECE_VALUES[BISHOP];
constexpr SCORE_TYPE MIN_MINOR_PIECE_VALUE = CANONICAL_PIECE_VALUES[KNIGHT];



constexpr SCORE_TYPE PIECE_VALUES[6] = {S(   120,   163), S(   491,   444), S(   490,   461), S(   671,   799), S(  1429,  1410), S(     0,     0)};

constexpr SCORE_TYPE PIECE_SQUARE_TABLES[6][64] = {
        {
                S(     0,     0), S(     0,     0), S(     0,     0), S(     0,     0), S(     0,     0), S(     0,     0), S(     0,     0), S(     0,     0),
                S(   127,    20), S(    77,    51), S(    47,    59), S(    92,    41), S(    64,    59), S(   137,    58), S(   -37,   104), S(    66,    73),
                S(   -22,    23), S(   -22,    25), S(     3,    13), S(    14,     1), S(    28,     4), S(    -4,    24), S(   -22,    34), S(   -28,    39),
                S(   -31,     1), S(   -23,   -10), S(   -18,   -19), S(    -2,   -37), S(    -2,   -28), S(     3,   -24), S(   -11,   -12), S(   -39,    -1),
                S(   -33,    -7), S(   -35,   -10), S(   -15,   -28), S(    -2,   -35), S(    -5,   -32), S(    -1,   -28), S(   -24,   -16), S(   -42,   -11),
                S(   -33,   -20), S(   -25,   -24), S(   -13,   -25), S(    -4,   -31), S(     2,   -19), S(     6,   -28), S(     6,   -26), S(   -23,   -25),
                S(   -34,   -12), S(   -31,   -21), S(   -33,   -11), S(   -19,   -18), S(   -22,    -2), S(    10,   -30), S(     3,   -22), S(   -26,   -18),
                S(     0,     0), S(     0,     0), S(     0,     0), S(     0,     0), S(     0,     0), S(     0,     0), S(     0,     0), S(     0,     0)
        },
        {
                S(  -205,   -42), S(  -119,    -5), S(  -101,    20), S(     0,   -13), S(    36,   -19), S(  -150,    13), S(   -25,   -55), S(  -139,   -76),
                S(   -18,   -22), S(   -36,     4), S(    30,   -18), S(    25,    22), S(    31,     8), S(    43,   -25), S(   -40,    -4), S(    -5,   -28),
                S(   -22,   -19), S(    25,    -3), S(    50,    29), S(    70,    29), S(    85,    12), S(    97,    21), S(    49,   -10), S(    37,   -37),
                S(    28,    -1), S(    44,    15), S(    49,    40), S(    64,    49), S(    50,    52), S(    61,    41), S(    33,    20), S(    29,     5),
                S(    10,    -3), S(    27,     4), S(    37,    37), S(    39,    43), S(    39,    37), S(    27,    35), S(    33,    17), S(    -2,     9),
                S(    -7,   -31), S(    11,   -11), S(    21,     0), S(    25,    25), S(    24,    22), S(    15,    -6), S(     5,   -15), S(    -9,   -19),
                S(   -32,   -20), S(   -24,     2), S(    -3,    -9), S(     9,     0), S(    -1,    13), S(    -6,   -12), S(   -24,    -8), S(   -14,   -19),
                S(  -102,   -29), S(   -10,   -25), S(   -27,    -3), S(   -11,     9), S(    -7,    -4), S(   -14,    -9), S(   -19,   -18), S(   -86,   -11)
        },
        {
                S(   -21,    -1), S(     5,     2), S(   -85,    11), S(   -64,    11), S(  -106,    17), S(   -98,     3), S(    45,   -15), S(    20,    -6),
                S(   -20,    11), S(     3,     5), S(     5,     6), S(   -20,     4), S(   -21,    13), S(     9,     7), S(   -16,     8), S(   -29,     6),
                S(    26,    -4), S(    29,     7), S(    58,     3), S(    20,     9), S(    34,     1), S(    31,    28), S(    27,    16), S(    15,     9),
                S(    -7,    -2), S(    26,    11), S(     4,    19), S(    47,    15), S(    20,    23), S(     5,    20), S(    -5,    20), S(   -15,     8),
                S(     3,   -13), S(     8,     3), S(    13,    12), S(    26,    15), S(    28,     6), S(    -3,    11), S(    -7,     2), S(    -5,   -13),
                S(     2,   -22), S(    15,    -9), S(    11,     0), S(     5,     4), S(     2,    15), S(    12,    -4), S(     5,    -2), S(     7,   -18),
                S(     2,   -24), S(    12,   -32), S(     7,   -15), S(    -6,   -10), S(    -2,    -6), S(    -3,   -17), S(    23,   -19), S(     6,   -39),
                S(    13,   -39), S(     4,   -25), S(   -15,     1), S(   -16,    -6), S(   -23,    -3), S(   -20,     5), S(   -12,    10), S(   -16,   -28)
        },
        {
                S(    36,     9), S(    31,    13), S(   -16,    28), S(   -32,    33), S(     8,    18), S(   -19,    23), S(     2,    15), S(    45,    10),
                S(    17,     6), S(     2,    12), S(    19,    12), S(    33,    12), S(     6,    18), S(    31,    -4), S(     0,     8), S(    16,     3),
                S(     0,     4), S(    29,    -5), S(    23,     0), S(    30,    -4), S(    26,    -8), S(    54,    -3), S(    82,   -18), S(    28,   -11),
                S(     1,     2), S(     8,    -4), S(    12,     6), S(    26,    -2), S(     9,     3), S(    17,     3), S(    -1,    -1), S(     0,     2),
                S(   -19,    -1), S(   -12,     4), S(   -26,    11), S(   -11,     7), S(   -24,     9), S(   -14,     3), S(   -12,     3), S(   -17,    -3),
                S(   -23,   -14), S(   -13,    -9), S(   -21,    -4), S(   -18,    -1), S(   -23,    -1), S(   -15,    -7), S(     4,    -9), S(   -26,   -11),
                S(   -38,    -8), S(   -15,   -13), S(   -19,    -6), S(    -9,   -11), S(   -19,    -2), S(   -10,   -12), S(   -11,   -18), S(   -81,     8),
                S(     2,   -17), S(    -7,    -7), S(    -4,    -7), S(     2,   -14), S(    -7,   -15), S(    -4,    -4), S(   -22,    -4), S(   -10,   -27)
        },
        {
                S(   -32,   -16), S(   -22,     9), S(   -21,     9), S(   -15,   -17), S(    -1,     5), S(     6,     6), S(    58,   -27), S(    23,   -10),
                S(    -5,   -15), S(   -56,    30), S(   -34,    23), S(   -83,    53), S(  -106,    75), S(   -35,    35), S(   -19,    26), S(   -11,    38),
                S(    -5,   -18), S(   -13,    -9), S(   -22,     4), S(   -46,    33), S(   -56,    49), S(    -6,    43), S(    10,    25), S(   -30,    80),
                S(     0,   -10), S(     1,     2), S(   -20,    20), S(   -24,    30), S(   -39,    54), S(   -29,    54), S(   -19,    49), S(    -8,    35),
                S(    18,   -25), S(    20,    11), S(    10,     9), S(    -3,    46), S(   -15,    38), S(    -5,    27), S(    11,     7), S(     5,     8),
                S(    23,   -31), S(    39,   -37), S(    27,     2), S(    13,     5), S(     4,     9), S(     5,    22), S(    20,    -3), S(     9,     6),
                S(    22,   -22), S(    40,   -41), S(    42,   -58), S(    28,    -5), S(    28,   -32), S(    25,   -48), S(    33,   -76), S(    12,   -46),
                S(    55,   -58), S(    35,   -54), S(    38,   -44), S(    41,   -47), S(    23,   -33), S(    13,   -49), S(    34,   -81), S(     8,   -63)
        },
        {
                S(  -261,   -24), S(    58,   -18), S(   169,   -14), S(    15,   -22), S(  -213,    30), S(   -68,    45), S(    39,    19), S(   -19,   -11),
                S(    59,   -11), S(   138,    16), S(   154,    20), S(    39,    24), S(    72,    14), S(   160,    21), S(    48,    44), S(  -134,    34),
                S(    51,    10), S(   136,    16), S(   195,    13), S(    38,    10), S(   133,    -7), S(   232,    23), S(   237,    17), S(     0,    19),
                S(   -22,    13), S(    94,    10), S(   127,    13), S(    75,    11), S(    46,    13), S(   162,    14), S(   128,    10), S(   -38,    17),
                S(   -38,    -9), S(   123,   -20), S(   119,     0), S(    38,    13), S(    97,     2), S(   132,     6), S(   130,    -6), S(   -46,     4),
                S(   -97,     5), S(   -12,     8), S(    34,     9), S(    66,     1), S(    82,    -2), S(    89,     7), S(    53,     6), S(   -30,     4),
                S(   -87,     3), S(   -58,     6), S(   -53,    15), S(   -68,    10), S(   -28,     5), S(   -33,    26), S(    -6,    11), S(   -22,    -5),
                S(   -94,   -57), S(   -54,   -26), S(   -95,     5), S(  -111,   -11), S(   -36,   -43), S(  -105,    19), S(   -20,   -10), S(   -24,   -61)
        }
};


constexpr SCORE_TYPE MOBILITY_VALUES[4][28] = {
        {S(   -32,   -77), S(   -17,   -26), S(    -9,    -7), S(    -5,    11), S(     1,    19), S(     4,    30), S(    11,    28), S(    16,    22), S(    30,     2), S(     0,     0), S(     0,     0), S(     0,     0), S(     0,     0), S(     0,     0), S(     0,     0), S(     0,     0), S(     0,     0), S(     0,     0), S(     0,     0), S(     0,     0), S(     0,     0), S(     0,     0), S(     0,     0), S(     0,     0), S(     0,     0), S(     0,     0), S(     0,     0), S(     0,     0)},
        {S(   -41,   -91), S(   -30,   -60), S(   -20,   -35), S(   -12,   -17), S(    -5,    -4), S(     0,    11), S(     0,    20), S(     1,    23), S(     2,    32), S(     7,    28), S(    11,    26), S(    35,    17), S(     1,    41), S(    49,     9), S(     0,     0), S(     0,     0), S(     0,     0), S(     0,     0), S(     0,     0), S(     0,     0), S(     0,     0), S(     0,     0), S(     0,     0), S(     0,     0), S(     0,     0), S(     0,     0), S(     0,     0), S(     0,     0)},
        {S(   -45,  -114), S(   -31,   -63), S(   -25,   -42), S(   -24,   -15), S(   -22,    -1), S(   -14,     6), S(    -9,    13), S(    -2,    15), S(     2,    23), S(     7,    28), S(    10,    34), S(    15,    38), S(    23,    38), S(    33,    31), S(    84,     9), S(     0,     0), S(     0,     0), S(     0,     0), S(     0,     0), S(     0,     0), S(     0,     0), S(     0,     0), S(     0,     0), S(     0,     0), S(     0,     0), S(     0,     0), S(     0,     0), S(     0,     0)},
        {S(   -22,  -440), S(   -27,  -180), S(   -24,  -159), S(   -19,   -72), S(   -21,   -47), S(   -18,   -41), S(   -15,   -16), S(   -15,    16), S(   -12,    23), S(    -7,    26), S(    -6,    40), S(    -3,    42), S(    -2,    51), S(     3,    50), S(    -3,    61), S(    -3,    71), S(    -7,    70), S(     5,    57), S(     3,    73), S(     8,    66), S(    27,    49), S(    29,    52), S(    17,    44), S(    38,    56), S(   -48,    64), S(    68,    18), S(    37,     3), S(    16,    24)}
};


constexpr SCORE_TYPE PASSED_PAWN_BONUSES[3][8] = {
        {S(     0,     0), S(    10,    28), S(   -70,    54), S(   -92,    83), S(   -54,    85), S(    13,   103), S(   112,   135), S(     0,     0)},
        {S(     0,     0), S(     0,     0), S(   -56,    54), S(   -86,    85), S(   -25,   107), S(    64,   162), S(   378,   163), S(     0,     0)},
        {S(     0,     0), S(     0,     0), S(   -34,    57), S(   -83,    59), S(    22,    78), S(   193,    61), S(   158,   150), S(     0,     0)}
};


constexpr SCORE_TYPE PASSED_PAWN_BLOCKERS[6][8] = {
        {S(     0,     0), S(     0,     0), S(     0,     0), S(     0,     0), S(     0,     0), S(     0,     0), S(     0,     0), S(     0,     0)},
        {S(     0,     0), S(     0,     0), S(    17,   -34), S(   -21,     0), S(   -11,   -26), S(   -16,   -68), S(   -26,   -94), S(    93,  -178)},
        {S(     0,     0), S(     0,     0), S(    -3,   -22), S(    -1,   -43), S(    -8,   -44), S(     7,   -67), S(   -25,  -114), S(    16,  -221)},
        {S(     0,     0), S(     0,     0), S(     5,   -15), S(   -21,    16), S(     2,     1), S(    -5,    -3), S(    -4,   -47), S(   -24,   -93)},
        {S(     0,     0), S(     0,     0), S(     8,   -34), S(   -49,    24), S(    16,   -44), S(   -14,     7), S(     6,   -38), S(    -1,    87)},
        {S(     0,     0), S(     0,     0), S(    52,   -17), S(    73,    -3), S(   -49,    -4), S(   -33,   -11), S(  -107,   -47), S(   -60,  -107)}
};


constexpr SCORE_TYPE PASSED_PAWN_BLOCKERS_2[6][8] = {
        {S(     0,     0), S(     0,     0), S(     0,     0), S(     0,     0), S(     0,     0), S(     0,     0), S(     0,     0), S(     0,     0)},
        {S(     0,     0), S(     0,     0), S(     0,     0), S(    -7,    -2), S(    -7,   -13), S(     1,    -9), S(     1,   -20), S(    41,  -103)},
        {S(     0,     0), S(     0,     0), S(     0,     0), S(    -6,     0), S(    10,    -4), S(   -11,     2), S(    -3,   -19), S(    25,   -60)},
        {S(     0,     0), S(     0,     0), S(     0,     0), S(    16,   -25), S(    17,   -14), S(    -8,    14), S(    12,     5), S(    19,   -40)},
        {S(     0,     0), S(     0,     0), S(     0,     0), S(     0,    11), S(   -10,    -9), S(    16,   -35), S(    14,    -7), S(     4,    59)},
        {S(     0,     0), S(     0,     0), S(     0,     0), S(   170,   -25), S(    76,   -17), S(   -12,    -7), S(    16,   -23), S(    18,   -78)}
};


constexpr SCORE_TYPE PHALANX_PAWN_BONUSES[8] = {S(     0,     0), S(     3,   -15), S(    -3,    -6), S(    12,    12), S(    40,    69), S(   116,   118), S(   219,   216), S(     0,     0)};

constexpr SCORE_TYPE ISOLATED_PAWN_PENALTY = S(   -17,   -17);

constexpr SCORE_TYPE BISHOP_PAIR_BONUS = S(    25,    68);

constexpr SCORE_TYPE TEMPO_BONUS = S(    21,    22);

constexpr SCORE_TYPE SEMI_OPEN_FILE_VALUES[6] = {S(     0,     0), S(     0,     0), S(     0,     0), S(    18,    -2), S(     2,     9), S(   -17,     9)};

constexpr SCORE_TYPE OPEN_FILE_VALUES[6] = {S(     0,     0), S(     0,     0), S(     0,     0), S(    36,     0), S(    -5,    15), S(   -50,    -1)};

constexpr SCORE_TYPE PIECE_THREATS[6][6] = {
        {S(     9,   -18), S(    65,    45), S(    63,    82), S(    97,     4), S(    66,    27), S(   170,    50)},
        {S(    -9,    10), S(     0,     0), S(    26,    45), S(    50,    25), S(    43,    10), S(   100,    14)},
        {S(     0,    10), S(    27,    44), S(    38,    20), S(    45,    30), S(    50,    47), S(    53,    64)},
        {S(   -11,    20), S(    -1,    23), S(     5,    27), S(    -4,   -19), S(    41,    53), S(    99,    21)},
        {S(    -3,    11), S(     4,    -2), S(     2,    22), S(    -7,    26), S(    -4,     5), S(    66,    85)},
        {S(     1,    41), S(     5,    18), S(   -22,    32), S(    -5,    22), S(  -115,     3), S(     0,     0)}
};


constexpr SCORE_TYPE KING_RING_ATTACKS[2][6] = {
        {S(    43,   -12), S(    30,   -11), S(    45,    -6), S(    42,   -10), S(    38,     2), S(     0,     0)},
        {S(    23,    -8), S(    23,    -1), S(    24,    -3), S(    17,    -3), S(    20,     8), S(     0,     0)}
};


constexpr SCORE_TYPE TOTAL_KING_RING_ATTACKS[40] = {S(    59,     0), S(    35,    -5), S(    12,    -5), S(    -7,    -3), S(   -26,     0), S(   -38,     1), S(   -49,     4), S(   -55,     4), S(   -60,     7), S(   -66,     5), S(   -62,     5), S(   -65,     6), S(   -59,     4), S(   -63,     6), S(   -57,     8), S(   -56,     9), S(   -40,     7), S(   -42,     7), S(   -40,     6), S(    -9,    -7), S(   -36,     9), S(    -5,    -3), S(    23,     0), S(   -59,    33), S(    56,   -15), S(    64,   -10), S(   183,   -74), S(   130,   -34), S(    71,     5), S(    30,   -35), S(   363,  -180), S(   214,    29), S(   208,    11), S(   179,   104), S(   293,   141), S(   -29,   -49), S(    11,    42), S(   227,    69), S(    -8,    -9), S(   -20,     8)};

constexpr SCORE_TYPE KING_PAWN_SHIELD[5][8] = {
        {S(    24,   -14), S(    43,    -9), S(    38,   -10), S(     0,     0), S(     0,     0), S(     5,     8), S(    25,    -8), S(     8,   -23)},
        {S(    28,    -9), S(    31,    -6), S(     8,    -2), S(     0,     0), S(     0,     0), S(    -6,    -5), S(     6,    -7), S(    14,    -6)},
        {S(    24,    -1), S(   -15,     5), S(     0,    -3), S(     0,     0), S(     0,     0), S(    -3,    -8), S(    -7,    -3), S(     9,    -5)},
        {S(     4,    17), S(   -10,    23), S(   -14,    25), S(     0,     0), S(     0,     0), S(    18,    -7), S(   -25,    13), S(    10,     5)},
        {S(   -19,    -3), S(   -25,    -7), S(   -10,    -8), S(     0,     0), S(     0,     0), S(   -17,    -3), S(   -24,     6), S(   -24,     7)}
};


constexpr SCORE_TYPE KING_PAWN_STORM[6][8] = {
        {S(    59,    26), S(    54,    31), S(    52,   -10), S(     0,     0), S(     0,     0), S(    18,    53), S(    -1,    39), S(   144,    10)},
        {S(    -2,     7), S(   -66,    22), S(   -70,    23), S(     0,     0), S(     0,     0), S(   -46,    18), S(   -35,    22), S(   -14,    17)},
        {S(     8,    13), S(    21,     5), S(    -6,     6), S(     0,     0), S(     0,     0), S(    10,     3), S(    23,    -2), S(     9,     0)},
        {S(     1,     3), S(     8,     6), S(     5,     4), S(     0,     0), S(     0,     0), S(     2,     0), S(     4,     4), S(    -7,     2)},
        {S(     3,    -2), S(    24,   -17), S(    18,    -1), S(     0,     0), S(     0,     0), S(     3,   -11), S(    14,    -6), S(     5,    -7)},
        {S(    -7,    -1), S(   -19,     3), S(    10,     4), S(     0,     0), S(     0,     0), S(     1,    -3), S(   -11,    -1), S(    -8,    -9)}
};


constexpr SCORE_TYPE OPP_KING_TROPISM[6] = {S(     0,     0), S(    -2,    -2), S(     2,     1), S(    -3,     1), S(    -7,     0), S(     0,     0)};

constexpr SCORE_TYPE OUR_KING_TROPISM[6] = {S(     0,     0), S(    -4,     0), S(    -5,     1), S(    -2,     2), S(    -2,     4), S(     0,     0)};

constexpr SCORE_TYPE DOUBLED_PAWN_PENALTY = S(   -19,   -32);

constexpr SCORE_TYPE SQUARE_OF_THE_PAWN = S(   -24,    42);

constexpr SCORE_TYPE BACKWARDS_PAWN_PENALTY[2] = {S(    -8,    -3), S(   -20,   -23)};

constexpr SCORE_TYPE PASSED_OUR_DISTANCE[8] = {S(     0,     0), S(    -3,     0), S(     2,    -7), S(     8,   -15), S(     8,   -21), S(     4,   -21), S(    -4,   -20), S(     0,     0)};

constexpr SCORE_TYPE PASSED_OPP_DISTANCE[8] = {S(     0,     0), S(     0,    -3), S(    11,    -4), S(    13,     2), S(     9,    16), S(     5,    33), S(    -5,    42), S(     0,     0)};




#endif //ALTAIRCHESSENGINE_EVALUATION_CONSTANTS_H
