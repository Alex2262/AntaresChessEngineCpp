
#ifndef ANTARESCHESSENGINE_CONSTANTS_H
#define ANTARESCHESSENGINE_CONSTANTS_H

#include <cstdint>

#define N_TUNING_PARAMETERS         16
#define FAIL_HIGH_STATS_COUNT       100
#define ALPHA_RAISE_STATS_COUNT     100

#define BENCH_DEPTH                 14

#define START_FEN "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - "
#define KIWIPETE_FEN "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - "

#define NO_PIECE_INDEX      -1

#define WHITE_COLOR         0
#define BLACK_COLOR         1

#define NO_HASH_ENTRY       0
#define USE_HASH_MOVE       1
#define RETURN_HASH_SCORE   2

#define TIME_INF            86400000
#define SCORE_INF           1000000
#define NO_EVALUATION       500000
#define MATE_SCORE          100000
#define MATE_BOUND          99000
#define NO_MOVE             0

#define MAX_AB_DEPTH        64
#define TOTAL_MAX_DEPTH     256
#define MAX_TT_SIZE         2666666

#define HASH_FLAG_EXACT     0
#define HASH_FLAG_ALPHA     1
#define HASH_FLAG_BETA      2

#define STARTING_WINDOW     26
#define MINIMUM_ASP_DEPTH   6

#define WHITE_PAWN          0
#define WHITE_KNIGHT        1
#define WHITE_BISHOP        2
#define WHITE_ROOK          3
#define WHITE_QUEEN         4
#define WHITE_KING          5

#define BLACK_PAWN          6
#define BLACK_KNIGHT        7
#define BLACK_BISHOP        8
#define BLACK_ROOK          9
#define BLACK_QUEEN         10
#define BLACK_KING          11

#define EMPTY               12
#define PADDING             13

#define MOVE_TYPE_NORMAL    0
#define MOVE_TYPE_EP        1
#define MOVE_TYPE_CASTLE    2
#define MOVE_TYPE_PROMOTION 3

#define A1      91
#define A8      21
#define H1      98
#define H8      28

#define E1      95
#define E8      25
#define C1      93
#define C8      23
#define G1      97
#define G8      27

enum NodeType {
    Exact_Node,
    Lower_Node,
    Upper_Node
};

typedef uint16_t PIECE_TYPE;
typedef int16_t SQUARE_TYPE;
typedef int16_t PLY_TYPE;
typedef int32_t SCORE_TYPE;
typedef uint32_t MOVE_TYPE;
typedef uint64_t HASH_TYPE;


constexpr int SQUARE_COLOR[64] = {
        0, 1, 0, 1, 0, 1, 0, 1,
        1, 0, 1, 0, 1, 0, 1, 0,
        0, 1, 0, 1, 0, 1, 0, 1,
        1, 0, 1, 0, 1, 0, 1, 0,
        0, 1, 0, 1, 0, 1, 0, 1,
        1, 0, 1, 0, 1, 0, 1, 0,
        0, 1, 0, 1, 0, 1, 0, 1,
        1, 0, 1, 0, 1, 0, 1, 0
};


constexpr SQUARE_TYPE STANDARD_TO_MAILBOX[64] = {
        21, 22, 23, 24, 25, 26, 27, 28,
        31, 32, 33, 34, 35, 36, 37, 38,
        41, 42, 43, 44, 45, 46, 47, 48,
        51, 52, 53, 54, 55, 56, 57, 58,
        61, 62, 63, 64, 65, 66, 67, 68,
        71, 72, 73, 74, 75, 76, 77, 78,
        81, 82, 83, 84, 85, 86, 87, 88,
        91, 92, 93, 94, 95, 96, 97, 98
};

constexpr SQUARE_TYPE MAILBOX_TO_STANDARD[120] = {
        99, 99, 99, 99, 99, 99, 99, 99, 99, 99,
        99, 99, 99, 99, 99, 99, 99, 99, 99, 99,
        99,  0,  1,  2,  3,  4,  5,  6,  7, 99,
        99,  8,  9, 10, 11, 12, 13, 14, 15, 99,
        99, 16, 17, 18, 19, 20, 21, 22, 23, 99,
        99, 24, 25, 26, 27, 28, 29, 30, 31, 99,
        99, 32, 33, 34, 35, 36, 37, 38, 39, 99,
        99, 40, 41, 42, 43, 44, 45, 46, 47, 99,
        99, 48, 49, 50, 51, 52, 53, 54, 55, 99,
        99, 56, 57, 58, 59, 60, 61, 62, 63, 99,
        99, 99, 99, 99, 99, 99, 99, 99, 99, 99,
        99, 99, 99, 99, 99, 99, 99, 99, 99, 99
};

constexpr short WHITE_INCREMENTS[6][8] = {
        {-11,  -9, -10, -20,   0,   0,   0,   0},
        {-21, -19,  -8,  12,  21,  19,   8, -12},
        {-11,  11,   9,  -9,   0,   0,   0,   0},
        {-10,   1,  10,  -1,   0,   0,   0,   0},
        {-11,  11,   9,  -9, -10,   1,  10,  -1},
        {-11, -10,  -9,   1,  11,  10,   9,  -1}
};

constexpr short BLACK_INCREMENTS[6][8] = {
        { 11,   9,  10,  20,   0,   0,   0,   0},
        {-21, -19,  -8,  12,  21,  19,   8, -12},
        {-11,  11,   9,  -9,   0,   0,   0,   0},
        {-10,   1,  10,  -1,   0,   0,   0,   0},
        {-11,  11,   9,  -9, -10,   1,  10,  -1},
        {-11, -10,  -9,   1,  11,  10,   9,  -1}
};

constexpr short WHITE_ATK_INCREMENTS[6][8] = {
        {-11,  -9,   0,   0,   0,   0,   0,   0},
        {-21, -19,  -8,  12,  21,  19,   8, -12},
        {-11,  11,   9,  -9,   0,   0,   0,   0},
        {-10,   1,  10,  -1,   0,   0,   0,   0},
        {-11,  11,   9,  -9, -10,   1,  10,  -1},
        {-11, -10,  -9,   1,  11,  10,   9,  -1}
};

constexpr short BLACK_ATK_INCREMENTS[6][8] = {
        { 11,   9,   0,   0,   0,   0,   0,   0},
        {-21, -19,  -8,  12,  21,  19,   8, -12},
        {-11,  11,   9,  -9,   0,   0,   0,   0},
        {-10,   1,  10,  -1,   0,   0,   0,   0},
        {-11,  11,   9,  -9, -10,   1,  10,  -1},
        {-11, -10,  -9,   1,  11,  10,   9,  -1}
};

constexpr char PIECE_MATCHER[12] = {'P', 'N', 'B', 'R', 'Q', 'K', 'p', 'n', 'b', 'r', 'q', 'k'};
constexpr int GAME_PHASE_SCORES[6] = {0, 1, 1, 2, 4, 0};

constexpr int MVV_LVA_VALUES[6] = {  87, 390, 429, 561,1234,   0};

constexpr SCORE_TYPE PIECE_VALUES_MID[6] = {  87, 390, 429, 561,1234,   0};

constexpr SCORE_TYPE PIECE_VALUES_END[6] = { 159, 377, 413, 741,1413,   0};

constexpr SCORE_TYPE PAWN_PST_MID[64] = {
        0,   0,   0,   0,   0,   0,   0,   0,
        173, 219, 142, 190, 157, 228,  36,   8,
        -31,  15,  55,  65,  87,  88,  35,  -7,
        -27,  12,  10,  29,  36,  37,  30, -16,
        -34,  -3,  -4,  12,  17,  12,  24, -16,
        -37, -10,  -6,  -7,   8,   5,  40,  -5,
        -43, -13, -22, -29, -16,  23,  47, -13,
        0,   0,   0,   0,   0,   0,   0,   0
};

constexpr SCORE_TYPE PAWN_PST_END[64] = {
        0,   0,   0,   0,   0,   0,   0,   0,
        217, 203, 192, 135, 148, 146, 217, 241,
        134, 133, 102,  78,  63,  53, 101, 109,
        38,  22,   9, -13, -20, -10,   8,  15,
        12,   6, -16, -21, -23, -25, -10, -13,
        -1,   6, -15,  -7,  -3, -14, -11, -21,
        16,  12,  13,  11,  17,   1,  -8, -24,
        0,   0,   0,   0,   0,   0,   0,   0
};

constexpr SCORE_TYPE KNIGHT_PST_MID[64] = {
        -218, -95, -46,  26, 156,-116,  -4,-121,
        6, -18,  94,  75,  87, 123,  22,  29,
        -15,  70,  86, 117, 154, 177, 125,  89,
        33,  45,  64,  86,  64,  94,  54,  58,
        12,  17,  41,  42,  55,  51,  69,  24,
        -1,  15,  36,  35,  45,  41,  44,   7,
        -26, -17,  14,  22,  19,  32,  12,  25,
        -133,  -6, -38, -24,  -5,   0,  -4, -47
};

constexpr SCORE_TYPE KNIGHT_PST_END[64] = {
        -24,  13,  49,  16,   5,  41, -22, -89,
        4,  42,  29,  62,  47,  15,  27,  -3,
        20,  38,  72,  78,  49,  54,  27,  -7,
        29,  70,  86,  94,  97,  91,  73,  40,
        37,  52,  88, 100,  88,  94,  60,  41,
        -14,  41,  45,  72,  65,  43,  27,  26,
        -3,  25,  16,  50,  59,  13,  21, -16,
        20, -17,  24,  32,  21,  23,   3, -11
};

constexpr SCORE_TYPE BISHOP_PST_MID[64] = {
        -39,  20, -90, -38, -47, -47,  11,  29,
        -10,  43,  24,  15,  49,  91,  46,  -7,
        13,  52,  85,  69,  84,  99,  86,  35,
        5,  33,  42,  84,  66,  76,  34,  20,
        12,  32,  31,  45,  63,  28,  29,  40,
        19,  31,  31,  32,  25,  33,  36,  38,
        13,  32,  40,  14,  21,  21,  47,  29,
        29,  11,  -4, -10, -18,  -4, -14,   0
};

constexpr SCORE_TYPE BISHOP_PST_END[64] = {
        30,  31,  50,  48,  52,  39,  31,  17,
        39,  41,  55,  47,  50,  31,  40,  31,
        47,  46,  48,  53,  41,  60,  50,  53,
        42,  66,  66,  65,  71,  54,  72,  54,
        35,  53,  68,  73,  61,  63,  44,  19,
        24,  43,  54,  54,  67,  52,  32,  24,
        22,  17,  31,  45,  40,  20,  43,   6,
        -7,  25,  32,  34,  33,  31,  36,   8
};

constexpr SCORE_TYPE ROOK_PST_MID[64] = {
        87,  91,  81,  92, 134, 107,  60, 168,
        51,  44,  83,  97, 102, 148,  73,  96,
        -1,  37,  58,  64,  78, 118, 139,  76,
        -8,   3,  31,  45,  27,  51,  13,  20,
        -39, -36, -16,  -7,  -9,   0,  18,   3,
        -50, -25, -26, -23, -19, -20,  12, -20,
        -60, -20, -25, -27, -17,  -7,   6, -88,
        -22, -16,  -7,  -2,  -7, -12, -16, -19
};

constexpr SCORE_TYPE ROOK_PST_END[64] = {
        65,  66,  69,  69,  54,  56,  65,  44,
        69,  77,  70,  72,  59,  35,  55,  48,
        71,  63,  63,  62,  53,  44,  31,  37,
        63,  63,  68,  63,  68,  55,  55,  52,
        60,  69,  66,  68,  61,  46,  44,  32,
        41,  46,  47,  52,  51,  41,  36,  25,
        40,  37,  50,  50,  43,  34,  27,  53,
        46,  54,  57,  53,  51,  46,  52,  25
};

constexpr SCORE_TYPE QUEEN_PST_MID[64] = {
        -31, -24,  35,  56, 117, 143,  83,  60,
        -11, -50, -18, -20, -12,  88,  30,  43,
        -19, -12,   5, -10,  13, 114, 121,  44,
        -18, -26, -15, -21, -15,  19,   7,   1,
        -22, -13, -14, -23, -12,  -4,  11,   1,
        -25,   5, -14, -11, -12,  -5,  16,  -8,
        -34,  -4,   7,  -9,  -1,  13,   8, -25,
        -6, -32, -15,  -4, -17, -28,  -7, -42
};

constexpr SCORE_TYPE QUEEN_PST_END[64] = {
        25,  82,  67,  52,  46,  21,  39,  44,
        13,  60,  86,  95, 119,  74,  85,  60,
        6,  25,  41, 111, 120,  64,  42, 107,
        -8,  51,  63,  98, 127, 109, 115, 130,
        4,  48,  45,  97,  79,  75,  59,  53,
        4, -46,  37,  17,  23,  28,   2,  22,
        -21, -39, -52,  -8, -26, -72, -65, -17,
        -33, -21, -33,  19, -34, -57, -57, -55
};

constexpr SCORE_TYPE KING_PST_MID[64] = {
        -1, 252, 176, 141,-182, -83,  64,  12,
        201,  54,  76,  97,  75,  58, -48,-177,
        53,  95,  73,  -2,  59, 115, 123, -44,
        -14,  -5,   4, -26, -67, -11,  -5,-123,
        -65,  58, -36, -91, -66, -73, -46,-103,
        -27, -43, -79, -92, -70, -72, -17, -43,
        -2,  -9, -44,-130, -92, -71,   7,  24,
        6,  52,   3,-130, -34, -92,  20,  27
};

constexpr SCORE_TYPE KING_PST_END[64] = {
        -96, -70, -45, -44,  15,  43,   7, -16,
        -46,  35,  26,   9,  21,  43,  63,  47,
        22,  35,  42,  35,  27,  55,  53,  33,
        10,  46,  51,  60,  62,  60,  49,  32,
        -15,  -1,  46,  66,  65,  56,  35,  11,
        -13,  15,  39,  52,  50,  41,  14,  -4,
        -20,  -8,  20,  37,  35,  27,  -7, -34,
        -94, -57, -28,   3, -36,   0, -34, -86
};


#endif //ANTARESCHESSENGINE_CONSTANTS_H