
#ifndef ANTARESCHESSENGINE_CONSTANTS_H
#define ANTARESCHESSENGINE_CONSTANTS_H

#include <cstdint>

#define START_FEN "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - "
#define KIWIPETE_FEN "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - "

#define SCORE_INF           1000000
#define MATE_SCORE          100000
#define NO_MOVE             0

#define TOTAL_MAX_DEPTH     256

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


#define TEMPO_BONUS     8


typedef uint16_t PIECE_TYPE;
typedef int16_t SQUARE_TYPE;
typedef int16_t PLY_TYPE;
typedef int32_t SCORE_TYPE;
typedef uint32_t MOVE_TYPE;
typedef uint64_t HASH_TYPE;


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
constexpr SCORE_TYPE PIECE_VALUES_MID[6] = {82, 326, 352, 486, 982, 0};
constexpr SCORE_TYPE PIECE_VALUES_END[6] = {96, 292, 304, 512, 936, 0};

constexpr SCORE_TYPE PST_MID[6][64] = {
        {    0,   0,   0,   0,   0,   0,   0,   0,
            80,  90,  95,  95,  95,  90,  80,  70,
            10,  15,  20,  30,  40,  20,  15,   5,
             3,   4,  15,  20,  25,  15,   4,  -2,
             0,   0,  10,  15,  17,   5,   0,  -4,
             2,   2,   0,   2,   4,  -5,  12,   0,
             0,   0,   3, -26, -26,  12,  15,  -5,
             0,   0,   0,   0,   0,   0,   0,   0},
        {  -70, -60, -30, -35,  -5, -30, -20, -70,
           -60,  -5,  40,  20,  20,  40,   5, -40,
           -30,  30,  30,  45,  45,  70,  10,  15,
             0,  10,  30,  50,  50,  60,  10,   5,
           -10,   0,  15,  40,  40,  15,   0, -30,
           -30,   5,  10,  20,  20,  10,  10, -30,
           -40, -20,   1,   5,   5,   1, -20, -40,
           -60, -40, -30, -30, -30, -20, -40, -40},
        {  -20, -15, -10, -10, -10, -10, -15, -20,
           -15,   0,   0,   5,  10,  20,   0, -15,
           -10,  20,   5,  45,  30,  45,   0, -10,
           -10,  15,   5,  45,  35,  35,  15, -10,
           -10,  12,  15,  15,  15,  15,  12, -10,
           -10,  10,  10,   7,   7,  10,  10, -10,
           -10,  10,   0,   0,   0,   0,  10, -10,
           -20, -10, -10, -10, -10, -10, -10, -20},
        {   30,  40,  30,  45,  50,  10,  30,  35,
            25,  30,  40,  50,  60,  50,  30,  30,
             5,  10,  10,  30,  20,  30,  10,   5,
           -20,  -5,  10,  15,  15,  20,  -5, -20,
            30,  -5,  -1,   0,   5,  -1,  -5, -20,
           -35,   0,   0,   0,   0,   0,   0, -30,
           -30, -10,   5,   6,   6,   5,  -5, -40,
           -10,  -8,  10,  18,  18,  10, -20, -20},
        {  -20, -10, -10,  -5,  -5, -10, -10, -20,
           -10,  -5,   5,  -5,  -1,   5,   5, -10,
           -10,   0,   5,   5,   5,   5,   0, -10,
            -5,   0,  10,   5,   5,   5,   0,  -5,
            -5,   5,  10,   1,  -1,   5,   5,  -5,
           -10,  15,  15,  15,  15,  15,  10, -10,
           -10,  -2,   5,   0,   0,  -2,   0, -10,
           -20, -10, -10,  -5,  -5, -15, -10, -20},
        {  -30, -40, -40, -50, -50, -40, -40, -30,
           -30, -40, -40, -50, -50, -40, -40, -30,
           -30, -40, -40, -50, -50, -40, -40, -30,
           -30, -40, -40, -50, -50, -40, -40, -30,
           -20, -30, -30, -40, -40, -30, -30, -20,
           -10, -20, -20, -40, -40, -20, -20, -10,
            14,  18, -10, -55, -55, -15,  15,  14,
            21,  35,   3, -50,   0, -20,  32,  22}
};


constexpr SCORE_TYPE PST_END[6][64] = {
        {    0,   0,   0,   0,   0,   0,   0,   0,
           145, 135, 130, 125, 125, 125, 135, 145,
            95,  90,  80,  70,  60,  60,  80,  85,
            20,  20,  10,  16,  16,  10,  15,  15,
            10,   0,   5,   4,   4,   5,   0,   0,
             2,   2,   0,   3,   3,   0,   2,   2,
            10,  10,   5,   5,   5,   3,   1,   0,
             0,   0,   0,   0,   0,   0,   0,   0},
        {  -60, -40, -30, -30, -30, -30, -40, -80,
           -40, -20,   0,   0,   0,   0, -20, -40,
           -30,   0,  20,  25,  25,  20,   0, -30,
           -30,   5,  25,  30,  30,  25,   5, -30,
           -30,   0,  25,  30,  30,  25,   0, -30,
           -30,   5,  20,  25,  25,  20,   5, -30,
           -40, -20,   0,   5,   5,   0, -20, -40,
           -30, -40, -30, -30, -30, -30, -40, -50},
        {  -20, -10, -10, -10, -10, -10, -10, -20,
           -10,   0,   0,   0,   0,   0,   0, -10,
           -10,   0,   5,  10,  10,   5,   0, -10,
           -10,  15,   5,  25,  25,   5,  15, -10,
           -10,   5,  20,  15,  15,  20,   5, -10,
           -10,  15,  15,  10,  10,  15,  15, -10,
           -10,   5,   0,   0,   0,   0,   5, -10,
           -20, -10, -10, -10, -10, -10, -10, -20},
        {   10,  10,  15,  15,  10,  10,   5,   5,
            20,  30,  33,  35,  35,  33,  30,  20,
             4,  18,  23,  25,  25,  23,  18,   4,
            -5,   0,   8,   8,   8,   8,   0,  -5,
            -5,   0,   0,   0,   0,   0,   0,  -5,
            -5,   0,   0,   0,   0,   0,   0,  -5,
            -5,   0,   5,   5,   5,   5,   0,  -5,
             0,   5,  10,  14,  14,  10,   5,   0},
        {  -20, -10, -10,  -5,  -5, -10, -10, -20,
           -10,   0,  30,  40,  60,  10,   0, -10,
           -10,   0,  20,  45,  50,  20,   0, -10,
            -5,   0,  10,  45,  55,  30,   0,  -5,
            -5,   0,  20,  45,  35,  20,   0,  -5,
           -10,   5,  20,  20,  20,  20,   5, -10,
           -10,   0,   5,   0,   0,   5,   0, -10,
           -20, -10, -10,  -5,  -5, -10, -10, -20},
        {    2,   8,  16,  14,  14,  16,   8,   2,
            14,  16,  20,  26,  26,  20,  16,  14,
            16,  25,  30,  31,  31,  30,  25,  16,
            16,  26,  32,  35,  35,  32,  26,  16,
             8,  25,  30,  33,  33,  30,  25,   8,
             2,   8,  16,  14,  14,  16,   8,   2,
           -18, -14, -10, -10, -10, -10, -14, -18,
           -20, -20, -20, -20, -20, -20, -20, -20}
};

#endif //ANTARESCHESSENGINE_CONSTANTS_H