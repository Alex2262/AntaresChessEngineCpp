

#ifndef ALTAIR_TYPES_H
#define ALTAIR_TYPES_H

#include <iostream>
#include <cstdint>

enum Color : int {
    WHITE,
    BLACK,
};

constexpr Color operator~(Color c) {
    return Color(c ^ 1);
}

constexpr uint32_t N_PIECES = 13;
enum Piece : uint32_t {
    WHITE_PAWN,
    WHITE_KNIGHT,
    WHITE_BISHOP,
    WHITE_ROOK,
    WHITE_QUEEN,
    WHITE_KING,
    BLACK_PAWN,
    BLACK_KNIGHT,
    BLACK_BISHOP,
    BLACK_ROOK,
    BLACK_QUEEN,
    BLACK_KING,
    EMPTY,
};

constexpr int COLOR_OFFSET = 6;

constexpr int N_SQUARES = 64;
enum Square : uint32_t {
    a1, b1, c1, d1, e1, f1, g1, h1,
    a2, b2, c2, d2, e2, f2, g2, h2,
    a3, b3, c3, d3, e3, f3, g3, h3,
    a4, b4, c4, d4, e4, f4, g4, h4,
    a5, b5, c5, d5, e5, f5, g5, h5,
    a6, b6, c6, d6, e6, f6, g6, h6,
    a7, b7, c7, d7, e7, f7, g7, h7,
    a8, b8, c8, d8, e8, f8, g8, h8,
    NO_SQUARE
};

enum PieceType : uint32_t {
    PAWN,
    KNIGHT,
    BISHOP,
    ROOK,
    QUEEN,
    KING,
    NONE
};

constexpr inline PieceType get_piece_type(Piece piece, Color color) {
    return static_cast<PieceType>(static_cast<int>(piece) - static_cast<int>(color * COLOR_OFFSET));
}

constexpr inline Piece get_piece(PieceType piece_type, Color color) {
    return static_cast<Piece>(static_cast<int>(piece_type) + static_cast<int>(color * COLOR_OFFSET));
}

constexpr inline Color get_color(Piece piece) {
    return static_cast<Color>(piece / COLOR_OFFSET);
}

constexpr inline bool is_major(Piece piece) {
    PieceType piece_type = get_piece_type(piece, get_color(piece));
    return piece_type == ROOK || piece_type == QUEEN;
}

constexpr inline bool is_minor(Piece piece) {
    PieceType piece_type = get_piece_type(piece, get_color(piece));
    return piece_type == KNIGHT || piece_type == BISHOP;
}

constexpr uint32_t N_DIRS = 8;
enum Direction : int32_t {
    NORTH = 8, NORTH_EAST = 9, EAST = 1, SOUTH_EAST = -7,
    SOUTH = -8, SOUTH_WEST = -9, WEST = -1, NORTH_WEST = 7,
    NORTH_NORTH = 16, SOUTH_SOUTH = -16
};

constexpr uint32_t N_FILES = 8;
using File = uint32_t;

constexpr File FILE_A = 0;
constexpr File FILE_B = 1;
constexpr File FILE_C = 2;
constexpr File FILE_D = 3;
constexpr File FILE_E = 4;
constexpr File FILE_F = 5;
constexpr File FILE_G = 6;
constexpr File FILE_H = 7;

const uint32_t N_RANKS = 8;
using Rank = uint32_t;

constexpr Rank RANK_1 = 0;
constexpr Rank RANK_2 = 1;
constexpr Rank RANK_3 = 2;
constexpr Rank RANK_4 = 3;
constexpr Rank RANK_5 = 4;
constexpr Rank RANK_6 = 5;
constexpr Rank RANK_7 = 6;
constexpr Rank RANK_8 = 7;

using PLY_TYPE = int16_t;
using HASH_TYPE = uint64_t;
using SCORE_TYPE = int32_t;

#endif //ALTAIR_TYPES_H
