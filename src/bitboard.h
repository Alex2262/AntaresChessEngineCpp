
#ifndef ALTAIR_BITBOARD_H
#define ALTAIR_BITBOARD_H

#include <cstdint>
#include <cassert>
#include <array>
#include "types.h"

using BITBOARD = uint64_t;

constexpr std::array<BITBOARD, 8> MASK_FILE = {
        0x101010101010101, 0x202020202020202, 0x404040404040404, 0x808080808080808,
        0x1010101010101010, 0x2020202020202020, 0x4040404040404040, 0x8080808080808080,
};

constexpr std::array<BITBOARD, 8> MASK_RANK = {
        0xff, 0xff00, 0xff0000, 0xff000000, 0xff00000000,
        0xff0000000000, 0xff000000000000, 0xff00000000000000
};

constexpr std::array<BITBOARD, 15> MASK_DIAGONAL = {
        0x80, 0x8040, 0x804020,
        0x80402010, 0x8040201008, 0x804020100804,
        0x80402010080402, 0x8040201008040201, 0x4020100804020100,
        0x2010080402010000, 0x1008040201000000, 0x804020100000000,
        0x402010000000000, 0x201000000000000, 0x100000000000000,
};

constexpr std::array<BITBOARD, 15> MASK_ANTI_DIAGONAL= {
        0x1, 0x102, 0x10204,
        0x1020408, 0x102040810, 0x10204081020,
        0x1020408102040, 0x102040810204080, 0x204081020408000,
        0x408102040800000, 0x810204080000000, 0x1020408000000000,
        0x2040800000000000, 0x4080000000000000, 0x8000000000000000,
};


void print_bitboard(BITBOARD bitboard);

constexpr inline BITBOARD from_square(Square square) {
    return 1ULL << square;
}

constexpr inline File file_of(Square square) {
    return File(square & 0b111);  // square % 8
}

constexpr inline Rank rank_of(Square square) {
    return Rank(square >> 3);  // square / 8
}

constexpr inline int32_t diagonal_of(Square s) {
    return 7 + rank_of(s) - file_of(s);
}

constexpr inline int32_t anti_diagonal_of(Square s) {
    return rank_of(s) + file_of(s);
}

template<Direction D>
constexpr BITBOARD shift(BITBOARD b) {
    if constexpr (D == NORTH) return b << 8;
    else if constexpr (D == SOUTH) return b >> 8;
    else if constexpr (D == NORTH + NORTH) return b << 16;
    else if constexpr (D == SOUTH + SOUTH) return b >> 16;
    else if constexpr (D == EAST) return (b & ~MASK_FILE[FILE_H]) << 1;
    else if constexpr (D == WEST) return (b & ~MASK_FILE[FILE_A]) >> 1;
    else if constexpr (D == NORTH_EAST) return (b & ~MASK_FILE[FILE_H]) << 9;
    else if constexpr (D == NORTH_WEST) return (b & ~MASK_FILE[FILE_A]) << 7;
    else if constexpr (D == SOUTH_EAST) return (b & ~MASK_FILE[FILE_H]) >> 7;
    else if constexpr (D == SOUTH_WEST) return (b & ~MASK_FILE[FILE_A]) >> 9;
    return 0;
}

template<Direction D>
constexpr BITBOARD fill(BITBOARD b) {
    if constexpr (D == NORTH) {
        b |= (b << 8);
        b |= (b << 16);
        return b | (b << 32);
    } else if constexpr (D == SOUTH) {
        b |= (b >> 8);
        b |= (b >> 16);
        return b | (b >> 32);
    }
}

constexpr inline BITBOARD fill(Direction D, BITBOARD b) {
    if (D == NORTH) {
        b |= (b << 8);
        b |= (b << 16);
        return b | (b << 32);
    } else if (D == SOUTH) {
        b |= (b >> 8);
        b |= (b >> 16);
        return b | (b >> 32);
    }

    return b;
}

constexpr Square operator+(Square s, Direction d) {
    return Square(static_cast<int32_t>(s) + static_cast<int32_t>(d));
}

template<Color color>
inline constexpr Direction get_up_direction() {
    return []() {
        if constexpr (color == WHITE) return NORTH;
        else return SOUTH;
    };
}

template<Color color>
inline constexpr Direction get_down_direction() {
    return []() {
        if constexpr (color == WHITE) return SOUTH;
        else return NORTH;
    };
}

[[nodiscard]] Square lsb(BITBOARD bitboard);
[[nodiscard]] Square msb(BITBOARD bitboard);

[[nodiscard]] uint32_t popcount(BITBOARD bitboard);
[[nodiscard]] Square poplsb(BITBOARD& bitboard);

#endif //ALTAIR_BITBOARD_H
