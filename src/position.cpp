
#include <stdexcept>
#include <iostream>
#include <bitset>
#include <regex>
#include "position.h"
#include "useful.h"
#include "bitboard.h"
#include "attacks.h"


void Position::clear_state_stack() {
    for (auto& state : state_stack) {
        state.in_check = -1;
        state.current_hash_key = 0ULL;
        state.move = NO_INFORMATIVE_MOVE;
        state.evaluation = NO_EVALUATION;
        state.current_ep_square = NO_SQUARE;
        state.current_castle_ability_bits = 0;
        state.current_fifty_move = 0;
        state.excluded_move = NO_MOVE;
    }
}

void Position::set_state(State_Struct& state_struct, PLY_TYPE fifty_move) const {
    state_struct.current_ep_square = ep_square;
    state_struct.current_castle_ability_bits = castle_ability_bits;
    state_struct.current_hash_key = hash_key;
    state_struct.current_fifty_move = fifty_move;
}

[[nodiscard]] BITBOARD Position::get_attacked_squares(Color color) const {
    BITBOARD attacks{};
    for (int piece = 0; piece < 6; piece++) {
        BITBOARD piece_bitboard = get_pieces(static_cast<PieceType>(piece), color);
        while (piece_bitboard) {
            Square square = poplsb(piece_bitboard);
            attacks |= get_piece_attacks(static_cast<Piece>(piece + COLOR_OFFSET * color), square,
                                         all_pieces & (~from_square(square)));
        }
    }

    attacks &= ~(color == side ? our_pieces : opp_pieces);
    return attacks;
}

Square Position::get_king_pos(Color color) const {
    return lsb(get_pieces(KING, color));
}

bool Position::is_attacked(Square square, Color color) const {
    BITBOARD occupancy = all_pieces;

    // Treat square like a pawn
    BITBOARD pawn_attacks = color == WHITE ? WHITE_PAWN_ATTACKS[square] : BLACK_PAWN_ATTACKS[square];
    if (pawn_attacks & get_pieces(PAWN, ~color)) return true;

    // Treat square like a knight
    BITBOARD knight_attacks = KNIGHT_ATTACKS[square];
    if (knight_attacks & get_pieces(KNIGHT, ~color)) return true;

    // Treat square like a bishop
    BITBOARD bishop_attacks = get_bishop_attacks(square, occupancy);
    if (bishop_attacks & (get_pieces(BISHOP, ~color) | get_pieces(QUEEN, ~color))) return true;

    // Treat square like a rook
    BITBOARD rook_attacks = get_rook_attacks(square, occupancy);
    if (rook_attacks & (get_pieces(ROOK, ~color) | get_pieces(QUEEN, ~color))) return true;

    // Treat square like a king
    BITBOARD king_attacks = KING_ATTACKS[square];
    if (king_attacks & (get_pieces(KING, ~color))) return true;

    return false;
}

uint32_t Position::get_non_pawn_material_count() const {
    return popcount(all_pieces ^
        (get_pieces(PAWN, WHITE) | get_pieces(PAWN, BLACK) | get_pieces(KING, WHITE) | get_pieces(KING, BLACK)));
}

void Position::remove_piece(Piece piece, Square square) {
    pieces[piece] &= ~(1ULL << square);
    board[square] = EMPTY;
}

void Position::place_piece(Piece piece, Square square) {
    pieces[piece] |= (1ULL << square);
    board[square] = piece;
}

void Position::compute_hash_key() {
    hash_key = 0;

    for (int piece = 0; piece < static_cast<int>(EMPTY); piece++) {
        BITBOARD piece_bitboard = get_pieces(static_cast<Piece>(piece));
        while (piece_bitboard) {
            Square square = poplsb(piece_bitboard);
            hash_key ^= ZobristHashKeys.piece_hash_keys[piece][square];
        }
    }

    if (ep_square != NO_SQUARE) hash_key ^= ZobristHashKeys.ep_hash_keys[ep_square];

    hash_key ^= ZobristHashKeys.castle_hash_keys[castle_ability_bits];

    if (side) hash_key ^= ZobristHashKeys.side_hash_key;
}

PLY_TYPE Position::set_fen(const std::string& fen_string) {

    std::string reduced_fen_string = std::regex_replace(fen_string, std::regex("^ +| +$|( ) +"), "$1");
    std::vector<std::string> fen_tokens = split(reduced_fen_string, ' ');

    if (fen_tokens.size() < 4) {
        throw std::invalid_argument("Fen is incorrect");
    }

    const std::string position = fen_tokens[0];
    const std::string player = fen_tokens[1];
    const std::string castling = fen_tokens[2];
    const std::string en_passant = fen_tokens[3];

    const std::string half_move_clock = fen_tokens.size() >= 5 ? fen_tokens[4] : "0";
    const std::string full_move_counter = fen_tokens.size() >= 6 ? fen_tokens[5] : "1";

    side = (player == "w") ? WHITE : BLACK;

    for (int piece = WHITE_PAWN; piece < static_cast<int>(EMPTY); piece++) {
        pieces[piece] = 0ULL;
    }

    for (auto & square : board) {
        square = EMPTY;
    }

    auto pos = static_cast<Square>(56);

    // Parsing the main 8x8 board part while adding appropriate padding
    for (char c : position) {
        if (c == '/' ) {
            pos = static_cast<Square>(pos - 16);
        } else if (std::isdigit(c)) {

            for (int empty_amt = 0; empty_amt < c - '0'; empty_amt++) {
                board[pos] = EMPTY;
                pos = static_cast<Square>(pos + 1);
            }

        }
        else if (std::isalpha(c)) {

            Piece piece = piece_to_num(c);
            place_piece(piece, pos);

            pos = static_cast<Square>(pos + 1);

        }
    }

    castle_ability_bits = 0;
    starting_rook_pos[WHITE][0] = h1;
    starting_rook_pos[WHITE][1] = a1;
    starting_rook_pos[BLACK][0] = h8;
    starting_rook_pos[BLACK][1] = a8;

    for (char c : castling) {
        if (fischer_random_chess) {

            Color castle_flag_color = isupper(c) ? WHITE : BLACK;
            char castle_flag = static_cast<char>(tolower(static_cast<char>(c)));

            if (castle_flag == 'k') {
                starting_rook_pos[castle_flag_color][0] = msb(get_pieces(ROOK, castle_flag_color));
                castle_ability_bits |= castle_flag_color == WHITE ? 1 : 4;
            }

            else if (castle_flag == 'q') {
                starting_rook_pos[castle_flag_color][1] = lsb(get_pieces(ROOK, castle_flag_color));
                castle_ability_bits |= castle_flag_color == WHITE ? 2 : 8;
            }

            else {
                int rook_file = castle_flag - 'a';
                int king_file = static_cast<int>(file_of(get_king_pos(castle_flag_color)));

                if (rook_file > king_file) {
                    starting_rook_pos[castle_flag_color][0] = static_cast<Square>(rook_file + 56 * castle_flag_color);
                    castle_ability_bits |= (castle_flag_color == WHITE) ? 1 : 4;
                }
                else {
                    starting_rook_pos[castle_flag_color][1] = static_cast<Square>(rook_file + 56 * castle_flag_color);
                    castle_ability_bits |= (castle_flag_color == WHITE) ? 2 : 8;
                }
            }
        }

        else {
            if (c == 'K') castle_ability_bits |= 1;
            else if (c == 'Q') castle_ability_bits |= 2;
            else if (c == 'k') castle_ability_bits |= 4;
            else if (c == 'q') castle_ability_bits |= 8;
        }
    }

    if (en_passant.size() > 1) {
        auto square = static_cast<Square>((en_passant[1] - '1') * 8 + en_passant[0] - 'a');
        ep_square = square;
    }
    else {
        ep_square = NO_SQUARE;
    }

    our_pieces = get_our_pieces();
    opp_pieces = get_opp_pieces();
    all_pieces = get_all_pieces();
    empty_squares = get_empty_squares();

    compute_hash_key();

    nnue_state.reset_nnue(*this);

    return static_cast<PLY_TYPE>(std::stoi(half_move_clock));
}

std::string Position::get_fen(PLY_TYPE fifty_move) {
    std::string fen;

    int empty = 0;
    for (int i = 0; i < 64; i++) {
        int square = i ^ 56;
        if (i != 0 && i % 8 == 0) {
            if (empty) fen += std::to_string(empty);
            empty = 0;
            fen += "/";
        }

        Piece piece = board[square];
        if (piece == EMPTY) empty++;
        else {
            if (empty) fen += std::to_string(empty);
            empty = 0;

            fen += PIECE_MATCHER[piece];
        }
    }

    fen += " ";
    fen += side == WHITE ? "w" : "b";

    fen += " ";

    if (castle_ability_bits == 0) fen += "-";
    if ((castle_ability_bits & 1) == 1) fen += "K";
    if ((castle_ability_bits & 2) == 2) fen += "Q";
    if ((castle_ability_bits & 4) == 4) fen += "k";
    if ((castle_ability_bits & 8) == 8) fen += "q";

    fen += " ";
    if (ep_square == NO_SQUARE) fen += "-";
    else {
        fen += char(ep_square % 8 + 'a');
        fen += char(ep_square / 8 + '1');
    }

    fen += " ";
    fen += std::to_string(fifty_move);

    return fen;
}

void Position::set_frc_side(Color color, int index) {

    // Based on the Scharnagl Numbering Scheme:
    // https://en.wikipedia.org/wiki/Fischer_random_chess_numbering_scheme

    std::vector<int> empty_back_rank;

    for (int i = 0; i < 8; i++) empty_back_rank.push_back(i ^ 56 * color);

    // Bishops
    Square bishop_1 = static_cast<Square>(bishop_ordering_1[color][index % 4]);
    index /= 4;

    Square bishop_2 = static_cast<Square>(bishop_ordering_2[color][index % 4]);
    index /= 4;

    place_piece(get_piece(BISHOP, color), bishop_1);
    place_piece(get_piece(BISHOP, color), bishop_2);

    empty_back_rank.erase(std::remove(empty_back_rank.begin(), empty_back_rank.end(), bishop_1));
    empty_back_rank.erase(std::remove(empty_back_rank.begin(), empty_back_rank.end(), bishop_2));

    // Queen
    place_piece(get_piece(QUEEN, color), static_cast<Square>(empty_back_rank[index % 6]));
    empty_back_rank.erase(empty_back_rank.begin() + (index % 6));

    index /= 6;

    // Knights
    for (int i = 4; i >= 1; i--) {
        if (index < i) {
            place_piece(get_piece(KNIGHT, color), static_cast<Square>(empty_back_rank[4 - i]));
            empty_back_rank.erase(empty_back_rank.begin() + (4 - i));

            place_piece(get_piece(KNIGHT, color), static_cast<Square>(empty_back_rank[index]));
            empty_back_rank.erase(empty_back_rank.begin() + (index));

            break;
        }
        index -= i;
    }

    place_piece(get_piece(ROOK, color), static_cast<Square>(empty_back_rank[0]));
    place_piece(get_piece(KING, color), static_cast<Square>(empty_back_rank[1]));
    place_piece(get_piece(ROOK, color), static_cast<Square>(empty_back_rank[2]));

}

void Position::set_dfrc(int index) {

    for (int piece = WHITE_PAWN; piece < static_cast<int>(EMPTY); piece++) {
        if (piece == WHITE_PAWN || piece == BLACK_PAWN) continue;
        pieces[piece] = 0ULL;
    }

    for (auto & square : board) {
        if (square == WHITE_PAWN || square == BLACK_PAWN) continue;
        square = EMPTY;
    }

    set_frc_side(WHITE, index % 960);
    set_frc_side(BLACK, index / 960);

    compute_hash_key();
    nnue_state.reset_nnue(*this);
}

std::ostream& operator << (std::ostream& os, const Position& position) {
    std::string new_board;

    auto pos = static_cast<Square>(56);
    for (int i = 0; i < 64; i++) {
        if (i != 0 && i % 8 == 0) {
            new_board += '\n';
            pos = static_cast<Square>(pos - 16);
        }

        Piece piece = position.board[pos];
        pos = static_cast<Square>(pos + 1);

        if (piece == EMPTY) {
            new_board += ". ";
            continue;
        }

        //assert((pieces[piece] & (1ULL << MAILBOX_TO_STANDARD[pos]) >> MAILBOX_TO_STANDARD[pos]) == 1);

        new_board += PIECE_MATCHER[piece];
        new_board += ' ';

    }

    os << new_board << std::endl << std::endl;
    os << "side: " << position.side << " ep: " << position.ep_square << " castle: " << position.castle_ability_bits
       << " hash: " << position.hash_key << std::endl << std::endl;

    /*
    for (int piece = WHITE_PAWN; piece < EMPTY; piece++) {
        os << "Piece: " << piece << " bitboard: \n";
        print_bitboard(position.pieces[piece]);
    }
    */

    return os;
}


void Position::get_knight_captures(FixedVector<ScoredMove, MAX_MOVES>& current_scored_moves) const {
    BITBOARD knights = get_pieces(KNIGHT, side);

    while (knights) {
        Square square = poplsb(knights);

        BITBOARD bitboard = KNIGHT_ATTACKS[square] & opp_pieces;
        while (bitboard) {
            Square new_square = poplsb(bitboard);
            current_scored_moves.push_back({
                Move(square, new_square),
                0
            });
        }
    }
}

void Position::get_knight_moves(FixedVector<ScoredMove, MAX_MOVES>& current_scored_moves) const {
    BITBOARD knights = get_pieces(KNIGHT, side);

    while (knights) {
        Square square = poplsb(knights);

        BITBOARD bitboard = KNIGHT_ATTACKS[square] & (~our_pieces);
        while (bitboard) {
            Square new_square = poplsb(bitboard);
            current_scored_moves.push_back({
                Move(square, new_square),
                0
            });
        }
    }
}

void Position::get_bishop_captures(FixedVector<ScoredMove, MAX_MOVES>& current_scored_moves) const {
    BITBOARD bishops = get_pieces(BISHOP, side);

    while (bishops) {
        Square square = poplsb(bishops);

        BITBOARD bishop_captures = get_bishop_attacks(square, all_pieces) & opp_pieces;

        while (bishop_captures) {
            Square new_square = poplsb(bishop_captures);
            current_scored_moves.push_back({
                Move(square, new_square),
                0
            });
        }
    }
}

void Position::get_bishop_moves(FixedVector<ScoredMove, MAX_MOVES>& current_scored_moves) const {
    BITBOARD bishops = get_pieces(BISHOP, side);

    while (bishops) {
        Square square = poplsb(bishops);

        BITBOARD bishop_attacks = get_bishop_attacks(square, all_pieces);
        BITBOARD bishop_moves = bishop_attacks & (~our_pieces);

        while (bishop_moves) {
            Square new_square = poplsb(bishop_moves);
            current_scored_moves.push_back({
                Move(square, new_square),
                0
            });
        }
    }
}

void Position::get_rook_captures(FixedVector<ScoredMove, MAX_MOVES>& current_scored_moves) const {
    BITBOARD rooks = get_pieces(ROOK, side);

    while (rooks) {
        Square square = poplsb(rooks);

        BITBOARD rook_attacks = get_rook_attacks(square, all_pieces) & opp_pieces;

        while (rook_attacks) {
            Square new_square = poplsb(rook_attacks);
            current_scored_moves.push_back({
                Move(square, new_square),
                0
            });
        }
    }
}

void Position::get_queen_captures(FixedVector<ScoredMove, MAX_MOVES>& current_scored_moves) const {
    BITBOARD queens = get_pieces(QUEEN, side);
    while (queens) {
        Square square = poplsb(queens);
        BITBOARD queen_captures = get_queen_attacks(square, all_pieces) & opp_pieces;

        while (queen_captures) {
            Square new_square = poplsb(queen_captures);
            current_scored_moves.push_back({
                Move(square, new_square),
                0
            });
        }
    }
}

void Position::get_queen_moves(FixedVector<ScoredMove, MAX_MOVES>& current_scored_moves) const {
    BITBOARD queens = get_pieces(QUEEN, side);
    while (queens) {
        Square square = poplsb(queens);

        BITBOARD queen_attacks = get_queen_attacks(square, all_pieces);
        BITBOARD queen_moves = queen_attacks & (~our_pieces);

        while (queen_moves) {
            Square new_square = poplsb(queen_moves);
            current_scored_moves.push_back({
                Move(square, new_square),
                0
            });
        }
    }
}

void Position::get_king_captures(FixedVector<ScoredMove, MAX_MOVES>& current_scored_moves) const {
    Square square = get_king_pos(side);
    BITBOARD king_captures = KING_ATTACKS[square] & opp_pieces;
    while (king_captures) {
        Square new_square = poplsb(king_captures);
        current_scored_moves.push_back({
                Move(square, new_square),
                0
        });
    }
}

void Position::get_king_moves(FixedVector<ScoredMove, MAX_MOVES>& current_scored_moves) const {
    Square square = get_king_pos(side);
    BITBOARD king_moves = KING_ATTACKS[square] & (~our_pieces);
    while (king_moves) {
        Square new_square = poplsb(king_moves);
        current_scored_moves.push_back({
                Move(square, new_square),
                0
        });
    }
}

void Position::make_null_move(State_Struct& state_struct, PLY_TYPE& fifty_move) {
    side = ~side;
    hash_key ^= ZobristHashKeys.side_hash_key;
    state_struct.move = NO_INFORMATIVE_MOVE;

    if (ep_square != NO_SQUARE) {
        hash_key ^= ZobristHashKeys.ep_hash_keys[ep_square];
        ep_square = NO_SQUARE;
    }

    fifty_move = 0;

    BITBOARD temp_our_pieces = our_pieces;
    our_pieces = opp_pieces;
    opp_pieces = temp_our_pieces;
}

void Position::undo_null_move(State_Struct& state_struct, PLY_TYPE& fifty_move) {
    side = ~side;
    ep_square = state_struct.current_ep_square;
    hash_key = state_struct.current_hash_key;
    fifty_move = state_struct.current_fifty_move;

    BITBOARD temp_our_pieces = our_pieces;
    our_pieces = opp_pieces;
    opp_pieces = temp_our_pieces;
}

