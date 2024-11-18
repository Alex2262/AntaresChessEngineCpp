
#include <stdexcept>
#include <iostream>
#include <bitset>
#include <regex>
#include "position.h"
#include "useful.h"
#include "bitboard.h"
#include "attacks.h"
#include "zobrist.h"


void Position::clear_state_stack() {
    for (auto& state : state_stack) {
        state.in_check = -1;

        state.current_hash_key = 0ULL;
        state.current_pawn_hash_key = 0ULL;
        state.current_np_hash_key = 0ULL;
        state.current_major_hash_key = 0ULL;
        state.current_minor_hash_key = 0ULL;
        state.current_non_major_hash_key = 0ULL;

        state.threats = 0ULL;
        state.move = NO_INFORMATIVE_MOVE;
        state.static_eval = NO_EVALUATION;
        state.current_ep_square = NO_SQUARE;
        state.current_castle_ability_bits = 0;
        state.current_fifty_move = 0;
        state.excluded_move = NO_MOVE;

        state.activations.clear();
        state.deactivations.clear();
        state.king_bucket_update = {};
        state.NNUE_pushed = false;
    }
}

void Position::set_state(State& state, PLY_TYPE fifty_move) const {
    state.current_ep_square = ep_square;
    state.current_castle_ability_bits = castle_ability_bits;
    state.current_hash_key = hash_key;
    state.current_pawn_hash_key = pawn_hash_key;
    state.current_np_hash_key = np_hash_key;
    state.current_major_hash_key = major_hash_key;
    state.current_minor_hash_key = minor_hash_key;
    state.current_non_major_hash_key = non_major_hash_key;
    state.current_fifty_move = fifty_move;
    state.threats = threats;
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
    BITBOARD occupancy = all_pieces & (~from_square(square));

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
    pawn_hash_key = 0;
    np_hash_key = 0;
    major_hash_key = 0;
    minor_hash_key = 0;
    non_major_hash_key = 0;

    for (int piece = 0; piece < static_cast<int>(EMPTY); piece++) {
        BITBOARD piece_bitboard = get_pieces(static_cast<Piece>(piece));
        while (piece_bitboard) {
            Square square = poplsb(piece_bitboard);
            hash_key ^= ZobristHashKeys.piece_hash_keys[piece][square];

            if (get_piece_type(static_cast<Piece>(piece), get_color(static_cast<Piece>(piece))) == PAWN) {
                pawn_hash_key ^= ZobristHashKeys.piece_hash_keys[piece][square];
            }

            else {
                if (is_major(static_cast<Piece>(piece)) ||
                    get_piece_type(static_cast<Piece>(piece), get_color(static_cast<Piece>(piece))) == KING) {
                    major_hash_key ^= ZobristHashKeys.piece_hash_keys[piece][square];
                }

                else if (is_minor(static_cast<Piece>(piece))) {
                    minor_hash_key ^= ZobristHashKeys.piece_hash_keys[piece][square];
                }

                np_hash_key ^= ZobristHashKeys.piece_hash_keys[piece][square];
            }

            if (!is_major(static_cast<Piece>(piece))) {
                non_major_hash_key ^= ZobristHashKeys.piece_hash_keys[piece][square];
            }
        }
    }

    if (ep_square != NO_SQUARE) hash_key ^= ZobristHashKeys.ep_hash_keys[ep_square];

    hash_key           ^= ZobristHashKeys.castle_hash_keys[castle_ability_bits];
    np_hash_key        ^= ZobristHashKeys.castle_hash_keys[castle_ability_bits];
    major_hash_key     ^= ZobristHashKeys.castle_hash_keys[castle_ability_bits];

    if (side) {
        hash_key ^= ZobristHashKeys.side_hash_key;
    }
}

FenInfo Position::set_fen(const std::string& fen_string) {

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
    compute_threats();

    nnue_state.reset_nnue(*this);

    FenInfo fen_info = FenInfo{static_cast<PLY_TYPE>(std::stoi(half_move_clock)),
                               static_cast<PLY_TYPE>(std::stoi(full_move_counter))};

    return fen_info;
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


void Position::ensure_stable() {
    nnue_state.reset_nnue(*this);
}


void Position::compute_threats() {
    threats = 0;

    const Color opp = ~side;

    const BITBOARD occupancy = get_all_pieces();

    BITBOARD pawns      = get_pieces(PAWN, opp);
    BITBOARD knights    = get_pieces(KNIGHT, opp);
    BITBOARD diagonal   = get_pieces(QUEEN, opp) | get_pieces(BISHOP, opp);
    BITBOARD horizontal = get_pieces(QUEEN, opp) | get_pieces(ROOK, opp);

    BITBOARD pawn_forward_squares = opp == WHITE ? shift<NORTH>(pawns) : shift<SOUTH>(pawns);
    threats |= shift<WEST>(pawn_forward_squares) | shift<EAST>(pawn_forward_squares);

    while (knights) {
        const Square square = poplsb(knights);
        threats |= get_regular_piece_type_attacks<KNIGHT>(square, occupancy);
    }

    while (diagonal) {
        const Square square = poplsb(diagonal);
        threats |= get_regular_piece_type_attacks<BISHOP>(square, occupancy);
    }

    while (horizontal) {
        const Square square = poplsb(horizontal);
        threats |= get_regular_piece_type_attacks<ROOK>(square, occupancy);
    }

    threats |= get_regular_piece_type_attacks<KING>(get_king_pos(opp), occupancy);
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


bool Position::is_pseudo_legal(Move move) {

    if (move == NO_MOVE) return false;

    Square origin_square = move.origin();
    Square target_square = move.target();
    Piece selected = board[origin_square];
    Piece occupied = board[target_square];
    MoveType move_type = move.type();

    if (selected == EMPTY || get_color(selected) != side) return false;
    PieceType selected_type = get_piece_type(selected, side);

    bool capture = false;

    if (occupied != EMPTY) {
        if (get_color(occupied) == side) {
            if (!(fischer_random_chess && selected_type == KING &&
                    (occupied == get_piece(ROOK, side) || occupied == selected)
                    && move_type == MOVE_TYPE_CASTLE)) return false;

        } else capture = true;
    }

    if (move_type == MOVE_TYPE_CASTLE) {

        if (selected_type != KING) return false;
        if (rank_of(origin_square) != side * RANK_8) return false;

        Square appropriate_target_square_q = static_cast<Square>(c1 ^ (56 * side));
        Square appropriate_target_square_k = static_cast<Square>(g1 ^ (56 * side));

        Square rook_target_square_q = appropriate_target_square_q + EAST;
        Square rook_target_square_k = appropriate_target_square_k + WEST;

        if (target_square == appropriate_target_square_q) {
            if ((side == WHITE && (castle_ability_bits & 2) != 2) ||
                (side == BLACK && (castle_ability_bits & 8) != 8)) return false;

            if (board[starting_rook_pos[side][1]] != get_piece(ROOK, side)) return false;

            for (int temp_pos = static_cast<int>(origin_square) - 1; temp_pos > static_cast<int>(starting_rook_pos[side][1]); temp_pos--) {
                if (board[temp_pos] != EMPTY) return false;
            }

            if (fischer_random_chess) {
                if (origin_square < rook_target_square_q) {
                    if (board[appropriate_target_square_q] != EMPTY && board[appropriate_target_square_q] != WHITE_KING) return false;
                    if (board[rook_target_square_q] != EMPTY) return false;
                } else if (origin_square > rook_target_square_q) {
                    for (int temp_square = target_square; temp_square < static_cast<int>(starting_rook_pos[side][1]); temp_square++) {
                        if (board[temp_square] != EMPTY) return false;
                    }
                }
            }

            return true;
        }

        else if (target_square == appropriate_target_square_k) {
            if ((side == WHITE && (castle_ability_bits & 1) != 1) ||
                (side == BLACK && (castle_ability_bits & 4) != 4)) return false;

            if (board[starting_rook_pos[side][0]] != get_piece(ROOK, side)) return false;

            for (int temp_pos = static_cast<int>(origin_square) + 1; temp_pos < static_cast<int>(starting_rook_pos[side][0]); temp_pos++) {
                if (board[temp_pos] != EMPTY) return false;
            }

            if (fischer_random_chess) {
                if (origin_square > rook_target_square_k) {
                    if (board[rook_target_square_k] != EMPTY) return false;
                } else if (origin_square < rook_target_square_k) {
                    for (int temp_square = target_square; temp_square > static_cast<int>(starting_rook_pos[side][0]); temp_square--) {
                        if (board[temp_square] != EMPTY) return false;
                    }
                }
            }

            return true;
        }

        return false;
    }

    else if (move_type == MOVE_TYPE_EP) {
        return target_square == ep_square &&
            (get_pawn_attacks(target_square, ~side) & get_pieces(PAWN, side) & from_square(origin_square));
    }

    if (selected_type == PAWN) {
        if ((side == WHITE && target_square <= origin_square) ||
            (side == BLACK && target_square >= origin_square)) return false;

        if (move_type == MOVE_TYPE_PROMOTION && rank_of(target_square) != ~side * RANK_8) return false;
        if (move_type != MOVE_TYPE_PROMOTION && rank_of(target_square) == ~side * RANK_8) return false;

        if (file_of(target_square) == file_of(origin_square)) {
            if (capture) return false;
        } else {
            if (!capture) return false;
            if (!(get_pawn_attacks(origin_square, side) & from_square(target_square))) return false;
        }

        int rank_diff = abs(static_cast<int>(rank_of(target_square)) - static_cast<int>(rank_of(origin_square)));
        if (rank_diff > 2) return false;
        else if (rank_diff == 2) {
            if (side == WHITE &&
                (rank_of(origin_square) != RANK_2 || board[origin_square + NORTH] != EMPTY)) return false;
            if (side == BLACK &&
                (rank_of(origin_square) != RANK_7 || board[origin_square + SOUTH] != EMPTY)) return false;
        }
    }

    else {
        if (move_type != MOVE_TYPE_NORMAL) return false;

        BITBOARD attacks = get_regular_piece_type_attacks_nt(selected_type, origin_square, all_pieces);
        if (!(attacks & from_square(target_square))) return false;
    }

    return true;
}


void Position::make_null_move(State& state, PLY_TYPE& fifty_move) {
    side = ~side;
    hash_key ^= ZobristHashKeys.side_hash_key;

    state.move = NO_INFORMATIVE_MOVE;
    state.NNUE_pushed = false;

    if (ep_square != NO_SQUARE) {
        hash_key ^= ZobristHashKeys.ep_hash_keys[ep_square];
        ep_square = NO_SQUARE;
    }

    fifty_move = 0;

    BITBOARD temp_our_pieces = our_pieces;
    our_pieces = opp_pieces;
    opp_pieces = temp_our_pieces;

    compute_threats();
}

void Position::undo_null_move(State& state, PLY_TYPE& fifty_move) {
    side       = ~side;
    ep_square  = state.current_ep_square;
    threats    = state.threats;
    fifty_move = state.current_fifty_move;

    hash_key           = state.current_hash_key;
    pawn_hash_key      = state.current_pawn_hash_key;
    np_hash_key        = state.current_np_hash_key;
    major_hash_key     = state.current_major_hash_key;
    minor_hash_key     = state.current_minor_hash_key;
    non_major_hash_key = state.current_non_major_hash_key;

    BITBOARD temp_our_pieces = our_pieces;
    our_pieces = opp_pieces;
    opp_pieces = temp_our_pieces;
}


template<bool NNUE>
bool Position::make_move(Move move, State& state, PLY_TYPE& fifty_move) {

    Square castled_pos[2] = {NO_SQUARE, NO_SQUARE};

    // Get move info
    Square origin_square = move.origin();
    Square target_square = move.target();
    Piece selected = board[origin_square];
    Piece occupied = board[target_square];
    MoveType move_type = move.type();

    PieceType selected_type = get_piece_type(selected, side);
    PieceType occupied_type = occupied == EMPTY ? NONE : get_piece_type(occupied, ~side);

    state.move = InformativeMove(move, selected, occupied);

    state.NNUE_pushed = false;

    if constexpr (NNUE) {
        state.activations.clear();
        state.deactivations.clear();
        state.king_bucket_update = {};
    }

    bool legal = true;

    fifty_move++;

    // Handle captures
    if (move.is_capture(*this)) {
        remove_piece(occupied, target_square);
        if constexpr (NNUE) state.deactivations.push_back({occupied, target_square});
        hash_key ^= ZobristHashKeys.piece_hash_keys[occupied][target_square];

        if (occupied_type == PAWN) pawn_hash_key ^= ZobristHashKeys.piece_hash_keys[occupied][target_square];
        else np_hash_key ^= ZobristHashKeys.piece_hash_keys[occupied][target_square];

        if (is_major(occupied)) major_hash_key ^= ZobristHashKeys.piece_hash_keys[occupied][target_square];
        else if (is_minor(occupied)) minor_hash_key ^= ZobristHashKeys.piece_hash_keys[occupied][target_square];

        if (!is_major(occupied)) non_major_hash_key ^= ZobristHashKeys.piece_hash_keys[occupied][target_square];

        fifty_move = 0;
    }

    // -- Make the actual pseudo-legal move --
    if (move_type == MOVE_TYPE_NORMAL) {
        place_piece(selected, target_square);
        if constexpr (NNUE) state.activations.push_back({selected, target_square});
        hash_key ^= ZobristHashKeys.piece_hash_keys[selected][target_square];

        if (selected_type == PAWN) pawn_hash_key ^= ZobristHashKeys.piece_hash_keys[selected][target_square];
        else np_hash_key ^= ZobristHashKeys.piece_hash_keys[selected][target_square];

        if (is_major(selected) || selected_type == KING) major_hash_key ^= ZobristHashKeys.piece_hash_keys[selected][target_square];
        else if (is_minor(selected)) minor_hash_key ^= ZobristHashKeys.piece_hash_keys[selected][target_square];

        if (!is_major(selected)) non_major_hash_key ^= ZobristHashKeys.piece_hash_keys[selected][target_square];
    }

    else if (move_type == MOVE_TYPE_EP) {
        place_piece(selected, target_square);
        if constexpr (NNUE) state.activations.push_back({selected, target_square});
        hash_key           ^= ZobristHashKeys.piece_hash_keys[selected][target_square];
        pawn_hash_key      ^= ZobristHashKeys.piece_hash_keys[selected][target_square];
        non_major_hash_key ^= ZobristHashKeys.piece_hash_keys[selected][target_square];

        // Find and remove the captured EP pawn
        auto captured_square = static_cast<Square>(target_square + static_cast<Square>(side == WHITE ? SOUTH : NORTH));
        if constexpr (NNUE) state.deactivations.push_back({board[captured_square], captured_square});
        hash_key           ^= ZobristHashKeys.piece_hash_keys[board[captured_square]][captured_square];
        pawn_hash_key      ^= ZobristHashKeys.piece_hash_keys[board[captured_square]][captured_square];
        non_major_hash_key ^= ZobristHashKeys.piece_hash_keys[board[captured_square]][captured_square];
        remove_piece(board[captured_square], captured_square);
    }

    else if (move_type == MOVE_TYPE_CASTLE) {
        legal = !is_attacked(get_king_pos(side), side);

        // Get rook locations
        if (target_square == c1 || target_square == c8) {   // Queen side
            castled_pos[0] = starting_rook_pos[side][1];                // Rook origin square
            castled_pos[1] = static_cast<Square>(target_square + 1);    // Rook target square
        } else {                                            // King side
            castled_pos[0] = starting_rook_pos[side][0];                // Rook origin square
            castled_pos[1] = static_cast<Square>(target_square - 1);    // Rook target square
        }

        // Move the Rook
        if (castled_pos[0] != castled_pos[1]) {
            if constexpr (NNUE) state.deactivations.push_back({board[castled_pos[0]], castled_pos[0]});
            if constexpr (NNUE) state.activations.push_back({board[castled_pos[0]], castled_pos[1]});
            hash_key ^= ZobristHashKeys.piece_hash_keys[board[castled_pos[0]]][castled_pos[0]];
            hash_key ^= ZobristHashKeys.piece_hash_keys[board[castled_pos[0]]][castled_pos[1]];
            np_hash_key ^= ZobristHashKeys.piece_hash_keys[board[castled_pos[0]]][castled_pos[0]];
            np_hash_key ^= ZobristHashKeys.piece_hash_keys[board[castled_pos[0]]][castled_pos[1]];
            major_hash_key ^= ZobristHashKeys.piece_hash_keys[board[castled_pos[0]]][castled_pos[0]];
            major_hash_key ^= ZobristHashKeys.piece_hash_keys[board[castled_pos[0]]][castled_pos[1]];
            remove_piece(board[castled_pos[0]], castled_pos[0]);
            place_piece(get_piece(ROOK, side), castled_pos[1]);
        }

        // Move the king now (after moving the rook due to FRC edge-cases where the king and rook swap places)
        if (origin_square != target_square) {
            place_piece(selected, target_square);
            if constexpr (NNUE) state.activations.push_back({selected, target_square});
            hash_key           ^= ZobristHashKeys.piece_hash_keys[selected][target_square];
            np_hash_key        ^= ZobristHashKeys.piece_hash_keys[selected][target_square];
            major_hash_key     ^= ZobristHashKeys.piece_hash_keys[selected][target_square];
            non_major_hash_key ^= ZobristHashKeys.piece_hash_keys[selected][target_square];
        }
    }

    else if (move_type == MOVE_TYPE_PROMOTION) {
        auto promotion_piece = static_cast<Piece>(move.promotion_type() + 1 + side * COLOR_OFFSET);
        place_piece(promotion_piece, target_square);
        if constexpr (NNUE) state.activations.push_back({promotion_piece, target_square});

        hash_key ^= ZobristHashKeys.piece_hash_keys[promotion_piece][target_square];
        np_hash_key ^= ZobristHashKeys.piece_hash_keys[selected][target_square];

        if (is_major(promotion_piece)) major_hash_key ^= ZobristHashKeys.piece_hash_keys[selected][target_square];
        else if (is_minor(promotion_piece)) minor_hash_key ^= ZobristHashKeys.piece_hash_keys[selected][target_square];

        if (!is_major(promotion_piece)) non_major_hash_key ^= ZobristHashKeys.piece_hash_keys[selected][target_square];
    }

    // Remove the piece from the source square except for some FRC edge cases
    // (If the king goes to the same location then no need to change anything)
    if (target_square != origin_square) {
        remove_piece(selected, origin_square);

        if constexpr (NNUE) state.deactivations.push_back({selected, origin_square});
        hash_key ^= ZobristHashKeys.piece_hash_keys[selected][origin_square];

        if (selected_type == PAWN) pawn_hash_key ^= ZobristHashKeys.piece_hash_keys[selected][origin_square];
        else np_hash_key ^= ZobristHashKeys.piece_hash_keys[selected][origin_square];

        if (is_major(selected) || selected_type == KING) major_hash_key ^= ZobristHashKeys.piece_hash_keys[selected][origin_square];
        else if (is_minor(selected)) minor_hash_key ^= ZobristHashKeys.piece_hash_keys[selected][origin_square];

        if (!is_major(selected)) non_major_hash_key ^= ZobristHashKeys.piece_hash_keys[selected][origin_square];

        // The rook and king have swapped in FRC, and we just removed the piece on the rook's target square,
        // so we need to set the rook back onto this location
        if (castled_pos[1] == origin_square) board[castled_pos[1]] = get_piece(ROOK, side);
    }

    // -- Legal move checking --
    // Return False if we are in check after our move or castling isn't legal.
    if (!legal) return false;

    // Update information for getting attacked squares
    our_pieces = get_our_pieces();
    opp_pieces = get_opp_pieces();
    all_pieces = get_all_pieces();
    empty_squares = get_empty_squares();

    // Continue legal move checking
    if (is_attacked(get_king_pos(side), side)) return false;
    if (castled_pos[0] != NO_SQUARE) {
        // We need to check all the squares in between the king's destination square and original square, excluding
        // the original square since we already checked that earlier.
        if (target_square == c1 || target_square == c8) {  // Queen side castling
            for (int temp_pos = static_cast<int>(origin_square) - 1; temp_pos > static_cast<int>(target_square); temp_pos--) {
                if (is_attacked(static_cast<Square>(temp_pos), side)) return false;
            }
        } else {                                           // King side castling
            for (int temp_pos = static_cast<int>(origin_square) + 1; temp_pos < static_cast<int>(target_square); temp_pos++) {
                if (is_attacked(static_cast<Square>(temp_pos), side)) return false;
            }
        }
    }

    // --- The move is legal ---
    if (selected == WHITE_PAWN || selected == BLACK_PAWN) fifty_move = 0;

    // -- En Passant Resetting --
    // Double Pawn Push
    if ((selected == WHITE_PAWN || selected == BLACK_PAWN) && abs(static_cast<int>(target_square - origin_square)) == NORTH_NORTH) {
        // Reset the previously hashed ep square if it exists
        if (ep_square != NO_SQUARE) hash_key ^= ZobristHashKeys.ep_hash_keys[ep_square];

        ep_square = static_cast<Square>(target_square + static_cast<Square>(side == WHITE ? SOUTH : NORTH));
        hash_key ^= ZobristHashKeys.ep_hash_keys[ep_square];  // Set new EP hash
    }

    // If it is not a double pawn push then we must reset it if there was a previous EP
    else if (ep_square != NO_SQUARE) {
        hash_key ^= ZobristHashKeys.ep_hash_keys[ep_square];
        ep_square = NO_SQUARE;
    }

    // -- Update Castling Rights --
    // Reset the castling rights first
    hash_key       ^= ZobristHashKeys.castle_hash_keys[castle_ability_bits];
    np_hash_key    ^= ZobristHashKeys.castle_hash_keys[castle_ability_bits];
    major_hash_key ^= ZobristHashKeys.castle_hash_keys[castle_ability_bits];

    // King moves
    if (selected == WHITE_KING) {
        castle_ability_bits &= ~(1 << 0);
        castle_ability_bits &= ~(1 << 1);

        int king_bucket = KING_BUCKET_MAP[target_square];
        if (king_bucket != nnue_state.current_accumulator->king_buckets[side]) {
            state.king_bucket_update.update_necessary = true;
            state.king_bucket_update.side = WHITE;
            state.king_bucket_update.bucket = king_bucket;
        }

    } else if (selected == BLACK_KING) {
        castle_ability_bits &= ~(1 << 2);
        castle_ability_bits &= ~(1 << 3);

        int king_bucket = KING_BUCKET_MAP[target_square ^ 56];
        if (king_bucket != nnue_state.current_accumulator->king_buckets[side]) {
            state.king_bucket_update.update_necessary = true;
            state.king_bucket_update.side = BLACK;
            state.king_bucket_update.bucket = king_bucket;
        }
    }

    // Rook moves or is captured
    if (origin_square == starting_rook_pos[WHITE][0] ||
        target_square == starting_rook_pos[WHITE][0]) castle_ability_bits &= ~(1 << 0);
    if (origin_square == starting_rook_pos[WHITE][1] ||
        target_square == starting_rook_pos[WHITE][1]) castle_ability_bits &= ~(1 << 1);
    if (origin_square == starting_rook_pos[BLACK][0] ||
        target_square == starting_rook_pos[BLACK][0]) castle_ability_bits &= ~(1 << 2);
    if (origin_square == starting_rook_pos[BLACK][1] ||
        target_square == starting_rook_pos[BLACK][1]) castle_ability_bits &= ~(1 << 3);

    // Hash it back
    hash_key       ^= ZobristHashKeys.castle_hash_keys[castle_ability_bits];
    np_hash_key    ^= ZobristHashKeys.castle_hash_keys[castle_ability_bits];
    major_hash_key ^= ZobristHashKeys.castle_hash_keys[castle_ability_bits];

    hash_key ^= ZobristHashKeys.side_hash_key;
    side = ~side;

    BITBOARD temp_our_pieces = our_pieces;
    our_pieces = opp_pieces;
    opp_pieces = temp_our_pieces;

    compute_threats();

    return true;
}

template bool Position::make_move<USE_NNUE>(Move move, State& state, PLY_TYPE& fifty_move);
template bool Position::make_move<NO_NNUE >(Move move, State& state, PLY_TYPE& fifty_move);

template<bool NNUE>
void Position::undo_move(Move move, State& state, PLY_TYPE& fifty_move) {

    Square castled_pos[2] = {NO_SQUARE, NO_SQUARE};

    // Get move info
    Square origin_square = move.origin();
    Square target_square = move.target();
    Piece selected = state.move.selected();
    Piece occupied = state.move.occupied();
    MoveType move_type = move.type();

    if (state.NNUE_pushed) nnue_state.pop();

    // Reset certain information
    side = get_color(selected);

    hash_key            = state.current_hash_key;
    pawn_hash_key       = state.current_pawn_hash_key;
    np_hash_key         = state.current_np_hash_key;
    major_hash_key      = state.current_major_hash_key;
    minor_hash_key      = state.current_minor_hash_key;
    non_major_hash_key  = state.current_non_major_hash_key;

    threats             = state.threats;
    fifty_move          = state.current_fifty_move;
    ep_square           = state.current_ep_square;
    castle_ability_bits = state.current_castle_ability_bits;

    if (move_type == MOVE_TYPE_EP) {
        // Find and replace the captured EP pawn
        auto captured_square = static_cast<Square>(target_square + static_cast<Square>(side == WHITE ? SOUTH : NORTH));
        place_piece(side == WHITE ? BLACK_PAWN : WHITE_PAWN, captured_square);
    }

    else if (move_type == MOVE_TYPE_CASTLE) {
        // Get rook locations
        if (target_square == c1 || target_square == c8) {   // Queen side
            castled_pos[0] = starting_rook_pos[side][1];                // Rook origin square
            castled_pos[1] = static_cast<Square>(target_square + 1);    // Rook target square
        } else {                                            // King side
            castled_pos[0] = starting_rook_pos[side][0];                // Rook origin square
            castled_pos[1] = static_cast<Square>(target_square - 1);    // Rook target square
        }

        // Move the Rook back
        remove_piece(board[castled_pos[1]], castled_pos[1]);
        place_piece(get_piece(ROOK, side), castled_pos[0]);
    }

    // Set the occupied piece back except in FRC edge cases
    if (castled_pos[0] == target_square) {
        // The rook and king have swapped in FRC, and the rook has been placed in this square; however,
        // the king in the bitboard hasn't been removed yet and must be removed.
        pieces[selected] &= ~(1ULL << target_square);
    } else {
        remove_piece(board[target_square], target_square);
        if (occupied < EMPTY) place_piece(occupied, target_square);
    }

    place_piece(selected, origin_square);

    our_pieces = get_our_pieces();
    opp_pieces = get_opp_pieces();
    all_pieces = get_all_pieces();
    empty_squares = get_empty_squares();
}

template void Position::undo_move<USE_NNUE>(Move move, State& state, PLY_TYPE& fifty_move);
template void Position::undo_move<NO_NNUE >(Move move, State& state, PLY_TYPE& fifty_move);

void Position::update_nnue(State& state) {
    state.NNUE_pushed = true;
    nnue_state.push();

    Color bucket_side = state.king_bucket_update.side;

    if (state.king_bucket_update.update_necessary) {
        nnue_state.current_accumulator->king_buckets[bucket_side] = state.king_bucket_update.bucket;
        nnue_state.reset_side(*this, bucket_side);
    }

    for (NNUpdate& nn_update : state.activations) {
        if (state.king_bucket_update.update_necessary)
            nnue_state.update_feature_side<ACTIVATE>(nn_update.piece, nn_update.square, ~bucket_side);
        else nnue_state.update_feature<ACTIVATE>(nn_update.piece, nn_update.square);
    }

    for (NNUpdate& nn_update : state.deactivations) {
        if (state.king_bucket_update.update_necessary)
            nnue_state.update_feature_side<DEACTIVATE>(nn_update.piece, nn_update.square, ~bucket_side);
        else nnue_state.update_feature<DEACTIVATE>(nn_update.piece, nn_update.square);
    }
}

