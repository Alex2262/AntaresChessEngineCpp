
#ifndef ALTAIR_POSITION_H
#define ALTAIR_POSITION_H

#include <string>
#include "constants.h"
#include "types.h"
#include "fixed_vector.h"
#include "bitboard.h"
#include "attacks.h"
#include "move.h"
#include "nnue.h"
#include "zobrist.h"


constexpr int bishop_ordering_1[2][4] = {
        { 1,  3,  5,  7},
        {57, 59, 61, 63}
};

constexpr int bishop_ordering_2[2][4] = {
        { 0,  2,  4,  6},
        {56, 58, 60, 62}
};

struct ScoredMove {
    Move move = NO_MOVE;
    SCORE_TYPE score = 0;
    bool winning_capture = false;
};

struct State_Struct {
    uint64_t current_hash_key = 0ULL;
    Square current_ep_square = NO_SQUARE;
    uint8_t current_castle_ability_bits = 0;
    PLY_TYPE current_fifty_move = 0;

    InformativeMove move = NO_INFORMATIVE_MOVE;
    Move excluded_move = NO_MOVE;

    SCORE_TYPE evaluation = NO_EVALUATION;

    int double_extensions = 0;
    int in_check = -1;
};


class Position {

public:

    Position() = default;

    NNUE_State nnue_state{};

    bool fischer_random_chess = false;

    BITBOARD all_pieces{};
    BITBOARD our_pieces{};
    BITBOARD opp_pieces{};
    BITBOARD empty_squares{};

    BITBOARD pieces[12]{};

    Piece board[64]{};

    Color side = WHITE;

    uint8_t castle_ability_bits = 0;
    Square starting_rook_pos[2][2]{};

    Square ep_square = NO_SQUARE;
    HASH_TYPE hash_key = 0;

    std::array<State_Struct, TOTAL_MAX_DEPTH> state_stack{};

    std::array<FixedVector<ScoredMove, MAX_MOVES>, TOTAL_MAX_DEPTH> scored_moves{};

    void clear_state_stack();
    void set_state(State_Struct& state_struct, PLY_TYPE fifty_move) const;

    [[nodiscard]] inline BITBOARD get_pieces(Piece piece) const {
        return pieces[piece];
    }

    [[nodiscard]] inline BITBOARD get_pieces(PieceType piece, Color color) const {
        return pieces[piece + color * COLOR_OFFSET];
    }

    [[nodiscard]] inline BITBOARD get_pieces(Color color) const {
        return get_pieces(PAWN, color) |
               get_pieces(KNIGHT, color) |
               get_pieces(BISHOP, color) |
               get_pieces(ROOK, color) |
               get_pieces(QUEEN, color) |
               get_pieces(KING, color);
    }

    [[nodiscard]] inline BITBOARD get_our_pieces() const {
        return get_pieces(side);
    }

    [[nodiscard]] inline BITBOARD get_opp_pieces() const {
        return get_pieces(~side);
    }

    [[nodiscard]] inline BITBOARD get_all_pieces() const {
        return our_pieces | opp_pieces;
    }

    [[nodiscard]] inline BITBOARD get_empty_squares() const {
        return ~all_pieces;
    }

    [[nodiscard]] BITBOARD get_attacked_squares(Color color) const;

    [[nodiscard]] Square get_king_pos(Color color) const;

    [[nodiscard]] bool is_attacked(Square square, Color color) const;

    uint32_t get_non_pawn_material_count() const;

    void remove_piece(Piece piece, Square square);
    void place_piece(Piece piece, Square square);

    void compute_hash_key();

    PLY_TYPE set_fen(const std::string& fen);
    std::string get_fen(PLY_TYPE fifty_move);

    void set_frc_side(Color color, int index);
    void set_dfrc(int index);

    friend std::ostream& operator<<(std::ostream& os, const Position& position);

    void get_knight_captures(FixedVector<ScoredMove, MAX_MOVES>& current_scored_moves) const;
    void get_knight_moves(FixedVector<ScoredMove, MAX_MOVES>& current_scored_moves) const;

    void get_bishop_captures(FixedVector<ScoredMove, MAX_MOVES>& current_scored_moves) const;
    void get_bishop_moves(FixedVector<ScoredMove, MAX_MOVES>& current_scored_moves) const;

    void get_rook_captures(FixedVector<ScoredMove, MAX_MOVES>& current_scored_moves) const;

    void get_queen_captures(FixedVector<ScoredMove, MAX_MOVES>& current_scored_moves) const;
    void get_queen_moves(FixedVector<ScoredMove, MAX_MOVES>& current_scored_moves) const;

    void get_king_captures(FixedVector<ScoredMove, MAX_MOVES>& current_scored_moves) const;
    void get_king_moves(FixedVector<ScoredMove, MAX_MOVES>& current_scored_moves) const;

    void make_null_move(State_Struct& state_struct, PLY_TYPE& fifty_move);
    void undo_null_move(State_Struct& state_struct, PLY_TYPE& fifty_move);

    template<Color color>
    inline bool make_move(Move move, State_Struct& state_struct, PLY_TYPE& fifty_move) {
        Square castled_pos[2] = {NO_SQUARE, NO_SQUARE};

        // Get move info
        Square origin_square = move.origin();
        Square target_square = move.target();
        Piece selected = board[origin_square];
        Piece occupied = board[target_square];
        MoveType move_type = move.type();

        PieceType selected_type = get_piece_type(selected, side);

        state_struct.move = InformativeMove(move, selected, occupied);

        bool legal = true;

        fifty_move++;

        nnue_state.push();

        // Handle captures
        if (move.is_capture(*this)) {
            remove_piece(occupied, target_square);
            nnue_state.update_feature<false>(occupied, target_square);
            hash_key ^= ZobristHashKeys.piece_hash_keys[occupied][target_square];
            fifty_move = 0;
        }

        // -- Make the actual pseudo-legal move --
        if (move_type == MOVE_TYPE_NORMAL) {
            place_piece(selected, target_square);
            nnue_state.update_feature<true>(selected, target_square);
            hash_key ^= ZobristHashKeys.piece_hash_keys[selected][target_square];
        }

        else if (move_type == MOVE_TYPE_EP) {
            place_piece(selected, target_square);
            nnue_state.update_feature<true>(selected, target_square);
            hash_key ^= ZobristHashKeys.piece_hash_keys[selected][target_square];

            // Find and remove the captured EP pawn
            auto captured_square = static_cast<Square>(target_square + static_cast<Square>(get_down_direction<color>()));
            nnue_state.update_feature<false>(board[captured_square], captured_square);
            hash_key ^= ZobristHashKeys.piece_hash_keys[board[captured_square]][captured_square];
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
            nnue_state.update_feature<false>(board[castled_pos[0]], castled_pos[0]);
            nnue_state.update_feature<true>(board[castled_pos[0]], castled_pos[1]);
            hash_key ^= ZobristHashKeys.piece_hash_keys[board[castled_pos[0]]][castled_pos[0]];
            hash_key ^= ZobristHashKeys.piece_hash_keys[board[castled_pos[0]]][castled_pos[1]];
            remove_piece(board[castled_pos[0]], castled_pos[0]);
            place_piece(get_piece(ROOK, side), castled_pos[1]);

            // Move the king now (after moving the rook due to FRC edge-cases where the king and rook swap places)
            place_piece(selected, target_square);
            nnue_state.update_feature<true>(selected, target_square);
            hash_key ^= ZobristHashKeys.piece_hash_keys[selected][target_square];
        }

        else if (move_type == MOVE_TYPE_PROMOTION) {
            auto promotion_piece = static_cast<Piece>(move.promotion_type() + 1 + side * COLOR_OFFSET);
            place_piece(promotion_piece, target_square);
            nnue_state.update_feature<true>(promotion_piece, target_square);
            hash_key ^= ZobristHashKeys.piece_hash_keys[promotion_piece][target_square];
        }

        // Remove the piece from the source square except for some FRC edge cases
        // (If the king goes is castling to the same location then no need to change anything)
        if (target_square != origin_square) {

            // The rook and king have swapped in FRC, and the rook has been placed in this square; however,
            // the king in the bitboard hasn't been removed yet and must be removed.
            if (castled_pos[1] == origin_square) pieces[selected] &= ~(1ULL << origin_square);
            else remove_piece(selected, origin_square);

            nnue_state.update_feature<false>(selected, origin_square);
            hash_key ^= ZobristHashKeys.piece_hash_keys[selected][origin_square];
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
        if (selected_type == PAWN) fifty_move = 0;

        // -- En Passant Resetting --
        // Double Pawn Push
        if (selected_type == PAWN && abs(static_cast<int>(target_square - origin_square)) == NORTH_NORTH) {
            // Reset the previously hashed ep square if it exists
            if (ep_square != NO_SQUARE) hash_key ^= ZobristHashKeys.ep_hash_keys[ep_square];

            ep_square = static_cast<Square>(target_square + static_cast<Square>(get_down_direction<color>()));
            hash_key ^= ZobristHashKeys.ep_hash_keys[ep_square];  // Set new EP hash
        }

        // If it is not a double pawn push then we must reset it if there was a previous EP
        else if (ep_square != NO_SQUARE) {
            hash_key ^= ZobristHashKeys.ep_hash_keys[ep_square];
            ep_square = NO_SQUARE;
        }

        // -- Update Castling Rights --
        // Reset the castling rights first
        hash_key ^= ZobristHashKeys.castle_hash_keys[castle_ability_bits];

        // King moves
        if (selected_type == KING) {
            if constexpr (color == WHITE) {
                castle_ability_bits &= ~(1 << 0);
                castle_ability_bits &= ~(1 << 1);
            } else {
                castle_ability_bits &= ~(1 << 2);
                castle_ability_bits &= ~(1 << 3);
            }
        }

        // Rook moves or is captured
        if constexpr (color == WHITE) {
            if (selected_type == ROOK) {
                if (origin_square == starting_rook_pos[WHITE][0]) castle_ability_bits &= ~(1 << 0);
                if (origin_square == starting_rook_pos[WHITE][1]) castle_ability_bits &= ~(1 << 1);
            } else {
                if (target_square == starting_rook_pos[BLACK][0]) castle_ability_bits &= ~(1 << 2);
                if (target_square == starting_rook_pos[BLACK][1]) castle_ability_bits &= ~(1 << 3);
            }
        } else {
            if (selected_type == ROOK) {
                if (origin_square == starting_rook_pos[BLACK][0]) castle_ability_bits &= ~(1 << 2);
                if (origin_square == starting_rook_pos[BLACK][1]) castle_ability_bits &= ~(1 << 3);
            } else {
                if (target_square == starting_rook_pos[WHITE][0]) castle_ability_bits &= ~(1 << 0);
                if (target_square == starting_rook_pos[WHITE][1]) castle_ability_bits &= ~(1 << 1);
            }
        }

        // Hash it back
        hash_key ^= ZobristHashKeys.castle_hash_keys[castle_ability_bits];

        hash_key ^= ZobristHashKeys.side_hash_key;
        side = ~side;

        BITBOARD temp_our_pieces = our_pieces;
        our_pieces = opp_pieces;
        opp_pieces = temp_our_pieces;

        return true;
    }

    template<Color color>
    inline void undo_move(Move move, State_Struct& state_struct, PLY_TYPE& fifty_move) {
        Square castled_pos[2] = {NO_SQUARE, NO_SQUARE};

        // Get move info
        Square origin_square = move.origin();
        Square target_square = move.target();
        Piece selected = state_struct.move.selected();
        Piece occupied = state_struct.move.occupied();
        MoveType move_type = move.type();

        nnue_state.pop();

        // Reset certain information
        side = static_cast<Color>(selected >= BLACK_PAWN);  // Can't do ~side because maybe we attempt a make move but don't flip side
        hash_key = state_struct.current_hash_key;
        fifty_move = state_struct.current_fifty_move;
        ep_square = state_struct.current_ep_square;
        castle_ability_bits = state_struct.current_castle_ability_bits;

        if (move_type == MOVE_TYPE_EP) {
            // Find and replace the captured EP pawn
            auto captured_square = static_cast<Square>(target_square + static_cast<Square>(get_down_direction<color>()));
            place_piece([](){
                            if constexpr (color == WHITE) return BLACK_PAWN;
                            else return WHITE_PAWN;
                        },
                        captured_square);
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

    template<Color color>
    inline void get_pawn_captures(FixedVector<ScoredMove, MAX_MOVES>& current_scored_moves) const {
        const BITBOARD pawns = get_pieces(PAWN, side);

        const auto pawn_forward_squares = [pawns]() {
            if constexpr (color == WHITE) return shift<NORTH>(pawns);
            else return shift<SOUTH>(pawns);
        } & ~(MASK_RANK[RANK_1] | MASK_RANK[RANK_8]);  // Prevent captures to the 1st/8th rank

        constexpr Direction down = get_down_direction<color>();

        BITBOARD west_attacks = shift<WEST>(pawn_forward_squares) & opp_pieces;
        BITBOARD east_attacks = shift<EAST>(pawn_forward_squares) & opp_pieces;

        while (west_attacks) {
            const Square new_square = poplsb(west_attacks);
            current_scored_moves.push_back({
                Move(new_square + down + EAST, new_square),
                0
            });
        }

        while (east_attacks) {
            const Square new_square = poplsb(east_attacks);
            current_scored_moves.push_back({
                Move(new_square + down + WEST, new_square),
                0
            });
        }
    }

    template<Color color>
    void get_pawn_moves(FixedVector<ScoredMove, MAX_MOVES>& current_scored_moves) const {
        const BITBOARD pawns = get_pieces(PAWN, side);

        const auto pawn_forward_squares = [pawns]() {
            if constexpr (color == WHITE) return shift<NORTH>(pawns);
            else return shift<SOUTH>(pawns);
        };

        constexpr Direction down = get_down_direction<color>();

        BITBOARD west_attacks = shift<WEST>(pawn_forward_squares) & opp_pieces;
        BITBOARD east_attacks = shift<EAST>(pawn_forward_squares) & opp_pieces;

        while (west_attacks) {
            const Square new_square = poplsb(west_attacks);

            // Promotion Captures
            if ([new_square]() {
                if constexpr (color == WHITE) return new_square >= 56;
                else return new_square <= 7;
            }) {
                for (PromotionType promotionType : {PROMOTION_KNIGHT, PROMOTION_BISHOP, PROMOTION_ROOK, PROMOTION_QUEEN}) {
                    current_scored_moves.push_back({
                        Move(new_square + down + EAST, new_square, MOVE_TYPE_PROMOTION, promotionType),
                        0
                    });
                }

                continue;
            }

            current_scored_moves.push_back({
                Move(new_square + down + EAST, new_square),
                0
            });
        }

        while (east_attacks) {
            const Square new_square = poplsb(east_attacks);

            // Promotion Captures
            if ([new_square]() {
                if constexpr (color == WHITE) return new_square >= 56;
                else return new_square <= 7;
            }) {
                for (PromotionType promotionType : {PROMOTION_KNIGHT, PROMOTION_BISHOP, PROMOTION_ROOK, PROMOTION_QUEEN}) {
                    current_scored_moves.push_back({
                        Move(new_square + down + WEST, new_square, MOVE_TYPE_PROMOTION, promotionType),
                        0
                    });
                }

                continue;
            }

            current_scored_moves.push_back({
                Move(new_square + down + WEST, new_square),
                0
            });
        }

        // En Passant Code
        if (ep_square != NO_SQUARE) {
            BITBOARD ep_pawns = [this]() {
                if constexpr (color == WHITE) BLACK_PAWN_ATTACKS[ep_square];
                else WHITE_PAWN_ATTACKS[ep_square];
            } & pawns;

            while (ep_pawns) {
                Square square = poplsb(ep_pawns);
                current_scored_moves.push_back({
                    Move(square, ep_square, MOVE_TYPE_EP),
                    0
                });
            }
        }

        // Pawn Pushes
        BITBOARD single_pushes = pawn_forward_squares & empty_squares;
        BITBOARD double_pushes = [single_pushes]() {
            if constexpr (color == WHITE) return shift<NORTH>(single_pushes) & MASK_RANK[RANK_4];
            else return shift<SOUTH>(single_pushes) & MASK_RANK[RANK_5];
        } & empty_squares;

        while (single_pushes) {
            const Square new_square = poplsb(single_pushes);

            // Single Push Promotions
            if ([new_square]() {
                if constexpr (color == WHITE) return new_square >= 56;
                else return new_square <= 7;
            }) {
                for (PromotionType promotionType: {PROMOTION_KNIGHT, PROMOTION_BISHOP, PROMOTION_ROOK, PROMOTION_QUEEN}) {
                    current_scored_moves.push_back({
                        Move(new_square + down, new_square, MOVE_TYPE_PROMOTION,
                             promotionType),
                        0
                    });
                }
                continue;
            }

            // Single Pushes
            current_scored_moves.push_back({
                Move(new_square + down, new_square),
                0
            });
        }

        while (double_pushes) {
            Square new_square = poplsb(double_pushes);
            current_scored_moves.push_back({
                Move(new_square + down + down, new_square),
                0
            });

        }
    }

    template<Color color>
    void get_rook_moves(FixedVector<ScoredMove, MAX_MOVES>& current_scored_moves) const {
        BITBOARD rooks = get_pieces(ROOK, side);
        Square king_pos = get_king_pos(side);

        while (rooks) {
            Square square = poplsb(rooks);

            BITBOARD rook_attacks = get_rook_attacks(square, all_pieces);
            BITBOARD rook_moves = rook_attacks & (~our_pieces);

            while (rook_moves) {
                Square new_square = poplsb(rook_moves);
                current_scored_moves.push_back({
                                                       Move(square, new_square),
                                                       0
                                               });
            }

            // -- Generate Castling moves --
            if (!(rook_attacks & get_pieces(KING, side))) continue;  // Guard clause

            Square starting_rook_pos_k = starting_rook_pos[side][0];
            Square starting_rook_pos_q = starting_rook_pos[side][1];

            Square target_pos_k = side == WHITE ? g1 : g8;
            Square target_pos_q = side == WHITE ? c1 : c8;

            Square important_pos_k = target_pos_k + WEST;  // F1 square for FRC (the square the rook will go to)
            Square important_pos_q = target_pos_q + EAST;  // D1 square for FRC (the square the rook will go to)

            // King side Castling

            if ([this, square, starting_rook_pos_k](){
                if (square != starting_rook_pos_k) return false;

                if constexpr (color == WHITE) return (castle_ability_bits & 1) == 1;
                else return (castle_ability_bits & 4) == 4;
            }) {

                // FRC Castling Cases
                if (fischer_random_chess) {
                    // The rook is to the right of the F1 square
                    if (king_pos > important_pos_k) {
                        // Ensure that the rook can go to the F1 square
                        if (board[important_pos_k] != EMPTY) continue;
                    }

                    // The rook is to the left of the F1 square
                    else if (king_pos < important_pos_k) {
                        // Ensure that all the squares between the rook's square,
                        // and its current square are empty
                        bool flag = false;
                        for (int temp_square = target_pos_k; temp_square > static_cast<int>(square); temp_square--) {
                            if (board[temp_square] != EMPTY) {
                                flag = true;
                                break;
                            }
                        }

                        if (flag) continue;
                    }
                }
                current_scored_moves.push_back({
                    Move(king_pos, target_pos_k, MOVE_TYPE_CASTLE),
                    0
                });
            }

            // Queen side Castling
            else if ([this, square, starting_rook_pos_q](){
                if (square != starting_rook_pos_q) return false;

                if constexpr (color == WHITE) return (castle_ability_bits & 2) == 2;
                else return (castle_ability_bits & 8) == 8;
            }) {

                // FRC Castling Cases
                if (fischer_random_chess) {

                    // The rook is to the left of the D1 square
                    if (king_pos < important_pos_q) {
                        // Guard certain cases
                        if (board[target_pos_q] != EMPTY && board[target_pos_q] != WHITE_KING) continue;
                        if (board[important_pos_q] != EMPTY) continue;
                    }

                    // The rook is to the right of the D1 square
                    else if (king_pos > important_pos_q){
                        // Ensure that all the squares between the rook's square,
                        // and its current square are empty
                        bool flag = false;
                        for (int temp_square = target_pos_q; temp_square < static_cast<int>(square); temp_square++) {
                            if (board[temp_square] != EMPTY) {
                                flag = true;
                                break;
                            }
                        }

                        if (flag) continue;
                    }
                }

                current_scored_moves.push_back({
                    Move(king_pos, target_pos_q, MOVE_TYPE_CASTLE),
                    0
                });
            }
        }
    }

    template<Color color>
    inline void get_pseudo_legal_captures(FixedVector<ScoredMove, MAX_MOVES>& current_scored_moves) const {
        current_scored_moves.clear();
        get_pawn_captures<color>(current_scored_moves);
        get_knight_captures(current_scored_moves);
        get_bishop_captures(current_scored_moves);
        get_rook_captures(current_scored_moves);
        get_queen_captures(current_scored_moves);
        get_king_captures(current_scored_moves);
    }

    template<Color color>
    inline void get_pseudo_legal_moves(FixedVector<ScoredMove, MAX_MOVES>& current_scored_moves) const {
        current_scored_moves.clear();
        get_pawn_moves<color>(current_scored_moves);
        get_knight_moves(current_scored_moves);
        get_bishop_moves(current_scored_moves);
        get_rook_moves<color>(current_scored_moves);
        get_queen_moves(current_scored_moves);
        get_king_moves(current_scored_moves);
    }

};





#endif //ALTAIR_POSITION_H
