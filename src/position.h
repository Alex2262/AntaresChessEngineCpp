
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

struct NNUpdate {
    Piece  piece;
    Square square;
};

struct KingBucketUpdate {
    bool update_necessary = false;
    Color side = WHITE;
    int bucket = 0;
};

struct State {
    HASH_TYPE current_hash_key = 0ULL;
    HASH_TYPE current_pawn_hash_key = 0ULL;
    HASH_TYPE current_np_hash_key = 0ULL;
    HASH_TYPE current_major_hash_key = 0ULL;
    HASH_TYPE current_minor_hash_key = 0ULL;
    HASH_TYPE current_non_major_hash_key = 0ULL;

    BITBOARD threats = 0ULL;

    Square current_ep_square = NO_SQUARE;
    uint8_t current_castle_ability_bits = 0;
    PLY_TYPE current_fifty_move = 0;

    InformativeMove move = NO_INFORMATIVE_MOVE;
    Move excluded_move = NO_MOVE;

    SCORE_TYPE static_eval = NO_EVALUATION;

    int double_extensions = 0;
    int in_check = -1;

    FixedVector<NNUpdate, 4> activations{};
    FixedVector<NNUpdate, 4> deactivations{};
    KingBucketUpdate king_bucket_update{};
    bool NNUE_pushed = false;
};

struct FenInfo {
    PLY_TYPE fifty_move_counter = 0;
    PLY_TYPE full_move_counter  = 1;
};

enum class Movegen : uint16_t {
    All,
    Noisy,
    Qsearch,
    Quiet
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
    BITBOARD threats{};

    BITBOARD pieces[12]{};

    Piece board[64]{};

    Color side = WHITE;

    uint8_t castle_ability_bits = 0;
    Square starting_rook_pos[2][2]{};

    Square ep_square = NO_SQUARE;

    HASH_TYPE hash_key           = 0;
    HASH_TYPE pawn_hash_key      = 0;
    HASH_TYPE np_hash_key        = 0;
    HASH_TYPE major_hash_key     = 0;
    HASH_TYPE minor_hash_key     = 0;
    HASH_TYPE non_major_hash_key = 0;

    std::array<State, TOTAL_MAX_DEPTH> state_stack{};

    std::array<FixedVector<ScoredMove, MAX_MOVES>, TOTAL_MAX_DEPTH> scored_moves{};

    void clear_state_stack();
    void set_state(State& state, PLY_TYPE fifty_move) const;

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

    FenInfo set_fen(const std::string& fen);
    std::string get_fen(PLY_TYPE fifty_move);

    void set_frc_side(Color color, int index);
    void set_dfrc(int index);

    void ensure_stable();

    void compute_threats();

    friend std::ostream& operator<<(std::ostream& os, const Position& position);

    template<Movegen movegen>
    inline void get_pawn_moves(FixedVector<ScoredMove, MAX_MOVES>& current_scored_moves) const {

        BITBOARD pawns = get_pieces(PAWN, side);
        BITBOARD pawn_forward_squares = side == WHITE ? shift<NORTH>(pawns) : shift<SOUTH>(pawns);
        Direction down = side == WHITE ? SOUTH : NORTH;

        if constexpr (movegen == Movegen::Qsearch) {
            pawn_forward_squares &= ~(MASK_RANK[RANK_1] | MASK_RANK[RANK_8]);
        }

        BITBOARD west_attacks = shift<WEST>(pawn_forward_squares) & opp_pieces;
        BITBOARD east_attacks = shift<EAST>(pawn_forward_squares) & opp_pieces;

        if constexpr (movegen != Movegen::Quiet) {
            while (west_attacks) {
                Square new_square = poplsb(west_attacks);

                // Promotion Captures
                if constexpr (movegen != Movegen::Qsearch) {
                    bool promotion = (side == WHITE && new_square >= 56) || (side == BLACK && new_square <= 7);

                    if (promotion) {
                        for (PromotionType promotionType : {PROMOTION_KNIGHT, PROMOTION_BISHOP, PROMOTION_ROOK, PROMOTION_QUEEN}) {
                            current_scored_moves.push_back({
                                                                   Move(new_square + down + EAST, new_square,
                                                                        MOVE_TYPE_PROMOTION, promotionType),
                                                                   0
                                                           });
                        }

                        continue;
                    }
                }

                // Regular Pawn Captures
                current_scored_moves.push_back({
                                                       Move(new_square + down + EAST, new_square),
                                                       0
                                               });
            }

            while (east_attacks) {
                Square new_square = poplsb(east_attacks);

                // Promotion Captures
                if constexpr (movegen != Movegen::Qsearch) {
                    bool promotion = (side == WHITE && new_square >= 56) || (side == BLACK && new_square <= 7);

                    if (promotion) {
                        for (PromotionType promotionType: {PROMOTION_KNIGHT, PROMOTION_BISHOP, PROMOTION_ROOK,
                                                           PROMOTION_QUEEN}) {
                            current_scored_moves.push_back({
                                                                   Move(new_square + down + WEST, new_square,
                                                                        MOVE_TYPE_PROMOTION, promotionType),
                                                                   0
                                                           });
                        }

                        continue;
                    }
                }

                // Regular Pawn Captures
                current_scored_moves.push_back({
                                                       Move(new_square + down + WEST, new_square),
                                                       0
                                               });
            }

            // En passant code
            if constexpr (movegen != Movegen::Qsearch) {
                if (ep_square != NO_SQUARE) {
                    BITBOARD ep_pawns =
                            (side == WHITE ? BLACK_PAWN_ATTACKS[ep_square] : WHITE_PAWN_ATTACKS[ep_square]) & pawns;

                    while (ep_pawns) {
                        Square square = poplsb(ep_pawns);
                        current_scored_moves.push_back({
                                                               Move(square, ep_square, MOVE_TYPE_EP),
                                                               0
                                                       });
                    }
                }
            }
        }

        if constexpr (movegen == Movegen::Noisy || movegen == Movegen::Qsearch) return;

        // Pawn Pushes
        BITBOARD single_pushes = pawn_forward_squares & empty_squares;
        BITBOARD double_pushes = (side == WHITE ? shift<NORTH>(single_pushes) & MASK_RANK[RANK_4]
                                                : shift<SOUTH>(single_pushes) & MASK_RANK[RANK_5]) & empty_squares;

        while (single_pushes) {
            Square new_square = poplsb(single_pushes);
            bool promotion = (side == WHITE && new_square >= 56) || (side == BLACK && new_square <= 7);

            // Single Push Promotions
            if (promotion) {
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

    template<Movegen movegen>
    void get_knight_moves(FixedVector<ScoredMove, MAX_MOVES>& current_scored_moves) const {
        BITBOARD knights = get_pieces(KNIGHT, side);

        const BITBOARD mask = [this]() -> BITBOARD {
            if constexpr (movegen == Movegen::Quiet) return empty_squares;
            else if constexpr (movegen == Movegen::All) return ~(our_pieces);
            return opp_pieces;
        }();

        while (knights) {
            Square square = poplsb(knights);

            BITBOARD bitboard = KNIGHT_ATTACKS[square] & mask;
            while (bitboard) {
                Square new_square = poplsb(bitboard);
                current_scored_moves.push_back({
                                                       Move(square, new_square),
                                                       0
                                               });
            }
        }
    }

    template<Movegen movegen>
    void get_bishop_moves(FixedVector<ScoredMove, MAX_MOVES>& current_scored_moves) const {
        BITBOARD bishops = get_pieces(BISHOP, side);

        const BITBOARD mask = [this]() -> BITBOARD {
            if constexpr (movegen == Movegen::Quiet) return empty_squares;
            else if constexpr (movegen == Movegen::All) return ~(our_pieces);
            return opp_pieces;
        }();

        while (bishops) {
            Square square = poplsb(bishops);

            BITBOARD bishop_attacks = get_bishop_attacks(square, all_pieces);
            BITBOARD bishop_moves = bishop_attacks & mask;

            while (bishop_moves) {
                Square new_square = poplsb(bishop_moves);
                current_scored_moves.push_back({
                                                       Move(square, new_square),
                                                       0
                                               });
            }
        }
    }

    template<Movegen movegen>
    void get_rook_moves(FixedVector<ScoredMove, MAX_MOVES>& current_scored_moves) const {
        BITBOARD rooks = get_pieces(ROOK, side);
        Square king_pos = get_king_pos(side);

        const BITBOARD mask = [this]() -> BITBOARD {
            if constexpr (movegen == Movegen::Quiet) return empty_squares;
            else if constexpr (movegen == Movegen::All) return ~(our_pieces);
            return opp_pieces;
        }();

        while (rooks) {
            Square square = poplsb(rooks);

            BITBOARD rook_attacks = get_rook_attacks(square, all_pieces);
            BITBOARD rook_moves = rook_attacks & mask;

            while (rook_moves) {
                Square new_square = poplsb(rook_moves);
                current_scored_moves.push_back({
                                                       Move(square, new_square),
                                                       0
                                               });
            }

            if constexpr (movegen == Movegen::Qsearch || movegen == Movegen::Noisy) continue;

            // -- Generate Castling moves --
            if (!(rook_attacks & get_pieces(KING, side))) continue;  // Guard clause

            Square starting_rook_pos_k = starting_rook_pos[side][0];
            Square starting_rook_pos_q = starting_rook_pos[side][1];

            Square target_pos_k = side == WHITE ? g1 : g8;
            Square target_pos_q = side == WHITE ? c1 : c8;

            Square important_pos_k = target_pos_k + WEST;  // F1 square for FRC (the square the rook will go to)
            Square important_pos_q = target_pos_q + EAST;  // D1 square for FRC (the square the rook will go to)

            // King side Castling

            if (((side == WHITE && (castle_ability_bits & 1) == 1) || (side == BLACK && (castle_ability_bits & 4) == 4))
                && square == starting_rook_pos_k) {
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
            else if (((side == WHITE && (castle_ability_bits & 2) == 2) || (side == BLACK && (castle_ability_bits & 8) == 8))
                     && square == starting_rook_pos_q) {

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

    template<Movegen movegen>
    void get_queen_moves(FixedVector<ScoredMove, MAX_MOVES>& current_scored_moves) const {
        BITBOARD queens = get_pieces(QUEEN, side);

        const BITBOARD mask = [this]() -> BITBOARD {
            if constexpr (movegen == Movegen::Quiet) return empty_squares;
            else if constexpr (movegen == Movegen::All) return ~(our_pieces);
            return opp_pieces;
        }();

        while (queens) {
            Square square = poplsb(queens);

            BITBOARD queen_attacks = get_queen_attacks(square, all_pieces);
            BITBOARD queen_moves = queen_attacks & mask;

            while (queen_moves) {
                Square new_square = poplsb(queen_moves);
                current_scored_moves.push_back({
                                                       Move(square, new_square),
                                                       0
                                               });
            }
        }
    }

    template<Movegen movegen>
    void get_king_moves(FixedVector<ScoredMove, MAX_MOVES>& current_scored_moves) const {
        Square square = get_king_pos(side);

        const BITBOARD mask = [this]() -> BITBOARD {
            if constexpr (movegen == Movegen::Quiet) return empty_squares;
            else if constexpr (movegen == Movegen::All) return ~(our_pieces);
            return opp_pieces;
        }();

        BITBOARD king_moves = KING_ATTACKS[square] & mask;
        while (king_moves) {
            Square new_square = poplsb(king_moves);
            current_scored_moves.push_back({
                                                   Move(square, new_square),
                                                   0
                                           });
        }
    }

    template<Movegen movegen, bool clear>
    void get_pseudo_legal_moves(FixedVector<ScoredMove, MAX_MOVES>& current_scored_moves) const {
        if constexpr (clear) current_scored_moves.clear();

        get_pawn_moves<movegen>(current_scored_moves);
        get_knight_moves<movegen>(current_scored_moves);
        get_bishop_moves<movegen>(current_scored_moves);
        get_rook_moves<movegen>(current_scored_moves);
        get_queen_moves<movegen>(current_scored_moves);
        get_king_moves<movegen>(current_scored_moves);
    }

    bool is_pseudo_legal(Move move);

    void make_null_move(State& state, PLY_TYPE& fifty_move);
    void undo_null_move(State& state, PLY_TYPE& fifty_move);

    template<bool NNUE>
    bool make_move(Move move, State& state, PLY_TYPE& fifty_move);
    template<bool NNUE>
    void undo_move(Move move, State& state, PLY_TYPE& fifty_move);

    void update_nnue(State& state);
};





#endif //ALTAIR_POSITION_H
