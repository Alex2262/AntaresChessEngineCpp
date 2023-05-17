
//
// Created by Alex Tian on 9/22/2022.
//

#include <chrono>
#include <iostream>
#include <cstring>
#include <cmath>
#include <algorithm>
#include <cassert>
#include "search.h"
#include "evaluation.h"
#include "evaluation_constants.h"
#include "move.h"
#include "useful.h"
#include "see.h"


static double LMR_REDUCTIONS_QUIET[MAX_AB_DEPTH + 1][64];
static double LMR_REDUCTIONS_NOISY[MAX_AB_DEPTH + 1][64];

void initialize_lmr_reductions(Engine& engine) {
    for (PLY_TYPE depth = 0; depth <= MAX_AB_DEPTH; depth++) {
        for (int moves = 0; moves <= MAX_AB_DEPTH; moves++) {
            LMR_REDUCTIONS_QUIET[depth][moves] =
                    std::max(0.0,
                             std::log(depth) * std::log(moves) / double(engine.tuning_parameters.LMR_divisor_quiet / 100.0)
                             + double(engine.tuning_parameters.LMR_base_quiet / 100.0));
            LMR_REDUCTIONS_NOISY[depth][moves] =
                    std::max(0.0,
                             std::log(depth) * std::log(moves) / double(engine.tuning_parameters.LMR_divisor_noisy / 100.0)
                             + double(engine.tuning_parameters.LMR_base_noisy / 100.0));
        }
    }
}


void Engine::clear_tt() {
    for (TT_Entry& tt_entry : transposition_table) {
        tt_entry.key = 0;
        tt_entry.score = 0;
        tt_entry.evaluation = NO_EVALUATION;
        tt_entry.move = NO_MOVE;
        tt_entry.depth = 0;
        tt_entry.flag = 0;
    }
}


void Engine::reset() {
    node_count = 0;
    selective_depth = 0;
    current_search_depth = 0;
    search_ply = 0;

    std::memset(pv_length, 0, sizeof(pv_length));
    std::memset(pv_table, 0, sizeof(pv_table));

    std::memset(killer_moves, 0, sizeof(killer_moves));
    //std::memset(history_moves, 0, sizeof(history_moves));
    //std::memset(capture_history, 0, sizeof(capture_history));
    std::memset(continuation_history, 0, sizeof(continuation_history));

    if (show_stats) {
        search_results.alpha_raised_count = 0;
        std::memset(search_results.qsearch_fail_highs, 0, sizeof(search_results.qsearch_fail_highs));
        std::memset(search_results.search_fail_highs, 0, sizeof(search_results.search_fail_highs));
        std::memset(search_results.search_fail_high_types, 0, sizeof(search_results.search_fail_high_types));
    }
}


void Engine::new_game() {
    reset();

    std::memset(repetition_table, 0, sizeof(repetition_table));
    std::memset(pv_length, 0, sizeof(pv_length));
    std::memset(pv_table, 0, sizeof(pv_table));

    std::memset(killer_moves, 0, sizeof(killer_moves));
    std::memset(history_moves, 0, sizeof(history_moves));
    std::memset(capture_history, 0, sizeof(capture_history));
    std::memset(continuation_history, 0, sizeof(continuation_history));
    clear_tt();

    game_ply = 0;
    fifty_move = 0;

    stopped = true;
}


bool Engine::detect_repetition() {

    for (int i = game_ply - 2; i >= game_ply - fifty_move; i--) {
        if (repetition_table[i] == repetition_table[game_ply]) {
            return true;
        }
    }

    return false;
}


short Engine::probe_tt_entry(HASH_TYPE hash_key, SCORE_TYPE alpha, SCORE_TYPE beta, PLY_TYPE depth,
                             TT_Entry& return_entry) {
    TT_Entry& tt_entry = transposition_table[hash_key % transposition_table.size()];

    if (tt_entry.key == hash_key) {
        return_entry.move = tt_entry.move;
        return_entry.depth = tt_entry.depth;
        return_entry.flag = tt_entry.flag;

        return_entry.score = tt_entry.score;
        if (return_entry.score < -MATE_BOUND) return_entry.score += search_ply;
        else if (return_entry.score > MATE_BOUND) return_entry.score -= search_ply;

        if (tt_entry.depth >= depth) {

            if (tt_entry.flag == HASH_FLAG_EXACT) return RETURN_HASH_SCORE;
            if (tt_entry.flag == HASH_FLAG_ALPHA && return_entry.score <= alpha) return RETURN_HASH_SCORE;
            if (tt_entry.flag == HASH_FLAG_BETA && return_entry.score >= beta) return RETURN_HASH_SCORE;
        }

        return USE_HASH_MOVE;
    }

    return NO_HASH_ENTRY;
}

void Engine::record_tt_entry(HASH_TYPE hash_key, SCORE_TYPE score, short tt_flag, MOVE_TYPE move, PLY_TYPE depth,
                             SCORE_TYPE static_eval) {

    TT_Entry& tt_entry = transposition_table[hash_key % transposition_table.size()];

    if (score < -MATE_BOUND) score -= search_ply;
    else if (score > MATE_BOUND) score += search_ply;

    if (tt_entry.key != hash_key || depth + 2 > tt_entry.depth || tt_flag == HASH_FLAG_EXACT) {
        tt_entry.key = hash_key;
        tt_entry.depth = depth;
        tt_entry.flag = tt_flag;
        tt_entry.score = score;
        tt_entry.evaluation = static_eval;
        tt_entry.move = move;
    }
}


short Engine::probe_tt_entry_q(HASH_TYPE hash_key, SCORE_TYPE alpha, SCORE_TYPE beta,
                               SCORE_TYPE& return_score, MOVE_TYPE& tt_move) {
    TT_Entry& tt_entry = transposition_table[hash_key % transposition_table.size()];

    if (tt_entry.key == hash_key) {
        tt_move = tt_entry.move;

        return_score = tt_entry.score;
        if (return_score < -MATE_BOUND) return_score += search_ply;
        else if (return_score > MATE_BOUND) return_score -= search_ply;

        if (tt_entry.flag == HASH_FLAG_EXACT) return RETURN_HASH_SCORE;
        if (tt_entry.flag == HASH_FLAG_ALPHA && return_score <= alpha) return RETURN_HASH_SCORE;
        if (tt_entry.flag == HASH_FLAG_BETA && return_score >= beta) return RETURN_HASH_SCORE;


        return USE_HASH_MOVE;
    }

    return NO_HASH_ENTRY;
}


void Engine::record_tt_entry_q(HASH_TYPE hash_key, SCORE_TYPE score, short tt_flag, MOVE_TYPE move,
                               SCORE_TYPE static_eval) {
    TT_Entry& tt_entry = transposition_table[hash_key % transposition_table.size()];

    if (score < -MATE_BOUND) score -= search_ply;
    else if (score > MATE_BOUND) score += search_ply;

    if (tt_entry.key != hash_key || tt_flag == HASH_FLAG_EXACT) { // TODO: try doing || depth == -1 for always replace in qs.
        tt_entry.key = hash_key;
        tt_entry.depth = -1;
        tt_entry.flag = tt_flag;
        tt_entry.score = score;
        tt_entry.evaluation = static_eval;
        tt_entry.move = move;
    }
}


SCORE_TYPE Engine::probe_tt_evaluation(HASH_TYPE hash_key) {
    TT_Entry& tt_entry = transposition_table[hash_key % transposition_table.size()];

    if (tt_entry.key == hash_key && tt_entry.evaluation != NO_EVALUATION) return tt_entry.evaluation;
    return NO_EVALUATION;
}

void Engine::tt_prefetch_read(HASH_TYPE hash_key) {
    __builtin_prefetch(&transposition_table[hash_key % transposition_table.size()]);
}

void Engine::tt_prefetch_write(HASH_TYPE hash_key) {
    __builtin_prefetch(&transposition_table[hash_key % transposition_table.size()], 1);
}


void update_history_entry(SCORE_TYPE& score, SCORE_TYPE bonus) {
    score -= (score * abs(bonus)) / 324;
    score += bonus * 32;
}


SCORE_TYPE qsearch(Engine& engine, Position& position, SCORE_TYPE alpha, SCORE_TYPE beta, PLY_TYPE depth) {

    engine.tt_prefetch_read(position.hash_key);

    // position.print_board();
    engine.node_count++;
    // engine.selective_depth = std::max(engine.search_ply, engine.selective_depth);

    // Check time and max nodes
    if (engine.current_search_depth >= engine.min_depth && (engine.node_count & 2047) == 0 ) {
        auto time = std::chrono::high_resolution_clock::now();
        uint64_t current_time = std::chrono::duration_cast<std::chrono::milliseconds>
                (std::chrono::time_point_cast<std::chrono::milliseconds>(time).time_since_epoch()).count();

        if (current_time - engine.start_time >= engine.hard_time_limit) {
            engine.stopped = true;
        }
    }

    if (engine.max_nodes && engine.node_count >= engine.max_nodes) engine.stopped = true;

    SCORE_TYPE tt_value = 0;
    MOVE_TYPE tt_move = NO_MOVE;
    short tt_return_type = engine.probe_tt_entry_q(position.hash_key, alpha, beta, tt_value, tt_move);

    if (tt_return_type == RETURN_HASH_SCORE) {
        return tt_value;
    }

    SCORE_TYPE static_eval = engine.probe_tt_evaluation(position.hash_key);
    if (static_eval == NO_EVALUATION) static_eval = evaluate(position);
    // SCORE_TYPE static_eval = evaluate(position);

    if (depth == 0 || static_eval >= beta) return static_eval;

    // Variable to record the hash flag
    short tt_hash_flag = HASH_FLAG_ALPHA;

    alpha = (static_eval > alpha) ? static_eval : alpha;

    // Set values for State
    position.set_state(engine.search_ply, engine.fifty_move);
    position.state_stack[engine.search_ply].evaluation = static_eval;

    position.get_pseudo_legal_captures(engine.search_ply);
    get_capture_scores(engine, position, position.moves[engine.search_ply], position.move_scores[engine.search_ply], tt_move);

    SCORE_TYPE best_score = static_eval;
    MOVE_TYPE best_move = NO_MOVE;

    int legal_moves = 0;
    for (int move_index = 0; move_index < static_cast<int>(position.moves[engine.search_ply].size()); move_index++) {

        sort_next_move(position.moves[engine.search_ply], position.move_scores[engine.search_ply], move_index);
        MOVE_TYPE move = position.moves[engine.search_ply][move_index];

        // Delta / Futility pruning
        // If the piece we capture plus a margin cannot even improve our score then
        // there is no point in searching it
        if (static_eval + PIECE_VALUES_MID[get_occupied(move) % BLACK_PAWN] +
            engine.tuning_parameters.delta_margin < alpha) {
            continue;
        }

        // SEE pruning
        if (static_eval + 60 <= alpha && !get_static_exchange_evaluation(position, move, 1)) {
            best_score = std::max(best_score, static_eval + 60);
            continue;
        }

        bool attempt = position.make_move(move, engine.search_ply, engine.fifty_move);

        if (!attempt) {
            position.undo_move(move, engine.search_ply, engine.fifty_move);
            continue;
        }

        engine.search_ply++;
        position.side ^= 1;

        SCORE_TYPE return_eval = -qsearch(engine, position, -beta, -alpha, depth - 1);

        position.side ^= 1;
        engine.search_ply--;

        position.undo_move(move, engine.search_ply, engine.fifty_move);

        if (engine.stopped) return 0;

        legal_moves++;

        if (return_eval > best_score) {
            best_score = return_eval;
            best_move = move;

            if (return_eval > alpha) {
                alpha = return_eval;
                tt_hash_flag = HASH_FLAG_EXACT;

                // Captures History Heuristic for move ordering
                SCORE_TYPE bonus = 2;
                update_history_entry(engine.capture_history[get_selected(move)]
                                     [get_occupied(move)][MAILBOX_TO_STANDARD[get_target_square(move)]],
                                     bonus);

                if (return_eval >= beta) {
                    if (engine.show_stats) {
                        if (legal_moves <= FAIL_HIGH_STATS_COUNT) {
                            engine.search_results.qsearch_fail_highs[legal_moves - 1]++;
                        }
                    }

                    engine.record_tt_entry_q(position.hash_key, best_score, HASH_FLAG_BETA, best_move, static_eval  );
                    return best_score;
                }
            }
        }

    }

    engine.tt_prefetch_write(position.hash_key);
    engine.record_tt_entry_q(position.hash_key, best_score, tt_hash_flag, best_move, static_eval);

    return best_score;
}


SCORE_TYPE negamax(Engine& engine, Position& position, SCORE_TYPE alpha, SCORE_TYPE beta, PLY_TYPE depth, bool do_null) {

    engine.tt_prefetch_read(position.hash_key);

    // Initialize PV length
    engine.pv_length[engine.search_ply] = engine.search_ply;
    engine.selective_depth = std::max(engine.search_ply, engine.selective_depth);

    bool root = !engine.search_ply;

    // Early search exits
    if (!root) {

        if (engine.search_ply >= MAX_AB_DEPTH - 1) return evaluate(position);

        // Detect repetitions and fifty move rule
        if (engine.fifty_move >= 100 || engine.detect_repetition()) return 3 - static_cast<int>(engine.node_count & 8);

        // Mate Distance Pruning from CPW
        SCORE_TYPE mating_value = MATE_SCORE - engine.search_ply;
        if (mating_value < beta) {
            beta = mating_value;
            if (alpha >= mating_value) return mating_value;
        }
        mating_value = -MATE_SCORE + engine.search_ply;
        if (mating_value > alpha) {
            alpha = mating_value;
            if (beta <= mating_value) return mating_value;
        }

    }

    bool in_check;
    if (position.state_stack[engine.search_ply].in_check != -1) {
        in_check = static_cast<bool>(position.state_stack[engine.search_ply].in_check);
    } else {
        in_check = position.is_attacked(position.king_positions[position.side]);
    }

    if (in_check) depth++;  // Check extension

    // Start quiescence search at the start of regular negamax search to counter the horizon effect.
    if (depth <= 0) {
        return qsearch(engine, position, alpha, beta, engine.max_q_depth);
    }

    // Increase node count
    engine.node_count++;

    // Set values for State
    position.set_state(engine.search_ply, engine.fifty_move);

    // TT probing
    TT_Entry tt_entry{};
    short tt_return_type = engine.probe_tt_entry(position.hash_key, alpha, beta, depth, tt_entry);

    SCORE_TYPE tt_value = tt_entry.score;
    MOVE_TYPE tt_move = tt_entry.move;

    // We are allowed to return the hash score
    if (tt_return_type == RETURN_HASH_SCORE && !root) {
        bool return_tt_value = true;

        // We have to check if there's a repetition before returning a score
        if (tt_move) {
            position.make_move(tt_move, engine.search_ply, engine.fifty_move);

            engine.search_ply++;
            engine.fifty_move++;
            engine.game_ply++;
            engine.repetition_table[engine.game_ply] = position.hash_key;
            position.side ^= 1;

            if (engine.fifty_move >= 100 || engine.detect_repetition()) return_tt_value = false;

            position.side ^= 1;
            engine.game_ply--;
            engine.search_ply--;

            position.undo_move(tt_move, engine.search_ply, engine.fifty_move);
        }
        else return_tt_value = false;

        if (return_tt_value) {
            return tt_value;
        }
    }

    // Variable to record the hash flag
    short tt_hash_flag = HASH_FLAG_ALPHA;

    // Hack to determine pv_node, because when it is not a pv node we are being searched by
    // a zero window with alpha == beta - 1
    bool pv_node = alpha != beta - 1;
    bool null_search = !do_null && !root;

    // if (!pv_node) std::cout << alpha << " " << beta << std::endl;

    // Get evaluation
    SCORE_TYPE static_eval = engine.probe_tt_evaluation(position.hash_key);
    if (static_eval == NO_EVALUATION) static_eval = evaluate(position);
    position.state_stack[engine.search_ply].evaluation = static_eval;

    // The "improving" heuristic is when the current position has a better static evaluation than the evaluation
    // from a full-move or two plies ago. When this is true, we can be more aggressive with
    // beta-reductions (eval is too high) as we will have more certainty that the position is better;
    // however, we should be less aggressive with alpha-reductions (eval is too low) as we have less certainty
    // that the position is awful.
    bool improving = false;

    if (!in_check && engine.search_ply >= 2) {
        SCORE_TYPE past_eval = position.state_stack[engine.search_ply - 2].evaluation;
        if (past_eval != NO_EVALUATION && static_eval > past_eval) improving = true;
    }

    // Internal Iterative Reduction. Rebel's idea
    if (tt_move == NO_MOVE) {
        if (depth >= 4) depth--;
        if (depth >= 8) depth--;
    }

    if (!pv_node && !in_check) {

        // Reverse Futility Pruning
        // If the last move was very bad, such that the static evaluation - a margin is still greater
        // than the opponent's best score, then return the static evaluation.

        if (depth <= engine.tuning_parameters.RFP_depth && static_eval -
            engine.tuning_parameters.RFP_margin * (depth - improving) >= beta) {
            return static_eval;
        }

        // Razoring
        // If the evaluation is really low, and we cannot improve alpha even with qsearch, then return.
        // If the evaluation is improving then increase the margin because we are less certain that the
        // position is terrible.
        constexpr int razoring_margins[3] = {0, 320, 800};
        if (depth <= 2 && static_eval + razoring_margins[depth] + improving * 120 < alpha) {
            SCORE_TYPE return_eval = qsearch(engine, position, alpha, beta, engine.max_q_depth);
            if (return_eval < alpha) return return_eval;
        }

        // Null move pruning
        // We give the opponent an extra move and if they are not able to make their position
        // any better, then our position is too good, and we don't need to search any deeper.
        if (depth >= engine.tuning_parameters.NMP_depth && do_null && static_eval >= beta &&
            position.non_pawn_material_count >= 1 + (depth >= 10)) {

            // Adaptive NMP
            int reduction = engine.tuning_parameters.NMP_base +
                            depth / engine.tuning_parameters.NMP_depth_divisor +
                            std::min(3, (static_eval - beta) / engine.tuning_parameters.NMP_eval_divisor);

            position.make_null_move(engine.search_ply, engine.fifty_move);

            engine.search_ply++;
            engine.game_ply++;

            // zero window search with reduced depth
            SCORE_TYPE return_eval = -negamax(engine, position, -beta, -beta + 1, depth - reduction, false);

            engine.game_ply--;
            engine.search_ply--;
            position.undo_null_move(engine.search_ply, engine.fifty_move);

            if (return_eval >= beta) {
                return beta;
            }
            else {
                if (depth >= 8 && return_eval <= -MATE_BOUND && position.non_pawn_material_count >= (1 + depth / 8)) {
                    depth++;
                }
            }
        }
    }

    if (pv_node && depth >= 4 && tt_move == NO_MOVE) {
        negamax(engine, position, alpha, beta, static_cast<PLY_TYPE>(depth - 3), true);  // TODO: test no null moves here
        tt_move = engine.transposition_table[position.hash_key % engine.transposition_table.size()].move;
    }

    int legal_moves = 0;
    MOVE_TYPE last_move_one = engine.search_ply >= 1 ? position.state_stack[engine.search_ply - 1].move : NO_MOVE;
    MOVE_TYPE last_move_two = engine.search_ply >= 2 ? position.state_stack[engine.search_ply - 2].move : NO_MOVE;

    bool recapture_found = false;

    // Retrieving the pseudo legal moves in the current position as a list of integers
    // Score the moves
    position.get_pseudo_legal_moves(engine.search_ply);
    get_move_scores(engine, position, position.moves[engine.search_ply], position.move_scores[engine.search_ply],
                    tt_move, last_move_one, last_move_two);

    // Best score for fail soft, and best move for tt
    SCORE_TYPE best_score = -SCORE_INF;
    MOVE_TYPE best_move = NO_MOVE;

    int alpha_raised_count = 0;

    // Iterate through moves and recursively search with Negamax
    for (int move_index = 0; move_index < static_cast<int>(position.moves[engine.search_ply].size()); move_index++) {

        // Sort the next move. If an early move causes a cutoff then we have saved time
        // by only sorting one or a few moves rather than the whole list.
        sort_next_move(position.moves[engine.search_ply], position.move_scores[engine.search_ply], move_index);
        MOVE_TYPE move = position.moves[engine.search_ply][move_index];
        SCORE_TYPE move_score = position.move_scores[engine.search_ply][move_index];

        SCORE_TYPE move_history_score = engine.history_moves
        [get_selected(move)][MAILBOX_TO_STANDARD[get_target_square(move)]];

        bool quiet = !get_is_capture(move) && get_move_type(move) != MOVE_TYPE_EP;

        // Pruning
        if (!pv_node && legal_moves > 0) {
            // Late Move Pruning
            if (depth <= engine.tuning_parameters.LMP_depth &&
                legal_moves >= depth * engine.tuning_parameters.LMP_margin) break;

            // Quiet Late Move Pruning
            if (quiet && depth <= engine.tuning_parameters.quiet_LMP_depth &&
                legal_moves >= depth * (engine.tuning_parameters.quiet_LMP_margin -
                                        !improving * engine.tuning_parameters.quiet_LMP_improving_margin)) break;
            if (quiet && best_score > -MATE_BOUND && depth <= 5 && static_eval + depth * 150 + 60 <= alpha) break;

            // History Pruning
            if (depth <= engine.tuning_parameters.history_pruning_depth &&
                move_history_score <= (depth + improving) * -engine.tuning_parameters.history_pruning_divisor) continue;

            // SEE Pruning
            if (depth <= (3 + 3 * !quiet) && legal_moves >= 3 &&
                 -MATE_BOUND < alpha && alpha < MATE_BOUND &&
                 move_history_score <= 5000 &&
                 !get_static_exchange_evaluation(position, move, (quiet ? -50 : -90) * depth))
                continue;

        }

        // Make the move
        bool attempt = position.make_move(move, engine.search_ply, engine.fifty_move);

        // The move put us in check, therefore it was not legal, and we must disregard it
        if (!attempt) {
            position.undo_move(move, engine.search_ply, engine.fifty_move);
            continue;
        }

        bool recapture = false;

        if (last_move_one != NO_MOVE && get_target_square(last_move_one) == get_target_square(move)) {
            recapture_found = true;
            recapture = true;
        }

        PLY_TYPE extensions = 0;

        bool passed_pawn = get_selected(move) == WHITE_PAWN + BLACK_PAWN * position.side &&
                           MAILBOX_TO_STANDARD[get_target_square(move)] / 8 == 1 + 5 * position.side;
        bool queen_promotion = get_move_type(move) == MOVE_TYPE_PROMOTION &&
                               get_promotion_piece(move) == WHITE_QUEEN + BLACK_PAWN * position.side;

        if (move_score >= 0 && (passed_pawn || queen_promotion)) extensions++;

        PLY_TYPE new_depth = depth + extensions - 1;

        engine.search_ply++;
        engine.fifty_move++;
        engine.game_ply++;
        engine.repetition_table[engine.game_ply] = position.hash_key;
        position.side ^= 1;

        bool move_gives_check = position.is_attacked(position.king_positions[position.side]);
        position.state_stack[engine.search_ply].in_check = static_cast<int>(move_gives_check);

        SCORE_TYPE return_eval = -SCORE_INF;

        double reduction;

        bool is_killer_move = move == engine.killer_moves[0][engine.search_ply - 1] ||
                              move == engine.killer_moves[1][engine.search_ply - 1];

        bool full_depth_zero_window;

        bool bad_capture = move_score < 10000;

        // Late Move Reductions
        // The idea that if moves are ordered well, then moves that are searched
        // later shouldn't be as good, and therefore we don't need to search them to a very high depth
        if (legal_moves >= 2
            && ((engine.search_ply && !pv_node) || legal_moves >= 4)
            && depth >= 3
            && (quiet || bad_capture)
            && !in_check
            ){

            reduction = quiet ? LMR_REDUCTIONS_QUIET[depth][std::min(legal_moves, 63)] :
                                LMR_REDUCTIONS_NOISY[depth][std::min(legal_moves, 63)];

            reduction -= pv_node;

            reduction -= improving * 0.9;

            reduction -= is_killer_move * 0.75;

            reduction -= move_gives_check * 0.6;

            reduction -= move_history_score > 0 ? move_history_score / 7200.0 : move_history_score / 16000.0;

            reduction -= recapture * 0.5;

            reduction += !recapture && recapture_found && quiet && !move_gives_check && move_history_score <= 0;

            reduction += static_cast<double>(alpha_raised_count) * (0.3 + 0.5 * get_is_capture(tt_move));

            // My idea that in a null move search you can be more aggressive with LMR
            reduction += null_search;

            // Idea from Weiss, where you reduce more if the TT move is a capture
            reduction += get_is_capture(tt_move) * 0.3;

            PLY_TYPE lmr_depth = static_cast<PLY_TYPE>(new_depth -
                                 std::min<PLY_TYPE>(new_depth, std::max<PLY_TYPE>(0, static_cast<PLY_TYPE>(reduction))));

            return_eval = -negamax(engine, position, -alpha - 1, -alpha, lmr_depth, true);

            full_depth_zero_window = return_eval > alpha && lmr_depth != new_depth;
        }

        else {
            full_depth_zero_window = !pv_node || legal_moves >= 1;
        }

        // Principle Variation Search
        // We assume that the first move should be the principle variation / best move, so the rest of the moves
        // should be searched with a zero window
        if (full_depth_zero_window)
            return_eval = -negamax(engine, position, -alpha - 1, -alpha, new_depth, true);

        if (return_eval == -SCORE_INF || (pv_node && ((return_eval > alpha && return_eval < beta) || legal_moves == 0)))
            return_eval = -negamax(engine, position, -beta, -alpha, new_depth, true);

        position.side ^= 1;
        engine.game_ply--;
        engine.search_ply--;

        position.undo_move(move, engine.search_ply, engine.fifty_move);

        if (engine.stopped) return 0;

        // Update legal move count
        legal_moves++;

        // This move is better than other moves searched
        if (return_eval > best_score) {
            best_score = return_eval;
            best_move = move;

            // Write moves into PV table
            engine.pv_table[engine.search_ply][engine.search_ply] = move;
            for (int next_ply = engine.search_ply+1; next_ply < engine.pv_length[engine.search_ply+1]; next_ply++) {
                engine.pv_table[engine.search_ply][next_ply] = engine.pv_table[engine.search_ply + 1][next_ply];
            }

            engine.pv_length[engine.search_ply] = engine.pv_length[engine.search_ply + 1];

            if (return_eval > alpha) {
                alpha_raised_count++;
                alpha = return_eval;

                // We have found a better move that increased achieved us an exact score
                tt_hash_flag = HASH_FLAG_EXACT;

                // History Heuristic for move ordering
                SCORE_TYPE bonus = depth * (depth + 1 + null_search) - 1;

                if (quiet) {
                    update_history_entry(engine.history_moves
                                         [get_selected(move)][MAILBOX_TO_STANDARD[get_target_square(move)]],
                                         bonus);

                    if (last_move_one != NO_MOVE) {
                        update_history_entry(engine.continuation_history
                                             [get_selected(last_move_one)]
                                             [MAILBOX_TO_STANDARD[get_target_square(last_move_one)]]
                                             [get_selected(move)]
                                             [MAILBOX_TO_STANDARD[get_target_square(move)]],
                                             bonus);
                    }

                    if (last_move_two != NO_MOVE) {
                        update_history_entry(engine.continuation_history
                                             [get_selected(last_move_two)]
                                             [MAILBOX_TO_STANDARD[get_target_square(last_move_two)]]
                                             [get_selected(move)]
                                             [MAILBOX_TO_STANDARD[get_target_square(move)]],
                                             bonus);
                    }

                } else {
                    update_history_entry(engine.capture_history[get_selected(move)]
                                         [get_occupied(move)][MAILBOX_TO_STANDARD[get_target_square(move)]],
                                         bonus);
                }

                // Deduct bonus for moves that don't raise alpha
                for (int failed_move_index = 0; failed_move_index < move_index; failed_move_index++) {
                    MOVE_TYPE temp_move = position.moves[engine.search_ply][failed_move_index];
                    if (!get_is_capture(temp_move) && get_move_type(move) != MOVE_TYPE_EP) {
                        update_history_entry(engine.history_moves
                                             [get_selected(temp_move)]
                                             [MAILBOX_TO_STANDARD[get_target_square(temp_move)]],
                                             -bonus);

                        if (last_move_one != NO_MOVE) {
                            update_history_entry(engine.continuation_history
                                                 [get_selected(last_move_one)]
                                                 [MAILBOX_TO_STANDARD[get_target_square(last_move_one)]]
                                                 [get_selected(temp_move)]
                                                 [MAILBOX_TO_STANDARD[get_target_square(temp_move)]],
                                                 -bonus);
                        }

                        if (last_move_two != NO_MOVE) {
                            update_history_entry(engine.continuation_history
                                                 [get_selected(last_move_two)]
                                                 [MAILBOX_TO_STANDARD[get_target_square(last_move_two)]]
                                                 [get_selected(temp_move)]
                                                 [MAILBOX_TO_STANDARD[get_target_square(temp_move)]],
                                                 -bonus);
                        }

                    } else {
                        update_history_entry(engine.capture_history[get_selected(temp_move)]
                                             [get_occupied(temp_move)]
                                             [MAILBOX_TO_STANDARD[get_target_square(temp_move)]],
                                             -bonus);
                    }
                }

                if (engine.show_stats) {
                    engine.search_results.alpha_raised_count++;
                    if (legal_moves <= ALPHA_RAISE_STATS_COUNT) {
                        engine.search_results.search_alpha_raises[legal_moves-1]++;
                    }
                }

                // Alpha - Beta cutoff. We have failed high here.
                if (return_eval >= beta) {
                    if (engine.show_stats) {
                        if (legal_moves <= FAIL_HIGH_STATS_COUNT) engine.search_results.search_fail_highs[legal_moves-1]++;
                        if (move == tt_move) engine.search_results.search_fail_high_types[0]++;
                        else if (move == engine.killer_moves[0][engine.search_ply]) engine.search_results.search_fail_high_types[1]++;
                        else if (move == engine.killer_moves[1][engine.search_ply]) engine.search_results.search_fail_high_types[2]++;
                        else if (quiet) engine.search_results.search_fail_high_types[4]++;
                        else engine.search_results.search_fail_high_types[5]++;
                    }

                    // Killer Heuristic for move ordering
                    if (quiet) {
                        engine.killer_moves[1][engine.search_ply] = engine.killer_moves[0][engine.search_ply];
                        engine.killer_moves[0][engine.search_ply] = move;
                    }

                    tt_hash_flag = HASH_FLAG_BETA;
                    break;
                }
            }
        }
    }

    engine.tt_prefetch_write(position.hash_key);

    if (legal_moves == 0 && !in_check) return 0;
    else if (legal_moves == 0) return -MATE_SCORE + engine.search_ply;


    engine.record_tt_entry(position.hash_key, best_score, tt_hash_flag, best_move, depth, static_eval);

    return best_score;
}


void print_thinking(Engine& engine, Position& position, NodeType node, SCORE_TYPE best_score) {

    PLY_TYPE depth = engine.current_search_depth;

    // Calculate the elapsed time and NPS
    auto end_time = std::chrono::high_resolution_clock::now();
    auto end_int = std::chrono::duration_cast<std::chrono::milliseconds>
            (std::chrono::time_point_cast<std::chrono::milliseconds>(end_time).time_since_epoch()).count();

    uint64_t elapsed_time = end_int - engine.start_time;
    elapsed_time = std::max<uint64_t>(elapsed_time, 1);

    auto nps = static_cast<uint64_t>(static_cast<double>(engine.node_count) /
                                     (static_cast<double>(elapsed_time) / 1000.0));

    // Format the scores for printing to UCI
    SCORE_TYPE format_score = best_score;
    std::string result_type = "cp ";
    if (abs(best_score) >= MATE_BOUND) {
        result_type = "mate ";
        format_score = best_score >= MATE_BOUND ?
                       (MATE_SCORE - best_score) / 2 + 1: (-MATE_SCORE - best_score) / 2;
    }

    result_type += std::to_string(format_score);

    // Identify the type of node / bound if necessary
    if (node == Lower_Node) result_type += " lowerbound";
    else if (node == Upper_Node) result_type += " upperbound";

    // PV information
    std::string pv_line;

    // If the depth is low, or we are printing a PV line from a failed aspiration search,
    // then only store a limited number of moves in the PV line
    int max_pv_length = engine.pv_length[0];
    if (depth <= 12 || node == Lower_Node || node == Upper_Node)
        max_pv_length = std::min(max_pv_length, 1 + max_pv_length / 3);

    // Get the PV line
    SQUARE_TYPE original_side = position.side;
    for (int c = 0; c < max_pv_length; c++) {
        pv_line += get_uci_from_move(engine.pv_table[0][c]);
        pv_line += " ";
        position.side ^= 1;
    }
    position.side = original_side;

    // Print the UCI search information
    std::cout << "info multipv 1 depth " << depth << " seldepth " << engine.selective_depth
              << " score " << result_type << " time " << elapsed_time
              << " nodes " << engine.node_count << " nps " << nps
              << " pv " << pv_line << std::endl;

}


SCORE_TYPE aspiration_window(Engine& engine, Position& position, SCORE_TYPE previous_score, PLY_TYPE& asp_depth) {

    SCORE_TYPE alpha = -SCORE_INF;
    SCORE_TYPE beta = SCORE_INF;
    SCORE_TYPE delta = std::max(6 + static_cast<int>(85 / (asp_depth - 2)), 10);

    PLY_TYPE depth = engine.current_search_depth;

    if (depth >= MINIMUM_ASP_DEPTH) {
        alpha = std::max(previous_score - delta, -SCORE_INF);
        beta  = std::min(previous_score + delta,  SCORE_INF);
    }

    SCORE_TYPE return_eval = 0;
    while (true) {

        // Completely expand the bounds once they have exceeded a certain score
        if (alpha <= -1000) alpha = -SCORE_INF;
        if (beta  >=  1000) beta  =  SCORE_INF;

        return_eval = negamax(engine, position, alpha, beta, depth, false);

        if (engine.stopped) break;

        // The aspiration window search has failed low.
        // The position is probably worse than we expect, so both alpha and beta should be relaxed.
        // Alpha will be relaxed with delta while beta will be skewed towards alpha.
        if (return_eval <= alpha) {
            alpha = std::max(alpha - delta, -SCORE_INF);
            beta  = (alpha + 3 * beta) / 4;
            depth = engine.current_search_depth;

            asp_depth--;
            asp_depth = std::max<PLY_TYPE>(6, asp_depth);

            if (depth >= 18) print_thinking(engine, position, Lower_Node, return_eval);
        }

        // The aspiration window search has failed high.
        // The position is probably better than we expect, so we can be more aggressive with aspiration windows
        // and only relax the beta bound while also reducing depth.
        else if (return_eval >= beta) {
            beta  = std::min(beta + delta, SCORE_INF);
            depth = std::max(engine.min_depth,
                             static_cast<PLY_TYPE>(static_cast<int>(depth) - (return_eval < MATE_BOUND)));

            asp_depth--;
            asp_depth = std::max<PLY_TYPE>(6, asp_depth);

            if (depth >= 18) print_thinking(engine, position, Upper_Node, return_eval);
        }

        // We have achieved an exact node where the score was between alpha and beta.
        // We are certain of our score and can now safely return.
        else {
            print_thinking(engine, position, Exact_Node, return_eval);
            break;
        }

        // Increase delta in the case that we must re-search the aspiration windows multiple times.
        delta += delta * 2 / 3;
    }

    return return_eval;
}


void iterative_search(Engine& engine, Position& position) {

    // Reset certain information
    engine.stopped = false;
    engine.terminated = false;

    position.clear_movelist();
    engine.reset();

    // Initialize the start time
    auto start_time = std::chrono::high_resolution_clock::now();
    engine.start_time = std::chrono::duration_cast<std::chrono::milliseconds>
            (std::chrono::time_point_cast<std::chrono::milliseconds>(start_time).time_since_epoch()).count();

    // Initialize variables
    SCORE_TYPE previous_score = 0;
    PLY_TYPE running_depth = 1;
    PLY_TYPE asp_depth = 6;

    MOVE_TYPE best_move = NO_MOVE;

    while (running_depth <= engine.max_depth) {
        engine.current_search_depth = running_depth;

        // Run a search with aspiration window bounds
        previous_score = aspiration_window(engine, position, previous_score, asp_depth);

        // Store the best move when the engine has finished a search (it hasn't stopped in the middle of a search)
        if (!engine.stopped) best_move = engine.pv_table[0][0];

        // Calculate the elapsed time
        auto end_time = std::chrono::high_resolution_clock::now();
        auto ms_int = std::chrono::duration_cast<std::chrono::milliseconds>(end_time
                                                                            - start_time);
        uint64_t elapsed_time = ms_int.count();
        elapsed_time = std::max<uint64_t>(elapsed_time, 1);

        // Stop the engine when we have exceeded the soft time limit
        if (running_depth >= engine.min_depth) {
            if (elapsed_time >= engine.soft_time_limit) engine.stopped = true;
        }

        // End the search when the engine has stopped running
        if (engine.stopped || running_depth == engine.max_depth) {
            break;
        }

        // Increase the aspiration depth for aspiration bounds scaling
        if (running_depth >= 6) {
            asp_depth++;
        }

        // Increase search depth
        running_depth++;
    }

    engine.search_results.depth_reached = running_depth;
    engine.search_results.node_count = engine.node_count;

    std::string best_move_str = get_uci_from_move(best_move);
    std::cout << "bestmove " << best_move_str << std::endl;

    engine.terminated = true;
}


void print_statistics(Search_Results& res) {


    std::cout << "------------------- SEARCH STATISTICS -------------------\n\n\n";

    std::cout << "Total Nodes Searched: " << res.node_count << "\n";
    std::cout << "Average Nodes Searched: " << res.node_count / res.num_searches << "\n\n";

    std::cout << "Average Branching Factor: " <<
              pow(res.node_count / res.num_searches, 1.0/res.depth_reached) << "\n\n";

    std::cout << "Average # of times alpha is raised per node: " <<
              static_cast<double>(res.alpha_raised_count) / static_cast<double>(res.node_count)
              << "\n\n";

    uint64_t search_total = 0;
    for (unsigned long long e : res.search_alpha_raises) {
        search_total += e;
    }

    for (int i = 0; i < ALPHA_RAISE_STATS_COUNT; i++) {
        uint64_t e = res.search_alpha_raises[i];
        std::cout << "search alpha raises at index " << i << ": " << double(e) / double(search_total) * 100 << "%" << std::endl;
    }

    std::cout << "\n";

    uint64_t qsearch_total = 0;
    search_total = 0;
    for (int i = 0; i < FAIL_HIGH_STATS_COUNT; i++) {
        uint64_t qe = res.qsearch_fail_highs[i];
        uint64_t e = res.search_fail_highs[i];
        qsearch_total += qe;
        search_total += e;
    }
    for (int i = 0; i < FAIL_HIGH_STATS_COUNT; i++) {
        uint64_t e = res.qsearch_fail_highs[i];
        std::cout << "q-search fail high at index " << i << ": " << double(e) / double(qsearch_total) * 100 << "%" << std::endl;
    }

    std::cout << "\n";

    for (int i = 0; i < FAIL_HIGH_STATS_COUNT; i++) {
        uint64_t e = res.search_fail_highs[i];
        std::cout << "search fail high at index " << i << ": " << double(e) / double(search_total) * 100 << "%" << std::endl;
    }

    std::cout << "\n";

    uint64_t total_fail_highs = 0;
    for (uint64_t e : res.search_fail_high_types) total_fail_highs += e;

    std::cout << "TT move fail highs: " << res.search_fail_high_types[0] << " percentage: " <<
              double(res.search_fail_high_types[0]) / double(total_fail_highs) * 100 << "%\n";
    std::cout << "Killer move 1 fail highs: " << res.search_fail_high_types[1] << " percentage: " <<
              double(res.search_fail_high_types[1]) / double(total_fail_highs) * 100 << "%\n";
    std::cout << "Killer move 2 fail highs: " << res.search_fail_high_types[2] << " percentage: " <<
              double(res.search_fail_high_types[2]) / double(total_fail_highs) * 100 << "%\n";
    std::cout << "Counter move fail highs: " << res.search_fail_high_types[3] << " percentage: " <<
              double(res.search_fail_high_types[3]) / double(total_fail_highs) * 100 << "%\n";
    std::cout << "Quiet move fail highs: " << res.search_fail_high_types[4] << " percentage: " <<
              double(res.search_fail_high_types[4]) / double(total_fail_highs) * 100 << "%\n";
    std::cout << "Noisy move fail highs: " << res.search_fail_high_types[5] << " percentage: " <<
              double(res.search_fail_high_types[5]) / double(total_fail_highs) * 100 << "%\n";

    std::cout << "\n\n---------------------------------------------------------" << std::endl;
}


void print_tuning_config(Tuning_Parameters& tuning_parameters) {
    std::cout << "{";
    for (auto & i : tuning_parameters.tuning_parameter_array) {
        std::cout << "\n\t\"" << i.name << "\": {"
                  << "\n\t\t\"value\": " << i.value
                  << ",\n\t\t\"min_value\": " << i.min
                  << ",\n\t\t\"max_value\": " << i.max
                  << ",\n\t\t\"step\": " << i.step
                  << "\n\t},"
                  << std::endl;
    }
    std::cout << "}" << std::endl;
}
