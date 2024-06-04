

#include <thread>
#include <chrono>
#include <iostream>
#include <cstring>
#include <cmath>
#include <algorithm>
#include <cassert>
#include "search.h"
#include "evaluation.h"
#include "move.h"
#include "useful.h"
#include "see.h"
#include "wdl.h"


// Initialize a table for Late Move Reductions, and the base reductions to be applied
void Engine::initialize_lmr_reductions() {
    for (PLY_TYPE depth = 0; depth < MAX_AB_DEPTH; depth++) {
        for (int moves = 0; moves < 64; moves++) {
            LMR_REDUCTIONS_QUIET[depth][moves] =
                    static_cast<int>(std::max(0.0,
                                     std::log(depth) * std::log(moves) / double(search_params.LMR_divisor_quiet.v / 100.0)
                                     + double(search_params.LMR_base_quiet.v / 100.0)));
        }
    }
}


void Engine::resize_tt(uint64_t mb) {
    size_t entries = static_cast<size_t>(mb) * 1048576 / 24;

    uint64_t exponent = std::floor(std::log2(entries));
    size_t new_entries = std::pow(2, exponent);

    transposition_table.resize(new_entries);

    // std::cout << new_entries << std::endl;
}


// Clear the transposition table
void Engine::clear_tt() {
    for (TT_Entry& tt_entry : transposition_table) {
        tt_entry.key = 0;
        tt_entry.score = SCORE_NONE;
        tt_entry.evaluation = NO_EVALUATION;
        tt_entry.move = NO_MOVE;
        tt_entry.depth = 0;
        tt_entry.flag = 0;
    }
}


// Resets the Engine object for a new search
void Engine::reset() {
    stopped = true;
    std::memset(node_table, 0, sizeof(node_table));

    for (Thread_State& thread_state : thread_states) {
        thread_state.current_search_depth = 1;
        thread_state.search_ply = 0;
        thread_state.selective_depth = 0;

        thread_state.node_count = 0;
        std::memset(thread_state.killer_moves, 0, sizeof(thread_state.killer_moves));
    }

    std::memset(pv_length, 0, sizeof(pv_length));
    std::memset(pv_table, 0, sizeof(pv_table));

#ifdef SHOW_STATISTICS
    search_results.alpha_raised_count = 0;
    std::memset(search_results.qsearch_fail_highs, 0, sizeof(search_results.qsearch_fail_highs));
    std::memset(search_results.search_fail_highs, 0, sizeof(search_results.search_fail_highs));
    std::memset(search_results.search_fail_high_types, 0, sizeof(search_results.search_fail_high_types));
#endif

    root_moves.clear();
}


// Resets the Engine object for a new game
void Engine::new_game() {
    reset();

    for (Thread_State& thread_state : thread_states) {
        std::memset(thread_state.repetition_table, 0, sizeof(thread_state.repetition_table));
        std::memset(thread_state.killer_moves, 0, sizeof(thread_state.killer_moves));
        std::memset(thread_state.history_moves, 0, sizeof(thread_state.history_moves));
        std::memset(thread_state.capture_history, 0, sizeof(thread_state.capture_history));
        std::memset(thread_state.continuation_history, 0, sizeof(thread_state.continuation_history));

        thread_state.game_ply = 0;
        thread_state.fifty_move = 0;

    }

    stopped = true;

    std::memset(pv_length, 0, sizeof(pv_length));
    std::memset(pv_table, 0, sizeof(pv_table));
    clear_tt();
}


// Detects a repetition (Checks for only 2 repetitions)
bool Thread_State::detect_repetition() {

    for (PLY_TYPE i = game_ply - 2; i >= std::max<PLY_TYPE>(game_ply - fifty_move, 0); i--) {
        if (repetition_table[i] == repetition_table[game_ply]) {
            return true;
        }
    }

    return false;
}

// Detects a repetition (Checks for 3 repetitions)
bool Thread_State::detect_repetition_3() {

    int count = 0;
    for (PLY_TYPE i = game_ply - 2; i >= std::max<PLY_TYPE>(game_ply - fifty_move, 0); i--) {
        if (repetition_table[i] == repetition_table[game_ply]) {
            count++;
            if (count >= 2) return true;
        }
    }

    return false;
}


SCORE_TYPE& Thread_State::get_continuation_history_entry(InformativeMove last_move, InformativeMove informative_move) {
    return continuation_history[last_move.selected()]
                               [last_move.target()]
                               [position.board[informative_move.origin()]]
                               [informative_move.target()];
}


// Probes the transposition table for a matching entry based on the position's hash key and other factors.
short Engine::probe_tt_entry(int thread_id, HASH_TYPE hash_key, SCORE_TYPE alpha, SCORE_TYPE beta, PLY_TYPE depth,
                             TT_Entry& return_entry) {
    TT_Entry& tt_entry = transposition_table[hash_key & (transposition_table.size() - 1)];

    if (tt_entry.key == hash_key) {
        return_entry.move = tt_entry.move;
        return_entry.depth = tt_entry.depth;
        return_entry.flag = tt_entry.flag;

        return_entry.score = tt_entry.score;

        // Adjust mate scores based on ply
        if (return_entry.score < -MATE_BOUND) return_entry.score += thread_states[thread_id].search_ply;
        else if (return_entry.score > MATE_BOUND) return_entry.score -= thread_states[thread_id].search_ply;

        if (tt_entry.depth >= depth) {

            if (tt_entry.flag == HASH_FLAG_EXACT) return RETURN_HASH_SCORE;
            if (tt_entry.flag == HASH_FLAG_UPPER && return_entry.score <= alpha) return RETURN_HASH_SCORE;
            if (tt_entry.flag == HASH_FLAG_LOWER && return_entry.score >= beta) return RETURN_HASH_SCORE;
        }

        return USE_HASH_MOVE;
    }

    return NO_HASH_ENTRY;
}


// Records an entry to the transposition table
void Engine::record_tt_entry(int thread_id, HASH_TYPE hash_key, SCORE_TYPE score, short tt_flag, Move move, PLY_TYPE depth,
                             SCORE_TYPE static_eval, bool pv_node) {

    TT_Entry& tt_entry = transposition_table[hash_key & (transposition_table.size() - 1)];

    if (score < -MATE_BOUND) score -= thread_states[thread_id].search_ply;
    else if (score > MATE_BOUND) score += thread_states[thread_id].search_ply;

    PLY_TYPE artificial_depth = depth + 3 + 2 * pv_node;

    if (tt_entry.key != hash_key || artificial_depth >= tt_entry.depth || tt_flag == HASH_FLAG_EXACT) {
        tt_entry.key = hash_key;
        tt_entry.depth = depth;
        tt_entry.flag = tt_flag;
        tt_entry.score = score;
        tt_entry.evaluation = static_eval;

        if (tt_flag != HASH_FLAG_UPPER || tt_entry.key != hash_key || tt_entry.move == NO_MOVE) {
            tt_entry.move = move;
        }
    }
}


// Probes the transposition table for a matching entry (only for quiescence search)
short Engine::probe_tt_entry_q(int thread_id, HASH_TYPE hash_key, SCORE_TYPE alpha, SCORE_TYPE beta,
                               SCORE_TYPE& return_score, Move& tt_move) {
    TT_Entry& tt_entry = transposition_table[hash_key & (transposition_table.size() - 1)];

    if (tt_entry.key == hash_key) {
        tt_move = tt_entry.move;

        return_score = tt_entry.score;
        if (return_score < -MATE_BOUND) return_score += thread_states[thread_id].search_ply;
        else if (return_score > MATE_BOUND) return_score -= thread_states[thread_id].search_ply;

        if (tt_entry.flag == HASH_FLAG_EXACT) return RETURN_HASH_SCORE;
        if (tt_entry.flag == HASH_FLAG_UPPER && return_score <= alpha) return RETURN_HASH_SCORE;
        if (tt_entry.flag == HASH_FLAG_LOWER && return_score >= beta) return RETURN_HASH_SCORE;


        return USE_HASH_MOVE;
    }

    return NO_HASH_ENTRY;
}


// Records an entry to the transposition table (only for quiescence search)
void Engine::record_tt_entry_q(int thread_id, HASH_TYPE hash_key, SCORE_TYPE score, short tt_flag, Move move,
                               SCORE_TYPE static_eval) {
    TT_Entry& tt_entry = transposition_table[hash_key & (transposition_table.size() - 1)];

    if (score < -MATE_BOUND) score -= thread_states[thread_id].search_ply;
    else if (score > MATE_BOUND) score += thread_states[thread_id].search_ply;

    if (tt_entry.key != hash_key || tt_flag == HASH_FLAG_EXACT) {
        tt_entry.key = hash_key;
        tt_entry.depth = -1;
        tt_entry.flag = tt_flag;
        tt_entry.score = score;
        tt_entry.evaluation = static_eval;

        if (tt_flag != HASH_FLAG_UPPER || tt_entry.key != hash_key || tt_entry.move == NO_MOVE) {
            tt_entry.move = move;
        }
    }
}


// Probes the transposition table for an evaluation
SCORE_TYPE Engine::probe_tt_evaluation(HASH_TYPE hash_key) {
    TT_Entry& tt_entry = transposition_table[hash_key % transposition_table.size()];

    if (tt_entry.key == hash_key && tt_entry.evaluation != NO_EVALUATION) return tt_entry.evaluation;
    return NO_EVALUATION;
}

// Prefetching for cache
void Engine::tt_prefetch_read(HASH_TYPE hash_key) {
    __builtin_prefetch(&transposition_table[hash_key % transposition_table.size()]);
}


bool Engine::check_time() {
    auto time = std::chrono::high_resolution_clock::now();
    uint64_t current_time = std::chrono::duration_cast<std::chrono::milliseconds>
            (std::chrono::time_point_cast<std::chrono::milliseconds>(time).time_since_epoch()).count();

    if (current_time - start_time >= hard_time_limit) {
        stopped = true;
        return true;
    }

    return false;
}

bool Engine::check_nodes() {

    uint64_t total_nodes = 0;
    for (Thread_State& thread_state_i : thread_states) {
        total_nodes += thread_state_i.node_count;
    }

    if (hard_node_limit && total_nodes >= hard_node_limit) {
        stopped = true;
        return true;
    }

    return false;
}


template<bool NNUE>
SCORE_TYPE Engine::evaluate(int thread_id) {
    Position& position = thread_states[thread_id].position;

    if constexpr (NNUE) return position.nnue_state.evaluate(position, position.side);
    return evaluate_classic(position);
}

template SCORE_TYPE Engine::evaluate<USE_NNUE>(int thread_id);
template SCORE_TYPE Engine::evaluate<NO_NNUE >(int thread_id);


// History entry updates with scaling
void update_history_entry(SCORE_TYPE& score, SCORE_TYPE bonus) {
    score -= (score * abs(bonus)) / 324;
    score += bonus * 32;
}


void update_histories(Thread_State& thread_state, InformativeMove informative_move,
                      InformativeMove last_moves[], bool quiet, bool winning_capture,
                      int bonus) {

    auto& searched_quiets = thread_state.searched_quiets[thread_state.search_ply];
    auto& searched_noisy  = thread_state.searched_noisy [thread_state.search_ply];

    Position& position = thread_state.position;
    Move move = informative_move.normal_move();

    if (quiet) {
        update_history_entry(thread_state.history_moves
                             [position.board[move.origin()]][move.target()],
                             bonus);

        for (int last_move_index = 0; last_move_index < LAST_MOVE_COUNTS; last_move_index++) {
            if (last_moves[last_move_index] != NO_INFORMATIVE_MOVE) {
                update_history_entry(thread_state.get_continuation_history_entry(last_moves[last_move_index], informative_move),
                                     bonus);
            }
        }

    } else {
        update_history_entry(thread_state.capture_history[winning_capture][position.board[move.origin()]]
                             [position.board[move.target()]][move.target()],
                             bonus);
    }

    // Deduct bonus for moves that don't raise alpha
    for (int failed_index = 0; failed_index < static_cast<int>(searched_quiets.size()) - 1; failed_index++) {
        ScoredMove& temp_scored_move = searched_quiets[failed_index];
        Move temp_move = temp_scored_move.move;

        update_history_entry(thread_state.history_moves
                             [position.board[temp_move.origin()]]
                             [temp_move.target()],
                             -bonus);


        InformativeMove temp_move_informative = InformativeMove(temp_move, position.board[temp_move.origin()], position.board[temp_move.target()]);
        for (int last_move_index = 0; last_move_index < LAST_MOVE_COUNTS; last_move_index++) {
            if (last_moves[last_move_index] != NO_INFORMATIVE_MOVE) {
                update_history_entry(thread_state.get_continuation_history_entry(last_moves[last_move_index], temp_move_informative),
                                     -bonus);
            }
        }

    }


    for (int failed_index = 0; failed_index < static_cast<int>(searched_noisy.size()) - 1; failed_index++) {
        ScoredMove& temp_scored_move = searched_noisy[failed_index];
        Move temp_move = temp_scored_move.move;

        assert(temp_move != move);

        if (temp_scored_move.winning_capture == winning_capture) {
            update_history_entry(thread_state.capture_history[temp_scored_move.winning_capture]
                                 [position.board[temp_move.origin()]]
                                 [position.board[temp_move.target()]]
                                 [temp_move.target()],
                                 -bonus);
        }
    }
}

// The Quiescence Search function
// This is used to counter the horizon effect in which negamax is unable to resolve
// noisy moves after a certain depth has been reached. The qsearch function will look at remaining captures
// until it reaches a quiet position.
template<bool NNUE>
SCORE_TYPE qsearch(Engine& engine, SCORE_TYPE alpha, SCORE_TYPE beta, PLY_TYPE depth, int thread_id) {

    // Initialize Variables
    Thread_State& thread_state = engine.thread_states[thread_id];
    Position& position = thread_state.position;
    Generator& generator = thread_state.generators[thread_state.search_ply];

    // Check the remaining time
    if (engine.stopped || (thread_id == 0 && thread_state.current_search_depth >= engine.min_depth &&
        (thread_state.node_count & 2047) == 0 &&
        (engine.check_nodes() || engine.check_time()))) {
        return 0;
    }

    // Probe the transposition table
    SCORE_TYPE tt_value = 0;
    Move tt_move = NO_MOVE;
    short tt_return_type = engine.probe_tt_entry_q(thread_id, position.hash_key, alpha, beta, tt_value, tt_move);

    if (tt_return_type == RETURN_HASH_SCORE) {
        return tt_value;
    }

    // Get the static evaluation of the position
    SCORE_TYPE eval = engine.probe_tt_evaluation(position.hash_key);
    if (eval == NO_EVALUATION) eval = engine.evaluate<NNUE>(thread_id);

    // Return the evaluation if we have reached a stand-pat, or we have reached the maximum depth
    if (depth == 0 || eval >= beta) return eval;

    // Set alpha to the greatest of either alpha or the evaluation
    alpha = std::max(alpha, eval);

    // Variable to record the hash flag
    short tt_hash_flag = HASH_FLAG_UPPER;

    // Set values for State
    position.set_state(position.state_stack[thread_state.search_ply], thread_state.fifty_move);
    position.state_stack[thread_state.search_ply].static_eval = eval;

    // Variables for getting information about the best score / best move
    SCORE_TYPE best_score = eval;
    Move best_move = NO_MOVE;

    // Search loop
    for (generator.reset_qsearch(tt_move); generator.stage != Stage::Terminated;) {
        ScoredMove scored_move = generator.next_move<true>();
        Move move = scored_move.move;

        bool winning_capture = scored_move.winning_capture;

        if (move == NO_MOVE) break; // No legal moves
        if (!winning_capture) break; // Gainer on god

        // SEE pruning
        if (eval + 60 <= alpha && !get_static_exchange_evaluation(position, move, 1)) {
            best_score = std::max(best_score, eval + 60);
            continue;
        }

        // Attempt the current pseudo-legal move
        bool attempt = position.make_move<NNUE>(move, position.state_stack[thread_state.search_ply], thread_state.fifty_move);
        engine.tt_prefetch_read(position.hash_key);  // Prefetch the TT for cache

        if (!attempt) {
            position.undo_move<NNUE>(move, position.state_stack[thread_state.search_ply], thread_state.fifty_move);
            continue;
        }

        position.update_nnue(position.state_stack[thread_state.search_ply]);

        thread_state.node_count++;

        // Recursively search
        thread_state.search_ply++;

        SCORE_TYPE return_eval = -qsearch<NNUE>(engine, -beta, -alpha, depth - 1, thread_id);

        thread_state.search_ply--;

        // Undo the move
        position.undo_move<NNUE>(move, position.state_stack[thread_state.search_ply], thread_state.fifty_move);

        if (engine.stopped) return 0;

        // Update information
        if (return_eval > best_score) {
            best_score = return_eval;
            best_move = move;

            if (return_eval > alpha) {
                alpha = return_eval;
                tt_hash_flag = HASH_FLAG_EXACT;

                SCORE_TYPE bonus = 2;
                if (move.is_capture(position) || move.type() == MOVE_TYPE_EP) {
                    update_history_entry(thread_state.capture_history[winning_capture]
                                         [position.board[move.origin()]]
                                         [position.board[move.target()]]
                                         [move.target()],
                                         bonus);
                } else {
                    update_history_entry(thread_state.history_moves
                                         [position.board[move.origin()]][move.target()],
                                         bonus);
                }

                if (return_eval >= beta) {

#ifdef SHOW_STATISTICS
                    if (legal_moves <= FAIL_HIGH_STATS_COUNT) {
                        engine.search_results.qsearch_fail_highs[legal_moves - 1]++;
                    }
#endif

                    engine.record_tt_entry_q(thread_id, position.hash_key, best_score, HASH_FLAG_LOWER, best_move,
                                             position.state_stack[thread_state.search_ply].static_eval);
                    return best_score;
                }
            }
        }

    }

    engine.record_tt_entry_q(thread_id, position.hash_key, best_score, tt_hash_flag, best_move,
                             position.state_stack[thread_state.search_ply].static_eval);

    return best_score;
}

template SCORE_TYPE qsearch<USE_NNUE>(Engine& engine, SCORE_TYPE alpha, SCORE_TYPE beta, PLY_TYPE depth, int thread_id);
template SCORE_TYPE qsearch<NO_NNUE >(Engine& engine, SCORE_TYPE alpha, SCORE_TYPE beta, PLY_TYPE depth, int thread_id);


// The core of the engine, negamax.
// This is a recursive search function based on the minimax algorithm that uses alpha-beta pruning
// among other heuristics.
template<bool NNUE>
SCORE_TYPE negamax(Engine& engine, SCORE_TYPE alpha, SCORE_TYPE beta, PLY_TYPE depth, bool do_null, bool cutnode, int thread_id) {

    // Initialize Variables
    Thread_State& thread_state = engine.thread_states[thread_id];
    Position& position = thread_state.position;
    Generator& generator = thread_state.generators[thread_state.search_ply];

    bool root = !thread_state.search_ply;
    bool pv_node = alpha != beta - 1;  // Hack to determine pv_node, a zero window is when alpha == beta - 1
    bool singular_search = position.state_stack[thread_state.search_ply].excluded_move != NO_MOVE;
    bool null_search = !do_null && !root;
    bool in_check;

    SCORE_TYPE eval = NO_EVALUATION;

    thread_state.selective_depth = std::max(thread_state.search_ply, thread_state.selective_depth);

    // Initialize PV length
    if (thread_id == 0) {
        engine.pv_length[thread_state.search_ply] = thread_state.search_ply;
    }

    // Check the remaining time
    if (engine.stopped || (thread_id == 0 && thread_state.current_search_depth >= engine.min_depth &&
        (thread_state.node_count & 2047) == 0 &&
        (engine.check_nodes() || engine.check_time()))) {
        return 0;
    }

    // Early search exits
    if (!root) {

        // Depth guard
        if (thread_state.search_ply >= MAX_AB_DEPTH - 2 || depth >= MAX_AB_DEPTH) return engine.evaluate<NNUE>(thread_id);

        // Detect repetitions and fifty move rule
        if (thread_state.fifty_move >= 100 || thread_state.detect_repetition()) return 3 - static_cast<int>(thread_state.node_count & 8);

        alpha = std::max(alpha, -MATE_SCORE + thread_state.search_ply);
        beta  = std::min(beta,   MATE_SCORE - thread_state.search_ply);
        if (alpha >= beta) {
            return alpha;
        }
    }

    // Start quiescence search at the start of regular negamax search to counter the horizon effect.
    if (depth <= 0) {
        return qsearch<NNUE>(engine, alpha, beta, engine.max_q_depth, thread_id);
    }

    // Set values for State
    position.set_state(position.state_stack[thread_state.search_ply], thread_state.fifty_move);

    // TT probing
    TT_Entry tt_entry{};
    short tt_return_type = engine.probe_tt_entry(thread_id, position.hash_key, alpha, beta, depth, tt_entry);
    SCORE_TYPE tt_value = tt_entry.score;
    Move tt_move = tt_entry.move;
    short tt_hash_flag = HASH_FLAG_UPPER;

    // TT cutoffs
    // 1. Probing must result in the flag that allows for returning the hash score
    // 2. No cutoff in pv node due to potential failure to detect repetitions in a principal variation
    // 3. No cutoff in singular searches, as you want to check for singularity
    if (tt_return_type == RETURN_HASH_SCORE && !pv_node && !singular_search) return tt_value;

    // Calculate in check
    if (position.state_stack[thread_state.search_ply].in_check != -1)
        in_check = static_cast<bool>(position.state_stack[thread_state.search_ply].in_check);
    else in_check = position.is_attacked(position.get_king_pos(position.side), position.side);

    // The "improving" heuristic is when the current position has a better static evaluation than the evaluation
    // from a full-move or two plies ago. When this is true, we can be more aggressive with
    // beta-reductions (eval is too high) as we will have more certainty that the position is better;
    // however, we should be less aggressive with alpha-reductions (eval is too low) as we have less certainty
    // that the position is awful.

    // The failing heuristic is the opposite. We are failing when the current evaluation is worse than the
    // previous evaluation by a certain margin.
    bool improving = false;
    bool failing = false;

    // Save the current evaluation
    if (!in_check) {
        eval = engine.probe_tt_evaluation(position.hash_key);
        if (singular_search) eval = position.state_stack[thread_state.search_ply - 1].static_eval;
        if (eval == NO_EVALUATION) eval = engine.evaluate<NNUE>(thread_id);
        position.state_stack[thread_state.search_ply].static_eval = eval;

        // TT Eval correction
        // 1. TT probe must have succeeded
        // 2. TT value must exist
        // 3. If tt_entry.flag == HASH_FLAG_EXACT then the condition is passed in all cases
        // 4. If tt_value >= eval then tt_entry.flag must equal HASH_FLAG_LOWER to be true
        // 5. If tt_value <  eval then tt_entry.flag must equal HASH_FLAG_UPPER to be true
        if (tt_return_type != NO_HASH_ENTRY && tt_value != SCORE_NONE &&
            tt_entry.flag & (tt_value >= eval ? HASH_FLAG_LOWER : HASH_FLAG_UPPER)) {
            eval = tt_value;
        }

        // Calculate if we are "improving", or "failing"
        if (thread_state.search_ply >= 2) {
            SCORE_TYPE past_eval = position.state_stack[thread_state.search_ply - 2].static_eval;

            if (past_eval != NO_EVALUATION) {
                improving = position.state_stack[thread_state.search_ply].static_eval > past_eval;
                failing = !pv_node && position.state_stack[thread_state.search_ply].static_eval < past_eval - (60 + 40 * depth);
            }
        }
    }

    // Forward Pruning Methods
    if (!pv_node && !in_check && !singular_search && abs(beta) < MATE_BOUND) {

        // Reverse Futility Pruning
        // If the last move was very bad, such that the static evaluation - a margin is still greater
        // than the opponent's best score, then return the static evaluation.
        if (depth <= search_params.RFP_depth.v && eval -
            search_params.RFP_margin.v * (depth - improving) >= beta) {
            return eval;
        }

        // Null move pruning
        // We give the opponent an extra move and if they are not able to make their position
        // any better, then our position is too good, and we don't need to search any deeper.
        if (depth >= search_params.NMP_depth.v && do_null &&
            eval >= position.state_stack[thread_state.search_ply].static_eval &&
            eval >= beta &&
            position.state_stack[thread_state.search_ply].static_eval >= beta + 108 - (12 + improving * 7) * depth &&
            position.get_non_pawn_material_count() >= 1 + (depth >= 10)) {

            // Adaptive NMP
            int reduction = search_params.NMP_base.v +
                            depth / search_params.NMP_depth_divisor.v +
                            std::clamp((eval - beta) / search_params.NMP_eval_divisor.v, -1, 3);

            position.make_null_move(position.state_stack[thread_state.search_ply], thread_state.fifty_move);

            thread_state.search_ply++;
            thread_state.game_ply++;

            // zero window search with reduced depth
            SCORE_TYPE return_eval = -negamax<NNUE>(engine, -beta, -beta + 1, depth - reduction, false, !cutnode, thread_id);

            thread_state.game_ply--;
            thread_state.search_ply--;
            position.undo_null_move(position.state_stack[thread_state.search_ply], thread_state.fifty_move);

            if (return_eval >= beta) {
                if (return_eval >= MATE_BOUND) return beta;
                if (depth <= 15) return return_eval;
                SCORE_TYPE verification_eval = negamax<NNUE>(engine, beta - 1, beta, depth - reduction, false, cutnode, thread_id);
                if (verification_eval > beta) return return_eval;
            }
        }
    }

    // Internal Iterative Reduction. Based on Rebel's idea
    if (tt_move == NO_MOVE && depth >= 4) {
        depth--;
        if (pv_node) depth--;
    }

    bool tt_move_noisy = false;

    // Used for the continuation history heuristic
    InformativeMove last_moves[LAST_MOVE_COUNTS] = {};

    for (int last_move_index = 0; last_move_index < LAST_MOVE_COUNTS; last_move_index++) {
        int last_move_ply = LAST_MOVE_PLIES[last_move_index];

        last_moves[last_move_index] = thread_state.search_ply >= last_move_ply ?
                position.state_stack[thread_state.search_ply - last_move_ply].move : NO_INFORMATIVE_MOVE;
    }

    auto& searched_quiets = thread_state.searched_quiets[thread_state.search_ply];
    auto& searched_noisy  = thread_state.searched_noisy [thread_state.search_ply];

    searched_quiets.clear();
    searched_noisy.clear();

    // Best score for fail soft, and best move for tt
    SCORE_TYPE best_score = -SCORE_INF;
    Move best_move = NO_MOVE;

    // Other information for pruning / reductions
    int alpha_raised_count = 0;
    int legal_moves = 0;

    for (generator.reset_negamax(tt_move, last_moves); generator.stage != Stage::Terminated;) {

        ScoredMove scored_move = generator.next_move<false>();
        Move move = scored_move.move;
        SCORE_TYPE move_score = move == tt_move ? static_cast<SCORE_TYPE>(MO_Margin::TT) : scored_move.score;

        if (move == NO_MOVE) break; // No legal moves

        InformativeMove informative_move = InformativeMove(move, position.board[move.origin()], position.board[move.target()]);
        bool winning_capture = scored_move.winning_capture;

        // Skip excluded moves
        if (root && (thread_state.excluded_root_moves.contains(move.internal_move())
                     || !engine.root_moves.contains(move.internal_move()))) continue;
        if (move == position.state_stack[thread_state.search_ply].excluded_move) continue;

        bool quiet = !move.is_capture(position) && move.type() != MOVE_TYPE_EP;
        if (move == tt_move) tt_move_noisy = !quiet;

        if (quiet) searched_quiets.push_back(scored_move);
        else searched_noisy.push_back(scored_move);

        SCORE_TYPE move_history_score = quiet ?
                                        // Quiet Histories
                                        thread_state.history_moves[position.board[move.origin()]][move.target()] :
                                        // Capture Histories
                                        thread_state.capture_history
                                        [winning_capture][position.board[move.origin()]][position.board[move.target()]][move.target()];

        if (quiet) {
            for (int last_move_index = 0; last_move_index < LAST_MOVE_COUNTS; last_move_index++) {
                if (last_moves[last_move_index] != NO_INFORMATIVE_MOVE) {
                    move_history_score += thread_state.get_continuation_history_entry(last_moves[last_move_index], informative_move);
                }
            }
        }

        PLY_TYPE base_reduction = engine.LMR_REDUCTIONS_QUIET[depth][std::min(legal_moves, 63)];
        PLY_TYPE lmr_base_depth = std::max(depth - base_reduction, 0);

        // Pruning
        if (!root && legal_moves >= 1 && abs(best_score) < MATE_BOUND && !engine.datagen) {
            // Late Move Pruning
            if (!pv_node && depth <= search_params.LMP_depth.v &&
                legal_moves >= depth * search_params.LMP_margin.v) break;

            // Quiet Late Move Pruning
            if (!pv_node && quiet &&
                legal_moves >= search_params.LMP_margin_quiet.v + depth * depth / (1 + !improving + failing)) break;

            // Futility Pruning
            if (!pv_node && quiet && depth <= search_params.FP_depth.v &&
                position.state_stack[thread_state.search_ply].static_eval +
                (depth - !improving) * search_params.FP_multiplier.v + search_params.FP_margin.v <= alpha) break;

            // History Pruning
            if ((quiet || !winning_capture) && !pv_node && depth <= search_params.history_pruning_depth.v &&
                move_history_score <= (depth + improving) * -search_params.history_pruning_divisor.v) continue;

            // SEE Pruning
            if (depth <= search_params.SEE_depth.v + base_reduction
                 && legal_moves >= 3 && move_history_score <= 5000 &&
                 !get_static_exchange_evaluation(position, move, (quiet ? -24 * lmr_base_depth * lmr_base_depth: -90 * depth)))
                continue;

        }

        // Make the move
        bool attempt = position.make_move<NNUE>(move, position.state_stack[thread_state.search_ply], thread_state.fifty_move);
        engine.tt_prefetch_read(position.hash_key);  // Prefetch the TT for cache

        // The move put us in check, therefore it was not legal, and we must disregard it
        if (!attempt) {
            position.undo_move<NNUE>(move, position.state_stack[thread_state.search_ply], thread_state.fifty_move);
            continue;
        }

        // Increase node count
        thread_state.node_count++;

        // Extensions
        PLY_TYPE extension = 0;

        // Pawn going to 7th rank must be passed
        bool passed_pawn = get_piece_type(informative_move.selected(), ~position.side) == PAWN &&
                           static_cast<int>(rank_of(move.target())) == 6 - 5 * (~position.side);  // side has already been flipped
        bool queen_promotion = move.type() == MOVE_TYPE_PROMOTION &&
                               static_cast<PieceType>(move.promotion_type() + 1) == QUEEN;

        int double_extensions = root ? 0 : position.state_stack[thread_state.search_ply].double_extensions;

        // Checking for singularity
        if (!root &&
            depth >= 6 &&
            move == tt_move &&
            tt_entry.depth >= depth - 2 &&
            tt_entry.flag != HASH_FLAG_UPPER &&
            position.state_stack[thread_state.search_ply].excluded_move == NO_MOVE &&
            abs(tt_entry.score) < MATE_BOUND) {

            position.undo_move<NO_NNUE>(move, position.state_stack[thread_state.search_ply], thread_state.fifty_move);

            int singular_beta = tt_entry.score - depth;

            thread_state.search_ply++;

            position.state_stack[thread_state.search_ply].excluded_move = move;
            SCORE_TYPE return_eval = negamax<NNUE>(engine, singular_beta - 1, singular_beta, (depth - 1) / 2,
                                                   false, cutnode, thread_id);
            position.state_stack[thread_state.search_ply].excluded_move = NO_MOVE;

            thread_state.search_ply--;

            // Singular Extensions
            if (return_eval < singular_beta) {
                extension++;
                if (!pv_node && return_eval < singular_beta - 16 && double_extensions <= 7) extension++;
            }

            // Multi-cut Pruning
            else if (singular_beta >= beta) {
                return singular_beta;
            }

            else if (tt_entry.score >= beta || tt_entry.score <= alpha) {
                extension--;
            }

            position.make_move<NO_NNUE>(move, position.state_stack[thread_state.search_ply], thread_state.fifty_move);
        }

        else if (move_score >= 0 && (passed_pawn || queen_promotion)) extension++;

        else if (in_check) extension++;

        extension = std::min<PLY_TYPE>(extension, std::min<PLY_TYPE>(2, MAX_AB_DEPTH - 1 - depth));

        position.state_stack[thread_state.search_ply].double_extensions = root ? 0 :
                position.state_stack[thread_state.search_ply - 1].double_extensions + (extension == 2);

        PLY_TYPE new_depth = depth + extension - 1;

        // Prepare for recursive searching
        position.update_nnue(position.state_stack[thread_state.search_ply]);

        thread_state.search_ply++;
        thread_state.game_ply++;
        thread_state.repetition_table[thread_state.game_ply] = position.hash_key;

        bool move_gives_check = position.is_attacked(position.get_king_pos(position.side), position.side);
        position.state_stack[thread_state.search_ply].in_check = static_cast<int>(move_gives_check);


        bool is_killer_move = informative_move == thread_state.killer_moves[0][thread_state.search_ply - 1] ||
                              informative_move == thread_state.killer_moves[1][thread_state.search_ply - 1];

        bool interesting = passed_pawn || queen_promotion || move_gives_check || is_killer_move;

        SCORE_TYPE return_eval = -SCORE_INF;

        uint64_t current_nodes = thread_state.node_count;

        int reduction = 0;
        bool full_depth_zero_window;

        // Late Move Reductions (LMR)
        // The idea that if moves are ordered well, then moves that are searched
        // later shouldn't be as good, and therefore we don't need to search them to a very high depth
        if (legal_moves >= 1 + root + pv_node
            && depth >= 3
            && (quiet || !winning_capture)
            ){

            // Get the base reduction based on depth and moves searched
            reduction = engine.LMR_REDUCTIONS_QUIET[depth][std::min(legal_moves, 63)];

            reduction -= !quiet;

            // Fewer reductions if we are in a pv node, since moves are likely to be better and more important
            reduction -= pv_node;

            // Fewer reductions when improving, since the current node and moves searched in it are more important
            reduction -= improving;
            reduction += failing;

            // Fewer reductions for interesting moves which we define above
            reduction -= interesting;

            // Do not reduce moves as much if we are in check, because the position is possibly volatile
            reduction -= in_check;

            // Scale the reduction based on the move's history score
            reduction -= move_history_score / 8192;

            // Scale reductions based on how many moves have already raised alpha
            reduction += static_cast<int>(static_cast<double>(alpha_raised_count) * (0.5 + 0.5 * tt_move_noisy));

            // My idea that in a null move search you can be more aggressive with LMR
            reduction += null_search;

            // Reduce more on cutnodes
            reduction += cutnode;

            // Clamp the LMR depth
            reduction = std::clamp<PLY_TYPE>(reduction, 0, new_depth - 1);
            auto lmr_depth = new_depth - reduction;

            // Recursively search
            return_eval = -negamax<NNUE>(engine, -alpha - 1, -alpha, lmr_depth, true, true, thread_id);

            // Check if we need to search at full depth with a zero window
            full_depth_zero_window = return_eval > alpha && lmr_depth != new_depth;
        }

        else {
            // Check if we need to search at full depth with a zero window
            full_depth_zero_window = !pv_node || legal_moves >= 1;
        }

        // Search at a full depth with a zero window
        // The zero window is a type of assumption that the current move being searched will not raise alpha,
        // which is why the bounds are [-alpha - 1, -alpha] instead of [-beta, -alpha]
        if (full_depth_zero_window) {
            if (return_eval != -SCORE_INF) {
                new_depth += return_eval >= best_score + 85;        // Deeper Search
                new_depth -= return_eval <  best_score + new_depth; // Shallower Search
            }

            return_eval = -negamax<NNUE>(engine, -alpha - 1, -alpha, new_depth, true, !cutnode, thread_id);
        }

        // Search to a full depth at normal bounds if necessary
        if (return_eval == -SCORE_INF || (pv_node && ((return_eval > alpha && return_eval < beta) || legal_moves == 0))) {
            return_eval = -negamax<NNUE>(engine, -beta, -alpha, new_depth, true, false, thread_id);
        }

        // Undo the move and other changes
        thread_state.game_ply--;
        position.state_stack[thread_state.search_ply].in_check = -1;
        thread_state.search_ply--;

        position.undo_move<NNUE>(move, position.state_stack[thread_state.search_ply], thread_state.fifty_move);

        // Return early if the engine is stopped in order to prevent changes
        if (engine.stopped) return 0;

        // Update legal move count
        legal_moves++;

        // This move is better than other moves searched
        if (return_eval > best_score) {
            best_score = return_eval;
            best_move = move;

            // Calculate nodes for each move for time management scaling
            if (root && thread_id == 0) {
                engine.node_table[position.board[best_move.origin()]][best_move.target()]
                    += thread_state.node_count - current_nodes;
            }

            // Write moves into the PV table
            if (thread_id == 0) {
                engine.pv_table[thread_state.search_ply][thread_state.search_ply] = move;

                for (int next_ply = thread_state.search_ply + 1; next_ply < engine.pv_length[thread_state.search_ply + 1]; next_ply++) {
                    engine.pv_table[thread_state.search_ply][next_ply] = engine.pv_table[thread_state.search_ply + 1][next_ply];
                }

                engine.pv_length[thread_state.search_ply] = engine.pv_length[thread_state.search_ply + 1];
            }

            // The move has raised alpha
            if (return_eval > alpha) {
                alpha_raised_count++;
                alpha = return_eval;

                // We have found a better move that increased achieved us an exact score
                tt_hash_flag = HASH_FLAG_EXACT;

                // History Heuristic for move ordering
                SCORE_TYPE bonus = depth * (depth + 1 + null_search + pv_node + improving) - 1;

                update_histories(thread_state, informative_move, last_moves, quiet, winning_capture, bonus);

#ifdef SHOW_STATISTICS
                engine.search_results.alpha_raised_count++;
                if (legal_moves <= ALPHA_RAISE_STATS_COUNT) {
                    engine.search_results.search_alpha_raises[legal_moves-1]++;
                }
#endif

                // Alpha - Beta cutoff. We have failed high here.
                if (return_eval >= beta) {

#ifdef SHOW_STATISTICS
                    if (legal_moves <= FAIL_HIGH_STATS_COUNT) engine.search_results.search_fail_highs[legal_moves-1]++;
                    if (move == tt_move) engine.search_results.search_fail_high_types[0]++;
                    else if (informative_move == thread_state.killer_moves[0][thread_state.search_ply]) engine.search_results.search_fail_high_types[1]++;
                    else if (informative_move == thread_state.killer_moves[1][thread_state.search_ply]) engine.search_results.search_fail_high_types[2]++;
                    else if (quiet) engine.search_results.search_fail_high_types[3]++;
                    else if (!winning_capture) engine.search_results.search_fail_high_types[4]++;
                    else engine.search_results.search_fail_high_types[5]++;
#endif

                    // Killer Heuristic for move ordering
                    if (quiet) {
                        if (!null_search || thread_state.killer_moves[0][thread_state.search_ply] == NO_INFORMATIVE_MOVE) {
                            thread_state.killer_moves[1][thread_state.search_ply] = thread_state.killer_moves[0][thread_state.search_ply];
                            thread_state.killer_moves[0][thread_state.search_ply] = informative_move;
                        } else {
                            thread_state.killer_moves[1][thread_state.search_ply] = informative_move;
                        }
                    }

                    tt_hash_flag = HASH_FLAG_LOWER;
                    break;
                }
            }
        }
    }

    if (legal_moves == 0) {
        if (singular_search) return alpha;
        if (!in_check) return 0;
        else return -MATE_SCORE + thread_state.search_ply;
    }

    engine.record_tt_entry(thread_id, position.hash_key, best_score, tt_hash_flag, best_move, depth,
                           position.state_stack[thread_state.search_ply].static_eval, pv_node);

    return best_score;
}

template SCORE_TYPE negamax<USE_NNUE>(Engine& engine, SCORE_TYPE alpha, SCORE_TYPE beta, PLY_TYPE depth, bool do_null, bool cutnode, int thread_id);
template SCORE_TYPE negamax<NO_NNUE >(Engine& engine, SCORE_TYPE alpha, SCORE_TYPE beta, PLY_TYPE depth, bool do_null, bool cutnode, int thread_id);


void print_thinking(Engine& engine, NodeType node, SCORE_TYPE best_score, int pv_number, int thread_id) {

    Thread_State& thread_state = engine.thread_states[thread_id];
    Position& position = thread_state.position;

    if (!engine.print_thinking) return;

    PLY_TYPE depth = thread_state.current_search_depth;

    // Calculate the elapsed time and NPS
    auto end_time = std::chrono::high_resolution_clock::now();
    auto end_int = std::chrono::duration_cast<std::chrono::milliseconds>
            (std::chrono::time_point_cast<std::chrono::milliseconds>(end_time).time_since_epoch()).count();

    uint64_t elapsed_time = end_int - engine.start_time;
    elapsed_time = std::max<uint64_t>(elapsed_time, 1);

    uint64_t total_nodes = 0;
    for (Thread_State& thread_state_i : engine.thread_states) {
        total_nodes += thread_state_i.node_count;
    }

    auto nps = static_cast<uint64_t>(static_cast<double>(total_nodes) /
                                     (static_cast<double>(elapsed_time) / 1000.0));

    // Format the scores for printing to UCI
    best_score = normalize_score(best_score, thread_state.get_full_game_ply());

    SCORE_TYPE format_score = best_score;
    std::string string_score = "cp ";

    if (abs(best_score) >= MATE_BOUND) {
        string_score = "mate ";
        format_score = best_score >= MATE_BOUND ?
                       (MATE_SCORE - best_score) / 2 + 1 :
                       (-MATE_SCORE - best_score) / 2;
    }

    string_score += std::to_string(format_score);

    // Identify the type of node / bound if necessary
    if (node == Lower_Node) string_score += " lowerbound";
    else if (node == Upper_Node) string_score += " upperbound";

    // PV information
    std::string pv_line;

    // If the depth is low, or we are printing a PV line from a failed aspiration search,
    // then only store a limited number of moves in the PV line
    int max_pv_length = engine.pv_length[0];
    if (depth <= 12 || node == Lower_Node || node == Upper_Node)
        max_pv_length = std::min(max_pv_length, 1 + max_pv_length / 3);

    // Get the PV line
    Color original_side = position.side;
    for (int c = 0; c < max_pv_length; c++) {
        pv_line += engine.pv_table[0][c].get_uci(position);
        pv_line += " ";
        position.side = ~position.side;
    }
    position.side = original_side;

    // Print the UCI search information
    std::cout << "info multipv " << pv_number + 1
              << " depth " << depth << " seldepth " << thread_state.selective_depth
              << " score " << string_score;

    if (engine.show_wdl) {
        if (best_score >= MATE_BOUND) std::cout << " wdl 1000 0 0";
        else if (best_score <= -MATE_BOUND) std::cout << " wdl 0 0 1000";
        else {
            auto [win, draw, loss] = get_wdl(best_score, thread_state.get_full_game_ply());
            std::cout << " wdl " << win << " " << draw << " " << loss;
        }
    }

    std::cout << " time " << elapsed_time
              << " nodes " << total_nodes << " nps " << nps
              << " pv " << pv_line << std::endl;

}


template<bool NNUE>
SCORE_TYPE aspiration_window(Engine& engine, SCORE_TYPE previous_score, PLY_TYPE& asp_depth, Move& best_move, int thread_id) {

    Thread_State& thread_state = engine.thread_states[thread_id];

    SCORE_TYPE alpha = -SCORE_INF;
    SCORE_TYPE beta = SCORE_INF;
    SCORE_TYPE delta = std::max(6 + static_cast<int>(85 / (asp_depth - 2)), 10);

    PLY_TYPE depth = thread_state.current_search_depth;

    if (depth >= MINIMUM_ASP_DEPTH) {
        alpha = std::max(previous_score - delta, -SCORE_INF);
        beta  = std::min(previous_score + delta,  SCORE_INF);
    }

    SCORE_TYPE return_eval = 0;
    while (true) {

        // Completely expand the bounds once they have exceeded a certain score
        if (alpha <= -1000) alpha = -SCORE_INF;
        if (beta  >=  1000) beta  =  SCORE_INF;

        return_eval = negamax<NNUE>(engine, alpha, beta, depth, false, false, thread_id);

        if (engine.stopped) break;

        // The aspiration window search has failed low.
        // The position is probably worse than we expect, so both alpha and beta should be relaxed.
        // Alpha will be relaxed with delta while beta will be skewed towards alpha.
        if (return_eval <= alpha) {
            alpha = std::max(alpha - delta, -SCORE_INF);
            beta  = (alpha + 3 * beta) / 4;
            depth = thread_state.current_search_depth;

            asp_depth--;
            asp_depth = std::max<PLY_TYPE>(6, asp_depth);

            if (depth >= 18 && thread_id == 0) print_thinking(engine, Lower_Node, return_eval, 0, thread_id);
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

            if (thread_id == 0) best_move = engine.pv_table[0][0];  // Safe to update the best move on fail highs.

            if (depth >= 18 && thread_id == 0) print_thinking(engine, Upper_Node, return_eval, 0, thread_id);
        }

        // We have achieved an exact node where the score was between alpha and beta.
        // We are certain of our score and can now safely return.
        else {
            if (thread_id == 0) {
                best_move = engine.pv_table[0][0];
                print_thinking(engine, Exact_Node, return_eval, 0, thread_id);
            }
            break;
        }

        // Increase delta in the case that we must re-search the aspiration windows multiple times.
        delta += delta * 2 / 3;
    }

    return return_eval;
}

template SCORE_TYPE aspiration_window<USE_NNUE>(Engine& engine, SCORE_TYPE previous_score, PLY_TYPE& asp_depth, Move& best_move, int thread_id);
template SCORE_TYPE aspiration_window<NO_NNUE >(Engine& engine, SCORE_TYPE previous_score, PLY_TYPE& asp_depth, Move& best_move, int thread_id);


template <bool NNUE>
SCORE_TYPE multi_pv_search(Engine& engine, SCORE_TYPE previous_score, PLY_TYPE& asp_depth, Move& best_move, int thread_id) {
    Thread_State& thread_state = engine.thread_states[thread_id];
    Position& position = thread_state.position;

    thread_state.excluded_root_moves.clear();

    position.clear_state_stack();
    int main_score = aspiration_window<NNUE>(engine, previous_score, asp_depth, best_move, thread_id);

    thread_state.excluded_root_moves.insert(best_move.internal_move());

    PLY_TYPE depth = thread_state.current_search_depth;

    int alternate_pvs = std::min(engine.multi_pv, static_cast<int>(engine.root_moves.size()));
    for (int i = 1; i < alternate_pvs; i++) {
        int pv_score = negamax<NNUE>(engine, -SCORE_INF, SCORE_INF, depth, false, false, thread_id);
        if (engine.stopped) break;

        if (thread_id == 0) print_thinking(engine, Exact_Node, pv_score, i, thread_id);
        thread_state.excluded_root_moves.insert(engine.pv_table[0][0].internal_move());
    }

    thread_state.excluded_root_moves.clear();

    return main_score;
}

template SCORE_TYPE multi_pv_search<USE_NNUE>(Engine& engine, SCORE_TYPE previous_score, PLY_TYPE& asp_depth, Move& best_move, int thread_id);
template SCORE_TYPE multi_pv_search<NO_NNUE >(Engine& engine, SCORE_TYPE previous_score, PLY_TYPE& asp_depth, Move& best_move, int thread_id);


template<bool NNUE>
void iterative_search(Engine& engine, int thread_id) {

    Thread_State& thread_state = engine.thread_states[thread_id];
    Position& position = thread_state.position;

    thread_state.thread_id = thread_id;
    thread_state.reset_generators();

    thread_state.terminated = false;

    // Initialize variables
    SCORE_TYPE previous_score = 0;
    PLY_TYPE running_depth = 1;
    PLY_TYPE asp_depth = 6;

    uint64_t original_soft_time_limit = engine.soft_time_limit;
    Move best_move = NO_MOVE;

    SCORE_TYPE low_depth_score = 0;

    while (running_depth <= engine.max_depth) {
        thread_state.current_search_depth = running_depth;

        // Run a search with aspiration window bounds
        previous_score = multi_pv_search<NNUE>(engine, previous_score, asp_depth, best_move, thread_id);

        // Update information when engine has finished a search (it hasn't stopped in the middle of a search)
        if (thread_id == 0 && !engine.stopped) engine.search_results.score = previous_score;

        if (thread_id == 0) {
            if (running_depth <= 4) low_depth_score = previous_score;

            if (running_depth >= 8) {
                auto best_node_percentage =
                        static_cast<double>(engine.node_table[position.board[best_move.origin()]] [best_move.target()]) /
                        static_cast<double>(thread_state.node_count);

                double node_scaling_factor = (1.45 - best_node_percentage) * 1.71;

                SCORE_TYPE score_difference = previous_score - low_depth_score;
                score_difference = abs(std::clamp(score_difference, -300, 150));

                double score_scaling_factor = 0.98 + (score_difference / 350.0);

                engine.soft_time_limit = static_cast<uint64_t>(static_cast<double>(original_soft_time_limit)
                        * node_scaling_factor
                        * score_scaling_factor);

                // std::cout << "Soft Time Limit changed to: " << engine.soft_time_limit << std::endl;
            }


            // Calculate the elapsed time
            auto end_time = std::chrono::high_resolution_clock::now();
            uint64_t elapsed_time = std::chrono::duration_cast<std::chrono::milliseconds>
                     (std::chrono::time_point_cast<std::chrono::milliseconds>(end_time).time_since_epoch()).count()
                     - engine.start_time;

            elapsed_time = std::max<uint64_t>(elapsed_time, 1);

            // Stop the engine when we have exceeded the soft time limit
            if (running_depth >= engine.min_depth && thread_id == 0) {
                if (elapsed_time >= engine.soft_time_limit) engine.stopped = true;
            }

            // Check remaining nodes to be searched
            uint64_t total_nodes = 0;
            for (Thread_State& thread_state_i : engine.thread_states) {
                total_nodes += thread_state_i.node_count;
            }
            if (engine.soft_node_limit && total_nodes >= engine.soft_node_limit) engine.stopped = true;
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

    uint64_t total_nodes = 0;
    for (Thread_State& thread_state_i : engine.thread_states) {
        total_nodes += thread_state_i.node_count;
    }

    engine.search_results.depth_reached = running_depth;
    engine.search_results.node_count = total_nodes;
    engine.search_results.best_move = best_move;

    if (thread_id == 0) {
        std::string best_move_str = best_move.get_uci(position);
        if (engine.print_thinking) std::cout << "bestmove " << best_move_str << std::endl;
    }

    thread_state.terminated = true;
}

template void iterative_search<USE_NNUE>(Engine& engine, int thread_id);
template void iterative_search<NO_NNUE >(Engine& engine, int thread_id);


template<bool NNUE>
void lazy_smp_search(Engine& engine) {

    std::vector<std::thread> search_threads;
    //std::vector<Position> new_positions;

    for (int thread_id = 1; thread_id < engine.num_threads; thread_id++) {
        // std::cout << "Creating Helper Thread #" << thread_id << std::endl;
        engine.thread_states[thread_id] = engine.thread_states[0];
        engine.thread_states[thread_id].position.ensure_stable();
        search_threads.emplace_back(iterative_search<NNUE>, std::ref(engine), thread_id);
    }

    //engine.thread_states[0].position.print_board();
    iterative_search<NNUE>(engine, 0);
    // std::cout << "Search ended" << std::endl;

    for (int thread_id = engine.num_threads - 1; thread_id >= 1; thread_id--) {
        search_threads[thread_id - 1].join();
        // std::cout << "Helper Thread #" << thread_id << " closed." << std::endl;
    }

#ifdef SHOW_STATISTICS
    print_statistics(engine.search_results);
#endif

}

template void lazy_smp_search<USE_NNUE>(Engine& engine);
template void lazy_smp_search<NO_NNUE >(Engine& engine);


void search(Engine& engine) {
    Thread_State& thread_state = engine.thread_states[0];
    Position& position = thread_state.position;

    engine.reset();

    engine.stopped = false;

    auto start_time = std::chrono::high_resolution_clock::now();
    engine.start_time = std::chrono::duration_cast<std::chrono::milliseconds>
            (std::chrono::time_point_cast<std::chrono::milliseconds>(start_time).time_since_epoch()).count();

    position.get_pseudo_legal_moves<Movegen::All, true>(position.scored_moves[0]);
    position.set_state(position.state_stack[0], thread_state.fifty_move);

    for (ScoredMove scored_move : position.scored_moves[0]) {
        bool attempt = position.make_move<NO_NNUE>(scored_move.move, position.state_stack[0], thread_state.fifty_move);
        position.undo_move<NO_NNUE>(scored_move.move, position.state_stack[0], thread_state.fifty_move);

        if (attempt) engine.root_moves.insert(scored_move.move.internal_move());
    }

    if (engine.use_nnue) lazy_smp_search<USE_NNUE>(engine);
    else lazy_smp_search<NO_NNUE>(engine);
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
    std::cout << "Quiet move fail highs: " << res.search_fail_high_types[3] << " percentage: " <<
              double(res.search_fail_high_types[3]) / double(total_fail_highs) * 100 << "%\n";
    std::cout << "Bad Noisy move fail highs: " << res.search_fail_high_types[4] << " percentage: " <<
              double(res.search_fail_high_types[4]) / double(total_fail_highs) * 100 << "%\n";
    std::cout << "Good Noisy move fail highs: " << res.search_fail_high_types[5] << " percentage: " <<
              double(res.search_fail_high_types[5]) / double(total_fail_highs) * 100 << "%\n";

    std::cout << "\n\n---------------------------------------------------------" << std::endl;
}


void print_search_tuning_config() {
    for (auto& param : search_params.all_parameters) {
        std::cout << param->name << ", "
                  << "int, "
                  << param->v << ", "
                  << param->min << ", "
                  << param->max << ", "
                  << param->step << ", "
                  << learning_rate << std::endl;
    }
}

