

#ifndef ALTAIR_SEARCH_H
#define ALTAIR_SEARCH_H

#include <vector>
#include <unordered_set>
#include "position.h"
#include "move_ordering.h"

// #define DO_SEARCH_TUNING
// #define SHOW_STATISTICS

constexpr double learning_rate = 0.002;

struct TT_Entry {
    HASH_TYPE key = 0;
    SCORE_TYPE score = SCORE_NONE;
    SCORE_TYPE evaluation = NO_EVALUATION;
    Move move = NO_MOVE;
    PLY_TYPE depth = 0;
    short flag = 0;
};


struct Search_Results {
    int num_searches = 1;
    uint64_t node_count = 0;

    uint64_t qsearch_fail_highs[FAIL_HIGH_STATS_COUNT]{};
    uint64_t search_fail_highs[FAIL_HIGH_STATS_COUNT]{};
    uint64_t search_fail_high_types[6]{};

    uint64_t alpha_raised_count = 0;
    uint64_t search_alpha_raises[ALPHA_RAISE_STATS_COUNT]{};

    PLY_TYPE depth_reached = 0;

    Move best_move = NO_MOVE;
    SCORE_TYPE score = 0;
};

struct T {
    int v;
    int min;
    int max;
    int step;

    std::string name;
};

struct SearchParameters {

    T LMR_divisor_quiet = {224, 150, 230, 20, "LMR_divisor_quiet"};
    T LMR_base_quiet    = {111, 100, 170, 20, "LMR_base_quiet"};

    T RFP_depth  = {  8,   5,  11,   2, "RFP_depth"};
    T RFP_margin = {103,  50, 200,  30, "RFP_margin"};

    T LMP_depth  = { 5,  1,  5,  1, "LMP_depth"};
    T LMP_margin = {10,  5, 15,  1, "LMP_margin"};

    T history_pruning_depth   = {9,  3, 20, 2, "history_pruning_depth"};
    T history_pruning_divisor = {6594, 1000, 20000, 1800, "history_pruning_divisor"};

    T NMP_depth = {1, 0, 4, 1, "NMP_depth"};
    T NMP_base  = {4, 1, 5, 1, "NMP_base"};
    T NMP_depth_divisor = {4, 2, 6, 1, "NMP_depth_divisor"};
    T NMP_eval_divisor  = {345, 100, 400, 40, "NMP_eval_divisor"};

    T SEE_depth = {7,  1, 10,  1, "SEE_base_depth"};

    T LMP_margin_quiet = {2, 1, 4, 1, "LMP_margin_quiet"};

    T FP_depth      = {7, 1, 8, 1, "FP_depth"};
    T FP_multiplier = {197, 80, 220, 20, "FP_multiplier"};
    T FP_margin     = { 45, 40, 150, 20, "FP_margin"};

    T* all_parameters[19] = {
            &LMR_divisor_quiet,
            &LMR_base_quiet,

            &RFP_depth,
            &RFP_margin,

            &LMP_depth,
            &LMP_margin,

            &history_pruning_depth,
            &history_pruning_divisor,

            &NMP_depth,
            &NMP_base,
            &NMP_depth_divisor,
            &NMP_eval_divisor,

            &SEE_depth,

            &LMP_margin_quiet,

            &FP_depth,
            &FP_multiplier,
            &FP_margin
    };
};

class Thread_State {

public:

    inline Thread_State() {
        reset_generators();
    };

    int thread_id = 0;

    PLY_TYPE selective_depth = 0;

    Position position{};

    PLY_TYPE current_search_depth = 0;
    PLY_TYPE search_ply = 0;
    PLY_TYPE base_full_moves = 1;
    PLY_TYPE game_ply = 0;
    PLY_TYPE fifty_move = 0;

    uint64_t node_count = 0;

    std::unordered_set<uint16_t> excluded_root_moves{};

    InformativeMove killer_moves[2][MAX_AB_DEPTH]{};  // killer moves (2) | max_depth (64)
    SCORE_TYPE history_moves[12][64]{}; // piece | target_square
    SCORE_TYPE capture_history[2][12][12][64]{};
    SCORE_TYPE continuation_history[12][64][12][64]{};

    HASH_TYPE repetition_table[TOTAL_MAX_DEPTH + 512] = {0};

    bool terminated = true;

    std::array<Generator, TOTAL_MAX_DEPTH> generators{};
    std::array<FixedVector<ScoredMove, MAX_MOVES>, TOTAL_MAX_DEPTH> searched_quiets{};
    std::array<FixedVector<ScoredMove, MAX_NOISY>, TOTAL_MAX_DEPTH> searched_noisy{};

    inline PLY_TYPE get_full_game_ply() const { return base_full_moves * 2 + game_ply + position.side; }
    bool detect_repetition();
    bool detect_repetition_3();

    SCORE_TYPE& get_continuation_history_entry(InformativeMove last_move, InformativeMove informative_move);

    inline void reset_generators() {
        for (int ply = 0; ply < static_cast<int>(generators.size()); ply++) {
            Generator& generator = generators[ply];
            generator = Generator(*this);
            generator.search_ply = ply;
        }
    }
};


class Engine {

public:

    Engine() = default;

    int num_threads = 1;
    std::vector<Thread_State> thread_states;

    bool stopped = true;
    bool use_nnue = true;
    bool print_thinking = true;
    bool datagen = false;
    bool show_wdl = false;

    int multi_pv = 1;

    int LMR_REDUCTIONS_QUIET[MAX_AB_DEPTH][64]{};

    PLY_TYPE max_depth = MAX_AB_DEPTH - 1;
    PLY_TYPE max_q_depth = TOTAL_MAX_DEPTH - MAX_AB_DEPTH;
    PLY_TYPE min_depth = 1;

    int move_overhead = 50;
    uint64_t hard_time_limit = 60000;
    uint64_t soft_time_limit = 60000;
    uint64_t start_time = 0;

    uint64_t soft_node_limit = 0;
    uint64_t hard_node_limit = 0;

    uint64_t node_table[12][64]{};

    Move pv_table[MAX_AB_DEPTH + 1][MAX_AB_DEPTH + 1]{};
    PLY_TYPE pv_length[MAX_AB_DEPTH + 1] = {0};

    // TT_Entry transposition_table[MAX_TT_SIZE]{};
    std::vector<TT_Entry> transposition_table;

    Search_Results search_results{};

    std::unordered_set<uint16_t> root_moves{};

    void clear_tt();

    void reset();
    void new_game();

    void initialize_lmr_reductions();

    void resize_tt(uint64_t mb);

    short probe_tt_entry(int thread_id, HASH_TYPE hash_key, SCORE_TYPE alpha, SCORE_TYPE beta, PLY_TYPE depth,
                         TT_Entry& return_entry);
    void record_tt_entry(int thread_id, HASH_TYPE hash_key, SCORE_TYPE score, short tt_flag, Move move, PLY_TYPE depth,
                         SCORE_TYPE static_eval, bool pv_node);
    short probe_tt_entry_q(int thread_id, HASH_TYPE hash_key, SCORE_TYPE alpha, SCORE_TYPE beta,
                           SCORE_TYPE& return_score, Move& tt_move);
    void record_tt_entry_q(int thread_id, HASH_TYPE hash_key, SCORE_TYPE score, short tt_flag, Move move,
                           SCORE_TYPE static_eval);
    SCORE_TYPE probe_tt_evaluation(HASH_TYPE hash_key);

    void tt_prefetch_read(HASH_TYPE hash_key);

    bool check_time();
    bool check_nodes();

    template<bool NNUE>
    SCORE_TYPE evaluate(int thread_id);
};

void update_history_entry(SCORE_TYPE& score, SCORE_TYPE bonus);
void update_histories(Thread_State& thread_state, InformativeMove informative_move,
                      InformativeMove last_moves[], bool quiet, bool winning_capture,
                      int bonus);

template<bool NNUE>
SCORE_TYPE qsearch(Engine& engine, SCORE_TYPE alpha, SCORE_TYPE beta, PLY_TYPE depth, int thread_id);
template<bool NNUE>
SCORE_TYPE negamax(Engine& engine, SCORE_TYPE alpha, SCORE_TYPE beta, PLY_TYPE depth,  bool do_null, bool cutnode, int thread_id);

void print_thinking(Engine& engine, NodeType node, SCORE_TYPE best_score, int pv_number, int thread_id);

template<bool NNUE>
SCORE_TYPE aspiration_window(Engine& engine, SCORE_TYPE previous_score, PLY_TYPE& asp_depth, Move& best_move, int thread_id);
template<bool NNUE>
SCORE_TYPE multi_pv_search(Engine& engine, SCORE_TYPE previous_score, PLY_TYPE& asp_depth, Move& best_move, int thread_id);
template<bool NNUE>
void iterative_search(Engine& engine, int thread_id);
template<bool NNUE>
void lazy_smp_search(Engine& engine);
void search(Engine& engine);

void print_statistics(Search_Results& res);

void print_search_tuning_config();

inline SearchParameters search_params{};

#endif //ALTAIR_SEARCH_H
