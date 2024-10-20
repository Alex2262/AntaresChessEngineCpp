

#ifndef ALTAIR_SEARCH_H
#define ALTAIR_SEARCH_H

#include <vector>
#include <unordered_set>
#include "position.h"
#include "move_ordering.h"

// #define DO_SEARCH_TUNING
// #define SHOW_STATISTICS

constexpr double learning_rate = 0.002;

constexpr int correction_history_grain = 256;
constexpr int correction_history_weight_scale = 1024;
constexpr int correction_history_size = 16384;
constexpr int correction_history_max = correction_history_grain * 64;


struct TT_Entry {                          // 22 bytes --> 24 bytes
    HASH_TYPE  key        = 0;             // 8 bytes
    SCORE_TYPE score      = SCORE_NONE;    // 4 bytes
    SCORE_TYPE evaluation = NO_EVALUATION; // 4 bytes
    Move       move       = NO_MOVE;       // 2 bytes
    PLY_TYPE   depth      = 0;             // 2 bytes
    short      flag       = 0;             // 1 byte
    bool       pv_node    = false;         // 1 byte
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

    //                             v   min  max step        name
    T LMR_divisor_quiet       = { 230, 150, 230, 20, "LMR_divisor_quiet"};
    T LMR_base_quiet          = { 140, 100, 170, 20, "LMR_base_quiet"};
    T LMR_history_divisor     = {10000, 4000, 14000, 1000, "LMR_history_divisor"};

    T RFP_depth               = {   7,   5,  11,   2, "RFP_depth"};
    T RFP_margin              = { 110,  50, 200,  30, "RFP_margin"};

    T LMP_depth               = {   4,   1,   5,   1, "LMP_depth"};
    T LMP_margin              = {  10,   5,  15,   1, "LMP_margin"};

    T history_pruning_depth   = {  10,   3,  20,   2, "history_pruning_depth"};
    T history_pruning_divisor = {9600, 1000, 20000, 1800, "history_pruning_divisor"};

    T NMP_depth               = {   1,   0,   4,   1, "NMP_depth"};
    T NMP_base                = {   5,   1,   5,   1, "NMP_base"};
    T NMP_depth_divisor       = {   4,   2,   6,   1, "NMP_depth_divisor"};
    T NMP_eval_divisor        = { 400, 100, 400,  40, "NMP_eval_divisor"};
    T NMP_condition_base      = { 128,   0, 200,  10, "NMP_condition_base"};
    T NMP_condition_depth     = {  11,   0,  20,   2, "NMP_condition_depth"};
    T NMP_condition_improving = {   3,   0,  20,   2, "NMP_condition_improving"};

    T SEE_base_depth          = {   2,   1,  10,   1, "SEE_base_depth"};
    T SEE_noisy_depth         = {   0,   0,   4,   1, "SEE_noisy_depth"};
    T SEE_pv_depth            = {   5,   0,   5,   1, "SEE_pv_depth"};
    T SEE_base_moves          = {   4,   1,   5,   1, "SEE_base_moves"};
    T SEE_base_history        = {5800, 1000, 10000, 1000, "SEE_base_history"};
    T SEE_quiet_multiplier    = {  40,  20,  80,   8, "SEE_quiet_multiplier"};
    T SEE_noisy_multiplier    = {  85,  50, 150,  10, "SEE_noisy_multiplier"};

    T SEE_MO_threshold        = {  85,  60, 120,   5, "SEE_MO_threshold"};

    T LMP_margin_quiet        = {   2,   1,   4,   1, "LMP_margin_quiet"};

    T FP_depth                = {   7,   1,   8,   1, "FP_depth"};
    T FP_multiplier           = { 170,  80, 220,  20, "FP_multiplier"};
    T FP_margin               = {  70,  40, 150,  20, "FP_margin"};

    T QSEE_base               = {  70,  40, 200,  10, "QSEE_base"};

    T IIR_base_depth          = {   4,   1,   8,   1, "IIR_base_depth"};

    T SE_base_depth           = {   6,   3,   9,   1, "SE_base_depth"};
    T SE_dext_margin          = {  12,   6,  25,   2, "SE_dext_margin"};
    T SE_dext_limit           = {   7,   4,  12,   1, "SE_dext_limit"};

    T deeper_margin           = {  80,  50, 130,   8, "deeper_margin"};

    T ASP_beta_scaler         = {   5,   1,  10,   1, "ASP_beta_scaler"};
    T ASP_alpha_scaler        = {   3,   1,  10,   1, "ASP_alpha_scaler"};
    T ASP_delta_scaler        = {   2,   1,  10,   1, "ASP_delta_scaler"};
    T ASP_delta_divisor       = {   5,   1,  10,   1, "ASP_delta_divisor"};
    T ASP_delta_min           = {   9,   4,  15,   1, "ASP_delta_min"};

    T H_max_quiet             = {  9000, 5000, 18000, 600, "H_max_quiet"};
    T H_max_noisy             = { 12000, 5000, 18000, 600, "H_max_noisy"};
    T H_max_cont              = { 11000, 5000, 18000, 600, "H_max_cont"};

    T TM_node_margin          = { 135, 100, 200,   8, "TM_node_margin"};
    T TM_node_scalar          = { 180, 140, 200,   6, "TM_node_scalar"};

    T TM_score_min            = { 276, 100, 500,  30, "TM_score_min"};
    T TM_score_max            = { 136,  50, 400,  30, "TM_score_max"};
    T TM_score_base           = {  98,  70, 100,   5, "TM_score_base"};
    T TM_score_divisor        = { 381, 150, 500,  25, "TM_score_divisor"};

    T TM_hard_scalar          = { 281, 150, 400,  20, "TM_hard_scalar"};
    T TM_soft_scalar          = {  78,  40, 150,  10, "TM_soft_scalar"};


    T* all_parameters[50] = {
            &LMR_divisor_quiet,
            &LMR_base_quiet,
            &LMR_history_divisor,

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
            &NMP_condition_base,
            &NMP_condition_depth,
            &NMP_condition_improving,

            &SEE_base_depth,
            &SEE_noisy_depth,
            &SEE_pv_depth,
            &SEE_base_moves,
            &SEE_base_history,
            &SEE_quiet_multiplier,
            &SEE_noisy_multiplier,

            &SEE_MO_threshold,

            &LMP_margin_quiet,

            &FP_depth,
            &FP_multiplier,
            &FP_margin,

            &QSEE_base,

            &IIR_base_depth,

            &SE_base_depth,
            &SE_dext_margin,
            &SE_dext_limit,

            &deeper_margin,

            &ASP_beta_scaler,
            &ASP_alpha_scaler,
            &ASP_delta_scaler,
            &ASP_delta_divisor,
            &ASP_delta_min,

            &H_max_quiet,
            &H_max_noisy,
            &H_max_cont,

            &TM_node_margin,
            &TM_node_scalar,

            &TM_score_min,
            &TM_score_max,
            &TM_score_base,
            &TM_score_divisor,

            &TM_hard_scalar,
            &TM_soft_scalar
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
    SCORE_TYPE history_moves[12][64][2][2]{}; // piece | target_square | origin_threat | target_threat
    SCORE_TYPE capture_history[2][12][12][64]{};
    SCORE_TYPE continuation_history[12][64][12][64]{};
    SCORE_TYPE correction_history[2][correction_history_size]{};
    SCORE_TYPE correction_history_np[2][correction_history_size]{};
    SCORE_TYPE correction_history_major[2][correction_history_size]{};
    SCORE_TYPE correction_history_minor[2][correction_history_size]{};

    HASH_TYPE repetition_table[TOTAL_MAX_DEPTH + 512] = {0};

    bool terminated = true;

    std::array<Generator, TOTAL_MAX_DEPTH> generators{};
    std::array<FixedVector<ScoredMove, MAX_MOVES>, TOTAL_MAX_DEPTH> searched_quiets{};
    std::array<FixedVector<ScoredMove, MAX_NOISY>, TOTAL_MAX_DEPTH> searched_noisy{};

    inline PLY_TYPE get_full_game_ply() const { return base_full_moves * 2 + game_ply + position.side; }
    bool detect_repetition();
    bool detect_repetition_3();

    SCORE_TYPE& get_continuation_history_entry(InformativeMove last_move, InformativeMove informative_move);

    void update_correction_history_entry(SCORE_TYPE& c_hist_entry, PLY_TYPE depth, SCORE_TYPE diff);
    void update_correction_history_score(PLY_TYPE depth, SCORE_TYPE diff);
    SCORE_TYPE get_correction_score(SCORE_TYPE& c_hist_entry);
    SCORE_TYPE correct_evaluation(SCORE_TYPE evaluation);

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
                         SCORE_TYPE static_eval, bool tt_pv);
    short probe_tt_entry_q(int thread_id, HASH_TYPE hash_key, SCORE_TYPE alpha, SCORE_TYPE beta,
                           TT_Entry& return_entry);
    void record_tt_entry_q(int thread_id, HASH_TYPE hash_key, SCORE_TYPE score, short tt_flag, Move move,
                           SCORE_TYPE static_eval, bool tt_pv);
    SCORE_TYPE probe_tt_evaluation(HASH_TYPE hash_key);

    void tt_prefetch_read(HASH_TYPE hash_key);

    bool check_time();
    bool check_nodes();

    template<bool NNUE>
    SCORE_TYPE evaluate(int thread_id);
};

void update_history_entry(SCORE_TYPE& score, SCORE_TYPE bonus, SCORE_TYPE max_score);
void update_histories(Thread_State& thread_state, InformativeMove informative_move,
                      InformativeMove last_moves[], bool quiet, bool winning_capture,
                      int bonus);

template<bool NNUE>
SCORE_TYPE qsearch(Engine& engine, SCORE_TYPE alpha, SCORE_TYPE beta, PLY_TYPE depth, int thread_id);
template<bool NNUE>
SCORE_TYPE negamax(Engine& engine, SCORE_TYPE alpha, SCORE_TYPE beta, PLY_TYPE depth,
                   bool do_null, bool cutnode, bool full_null_search, int thread_id);

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
