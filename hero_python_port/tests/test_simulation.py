# hero_python_port/tests/test_simulation.py

import pytest
import numpy as np
import pandas as pd
from typing import List, Dict # Added Dict

from hero_py.parameters import Parameters
from hero_py.state import State
from hero_py.transitions import TransitionMatrix
from hero_py.strategy import Strategy
from hero_py.simulation import SimulationOutput, run_simulation
from hero_py.survival import ExponentialSurvival # For PSM tests
from hero_py.psm import PartitionedSurvivalModelDefinition as PSMDef # For PSM tests


# --- Fixtures for a simple 2-state Markov model ---
@pytest.fixture
def s1_markov() -> State: # Renamed for clarity
    return State(name="Well", cost=100, effect=1.0)

@pytest.fixture
def s2_markov() -> State: # Renamed for clarity
    return State(name="Sick", cost=500, effect=0.5)

@pytest.fixture
def states_list_markov(s1_markov: State, s2_markov: State) -> List[State]:
    return [s1_markov, s2_markov]

@pytest.fixture
def tm_simple(states_list_markov: List[State]) -> TransitionMatrix:
    tm = TransitionMatrix(state_names=[s.name for s in states_list_markov])
    tm.set_row_probs("Well", {"Well": 0.8, "Sick": 0.2})
    tm.set_row_probs("Sick", {"Well": 0.1, "Sick": 0.9})
    return tm

@pytest.fixture
def strategy_markov_simple(states_list_markov: List[State], tm_simple: TransitionMatrix) -> Strategy:
    return Strategy(name="SimpleMarkov", states=states_list_markov, transition_definition=tm_simple)

@pytest.fixture
def global_params_empty() -> Parameters:
    return Parameters()
# --- End Markov Fixtures ---


# --- Fixtures for a simple 3-state PSM ---
@pytest.fixture
def pfs_state_psm() -> State:
    return State(name="ProgressionFree", cost=100, qaly=0.9)

@pytest.fixture
def prog_state_psm() -> State:
    return State(name="Progressed", cost=300, qaly=0.6)

@pytest.fixture
def dead_state_psm() -> State:
    return State(name="Dead", cost=0, qaly=0)

@pytest.fixture
def states_list_psm(pfs_state_psm: State, prog_state_psm: State, dead_state_psm: State) -> List[State]:
    # Order here is for Strategy's list of states for value assignment.
    # The PSM output order will be PFS, PROG, DEAD as per PSMDef.
    return [pfs_state_psm, prog_state_psm, dead_state_psm]

@pytest.fixture
def psm_definition_simple() -> PSMDef:
    pfs_curve = ExponentialSurvival(name="PFS_exp", rate=0.2) # Median PFS = 3.46 years
    os_curve = ExponentialSurvival(name="OS_exp", rate=0.1)   # Median OS = 6.93 years
    mapping = {
        PSMDef.ROLE_PFS: "ProgressionFree",
        PSMDef.ROLE_PROGRESSED: "Progressed",
        PSMDef.ROLE_DEATH: "Dead"
    }
    return PSMDef(pfs_curve=pfs_curve, os_curve=os_curve, state_role_mapping=mapping)

@pytest.fixture
def strategy_psm_simple(states_list_psm: List[State], psm_definition_simple: PSMDef) -> Strategy:
    return Strategy(
        name="SimplePSM",
        states=states_list_psm, # States for value assignment
        transition_definition=psm_definition_simple
    )
# --- End PSM Fixtures ---


def test_simulation_output_initialization_and_accessors():
    value_attrs = ["cost", "effect"]
    output = SimulationOutput(
        strategy_name="TestStrat",
        num_cycles=2,
        state_names=["s1", "s2"],
        value_attributes_tracked=value_attrs,
        discount_rates={"cost":0.03, "effect":0.03}
    )
    assert output.strategy_name == "TestStrat"
    assert output.num_cycles == 2
    assert output.state_names == ["s1", "s2"]
    assert output.value_attributes_tracked == value_attrs
    assert output.cohort_distribution.shape == (3, 2) # num_cycles + 1 rows
    assert output.discount_rates["cost"] == 0.03

    # Test initial empty state of value stores
    assert "cost" in output.cycle_values_per_state
    assert output.cycle_values_per_state["cost"].shape == (2, 2) # num_cycles rows
    assert np.all(output.cycle_values_per_state["cost"] == 0)
    assert "effect" in output.aggregated_cycle_values
    assert len(output.aggregated_cycle_values["effect"]) == 2 # num_cycles
    assert np.all(output.aggregated_cycle_values["effect"] == 0)
    assert output.total_discounted_values["cost"] == 0.0
    assert output.total_undiscounted_values["effect"] == 0.0

    # Test summary on empty/initial data
    summary_df_init = output.summary()
    assert isinstance(summary_df_init, pd.DataFrame)
    assert list(summary_df_init.columns) == ["Undiscounted Total", "Discounted Total"]
    assert list(summary_df_init.index) == value_attrs
    assert np.all(summary_df_init == 0.0)

    # Test get_cohort_trace on initial data
    trace_df_init = output.get_cohort_trace()
    assert trace_df_init.shape == (3,2)
    assert np.all(trace_df_init == 0.0) # Before record_cohort_distribution

    # Test get_aggregated_cycle_values_for_attribute on initial data
    agg_cost_init = output.get_aggregated_cycle_values_for_attribute("cost")
    assert isinstance(agg_cost_init, pd.Series)
    assert len(agg_cost_init) == 2
    assert np.all(agg_cost_init == 0.0)
    assert output.get_aggregated_cycle_values_for_attribute("non_existent_attr") is None


# --- Markov Model Simulation Tests (from previous implementation) ---
def test_run_simulation_markov_zero_cycles(strategy_markov_simple, global_params_empty):
    initial_pop = {"Well": 1000.0, "Sick": 0.0}
    output = run_simulation(
        strategy_markov_simple, global_params_empty, 0, initial_pop,
        value_attributes=["cost", "effect"],
        discount_rates={"cost": 0.0, "effect": 0.0}
    )
    assert output.num_cycles == 0
    assert output.cohort_distribution.iloc[0]["Well"] == 1000.0
    assert output.cohort_distribution.iloc[0]["Sick"] == 0.0
    assert len(output.cohort_distribution) == 1
    assert output.total_discounted_values.get("cost", 0) == 0.0

def test_run_simulation_markov_one_cycle_no_discount(strategy_markov_simple, global_params_empty, s1_markov, s2_markov):
    initial_pop = {"Well": 1000.0, "Sick": 0.0}
    output = run_simulation(
        strategy_markov_simple, global_params_empty, 1, initial_pop,
        value_attributes=["cost", "effect"],
        discount_rates={"cost": 0.0, "effect": 0.0}
    )
    expected_cost_c0 = 1000.0 * s1_markov.get_value("cost") + 0.0 * s2_markov.get_value("cost")
    assert output.aggregated_cycle_values["cost"].iloc[0] == expected_cost_c0
    assert output.cohort_distribution.iloc[1]["Well"] == 800.0
    assert output.cohort_distribution.iloc[1]["Sick"] == 200.0
    assert output.total_undiscounted_values["cost"] == expected_cost_c0

def test_run_simulation_markov_two_cycles_with_discount(strategy_markov_simple, global_params_empty, s1_markov, s2_markov):
    initial_pop = {"Well": 1.0, "Sick": 0.0}
    dr = 0.05
    output = run_simulation(
        strategy_markov_simple, global_params_empty, 2, initial_pop,
        value_attributes=["cost", "effect"],
        discount_rates={"cost": dr, "effect": dr}
    )
    cost_s1 = s1_markov.get_value("cost")
    cost_s2 = s2_markov.get_value("cost")
    effect_s1 = s1_markov.get_value("effect")
    effect_s2 = s2_markov.get_value("effect")

    cost_c0 = 1.0 * cost_s1 # 100
    effect_c0 = 1.0 * effect_s1 # 1.0

    pop_c1_s1 = 0.8
    pop_c1_s2 = 0.2
    cost_c1 = pop_c1_s1 * cost_s1 + pop_c1_s2 * cost_s2 # 0.8*100 + 0.2*500 = 180
    effect_c1 = pop_c1_s1 * effect_s1 + pop_c1_s2 * effect_s2 # 0.8*1.0 + 0.2*0.5 = 0.9

    total_undisc_cost = cost_c0 + cost_c1
    assert output.total_undiscounted_values["cost"] == pytest.approx(total_undisc_cost)
    df1, df2 = 1/(1+dr)**1, 1/(1+dr)**2
    total_disc_cost = cost_c0 * df1 + cost_c1 * df2
    assert output.total_discounted_values["cost"] == pytest.approx(total_disc_cost)

# --- PSM Simulation Tests ---
def test_run_simulation_psm_zero_cycles(strategy_psm_simple, global_params_empty):
    # Initial pop for PSM: total cohort size, assumed to start in PFS state
    initial_pop = {"ProgressionFree": 1000.0}
    output = run_simulation(
        strategy_psm_simple, global_params_empty, 0, initial_pop,
        value_attributes=["cost", "qaly"],
        discount_rates={"cost": 0.0, "qaly": 0.0}
    )
    assert output.num_cycles == 0
    assert output.state_names == ["ProgressionFree", "Progressed", "Dead"]
    assert output.cohort_distribution.iloc[0]["ProgressionFree"] == 1000.0
    assert output.cohort_distribution.iloc[0]["Progressed"] == 0.0
    assert output.cohort_distribution.iloc[0]["Dead"] == 0.0
    assert len(output.cohort_distribution) == 1
    assert output.total_discounted_values.get("cost", 0) == 0.0


def test_run_simulation_psm_one_cycle_no_discount(
    strategy_psm_simple: Strategy,
    global_params_empty: Parameters,
    pfs_state_psm: State # For accessing cost/qaly values
):
    initial_pop = {"ProgressionFree": 1.0} # Total cohort = 1
    num_cycles = 1
    output = run_simulation(
        strategy_psm_simple, global_params_empty, num_cycles, initial_pop,
        value_attributes=["cost", "qaly"],
        discount_rates={"cost": 0.0, "qaly": 0.0}
    )

    psm_def = strategy_psm_simple.transition_definition
    assert isinstance(psm_def, PSMDef)

    # Initial state (time t=0, start of cycle 0)
    assert output.cohort_distribution.iloc[0]["ProgressionFree"] == 1.0
    assert output.cohort_distribution.iloc[0]["Progressed"] == 0.0
    assert output.cohort_distribution.iloc[0]["Dead"] == 0.0

    # Values accrued during cycle 0 (based on pop at t=0)
    # Only PFS state has cohort, so only its cost/qaly contribute
    cost_pfs_state = pfs_state_psm.get_value("cost") # 100
    qaly_pfs_state = pfs_state_psm.get_value("qaly") # 0.9

    expected_cost_cycle0 = 1.0 * cost_pfs_state
    expected_qaly_cycle0 = 1.0 * qaly_pfs_state

    assert output.aggregated_cycle_values["cost"].iloc[0] == pytest.approx(expected_cost_cycle0)
    assert output.aggregated_cycle_values["qaly"].iloc[0] == pytest.approx(expected_qaly_cycle0)

    # Population at start of cycle 1 (time t=1, end of cycle 0)
    # Proportions at t=1
    props_t1_df = psm_def.get_state_proportions(t=1.0)
    prop_pfs_t1 = props_t1_df.loc[1.0, "ProgressionFree"]
    prop_prog_t1 = props_t1_df.loc[1.0, "Progressed"]
    prop_dead_t1 = props_t1_df.loc[1.0, "Dead"]

    # Cohort = TotalInitialPop * Proportions
    assert output.cohort_distribution.iloc[1]["ProgressionFree"] == pytest.approx(1.0 * prop_pfs_t1)
    assert output.cohort_distribution.iloc[1]["Progressed"] == pytest.approx(1.0 * prop_prog_t1)
    assert output.cohort_distribution.iloc[1]["Dead"] == pytest.approx(1.0 * prop_dead_t1)

    assert output.total_undiscounted_values["cost"] == pytest.approx(expected_cost_cycle0)
    assert output.total_undiscounted_values["qaly"] == pytest.approx(expected_qaly_cycle0)

def test_simulation_output_methods_after_run(strategy_markov_simple, global_params_empty, s1_markov):
    """Test summary, get_cohort_trace, get_aggregated_cycle_values after a run."""
    initial_pop = {"Well": 1000.0, "Sick": 0.0}
    value_attrs = ["cost", "effect"]
    discount_r = {"cost": 0.03, "effect": 0.03}
    num_cycles = 2
    output = run_simulation(
        strategy_markov_simple, global_params_empty, num_cycles, initial_pop,
        value_attributes=value_attrs, discount_rates=discount_r
    )

    # Test summary()
    summary_df = output.summary()
    assert isinstance(summary_df, pd.DataFrame)
    assert list(summary_df.index) == value_attrs
    assert "Undiscounted Total" in summary_df.columns
    assert "Discounted Total" in summary_df.columns
    assert summary_df.loc["cost", "Undiscounted Total"] > 0
    assert summary_df.loc["effect", "Discounted Total"] > 0
    assert summary_df.loc["cost", "Discounted Total"] <= summary_df.loc["cost", "Undiscounted Total"]

    # Test get_cohort_trace()
    trace_all = output.get_cohort_trace()
    assert isinstance(trace_all, pd.DataFrame)
    assert trace_all.shape == (num_cycles + 1, 2) # 2 states
    assert list(trace_all.columns) == ["Well", "Sick"]
    assert np.isclose(trace_all.iloc[0]["Well"], 1000.0)
    assert np.isclose(trace_all.sum(axis=1).iloc[0], 1000.0) # Pop conservation (approx)
    assert np.isclose(trace_all.sum(axis=1).iloc[num_cycles], 1000.0)


    trace_well = output.get_cohort_trace(states_to_plot=["Well"])
    assert isinstance(trace_well, pd.DataFrame)
    assert list(trace_well.columns) == ["Well"]
    assert len(trace_well) == num_cycles + 1

    with pytest.raises(ValueError, match="States not found in simulation output: \\['NonExistentState'\\]"):
        output.get_cohort_trace(states_to_plot=["NonExistentState"])

    # Test get_aggregated_cycle_values_for_attribute()
    agg_cost_series = output.get_aggregated_cycle_values_for_attribute("cost")
    assert isinstance(agg_cost_series, pd.Series)
    assert len(agg_cost_series) == num_cycles
    assert agg_cost_series.name == "cost"
    # Cost for cycle 0: 1000 people in "Well" (cost 100) = 1000 * 100 = 100000
    assert agg_cost_series.iloc[0] == pytest.approx(1000.0 * s1_markov.get_value("cost"))

    assert output.get_aggregated_cycle_values_for_attribute("non_existent_attr") is None


def test_run_simulation_psm_initial_pop_validation(strategy_psm_simple, global_params_empty):
    # Test error if initial pop doesn't specify the PFS state
    with pytest.raises(ValueError, match="For PSM, initial_population_dist must specify the cohort size for the progression-free state role"):
        run_simulation(
            strategy_psm_simple, global_params_empty, 1, {"WrongState": 1.0},
            value_attributes=["cost", "qaly"], discount_rates={}
        )

    # Test warning if key is different but only one entry (assumed total for PFS)
    with pytest.warns(UserWarning, match="Initial population for PSM provided with key 'TotalCohort'. Assuming it's for 'ProgressionFree'."):
        run_simulation(
            strategy_psm_simple, global_params_empty, 1, {"TotalCohort": 1.0},
            value_attributes=["cost", "qaly"], discount_rates={}
        )


def test_simulation_output_psm_structure(strategy_psm_simple, global_params_empty):
    output = run_simulation(
        strategy_psm_simple, global_params_empty, 5, {"ProgressionFree": 100},
        value_attributes=["cost", "qaly"], discount_rates={"cost":0.03, "qaly":0.01}
    )
    assert output.strategy_name == "SimplePSM"
    assert output.num_cycles == 5
    # PSM output states are always the mapped PFS, Progressed, Dead roles in order
    assert output.state_names == ["ProgressionFree", "Progressed", "Dead"]
    assert output.cohort_distribution.shape == (6, 3) # 5 cycles + initial row
    assert "cost" in output.cycle_values_per_state
    assert "qaly" in output.aggregated_cycle_values
    assert output.cycle_values_per_state["cost"].shape == (5, 3) # 5 cycles, 3 states
    assert len(output.aggregated_cycle_values["qaly"]) == 5

    summary = output.summary()
    assert "cost" in summary.columns
    assert "qaly" in summary.columns
    assert "Undiscounted Total" in summary.index
    assert "Discounted Total" in summary.index

```
This updated `test_simulation.py` now includes:
*   **PSM-specific fixtures:** `pfs_state_psm`, `prog_state_psm`, `dead_state_psm`, `states_list_psm` (for strategy value assignment), `psm_definition_simple`, and `strategy_psm_simple`.
*   **Markov test fixtures renamed:** `s1_markov`, `s2_markov`, `states_list_markov`, `strategy_markov_simple` for clarity. Original Markov tests are kept.
*   **`test_run_simulation_psm_zero_cycles`:** Tests PSM run with 0 cycles.
*   **`test_run_simulation_psm_one_cycle_no_discount`:**
    *   Verifies initial cohort distribution for PSM (all in PFS state).
    *   Checks that costs/QALYs accrued in cycle 0 are based on the PFS state's values and the initial cohort.
    *   Checks cohort distribution at the start of cycle 1 (time t=1) by comparing against proportions calculated directly from `PSMDef.get_state_proportions()`.
*   **`test_run_simulation_psm_initial_pop_validation`:** Tests error handling and warnings for `initial_population_dist` in PSMs.
*   **`test_simulation_output_psm_structure`:** Checks the general structure of the `SimulationOutput` for a PSM run.

More detailed tests for PSM value accumulation with discounting, similar to the Markov 2-cycle test, would be beneficial but are more complex to manually verify due to the continuous nature of survival curves feeding into discrete cycle calculations. The current tests verify the core mechanics of cohort flow and value attribution for PSMs.

Both `hero_python_port/tests/test_psm.py` and `hero_python_port/tests/test_simulation.py` are now created/updated.
