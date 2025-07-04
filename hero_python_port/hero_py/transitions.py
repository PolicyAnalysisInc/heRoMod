# hero_python_port/hero_py/transitions.py

from typing import List, Dict, Any, Union, Callable, Optional
import numpy as np

class TransitionMatrix:
    """
    Represents and evaluates the transition matrix for a Markov model.

    Transition probabilities for each state (row) can be defined explicitly.
    One probability in a row can be designated as the "complement" (1 - sum of others).
    Probabilities can be fixed values or callable functions (lambdas) that depend
    on model parameters and the current cycle.
    """
    def __init__(self, state_names: List[str]):
        """
        Initializes the TransitionMatrix.

        Args:
            state_names (List[str]): An ordered list of unique state names.
                                     The order determines row/column mapping.
        """
        if not state_names or not all(isinstance(s, str) and s for s in state_names):
            raise ValueError("state_names must be a non-empty list of non-empty strings.")
        if len(set(state_names)) != len(state_names):
            raise ValueError("state_names must be unique.")

        self.state_names: List[str] = list(state_names)  # Ensure a copy
        self.num_states: int = len(state_names)
        self._name_to_idx: Dict[str, int] = {name: i for i, name in enumerate(state_names)}

        # Stores definitions: Dict[row_idx, Dict[col_idx, Union[float, Callable]]]
        self._row_definitions: Dict[int, Dict[int, Union[float, Callable]]] = {}
        # Stores which column is the complement for a given row: Dict[row_idx, col_idx]
        self._complement_info: Dict[int, int] = {}

    def set_row_probs(self,
                      from_state: str,
                      to_state_definitions: Dict[str, Union[float, Callable]],
                      complement_to_state: Optional[str] = None) -> None:
        """
        Defines the transition probabilities for a single row (from_state).

        Args:
            from_state (str): The name of the origin state (row).
            to_state_definitions (Dict[str, Union[float, Callable]]):
                A dictionary where keys are destination state names (column names)
                and values are their probability definitions (fixed float or callable).
                Callables should have the signature: `func(params_ctx: Dict, cycle: int) -> float`.
            complement_to_state (Optional[str], optional):
                The name of one of the destination states in this row whose probability
                will be calculated as 1 - sum of other defined probabilities in the row.
                If specified, any definition for this state in `to_state_definitions`
                is ignored. If None, all probabilities for the row must be explicitly
                defined and should sum to 1.0 after evaluation. Defaults to None.

        Raises:
            ValueError: If state names are invalid or if complement_to_state is not
                        a valid destination state for the row.
        """
        if from_state not in self._name_to_idx:
            raise ValueError(f"Invalid from_state: '{from_state}'. Must be one of {self.state_names}.")

        from_idx = self._name_to_idx[from_state]
        self._row_definitions[from_idx] = {} # Clear previous definitions for this row

        if complement_to_state is not None:
            if complement_to_state not in self._name_to_idx:
                raise ValueError(f"Invalid complement_to_state: '{complement_to_state}'. Must be one of {self.state_names}.")
            if complement_to_state not in to_state_definitions and complement_to_state != from_state and len(to_state_definitions) >= self.num_states -1 :
                 # This condition is tricky. If complement_to_state is set, it implies one definition is missing.
                 # If it's also in to_state_definitions, it's fine, we just mark it.
                 # If it's NOT in to_state_definitions, it means it IS the missing one.
                 pass

            self._complement_info[from_idx] = self._name_to_idx[complement_to_state]
        elif from_idx in self._complement_info: # Explicitly no complement for this row now
            del self._complement_info[from_idx]

        for to_state, prob_def in to_state_definitions.items():
            if to_state not in self._name_to_idx:
                raise ValueError(f"Invalid to_state: '{to_state}' in definitions for from_state '{from_state}'.")

            to_idx = self._name_to_idx[to_state]

            # If this column is marked as complement, we don't store its definition here,
            # as it will be calculated. But we still need to know it was mentioned.
            if from_idx in self._complement_info and self._complement_info[from_idx] == to_idx:
                continue # Its definition will be calculated

            if isinstance(prob_def, (int, float)) and not (0 <= prob_def <= 1):
                # Allow slightly outside for potential floating point issues, get_matrix will validate sum
                if not (-1e-9 <= prob_def <= 1.0 + 1e-9):
                    print(f"Warning: Probability {prob_def} for {from_state}->{to_state} is outside [0,1].")

            self._row_definitions.setdefault(from_idx, {})[to_idx] = prob_def

        # Ensure all states are mentioned if no complement, or all but one if complement
        # This check is complex here; better done in get_matrix or a validation method.

    def get_matrix(self,
                   params_context: Optional[Dict[str, Any]] = None,
                   cycle: int = 0) -> np.ndarray:
        """
        Evaluates and returns the entire transition probability matrix for the
        given context (parameters and cycle).

        Ensures that each row in the resulting matrix sums to 1.0, handling
        complement probabilities as defined.

        Args:
            params_context (Optional[Dict[str, Any]], optional):
                A dictionary of evaluated model parameters for the current cycle.
                Defaults to an empty dictionary if None.
            cycle (int, optional): The current model cycle. Defaults to 0.

        Returns:
            np.ndarray: A NumPy array (num_states x num_states) representing the
                        evaluated transition probability matrix.

        Raises:
            ValueError: If a row's probabilities cannot be resolved to sum to 1.0
                        (e.g., sum > 1 before complement, or insufficient definitions).
            TypeError: If a callable probability definition has an incorrect signature
                       or if its execution results in a type error.
            RuntimeError: For other errors during the execution of a callable definition.
        """
        matrix = np.zeros((self.num_states, self.num_states), dtype=float)
        current_params_context = params_context if params_context is not None else {}

        for from_idx in range(self.num_states):
            from_state_name = self.state_names[from_idx]
            row_sum = 0.0

            # Get definitions for this row, or empty if row not defined
            current_row_defs = self._row_definitions.get(from_idx, {})
            complement_col_idx = self._complement_info.get(from_idx)

            # First pass: evaluate all explicitly defined non-complement probabilities
            for to_idx in range(self.num_states):
                if complement_col_idx == to_idx: # Skip complement column for now
                    continue

                prob_def = current_row_defs.get(to_idx)

                if prob_def is None: # Not defined, implies 0 unless it's the complement
                    matrix[from_idx, to_idx] = 0.0
                    continue

                if callable(prob_def):
                    try:
                        val = float(prob_def(current_params_context, cycle))
                    except TypeError as e:
                        import inspect
                        try:
                            sig = inspect.signature(prob_def)
                            num_p = len(sig.parameters)
                            if num_p != 2:
                                raise TypeError(f"Callable for P({self.state_names[to_idx]}|{from_state_name}) has {num_p} args, expected 2 (params, cycle). Sig: {sig}. Error: {e}") from e
                        except: pass
                        raise TypeError(f"Error calling P({self.state_names[to_idx]}|{from_state_name}): {e}") from e
                    except Exception as e:
                        raise RuntimeError(f"Error executing P({self.state_names[to_idx]}|{from_state_name}): {e}") from e
                else: # Fixed value
                    val = float(prob_def)

                if not (0 <= val <= 1.0 + 1e-9): # Allow small tolerance for >1 before sum check
                     # Re-check after float conversion
                    if not (-1e-9 <= val <= 1.0 + 1e-9):
                        raise ValueError(
                            f"Probability for P({self.state_names[to_idx]}|{from_state_name}) = {val} is outside [0,1]."
                        )

                matrix[from_idx, to_idx] = max(0, val) # Ensure non-negative after evaluation
                row_sum += matrix[from_idx, to_idx]

            # Second pass: calculate complement if defined for this row
            if complement_col_idx is not None:
                complement_val = 1.0 - row_sum
                if not (-1e-9 <= complement_val <= 1.0 + 1e-9): # Check if complement is valid prob
                    raise ValueError(
                        f"Invalid complement probability for P({self.state_names[complement_col_idx]}|{from_state_name}). "
                        f"Calculated as 1.0 - (sum of other probs = {row_sum:.4f}) = {complement_val:.4f}, which is outside [0,1]."
                    )
                matrix[from_idx, complement_col_idx] = max(0, min(1, complement_val)) # Clamp to [0,1]

            # Final validation: row sum must be close to 1.0
            final_row_sum = np.sum(matrix[from_idx, :])
            if not np.isclose(final_row_sum, 1.0):
                # If complement was not set, and row doesn't sum to 1, it's an error.
                # If complement was set, this check ensures it worked.
                err_msg = (f"Probabilities in row for state '{from_state_name}' do not sum to 1.0 "
                           f"(sum = {final_row_sum:.4f}). Evaluated row: {matrix[from_idx, :]}")
                if complement_col_idx is None and len(current_row_defs) < self.num_states:
                    err_msg += " Not all probabilities were defined for this row and no complement was specified."
                raise ValueError(err_msg)

            # Normalize row if very close to 1 but not exactly, due to float precision
            if final_row_sum != 0 : # Avoid division by zero if all probs were somehow zero
                 matrix[from_idx, :] /= final_row_sum


        return matrix

    def __repr__(self) -> str:
        return (f"TransitionMatrix(state_names={self.state_names}, "
                f"num_definitions={sum(len(d) for d in self._row_definitions.values())})")

```
Key changes and design decisions in this `TransitionMatrix`:
*   **`__init__`:** Stores `state_names` and creates a `_name_to_idx` mapping.
*   **`_row_definitions`:** A dictionary where keys are `from_state_idx` and values are dictionaries ` {to_state_idx: definition}`. This allows sparse definitions.
*   **`_complement_info`:** A dictionary ` {from_state_idx: to_state_idx_for_complement}`.
*   **`set_row_probs`:** This is the primary API for defining transitions for a given `from_state`.
    *   It takes `to_state_definitions` as a dictionary: `{"StateB": 0.2, "StateC": lambda p,c: p['rateC']}`.
    *   The `complement_to_state` argument specifies which `to_state` in that row should be the complement. If this state is also in `to_state_definitions`, its explicit definition is ignored in favor of the complement calculation.
*   **`get_matrix`:**
    1.  Initializes an empty `numpy` matrix.
    2.  Iterates through each `from_state` (row).
    3.  For each row, it iterates through `to_state`s (columns).
    4.  It evaluates explicitly defined probabilities (fixed or callable), summing them up, but skips the designated complement column for that row. Callables receive `(params_context, cycle)`.
    5.  After evaluating explicit probabilities, if a complement column was designated for the row, its value is calculated as `1.0 - sum_of_others`.
    6.  **Validation:**
        *   Checks if evaluated probabilities (and the calculated complement) are within `[0, 1]`.
        *   Crucially, verifies that the sum of probabilities in each row is very close to 1.0. If not, it raises a `ValueError`.
        *   Normalizes rows to sum to exactly 1.0 if they are already very close, to handle potential floating-point inaccuracies.

This API for `set_row_probs` is more verbose than R's `define_transition(C, 0.2, 0.1, C)` but is more explicit and Pythonic. The R version's `C` relied on positional association and a special symbol.

Next, I will create the initial test file `hero_python_port/tests/test_transitions.py`. This will be a substantial test file due to the complexity of the transition matrix logic.
