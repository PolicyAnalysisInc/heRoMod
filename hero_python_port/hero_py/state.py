# hero_python_port/hero_py/state.py

from typing import Dict, Any, Callable, Optional

class State:
    """
    Represents a single health state in a model.

    A state is defined by its name and a set of attributes (e.g., cost, utility).
    Values for these attributes can be fixed or defined by functions (lambdas),
    which can depend on model parameters (passed as a dictionary) and/or the
    current model cycle number.
    """
    def __init__(self, name: str, **value_definitions: Any):
        """
        Initializes a State object.

        Args:
            name (str): The unique name of the state. Must be non-empty.
            **value_definitions: Keyword arguments representing state attributes
                                 (e.g., cost=100, utility=lambda p,c: p['base_util'] - c*0.01).
                                 Definitions can be fixed values or callables.
                                 Callables are expected to have the signature:
                                 `func(params_context: Dict[str, Any], cycle: int) -> Any`
                                 where `params_context` is a dictionary of evaluated
                                 model parameters for the current cycle.
        """
        if not name or not isinstance(name, str):
            raise ValueError("State name must be a non-empty string.")

        self.name: str = name
        # Stores the raw definitions (fixed values or callables)
        self._value_definitions: Dict[str, Any] = {}

        for attr_name, definition in value_definitions.items():
            self.define_value(attr_name, definition)

    def define_value(self, attr_name: str, definition: Any) -> None:
        """
        Defines or updates an attribute (e.g., cost, utility) for this state.

        Args:
            attr_name (str): The name of the attribute.
            definition (Any): The fixed value or callable that defines this attribute.
                              Callables should follow the signature convention
                              `func(params_context: Dict[str, Any], cycle: int)`.
        """
        if not attr_name or not isinstance(attr_name, str):
            raise ValueError("Attribute name must be a non-empty string.")
        self._value_definitions[attr_name] = definition

    def get_value(self,
                  attr_name: str,
                  params_context: Optional[Dict[str, Any]] = None,
                  cycle: int = 0) -> Any:
        """
        Evaluates and returns the value of a specific state attribute for the
        given context (parameters and cycle).

        Args:
            attr_name (str): The name of the attribute to evaluate.
            params_context (Optional[Dict[str, Any]], optional):
                A dictionary of evaluated model parameters for the current cycle.
                Defaults to an empty dictionary if None.
            cycle (int, optional): The current model cycle. Defaults to 0.

        Returns:
            Any: The evaluated value of the attribute.

        Raises:
            KeyError: If the attribute `attr_name` is not defined for this state.
            TypeError: If a callable attribute definition has an incorrect signature
                       or if its execution results in a type error.
            RuntimeError: For other errors during the execution of a callable definition.
        """
        if attr_name not in self._value_definitions:
            raise KeyError(f"Attribute '{attr_name}' not defined for state '{self.name}'.")

        definition = self._value_definitions[attr_name]

        if callable(definition):
            # Ensure params_context is a dict, even if None was passed
            current_params_context = params_context if params_context is not None else {}
            try:
                # Convention: callables take (params_dict, cycle_num)
                return definition(current_params_context, cycle)
            except TypeError as e:
                import inspect
                try:
                    sig = inspect.signature(definition)
                    expected_params = len(sig.parameters)
                    if expected_params != 2: # Check if it deviates from convention
                        raise TypeError(
                            f"Callable for attribute '{attr_name}' in state '{self.name}' has {expected_params} arguments, "
                            f"but expected 2 (params_dict, cycle_num). Signature: {sig}. Original error: {e}"
                        ) from e
                except (ImportError, AttributeError, ValueError):
                    pass # Fallback to original error if inspect fails
                raise TypeError(
                    f"Error calling definition for attribute '{attr_name}' in state '{self.name}': {e}. "
                    "Ensure callable signature is `func(params_dict, cycle_num)`."
                ) from e
            except Exception as e: # Catch other errors during callable execution
                raise RuntimeError(f"Error executing definition for attribute '{attr_name}' in state '{self.name}': {e}") from e
        else: # Fixed value
            return definition

    def evaluate_all_values(self,
                            params_context: Optional[Dict[str, Any]] = None,
                            cycle: int = 0) -> Dict[str, Any]:
        """
        Evaluates all defined attributes for this state in the given context.

        Args:
            params_context (Optional[Dict[str, Any]], optional):
                A dictionary of evaluated model parameters for the current cycle.
                Defaults to an empty dictionary if None.
            cycle (int, optional): The current model cycle. Defaults to 0.

        Returns:
            Dict[str, Any]: A dictionary where keys are attribute names and
                            values are their evaluated values for the given context.
        """
        evaluated_values: Dict[str, Any] = {}
        # Ensure params_context is a dict for get_value calls
        current_params_context = params_context if params_context is not None else {}

        for attr_name in self._value_definitions:
            evaluated_values[attr_name] = self.get_value(attr_name, current_params_context, cycle)
        return evaluated_values

    def __repr__(self) -> str:
        # Make repr more readable by showing defined attributes
        attrs_repr = ", ".join(f"{k}={v!r}" for k, v in self._value_definitions.items())
        return f"State(name='{self.name}', {attrs_repr})"

    def __str__(self) -> str:
        return self.name

    @property
    def defined_attributes(self) -> List[str]:
        """Returns a list of attribute names defined for this state."""
        return list(self._value_definitions.keys())

```
Key refinements in this version of `State` class:
*   Constructor `__init__` now takes `**value_definitions` directly for attribute definitions, making it cleaner.
*   `define_value` ensures `attr_name` is a non-empty string.
*   `get_value` and `evaluate_all_values` now ensure `params_context` defaults to an empty dictionary if `None` is passed, making the call to attribute callables more consistent: `definition({}, cycle)` if no parameters are relevant.
*   The callable signature convention `func(params_context: Dict[str, Any], cycle: int)` is now more strictly hinted at in docstrings and error messages. The `inspect.signature` check is more targeted.
*   `__repr__` is improved to show the defined attributes.
*   Added a `defined_attributes` property.

Next, I will update/expand the unit tests in `hero_python_port/tests/test_state.py`. I'll need to recreate this test file first as it was also lost.

**Recreating `hero_python_port/tests/test_state.py` (based on previous stub):**
```python
# hero_python_port/tests/test_state.py
import pytest
from hero_py.state import State
# from hero_py.parameters import Parameters # Will need this for more complex tests

def test_state_creation_basic():
    state = State(name="Healthy", cost=100, utility=1.0)
    assert state.name == "Healthy"
    assert state.get_value("cost") == 100
    assert state.get_value("utility") == 1.0

# ... other tests that were previously stubbed ...
```
Now, expanding this with more comprehensive tests.
