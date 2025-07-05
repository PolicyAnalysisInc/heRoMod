from collections import OrderedDict
import warnings
from .param_definition import UnevalCollection

class State(UnevalCollection):
    def __init__(self, **kwargs):
        super().__init__(**kwargs)
    def get_value_names(self) -> list[str]:
        return list(self.expressions.keys())

    def modify(self, **kwargs_new_values) -> 'State':
        current_value_names = self.get_value_names()
        for name in kwargs_new_values.keys():
            if name not in current_value_names:
                raise ValueError(f"Cannot add new value '{name}'. Only existing values can be modified.")
            if not isinstance(kwargs_new_values[name], (str, int, float, bool)):
                 warnings.warn(
                    f"Expression for '{name}' in modify was not a string/literal. Converting to string.", UserWarning)
        new_expressions = self.expressions.copy()
        temp_new_values_collection = UnevalCollection(**kwargs_new_values)
        new_expressions.update(temp_new_values_collection.expressions)
        return State(**new_expressions)

def define_state(**kwargs) -> State:
    return State(**kwargs)

class StateTransition(UnevalCollection):
    def __init__(self, from_state: str = None, to_state: str = None, **kwargs_values):
        super().__init__(**kwargs_values)
        self.from_state = from_state
        self.to_state = to_state
        if from_state is not None and not isinstance(from_state, str):
            raise TypeError("'from_state' must be a string or None.")
        if to_state is not None and not isinstance(to_state, str):
            raise TypeError("'to_state' must be a string or None.")
        if from_state is not None and to_state is not None and from_state == to_state:
            raise ValueError(f"StateTransition 'from' ({from_state}) and 'to' ({to_state}) states cannot be the same if both specified.")

    def __repr__(self):
        return f"StateTransition(from='{self.from_state}', to='{self.to_state}', values={self.expressions!r})"

    def modify(self, **kwargs_new_values) -> 'StateTransition':
        current_value_names = self.get_defined_names()
        for name in kwargs_new_values.keys():
            if name not in current_value_names:
                raise ValueError(f"Cannot add new value '{name}' to StateTransition. Only existing values can be modified.")
            if not isinstance(kwargs_new_values[name], (str, int, float, bool)):
                 warnings.warn(
                    f"Expression for '{name}' in StateTransition.modify was not a string/literal. Converting to string.", UserWarning)

        new_expressions = self.expressions.copy()
        temp_new_values_collection = UnevalCollection(**kwargs_new_values)
        new_expressions.update(temp_new_values_collection.expressions)

        return StateTransition(
            from_state=self.from_state,
            to_state=self.to_state,
            **new_expressions
        )

def define_state_transition(from_state: str = None, to_state: str = None, **kwargs_values) -> StateTransition:
    return StateTransition(from_state=from_state, to_state=to_state, **kwargs_values)

class UnevalStateList:
    def __init__(self, states: OrderedDict[str, State], transitions: list[StateTransition] = None):
        self.states: OrderedDict[str, State] = states
        self.transitions: list[StateTransition] = transitions if transitions is not None else []
        self._check_state_value_consistency()
        self._validate_transitions()

    def _check_state_value_consistency(self):
        if not self.states: return
        first_names = None
        for name, state_obj in self.states.items():
            if not isinstance(state_obj, State): raise TypeError(f"Obj for state '{name}' not State.")
            current_names = sorted(state_obj.get_value_names())
            if first_names is None: first_names = current_names
            elif first_names != current_names:
                raise ValueError("State value names differ between states.")

    def _validate_transitions(self):
        defined_state_names = self.get_state_names()
        for i, trans in enumerate(self.transitions):
            if not isinstance(trans, StateTransition):
                raise TypeError(f"Transition object at index {i} is not a StateTransition instance.")
            from_s = trans.from_state
            to_s = trans.to_state
            if from_s is not None and from_s not in defined_state_names:
                raise ValueError(f"Invalid 'from_state' in transition {i}: '{from_s}'. Not among defined states: {defined_state_names}")
            if to_s is not None and to_s not in defined_state_names:
                raise ValueError(f"Invalid 'to_state' in transition {i}: '{to_s}'. Not among defined states: {defined_state_names}")

    def get_state_names(self) -> list[str]: return list(self.states.keys())
    def get_state_count(self) -> int: return len(self.states)
    def get_state_value_names(self) -> list[str]:
        return list(self.states.values())[0].get_value_names() if self.states else []
    def __repr__(self):
        return f"UnevalStateList(states={list(self.states.keys())}, num_transitions={len(self.transitions)})"

    def modify(self, *args_new_transitions, **kwargs_new_states) -> 'UnevalStateList':
        new_states = self.states.copy()
        new_transitions = list(self.transitions)
        for name, state_obj in kwargs_new_states.items():
            if not isinstance(state_obj, State):
                raise TypeError(f"Object for state '{name}' in modify is not a State instance.")
            new_states[name] = state_obj
        for i, trans_obj in enumerate(args_new_transitions):
            if not isinstance(trans_obj, StateTransition):
                raise TypeError(f"Positional argument {i} in modify is not a StateTransition instance.")
            new_transitions.append(trans_obj)
        return UnevalStateList(states=new_states, transitions=new_transitions)

def define_state_list(*args_transitions, **kwargs_states) -> UnevalStateList:
    states_dict = OrderedDict()
    transitions_list = []
    for name, obj in kwargs_states.items():
        if isinstance(obj, State):
            if not name or not isinstance(name, str): raise ValueError("States must be named strings.")
            if name in states_dict: raise ValueError(f"Duplicate state name '{name}'.")
            states_dict[name] = obj
        else:
            raise TypeError(f"Keyword argument '{name}' is not a State object. Got {type(obj)}.")
    for i, arg_obj in enumerate(args_transitions):
        if isinstance(arg_obj, StateTransition):
            transitions_list.append(arg_obj)
        elif isinstance(arg_obj, State):
             raise TypeError(f"State object found at positional argument {i}. States must be provided as named keyword arguments.")
        else:
            raise TypeError(f"Positional argument at index {i} is not a StateTransition object. Got {type(arg_obj)}.")
    return UnevalStateList(states=states_dict, transitions=transitions_list)
