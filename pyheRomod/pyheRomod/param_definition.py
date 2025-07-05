from collections import OrderedDict
import warnings

class UnevalCollection:
    def __init__(self, **kwargs):
        self.expressions = OrderedDict()
        for name, expr_str in kwargs.items():
            if not isinstance(expr_str, (str, int, float, bool)):
                warnings.warn(
                    f"Expression for '{name}' was not a string. Converting '{expr_str}' to string for storage. "
                    "For expressions referencing other parameters, ensure they are passed as strings.",
                    UserWarning
                )
                self.expressions[name] = str(expr_str)
            else:
                self.expressions[name] = str(expr_str)
        self._check_names(self.expressions.keys())

    def __repr__(self):
        return f"{self.__class__.__name__}({self.expressions!r})"

    def _check_names(self, names):
        if any(name == "" for name in names):
            raise ValueError("Parameter names cannot be empty.")
        reserved_keywords = {"markov_cycle", "strategy", "model_time", "state_time", "C"}
        forbidden_prefixes = {".", "_"}
        for name in names:
            if name is None or not isinstance(name, str):
                raise ValueError(f"Parameter name must be a string, got {type(name).__name__}")
            if name in reserved_keywords:
                raise ValueError(f"Parameter name '{name}' is a reserved keyword.")
            if any(name.startswith(prefix) for prefix in forbidden_prefixes):
                warnings.warn(
                    f"Parameter name '{name}' starts with a typically reserved prefix ('{name[0]}').",
                    UserWarning
                )
        pass

    def get_defined_names(self):
        return list(self.expressions.keys())

class UnevalParameters(UnevalCollection):
    def __init__(self, **kwargs):
        super().__init__(**kwargs)
    def get_parameter_names(self):
        reserved_names = {"markov_cycle", "strategy", "model_time", "state_time"}
        return [name for name in self.expressions.keys() if name not in reserved_names]

class UnevalInflow(UnevalCollection):
    def __init__(self, **kwargs): super().__init__(**kwargs)
class UnevalInit(UnevalCollection):
    def __init__(self, **kwargs): super().__init__(**kwargs)
class UnevalStartingValues(UnevalCollection):
    def __init__(self, **kwargs): super().__init__(**kwargs)

def define_parameters(**kwargs) -> UnevalParameters: return UnevalParameters(**kwargs)
def define_inflow(**kwargs) -> UnevalInflow: return UnevalInflow(**kwargs)
def define_init(**kwargs) -> UnevalInit: return UnevalInit(**kwargs)
def define_starting_values(**kwargs) -> UnevalStartingValues: return UnevalStartingValues(**kwargs)

def modify(target_object: UnevalCollection, **kwargs) -> UnevalCollection:
    if not isinstance(target_object, UnevalCollection):
        raise TypeError(f"Object to modify must be an instance of UnevalCollection. Got {type(target_object)}")
    new_expressions = target_object.expressions.copy()
    processed_kwargs = OrderedDict()
    for name, expr_val in kwargs.items():
        if not isinstance(expr_val, (str, int, float, bool)):
            warnings.warn(
                f"Expression for '{name}' in modify was not a string. Converting '{expr_val}' to string.",
                UserWarning
            )
            processed_kwargs[name] = str(expr_val)
        else:
            processed_kwargs[name] = str(expr_val)
    new_expressions.update(processed_kwargs)
    new_obj = type(target_object)(**new_expressions)
    return new_obj
