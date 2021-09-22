#include <Rcpp.h>
using namespace Rcpp;

bool cpp_is_cell_empty(String cell) {
  return cell == "";
}

int cpp_get_last_nonempty_row_index(CharacterMatrix cells) {
  int n_input_rows = cells.nrow();
  int n_input_cols = cells.ncol();
  
  int last_nonempty_row_index;
  
  // Start at the bottom and stop when a nonempty
  // cell is found
  for (int i = n_input_rows - 1; i >= 0; i--) {
    for (int j = 0; j < n_input_cols; j++) {
      last_nonempty_row_index = i;
      if (!cpp_is_cell_empty(cells(i, j))) {
        goto endloop;
      }
    }
  }
  endloop:
    return last_nonempty_row_index;
}

int cpp_get_last_nonempty_col_index(CharacterMatrix cells) {
  int n_input_rows = cells.nrow();
  int n_input_cols = cells.ncol();
  
  int last_nonempty_col_index;
  
  // Start at the right and stop when a nonempty
  // cell is found
  for (int i = n_input_cols - 1; i >= 0; i--) {
    for (int j = 0; j < n_input_rows; j++) {
      last_nonempty_col_index = i;
      if (!cpp_is_cell_empty(cells(j, i))) {
        goto endloop;
      }
    }
  }
  endloop:
    return last_nonempty_col_index;
}

CharacterVector cpp_get_column(CharacterMatrix cells, int column_index, int max_rows) {
  CharacterVector out = CharacterVector(max_rows);
  for (int i = 1; i <= max_rows; i++) {
    out(i-1) = cells(i, column_index);
  }
  
  return out;
}

CharacterVector cpp_get_col_names(CharacterMatrix cells, int max_cols) {
  CharacterVector char_col = CharacterVector(max_cols + 1);
  for (int i = 0; i <= max_cols; i++) {
    String cellValue = cells(0, i);
    if (cellValue == "") {
      char_col(i) = "..column_" + std::to_string(i);
    } else {
      char_col(i) = cells(0, i);
    }
  }
  return char_col;
}

DataFrame cpp_convert_table(CharacterMatrix cells) {
  int last_nonempty_row_index = cpp_get_last_nonempty_row_index(cells);
  int last_nonempty_col_index = cpp_get_last_nonempty_col_index(cells);
  CharacterVector colNames = cpp_get_col_names(cells, last_nonempty_col_index);
  
  Rcpp::Environment base("package:base");
  Rcpp::Function asNumericVector = base["as.numeric"];
  Rcpp::Function isNa = base["is.na"];
  Rcpp::Function any = base["any"];
  Rcpp::Function asDataFrame = base["as.data.frame"];
  
  List columns;
  
  for (int i = 0; i <= last_nonempty_col_index; i++) {
    CharacterVector char_col = cpp_get_column(cells, i, last_nonempty_row_index);
    NumericVector num_col = asNumericVector(char_col);
    LogicalVector anyMissing = any(isNa(num_col));
    if (anyMissing(0)) {
      columns.push_back(char_col);
    } else {
      columns.push_back(num_col);
    }
  }
  
  DataFrame df = asDataFrame(columns);
  
  df.attr("names") = colNames;
  
  return df;
}

// [[Rcpp::export]]
List cpp_convert_tables(List matrices, CharacterVector names) {
  int len = matrices.length();
  List res;
  
  for (int i = 0; i < len; i++) {
    res.push_back(cpp_convert_table(matrices(i)));
  }
  
  res.attr("names") = names;
  
  return res;
}