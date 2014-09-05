# Regular expressions for easier handling and processing of the code.

# Regularexpression for validating the symbol '%' in a string.
def regex_verify_string_comment
  /('(?:[^']|(?:\\'))*%(?:[^']|(?:\\'))*'|"(?:[^"]|(?:\\"))*%(?:[^"]|(?:\\"))*")/
end

def regex_find_strings
  /(?:'(?:[^']|(?:\\'))*'|"(?:[^"]|(?:\\"))*")/
end


def regex_find_operations
  /(?:\bmove\(|\btake\(|\blook\(|\bputs\(|\bturn\(|->|\.|;|\bend\b|\bcase\b|\bif\b)/
end

def regex_arrow_prefix
  Regexp.new("->line#{$prefix}")
end

def regex_end_prefix
  Regexp.new("endline#{$prefix}")
end

def regex_stop_prefix
  Regexp.new("\\.line#{$prefix}")
end

def regex_op_prefix
  Regexp.new("\\(line#{$prefix}")
end

def regex_arrow_with_function
  Regexp.new("-> a#{$prefix}_line\\(\\d+\\),")
end

def regex_stop_or_semicolon
  Regexp.new("(?:\\.line#{$prefix}|;line#{$prefix})")
end

def regex_lineprefix
  Regexp.new("line#{$prefix}")
end