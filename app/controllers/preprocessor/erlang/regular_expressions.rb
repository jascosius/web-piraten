# Regular expressions for handling and processing of the code.

# Regularexpression for validating the symbol '%' in a string.
def regex_verify_string_comment
  /('(?:[^']|(?:\\'))*%(?:[^']|(?:\\'))*'|"(?:[^"]|(?:\\"))*%(?:[^"]|(?:\\"))*")/
end

# Verfies start and ends of strings ans atoms
def regex_find_strings
  /(?:'(?:[^']|(?:\\'))*'|"(?:[^"]|(?:\\"))*")/
end

# Finds occurences of important keywords.
def regex_find_operations
  /(?:\bmove\(|\btake\(|\blook\(|\bputs\(|\bturn\(|->|\.|;|\bend\b|\bcase\b|\bif\b)/
end

# Checks for arrow with processing prefix.
def regex_arrow_prefix
  Regexp.new("->line#{VM_PREFIX}")
end

# Checks for end with processing prefix.
def regex_end_prefix
  Regexp.new("endline#{VM_PREFIX}")
end

# Checks for fullstop with processing prefix.
def regex_stop_prefix
  Regexp.new("\\.line#{VM_PREFIX}")
end

# Checks for left parenthesis with processing prefix.
def regex_op_prefix
  Regexp.new("\\(line#{VM_PREFIX}")
end

# Checks for arrow with line function.
def regex_arrow_with_function
  Regexp.new("-> a#{VM_PREFIX}_line\\(\\d+\\),")
end

# Checks for fullstop or semicolon with processing prefix.
def regex_stop_or_semicolon
  Regexp.new("(?:\\.line#{VM_PREFIX}|;line#{VM_PREFIX})")
end

# Checks for processing prefix.
def regex_lineprefix
  Regexp.new("line#{VM_PREFIX}")
end