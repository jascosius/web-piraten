def regex_find_strings
  /(?:'(?:[^']|(?:\\'))*'|"(?:[^"]|(?:\\"))*")/
end

def regex_find_operations
  /(?:\bmove\(|\btake\(|\blook\(|\bputs\(|\bturn\(|->|\.)/
end

def regex_arrow_prefix
  Regexp.new("->line#{$prefix}")
end

def regex_semicolon_prefix
  Regexp.new(";line#{$prefix}")
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