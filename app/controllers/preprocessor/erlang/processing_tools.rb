require 'preprocessor/erlang/regular_expressions'

# Method that removes the user's comments by processing the code
# line by line and searching for comments. Returns code without
# comments.
def remove_comments(code_msg)
  codes = ''
  code_msg.each_line do |line|
    res_c = []
    line.scan('%') do
      res_c << Regexp.last_match.offset(0).first
    end
    res_s = scan_for_index_start_and_end(line, regex_verify_string_comment)
    if !res_c.empty? && !res_s.empty?
      res_c.each do |res|
        if is_not_in_string?(res, res_s)
          line.slice!(res..-1)
        end
      end
    elsif !res_c.empty?
      line.slice!(res_c[0]..-1)
    end
    codes += line.chomp + "\n"
  end
  codes
end

# Method that scans the given code for a given regular expression
# (e.g. strings) and returns an array.
# If there are matches it's empty, elsewise it contains hashes with
# start- and endpoints of the matches.
def scan_for_index_start_and_end(code, regex)
  res = []
  code.scan(regex) do
    res << {starts: Regexp.last_match.offset(0).first, ends: Regexp.last_match.offset(0).last}
  end
  res
end

# Method that gets an index and an array that contains start- and
# endpoints of strings in the code. Then proves if the given index
# lies inside one of the strings and returns a boolean.
def is_not_in_string?(index, string_indexes)
  not_in_string = true
  string_indexes.each do |pair|
    if index > pair[:starts] && index <= pair[:ends]
      not_in_string = false
    end
  end
  not_in_string
end

# Method handles the process of how the highlighting functionality
# is inserted in the code and returns the processed code.
def insert_highlighting(new_code, vars)
  string_indexes = scan_for_index_start_and_end(new_code, regex_find_strings)
  operation_indexes = scan_for_index_start_and_end(new_code, regex_find_operations)
  # Set the array of tracing variables to empty, if there only is
  # the underscore, which by default is in erlang undefined.
  vars = [] if vars.length == 1 && vars[0] == '_'

  if string_indexes.empty? && !operation_indexes.empty?
    new_code = insert_prefix(new_code, operation_indexes, [])
    new_code = change_prefix_2_line_number(new_code)
  elsif !string_indexes.empty? && !operation_indexes.empty?
    new_code = insert_prefix(new_code, operation_indexes, string_indexes)
    new_code = change_prefix_2_line_number(new_code)
  end
  if vars.empty?
    new_code
  else
    regex_array = []
    vars.each do |var|
      regex_array << Regexp.new("\\b#{var}\\b")
    end
    debug_code = ''
    new_code.each_line do |line|
      vars.each_with_index do |variable, index|
        if variable != '_'
          pos_variable = line.index(regex_array[index])
          pos_arrow = line.index(regex_arrow_with_function)
          if pos_variable && pos_arrow && pos_variable < pos_arrow
            line = line.insert(pos_arrow.to_i + 2, " a#{$prefix}_debug(#{index}, #{variable}),")
          end
        end
      end
      debug_code += line
    end
    debug_code
  end
end

# Method takes code, an array of start- and endpoints of the pirate
# ship's functions as well as an array of start- and endpoints of
# the codes strings. Then inserts a prefix for later processing
# like line highlighting information.
def insert_prefix(code, operations, strings)
  if strings.empty?
    operations.reverse_each do |op|
      code.insert(op[:ends], "line#{$prefix}")
    end
  else
    operations.reverse_each do |op|
      code.insert(op[:ends], "line#{$prefix}") if is_not_in_string?(op[:starts], strings)
    end
  end
  code
end

# Takes the given code and processes the code line by line while
# searching for the beforehand inserted prefixes to exchange them
# with break-functions, functions for line highlighting and in
# case of functions of the pirates the line number.
def change_prefix_2_line_number(code)
  new_code = ''
  number = 1
  first_arrow = true
  code.each_line do |line|
    if first_arrow
      first_arrow = false if line.gsub!(regex_arrow_prefix, "-> a#{$prefix}_line(#{number}), a#{$prefix}_break(fun() ->")
    else
      line.gsub!(regex_arrow_prefix, "-> a#{$prefix}_line(#{number}), ")
    end
    first_arrow = true if line.gsub!(regex_stop_prefix, ' end).')
    line.gsub!(regex_op_prefix, "(#{number}, ")
    line.gsub!(/\(\d+,\s+\)/, "(#{number})")
    new_code += line.chomp + "  % #{$prefix}_(#{number}#{$prefix}_)\n"
    number += 1
  end
  new_code
end

