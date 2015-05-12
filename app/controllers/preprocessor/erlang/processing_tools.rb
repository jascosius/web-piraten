# Like the name processing_tools says, this file contains methods
# (tools) to process the code. They are called from the
# erlang_preprocessor.

require 'preprocessor/erlang/regular_expressions'

# Method that removes the user's comments by processing the code
# line by line and searching for comments. Returns code without
# comments.
def remove_comments(code_msg)
  codes = ''
  code_msg.each_line do |line|
    res_c = []
    line.scan('%') do
      # scans the code for '%' to determine comments
      res_c << Regexp.last_match.offset(0).first
    end
    res_s = scan_for_index_start_and_end(line, regex_verify_string_comment)
    # find all those '%' that are inside of strings
    if !res_c.empty? && !res_s.empty?
      res_c.each do |res|
        # removes every comment at line end
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
# If there are no matches it's empty, elsewise it contains hashes
# with start- and endpoints of the matches.
def scan_for_index_start_and_end(code, regex)
  res = []
  code.scan(regex) do
    res << {starts: Regexp.last_match.offset(0).first,
            ends: Regexp.last_match.offset(0).last}
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
# is inserted in the code and returns the processed code, also
# deletes all inserted helping-prefixes (for keyword recognition).
def insert_highlighting(new_code, vars)
  string_indexes = scan_for_index_start_and_end(new_code, regex_find_strings) +
      scan_for_index_start_and_end(new_code, /\d+\.\d+/)
  # scan also for floating point numbers to prevent wrong
  # handling of the "."
  string_indexes
  operation_indexes = scan_for_index_start_and_end(new_code, regex_find_operations)
  # Set the array of tracing variables to empty, if there only is
  # the underscore, which by default in erlang is undefined.
  vars = [] if vars.length == 1 && vars[0] == '_'

  unless vars.empty?
    # fill array for operation_indexes with indexes of given variables
    vars.each do |var|
      if var != '_'
        operation_indexes += scan_for_index_start_and_end(new_code, Regexp.new("\\b#{var}\\b"))
      end
    end
    operation_indexes = operation_indexes.sort_by { |hsh| hsh[:starts] }
  end

  unless string_indexes.empty? && operation_indexes.empty?
    new_code = insert_prefix(new_code, operation_indexes, string_indexes)
  end

  # insert information of line numbers
  new_code = change_prefix_2_line_number(new_code)

  # insert debugging functionality
  new_code = change_prefix_2_debug(new_code, vars) unless vars.empty?

  # delete all for processing inserted prefixes
  new_code.gsub!(regex_lineprefix, '')

  new_code
end

# Method takes code, an array of start- and endpoints of the pirate
# ship's functions as well as an array of start- and endpoints of
# the codes strings. Then inserts a prefix for later processing
# like line highlighting information.
def insert_prefix(code, operations, strings)
  if strings.empty?
    operations.reverse_each do |op|
      code.insert(op[:ends], "line#{VM_PREFIX}")
    end
  else
    operations.reverse_each do |op|
      code.insert(op[:ends], "line#{VM_PREFIX}") if is_not_in_string?(op[:starts], strings)
    end
  end
  code
end

# Takes the given code and processes the code line by line while
# searching for the beforehand inserted prefixes to exchange them
# with break-functions, functions for line highlighting and in
# case of functions of the pirates the line number.
def change_prefix_2_line_number(code)
  break_counter = 0
  starts_and_ends = []
  code.scan(Regexp.new("(?:->|case|if|fun\\s*(?:\\s[A-Z]\\w*)?\\(.*\\)\\s*(?:when.*)?->)line#{VM_PREFIX}")) do
    starts_and_ends << {type: 'start',
                        starts: Regexp.last_match.offset(0).first,
                        ends: Regexp.last_match.offset(0).last}
  end
  code.scan(Regexp.new("(?:\\.|;|end)line#{VM_PREFIX}")) do
    starts_and_ends << {type: 'end',
                        starts: Regexp.last_match.offset(0).first,
                        ends: Regexp.last_match.offset(0).last}
  end
  starts_and_ends.sort_by! { |hsh| hsh[:starts] }

  break_array = []
  starts_and_ends.each do |element|
    # filter start- and endpoints of functions to correctly insert
    # break functionality
    if element[:type] == 'start' && break_counter == 0
      break_array << element
      break_counter = 1
    elsif element[:type] == 'end' && break_counter == 1
      break_array << element
      break_counter = 0
    elsif element[:type] == 'start'
      break_counter += 1 unless code[element[:starts], 4] == 'case' || code[element[:starts], 2] == 'if'
    elsif element[:type] == 'end'
      break_counter -= 1
    end
  end

  break_code = code
  break_array.reverse_each do |element|
    if element[:type] == 'start'
      break_code.slice!(element[:starts]..element[:ends]-1)
      break_code = code.insert(element[:starts],
                               "-> a#{VM_PREFIX}_line(number#{VM_PREFIX}), a#{VM_PREFIX}_break(fun() -> ")
    elsif element[:type] == 'end'
      break_code = code.insert(element[:starts], " end)")
    end
  end

  return_code = ''
  number = 1
  break_code.each_line do |line|

    # every arrow with prefix for processing gets a line-function
    # for highlighting information
    line.gsub!(regex_arrow_prefix, "-> a#{VM_PREFIX}_line(#{number}), ")

    # insert line number in former inserted line-functions
    line.gsub!(Regexp.new("a#{VM_PREFIX}_line\\(number#{VM_PREFIX}\\)"), "a#{VM_PREFIX}_line(#{number})")

    # find pirate-operations and insert line-number
    line.gsub!(regex_op_prefix, "(#{number}, ")
    # special treatment for functions with zero parameters,
    # delete previously inserted comma
    line.gsub!(/\(\d+,\s+\)/, "(#{number})")

    # add comma with line-number information
    return_code += line.chomp + "  % #{VM_PREFIX}_(#{number}#{VM_PREFIX}_)\n"
    number += 1
  end
  return_code
end

# Takes the code and variables to trace and scans the code line by
# line for arrows with functions and the beforehand marked variables.
# If there is a variable on the left-hand side of an arrow it inserts
# the tracing- (a.k.a. debug-)information; if there are assignments
# to the variable it also inserts tracing-information and slices the
# inserted prefixes out of the code.
def change_prefix_2_debug(code, variables)
  debug_code = code
  variables.each_with_index do |var, index|
    debug_code = debug_code.gsub(Regexp.new("\\b#{var}line#{VM_PREFIX}\\s*=="),
                                 " #{var} ==")
    #
    debug_code = debug_code.gsub(Regexp.new("\\b#{var}line#{VM_PREFIX}\\s*="),
                                 " spawn(fun() -> a#{VM_PREFIX}_performdebugs(#{index}) end)! #{var} =")
    full_debug_code = ''
    debug_code.each_line do |line|
      my_array = scan_for_index_start_and_end(line, Regexp.new("\\b#{var}line#{VM_PREFIX}"))
      my_array.reverse_each do |stuff|
        stop = line.index(regex_stop_or_semicolon, stuff[:ends])
        arrow = line.index(regex_arrow_with_function, stuff[:ends])
        if arrow && stop && arrow > stop
          # do nothing, because variable is not a left-hand side
          # of a function
        elsif arrow
          arrow_end = line.index(')', arrow) + 1
          line = line.insert(arrow_end, ", a#{VM_PREFIX}_performdebugs(#{index}, #{var}) ")
        end
      end
      full_debug_code += line
    end
    debug_code = full_debug_code
  end
  debug_code
end