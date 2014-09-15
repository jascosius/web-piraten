require 'preprocessor/ruby/regular_expressions'


# Processing of the code if there are multiline strings and no other structures
# that need a special treatment. It's done through matching the given code lines
# to specific patterns that determine the beginnings and ends of multiline
# strings.
def multiline_processing(s, booleans)
  single_q = booleans[:single_q]
  double_q = booleans[:double_q]
  dont_skip_line = booleans[:dont_skip_line]
  if single_q && !double_q
    if s =~ regex_multln_sngl_q_end_appnd
      single_q = false
      if s =~ regex_multln_sngl_end_dbl_strt
        double_q = true
      elsif s =~ regex_multln_sngl_end_and_strt
        single_q = true
      else
        dont_skip_line = true
      end
    elsif s =~ regex_multiline_single_q_end
      single_q = false
      dont_skip_line = true
    end
  elsif double_q && !single_q
    if s =~ regex_multln_dbl_end_appnd
      double_q = false
      if s =~ regex_multln_dbl_end_dbl_strt
        double_q = true
      elsif s =~ regex_multln_dbl_end_sngl_strt
        single_q = true
      else
        dont_skip_line = true
      end
    elsif s=~ regex_multiline_double_q_end
      double_q = false
      dont_skip_line = true
    end
  elsif s =~ regex_start_multiline_double_q
    double_q = true
    dont_skip_line = false
  elsif s =~ regex_start_multiline_single_q
    single_q = true
    dont_skip_line = false
  end
  booleans[:single_q] = single_q
  booleans[:double_q] = double_q
  booleans[:dont_skip_line] = dont_skip_line
end


# Method that processes the code, if there is a case block in the user's code.
# The code is processed line by line and the further insertion of information is
# controlled by booleans and verifying regular expressions.
def case_block_processing(s, bools)
  if bools[:multiline_string] # processes a found multiline string
    bool = multiline_processing(s, bools)
    bool.each_key do |key|
      bools[key] = bool[key]
    end
    if bools[:dont_skip_line] && s =~ regex_verify_when_line_end
      bools[:multiline_string] = false
      bools[:found_case] = true
      bools[:found_when] = true
      bools[:dont_skip_line] = false
    elsif bools[:dont_skip_line] && s =~ regex_case_after_multiline
      bools[:multiline_string] = false
      bools[:found_case] = true
      bools[:dont_skip_line] = false
    elsif bools[:dont_skip_line]
      # if multiline string ends change back to standard processing for 'normal' code lines
      bools[:multiline_string] = false
    end
  elsif !bools[:found_case] && !bools[:found_when] # Standard setting for processing of
    # 'normal' code unless other flags are set.
    if s =~ regex_verify_case_when
      if s =~ regex_verify_when_sq
        bools[:single_q] = true
        bools[:dont_skip_line] = false
        bools[:found_case] = true
        bools[:found_when] = true
      elsif s =~ regex_verify_when_dq
        bools[:double_q] = true
        bools[:dont_skip_line] = false
        bools[:found_case] = true
        bools[:found_when] = true
      elsif s =~ regex_verify_when_complete

      else
        bools[:found_case] = true
        bools[:found_when] = true
        bools[:dont_skip_line] = false
      end
    elsif s =~ regex_case
      bools[:dont_skip_line] = false
      bools[:found_case] = true
      if s =~ regex_case_with_sngl_q_multln
        bools[:single_q] = true
        bools[:no_multiline_case_statement] = false
      elsif s =~ regex_case_with_dbl_q_multln
        bools[:double_q] = true
        bools[:no_multiline_case_statement] = false
      end
    elsif s =~ regex_normal_multiline_start
      bools = multiline_processing(s, bools)
      bools[:multiline_string] = true
    elsif s =~ regex_verify_when_in_single_q
      # Prevents the word 'when' in single-line strings to be mistaken for a when
      # of a case statement.

    elsif s =~ regex_verify_when_in_double_q

    elsif s =~ regex_when
      bools[:found_case] = true
      bools[:found_when] = true
      bools[:dont_skip_line] = false
      if s =~ regex_verify_when_line_end
      elsif s =~ regex_verify_when_sq
        bools[:single_q] = true
      elsif s =~ regex_verify_when_dq
        bools[:double_q] = true
      elsif s =~ regex_verify_when_complete
        bools[:found_case] = false
        bools[:found_when] = false
        bools[:dont_skip_line] = true
      end
    end
  elsif bools[:found_case] && !bools[:found_when] # beginning of a case statement has been found
    if !bools[:no_multiline_case_statement] # found a multiline string for statement
      if bools[:single_q] && s =~ regex_single_q_end
        bools[:single_q] = false
        bools[:no_multiline_case_statement] = true
        if s =~ regex_verify_when_line_end
          bools[:found_when] = true
        elsif s =~ regex_verify_when_sq
          bools[:single_q] = true
          bools[:found_when] = true
        elsif s =~ regex_verify_when_dq
          bools[:double_q] = true
          bools[:found_when] = true
        elsif s =~ regex_verify_when_complete
          bools[:dont_skip_line] = true
          bools[:found_case] = false
        end
      elsif bools[:double_q] && s =~ regex_double_q_end
        bools[:double_q] = false
        bools[:no_multiline_case_statement] = true
        if s =~ regex_verify_when_line_end
          bools[:found_when] = true
        elsif s=~ regex_verify_when_sq
          bools[:single_q] = true
          bools[:found_when] = true
        elsif s =~ regex_verify_when_dq
          bools[:double_q] = true
          bools[:found_when] = true
        elsif s =~ regex_verify_when_complete
          bools[:dont_skip_line] = true
          bools[:found_case] = false
        end
      end
    elsif s =~ regex_case_end_complete_when
      # found rest of case statement and the following when
      if s =~ regex_verify_when_line_end
        bools[:found_when] = true
      elsif s=~ regex_verify_when_sq
        bools[:single_q] = true
        bools[:found_when] = true
      elsif s =~ regex_verify_when_dq
        bools[:double_q] = true
        bools[:found_when] = true
      elsif s =~ regex_verify_when_complete
        bools[:dont_skip_line] = true
        bools[:found_case] = false
      end
    elsif s =~ regex_single_q_multiline_start # verifies beginning of case multiline string, single quoted
      bools[:single_q] = true
      bools[:no_multiline_case_statement] = false
    elsif s =~ regex_double_q_multiline_start # verifies beginning of case multiline string, double quoted
      bools[:double_q] = true
      bools[:no_multiline_case_statement] = false
    elsif s =~ regex_when
      bools[:found_when] = true
    end
  elsif bools[:found_case] && bools[:found_when]
    if bools[:single_q] && s =~ regex_single_q_end
      # verifies end of multiline string, single quotes
      bools[:found_case] = false
      bools[:found_when] = false
      bools[:single_q] = false
      bools[:dont_skip_line] = true
    elsif bools[:double_q] && s =~ regex_double_q_end
      # verifies end of multiline string, double quotes
      bools[:found_case] = false
      bools[:found_when] = false
      bools[:double_q] = false
      bools[:dont_skip_line] = true
    elsif bools[:double_q] || bools[:single_q]

    elsif s=~ regex_verify_complete_when
      # verifies end of first when statement
      bools[:found_case] = false
      bools[:found_when] = false
      bools[:dont_skip_line] = true
    elsif s =~ regex_single_q_multiline_start # verifies multiline string, single quotes
      bools[:single_q] = true
    elsif s =~ regex_double_q_multiline_start # verifies multiline string, double quotes
      bools[:double_q] = true
    end
  else # Just in case something goes wrong process the code without inserting
    # any information.
    bools[:dont_skip_line] = false
  end

  bools
end
