# Regular expressions for easier handling and processing of the code.

####################################################################
#            Regular expressions for handling decisions            #
####################################################################

# Regularexpression for validating case-blocks in the code.
def regex_verify_case_statement
  /(?:.*;)?\s*case\s*(?:#+.*\s*)*(?:\s:*\w+(?:\(:?.*\))?\s|'(?:[^']?(?:\\')?)*'|"(?:[^"]?(?:\\")?)*")\s*(?:#+.*\s*)*\s*when\s*(?:#+.*\s*)*(?:\s:*\w+(?:\(:?.*\))?\s|'(?:[^']?(?:\\')?)*'|"(?:[^"]?(?:\\")?)*")/
end

# Regular expression that verifies the existence of a multiline string in
# the code.
def regex_verify_multiline_string
  /(?:'(?:[^']?(?:\\')?)*(?:\n|\r|\r\n)(?:[^']?(?:\\')?)*'|"(?:[^"]?(?:\\")?)*(?:\n|\r|\r\n)(?:[^"]?(?:\\")?)*")/
end

####################################################################
#           Regular expressions for multiline processing           #
####################################################################

# checks for start of multilinestring with double quotes
def regex_start_multiline_double_q
  /^(?:[^"']?|"(?:[^"]|(?:\\"))*"|'(?:[^']|(?:\\'))*')*"(?:[^"]|(?:\\"))*$/
end

# checks for start of multilinestring with single quotes
def regex_start_multiline_single_q
  /^(?:[^"']?|"(?:[^"]|(?:\\"))*"|'(?:[^']|(?:\\'))*')*'(?:[^']|(?:\\'))*$/
end

# validates end of multiline string with single quotes and start of
# a multiline string with double quotes
def regex_multln_sngl_end_dbl_strt
  /^(?:[^']|(?:\\'))*'(?:[^"']?|"(?:[^"]|(?:\\"))*"|'(?:[^']|(?:\\'))*')*"(?:[^"]|(?:\\"))*$/
end

# validates end of multiline string with single quotes and start of
# a multiline string with single quotes
def regex_multln_sngl_end_and_strt
  /^(?:[^']|(?:\\'))*'(?:[^"']?|"(?:[^"]|(?:\\"))*"|'(?:[^']|(?:\\'))*')*'(?:[^']|(?:\\'))*$/
end

# validates end of a single quoted multiline string with following
# method, that indicates further statements in the same line
def regex_multln_sngl_q_end_appnd
  /^(?:[^']?(?:\\')?)*[^\\]?'\s*(?:;|\+|<<)/
end

# validates end of a single quoted multiline string
def regex_multiline_single_q_end
  /^(?:[^']*(?:\\')?)*'\s*(?:#.*)?/
end

# validates end of multiline string with double quotes and start of
# a multiline string with double quotes
def regex_multln_dbl_end_dbl_strt
  /^(?:[^"]|(?:\\"))*"(?:[^"']?|"(?:[^"]|(?:\\"))*"|'(?:[^']|(?:\\'))*')*"(?:[^"]|(?:\\"))*$/
end

# validates end of multiline string with double quotes and start of
# a multiline string with single quotes
def regex_multln_dbl_end_sngl_strt
  /^(?:[^"]|(?:\\"))*"(?:[^"']?|"(?:[^"]|(?:\\"))*"|'(?:[^']|(?:\\'))*')*'(?:[^']|(?:\\'))*$/
end

# validates end of a double quoted multiline string with following
# method, that indicates further statements in the same line
def regex_multln_dbl_end_appnd
  /^(?:[^"]?(?:\\")?)*[^\\]?"\s*(?:;|\+|<<)/
end

# validates end of a double quoted multiline string
def regex_multiline_double_q_end
  /^(?:[^"]?(?:\\")?)*"\s*(?:#.*)?/
end

###################################################################
#          Regular expressions for case-block processing          #
###################################################################

# Verifies if the given line has a complete case statement from the case to
# the first when.
def regex_verify_case_when
  /case\s*(?:\s:*\w+(?:\(:?.*\))?\s|'(?:[^']?(?:\\')?)*'|"(?:[^"]?(?:\\")?)*")\s*when/
end

# In case there is a case after a multiline string that is not closed through
# a complete when statement.
def regex_case_after_multiline
  /case\s*(?:\s:*\w+(?:\(:?.*\))?(?:\(:?.*\))?\s|'(?:[^']?(?:\\')?)*'|"(?:[^"]?(?:\\")?)*")?\s*(?:#.*)?$/
end

# Verifies start of multiline string with single quotation marks.
def regex_verify_when_sq
  /when\s*(?:'(?:[^']?(?:\\')?)*)$/
end

# Verifies start of multiline string with double quotation marks.
def regex_verify_when_dq
  /when\s*(?:"(?:[^"]?(?:\\")?)*)$/
end

# Verifies that the when has a complete statement.
def regex_verify_when_complete
  /when\s*(?:\s:*\w+(?:\(:?.*\))?\s|'(?:[^']?(?:\\')?)*'|"(?:[^"]?(?:\\")?)*")\s*(?:#.*)?$/
end

# Verfies when at line end
def regex_verify_when_line_end
  /when\s*(?:#+.*)?$/
end

# Verfies word case
def regex_case
  /;?\s*case\s*/
end

# Verfies case with single quoted multiline string for statement
def regex_case_with_sngl_q_multln
  /case\s*(?:'(?:[^']?(?:\\')?)*)$/
end

# Verfies case with double quoted multiline string for statement.
def regex_case_with_dbl_q_multln
  /case\s*(?:"(?:[^"]?(?:\\")?)*)$/
end

# Verfies beginning of a multiline string.
def regex_normal_multiline_start
  /;?\s*\w*[!\?]*\s*=\s*(?:"(?:[^"]*(?:\\")?)*|'(?:[^']*(?:\\')?)*)$/
end

# Verifies the word when is inside a single quoted string.
def regex_verify_when_in_single_q
  /'.*when.*'/
end

# Verfies the word when is inside a double quoted string.
def regex_verify_when_in_double_q
  /".*when.*"/
end

def regex_when
  /when/
end

# Verfies end of a single quoted multiline string.
def regex_single_q_end
  /^(?:[^']?(?:\\')?)*[^\\]?'/
end

# Verfies end of a double quoted multiline string.
def regex_double_q_end
  /^(?:[^"]?(?:\\")?)*[^\\]?"/
end

# Verfies that an argument for case is given and the following when
# is found.
def regex_case_end_complete_when
  /\s*(?::*\w+(?:\(:?.*\))?\s|'(?:[^']?(?:\\')?)*'|"(?:[^"]?(?:\\")?)*")?\s*when/
end

# Verfies the beginning of a single quoted multiline string.
def regex_single_q_multiline_start
  /(?:'(?:[^']?(?:\\')?)*)$/
end

# Verfies the beginning of a double quoted multiline string.
def regex_double_q_multiline_start
  /(?:"(?:[^"]?(?:\\")?)*)$/
end

# Verfies if the statement of the when is given and complete.
def regex_verify_complete_when
  /(?:\s:*\w+(?:\(:?.*\))?\s|'(?:[^']?(?:\\')?)*'|"(?:[^"]?(?:\\")?)*")\s*(?:#.*)?/
end