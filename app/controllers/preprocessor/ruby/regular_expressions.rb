# Regular expressions for easier handling and processing of the code.

# Regularexpression for validating case-blocks in the code.
def regex_verify_case_statement
  /(?:.*;)?\s*case\s*(?:#+.*\s*)*(?:\s:*\w+\s|'(?:[^']?(?:\\')?)*'|"(?:[^"]?(?:\\")?)*")\s*(?:#+.*\s*)*\s*when\s*(?:#+.*\s*)*(?:\s:*\w+\s|'(?:[^']?(?:\\')?)*'|"(?:[^"]?(?:\\")?)*")/
end

# Regular expression that verifies the existence of a multiline string in
# the code.
def regex_verify_multiline_string
  /(?:'(?:[^']?(?:\\')?)*(?:\n|\r|\r\n)(?:[^']?(?:\\')?)*'|"(?:[^"]?(?:\\")?)*(?:\n|\r|\r\n)(?:[^"]?(?:\\")?)*")/
end

# Verifies if the given line has a complete case statement from the case to
# the first when.
def regex_verify_case_when
  /case\s*(?:\s:*\w+\s|'(?:[^']?(?:\\')?)*'|"(?:[^"]?(?:\\")?)*")\s*when/
end

# In case there is a when statement after a multiline string and to
# prevent that there is a when inside the string mistaken as one of
# a case statement.
def regex_verify_when_after_multiline
  /(?:(?:[^']?(?:\\')?)*when(?:[^']?(?:\\')?)*[^\\]'|(?:[^"]?(?:\\")?)*when(?:[^"]?(?:\\")?)*[^\\]?"|(?:[^']?(?:\\')?)*[^\\]'|(?:[^"]?(?:\\")?)*[^\\]");?\s*when/
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
  /when\s*(?:\s:*\w+\s|'(?:[^']?(?:\\')?)*'|"(?:[^"]?(?:\\")?)*")\s*(?:#.*)?$/
end