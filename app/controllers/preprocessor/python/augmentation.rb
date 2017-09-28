# -*- encoding : utf-8 -*-
# This class knows how to augment Python code with the information required to support the
# programming environment's debugging features.
class PythonCodeAugmenter

  def initialize(code, tracing_vars)
    # The code to augment
    @code = code
    # The augmented code
    @augmented_code = nil
    # The variables whose values are to be traced
    @tracing_vars = tracing_vars
    # Our current state. This is updated on every new line we process
    @state = {
      # The current line number in the user's original code. Used to generate line number comments
      :line_number => 0,
      # Whether the current line starts a logical line (Python code is divided into logical lines)
      :ll_start => true,
      # Whether the current line ends a logical line. If it does not, the next line won't start one.
      :ll_end => true,
      # This is either nil (if we're not currently in a string) or the string delimiter (", ', """, ''').
      :in_string => nil,
      # Number of braces that have been opened, but not closed
      :open_braces => 0,
      # Whether the last encountered symbol was a \ which continues the current line to the next. The
      # symbol must have been found outside os a multiline string. This is always reset at the
      # beginning of a new line.
      :line_continuator => false,
      # If the current line starts a logical line, this contains the indentation part of the string,
      # that is, the (possibly empty) prefix that only contains tabs and spaces. If the line does
      # not start a logical line, this is pretty much meaningless.
      :indent_string => ""
    }

    augment_code()
  end

  # Returns the augmented code.
  def to_a()
    return @augmented_code
  end



  ######                                      #    ######  ###
  #     # #    # #####  #      #  ####       # #   #     #  #
  #     # #    # #    # #      # #    #     #   #  #     #  #
  ######  #    # #####  #      # #         #     # ######   #
  #       #    # #    # #      # #         ####### #        #
  #       #    # #    # #      # #    #    #     # #        #
  #        ####  #####  ###### #  ####     #     # #       ###

  # Checks if the given line has a line number comment. If so, returns the line number
  # as a String. Otherwise, returns nil.
  def self.extract_line_number(line)
    match = / # WPLINE_(\d+)$/.match(line)
    if match
      return match[1]
    else
      return nil
    end
  end

  # Returns a version of the line with the line number comment removed.
  def self.remove_line_number(line)
    return line.sub(/ # WPLINE_\d+$/, "")
  end



     #
    # #   #    #  ####  #    # ###### #    # #####   ##   ##### #  ####  #    #
   #   #  #    # #    # ##  ## #      ##   #   #    #  #    #   # #    # ##   #
  #     # #    # #      # ## # #####  # #  #   #   #    #   #   # #    # # #  #
  ####### #    # #  ### #    # #      #  # #   #   ######   #   # #    # #  # #
  #     # #    # #    # #    # #      #   ##   #   #    #   #   # #    # #   ##
  #     #  ####   ####  #    # ###### #    #   #   #    #   #   #  ####  #    #
  private

  # Does the actual work of augmenting the code.
  def augment_code()
    @augmented_code = ""

    # Configure variable tracing (the variables to be traced are declared at the
    # beginning of the program)
    @augmented_code += generate_tracing_configuration()

    # Iterate over the original code's lines
    @code.each_line do |line|
      @state[:line_number] += 1

      # Remove trailing whitespace. If the line is not empty, parse and process it.
      line.rstrip!
      if !line.empty?
        # Update our state based on the current line
        parse_line(line)

        # Add stuff if necessary
        line = possibly_add_line_number_comment(line)
        line = possibly_add_trace_info_calls(line)
      end

      @augmented_code += line + "\n"
    end
  end

  # Generates the necessary library calls to trace variables.
  def generate_tracing_configuration()
    result = ""

    if @tracing_vars
      @tracing_vars.each_with_index do |var_name, var_index|
        result += "garbledwebpiratenlibraryname.add_traced_variable('#{var_name}', #{var_index})\n"
      end
      result += "\n"
    end

    return result
  end

  # Returns a version of the line with a line number comment added, if this line should in
  # fact have the line number comment added. Otherwise, simply returns the line as is. This
  # will also add a line number comment to a comment-only line, but that doesn't hurt.
  def possibly_add_line_number_comment(line)
    if @state[:ll_end]
      return line + " # WPLINE_#{@state[:line_number]}"
    else
      return line
    end
  end

  # The keywords we use to decide that we don't want a line number call before a line
  KEYWORDS_WITHOUT_LINE_NUMBERS = [
    "from", "import",                                           # Import statements
    "def", "class",                                             # Function and class definitions
    "if", "elif", "else", "else:",                              # Conditional control statements
    "try", "try:", "except", "except:", "finally", "finally:"   # Exception handling
  ]

  # Returns a version of the line with calls to the tracing methods prepended. These
  # are calls to send the current line number and values of watched variables back to
  # the server. If this line shouldn't have these calles added, simply returns the line
  # as is.
  def possibly_add_trace_info_calls(line)
    # If the current line does not start a logical line, don't bother
    if not @state[:ll_start]
      return line
    end

    # We will be making decisions based on the first word on the line
    line_words = line.split
    if line_words.empty?
      # The line is empty, so we disregard it
      return line
    end
    first_word = line_words[0]

    # Extract first word and check if it makes us want to refrain from adding a line number call
    if KEYWORDS_WITHOUT_LINE_NUMBERS.include?(first_word)
      # It's one of the statements we don't want
      return line

    elsif ["'", '"', "#"].include?(first_word[0])
      # The line starts with a string or with a comment
      return line
    end

    # Do include a line number call
    return "#{@state[:indent_string]}garbledwebpiratenlibraryname.debug(locals(), globals())\n" +
           "#{@state[:indent_string]}garbledwebpiratenlibraryname.line(#{@state[:line_number]})\n" +
           "#{line}"
  end



  ######
  #     #   ##   #####   ####  # #    #  ####
  #     #  #  #  #    # #      # ##   # #    #
  ######  #    # #    #  ####  # # #  # #
  #       ###### #####       # # #  # # #  ###
  #       #    # #   #  #    # # #   ## #    #
  #       #    # #    #  ####  # #    #  ####

  # Parses the current line and updates our current state. That state is what several of our
  # decisions depends on when it comes to deciding whether or not to add certain debug information.
  #
  # Note: This method expects to be called on a line that has had trailing whitespace removed.
  def parse_line(line)
    # If the previous line didn't and a logical line, we're not going to start one. If it did,
    # we're indeed going to start a new logical line
    @state[:ll_start] = @state[:ll_end]

    # We will start with the assumption that we're going to end the current logical line. We may layer
    # find out that we did not, in fact, do so.
    @state[:ll_end] = true

    # Reset the line continuator flag the the last line may have set to true
    @state[:line_continuator] = false

    # Find the first non-(space/tab) character
    index = 0
    while index < line.length && [" ", "\t"].include?(line[index])
      index += 1
    end
    @state[:indent_string] = line[0...index]

    # Iterate over the line's characters as long as there are any. We use different iteration
    # methods depending on whether we're inside a string or not
    index = 0
    while index < line.length
      if @state[:in_string].nil?
        index = parse_characters_normal(line, index)
      else
        index = parse_characters_in_string(line, index)
      end
    end

    # We have reached the end of the line. Decide whether or not the logical line ends here.
    @state[:ll_end] = @state[:in_string].nil? && @state[:open_braces] == 0 && !@state[:line_continuator]
  end

  # Parses a line starting at the given index under the assumption that the start index is not inside
  # a string. Returns the index of the next character to be parsed. This method parses either until the
  # end of the line is reached (in which case the returned index is equal to the line's length) or until
  # it runs inside a string (in which case the returned index points to the string's first character).
  #
  # Note: This method expects to be called on a line that has had trailing whitespace removed.
  def parse_characters_normal(line, start_index)
    index = start_index

    while index < line.length
      if [ "(", "{", "[" ].include?(line[index])
        # Opening brace
        @state[:open_braces] += 1
        index += 1

      elsif [ ")", "}", "]" ].include?(line[index])
        # Closing brace
        @state[:open_braces] -= 1
        index += 1

      elsif line[index] == "#"
        # We have found the start of a comment, so ignore the rest of the line
        index = line.length

      elsif line[index] == "\\" && index == line.length - 1
        # We have found a backslash as the last character -> line continuation
        @state[:line_continuator] = true
        index += 1

      elsif line[index] == '"""'
        # We have found a triple-quoted string. Advance index and leave
        @state[:in_string] = '"""'
        index += 3
        break

      elsif line[index] == "'''"
        # We have found a triple-quoted string. Advance index and leave
        @state[:in_string] = "'''"
        index += 3
        break

      elsif line[index] == '"'
        # We have found a single-quoted string. Advance index and leave
        @state[:in_string] = '"'
        index += 1
        break

      elsif line[index] == "'"
        # We have found a single-quoted string. Advance index and leave
        @state[:in_string] = "'"
        index += 1
        break

      else
        # Advance a character
        index += 1
      end
    end

    return index
  end

  # Parses a line starting at the given index under the assumption that the start index is inside a
  # string. Returns the index of the next character to be parsed. This method parses either until the end
  # of the line is reached (in which case the returned index is equal to the line's length) or until it
  # finds the end of the string (in which case the returned index points to the character after the
  # string's closing delimiter(s)).
  #
  # We almost completely disregard escape characters, except for when they precede the current string
  # delimiter. This is because all other escapes are of no concern to us. We don't even need to check
  # whether a single-quoted string ends with an escape character to continue at the next line, because
  # if the escape character weren't there, the code wouldn't have passed syntax validation in the first
  # place.
  #
  # Note: This method expects to be called on a line that has had trailing whitespace removed.
  def parse_characters_in_string(line, start_index)
    index = start_index

    # The delimiter that would end the current string
    delim = @state[:in_string]

    while index < line.length
      if line[index] == delim[0]
        # We have encountered the string's delimiter character. Check whether the user has escaped it
        if index > 0 && line[index - 1] == "\\"
          # It was escaped, so simply advance one character
          index += 1

        else
          # It was not escaped. Check whether we have as many delimiting characters as we need
          if line[index...index + delim.length] == delim
            # We have indeed reached the end of the string
            @state[:in_string] = nil
            index += delim.length
            break

          else
            # We have not reached the end of the string yet. Advance to the next character
            index += 1
          end
        end

      else
        # Advance a character
        index += 1
      end
    end

    return index
  end
end
