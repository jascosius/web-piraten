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
      :line_number => 0
    }

    augment_code()
  end

  # Returns the augmented code.
  def to_a()
    return @augmented_code
  end


  private


     #
    # #   #    #  ####  #    # ###### #    # #####   ##   ##### #  ####  #    #
   #   #  #    # #    # ##  ## #      ##   #   #    #  #    #   # #    # ##   #
  #     # #    # #      # ## # #####  # #  #   #   #    #   #   # #    # # #  #
  ####### #    # #  ### #    # #      #  # #   #   ######   #   # #    # #  # #
  #     # #    # #    # #    # #      #   ##   #   #    #   #   # #    # #   ##
  #     #  ####   ####  #    # ###### #    #   #   #    #   #   #  ####  #    #

  # Does the actual work of augmenting the code.
  def augment_code()
    @augmented_code = ""

    # Iterate over the original code's lines
    @code.each_line do |line|
      # Remove trailing whitespace
      line.rstrip!

      # Update our state based on the current line
      parse_line(line)

      # Append the line number as a comment
      if should_append_line_number?()
        line += " # WPLINE_#{@state[:line_number]}"
      end

      # Prepend debugging commands
      if should_prepend_debugging_commands?()

      end

      @augmented_code += line + "\n"
    end
  end



  #     #
  #     # ##### # #      # ##### # ######  ####
  #     #   #   # #      #   #   # #      #
  #     #   #   # #      #   #   # #####   ####
  #     #   #   # #      #   #   # #           #
  #     #   #   # #      #   #   # #      #    #
   #####    #   # ###### #   #   # ######  ####

  # Parses the current line and updates our current state. That state is what several of our
  # decisions depends on when it comes to deciding whether or not to add certain debug information.
  def parse_line(line)
    stripped_line = line.strip

    @state[:line_number] += 1
    @state[:line_empty] = stripped_line.empty?
    @state[:line_is_comment] = stripped_line[0] == "#"

    # TODO Implement. Right now, there's no line we ignore
  end

  # Returns whether or not line number information should be appended to the current line in the
  # form of a comment.
  def should_append_line_number?()
    # TODO Be more intelligent
    return !@state[:line_empty] && !@state[:line_is_comment]
  end

  # Returns whether or not debugging statements should be inserted before the current line.
  def should_prepend_debugging_commands?()
    # TODO Implement based on current state
    return true
  end

end